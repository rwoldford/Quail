;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               ref.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1991, 1993.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          ref
;;;          ref-kernel
;;;          ref-list
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(ref ref-instantiate ref-kernel ind)))

;--------------------------------------------------------------------------------

;;;
;  Utility functions for ref methods
;

;
; ref-true-args
;   allows objects (esp. with dimensions-rank < 2) to be ref'd with extraneous args,
;   if the extraneous args are some sensible use of 0, '(0), T, or '(:C).
;   If the object is direct-ref and 1-dimensional

(defvar *true-args-maskable* (list 0 '(0) T '(:c)))

(defun ref-err (r args)
  (quail-error "Improper arguments ~S provided to REF for object ~S ~
                        with dimensions ~S."
                       args
                       r
                       (dimensions-of r)))

;; this method is only called when dim-rank-r < dim-rank-args.
;; If dim-rank-r = 1, then must have dim-rank-args = 2  (column vs row discrimination)

(defun ref-true-args (r args dim-rank-r dim-rank-args)
  (declare (special *true-args-maskable*))
  (let ((true-args (cond ((and (= dim-rank-args 2)
                               (= dim-rank-r 1))
                          (if (member (second args) *true-args-maskable* :test #'equal)
                            (list (first args))
                            :err))
                         
                         ((= dim-rank-r 0)
                          (if (find-if-not 
                               #'(lambda (arg)
                                   (member arg *true-args-maskable* :test #'equal))
                               args)
                            :err))
                         
                         (t :err))))
    (if (eq :err true-args)
      (ref-err r args))
    true-args))
                     
;;;
;  DEFGENERIC of ref
;

#|
;; I thought this was too obscure too leave on the doc string for now ... dga 93 03

  "ref is called as if it had lambda list (r &rest specifiers &key shape), ~
  except that the keys do not get included in specifiers, and specifiers do not ~
  get processed as keys (in other words, this works as it intuitively would,  ~
  whereas the Common Lisp specification does something quite different ... CLtL p61). ~
 ~
  Returns an object with same class as r  ~
  which is built according to specifiers.  All formats for specifiers ~
  have not yet (89 12 01) been determined. Proposed classes for r include ~
  sequences, arrays, ref-arrays (including foreign-arrays). ~
  All such classes must have eref and  ~
  dimensions-of methods."
|#

(defgeneric ref (r &rest args)
  (:documentation
   "Returns a reference to the object r.  Dimensions and contents are ~
    determined by the remaining arguments. ~
    If a copy is desired instead, use sel.
    See examples file for usage. ~
    (:see-also sel eref row-major-ops) ~
    "
   )
  )

;--------------------------------------------------------------------------------

;;;
;  Kernel functions required by the ref defmethods.
;

(defgeneric ref-instantiate (r)
  (:documentation
   "Makes an instance of some subclass of ref-object, to eventually be returned ~
    by ref.  Which class is created depends on the class of r."
  ))

(defmethod ref-instantiate ((self dimensioned-ref-object))
  (make-instance (class-of self)   
                 :proto self))

#|
;--------------------------------------------------------------------------
; Should probably change ref-instantiate for dimensioned-ref-object
; to calculate return-class (not automatically use class of 
; referenced object).
;--------------------------------------------------------------------------
(defmethod ref-instantiate ((self dimensioned-ref-object))
  (make-instance (get-return-class self self)   ;; semantics not quite right
                 :proto self))

|#

(defmethod ref-instantiate ((self number))
  self)

(defmethod ref-instantiate ((self symbol))
  self)

(defmethod ref-instantiate ((self sequence))
  (make-dimensioned-ref-object))

;;;  array also handles the string case

(defmethod ref-instantiate ((self array))
  (make-dimensioned-ref-object))

;--------------------------------------------------------------------------

(defgeneric ref-kernel (ref-r r args)
  (:documentation
   "Does the guts of the ref computations.  ref-r is an instance which was ~
    created by calling ref-instantiate on r.")
  )

(defmethod ref-kernel ((ref-r t) r args)
  (missing-method 'ref-kernel ref-r r args))

(defmethod ref-kernel ((ref-r dimensioned-ref-object) r args)
  (multiple-value-bind (specifiers shape)
                       (interpret args '(:shape))
    (if (margin-specs-p (first specifiers))
      (ref-by-margin ref-r r specifiers shape)
      (ref-by-indices ref-r r specifiers shape)))
  ref-r)

(defun ref-by-margin (ref-r r specifiers shape)
  (if (direct-ref-p r)
    ;;
    ;; simplest case;  just stuff the info into an indirect
    ;; ref-object.
    ;;
    (let* ((specs (expand-margin-specs r specifiers))
           (dim (compute-margin-dim (dimensions-of r) specs)))
      (finish-ref ref-r r dim shape specs))                          ; --> margin
    ;;
    ;; Here r is an indirect-ref, so we want to figure out how
    ;; to configure a new indirect-ref which refers to the same
    ;; direct ref-obj as r, namely, (ref-obj-of r).
    ;;
    (let* ((r-specs (specs-of r))
           (r-mask (specs-mask-of r)))           
      (if (margin-specs-p r-specs)
        ;;
        ;; Here r is a margin-ref, and so will be our
        ;; returned ref-object.  Its ref-obj will be (ref-obj-of r).
        ;; Hence we need to refer back to the dimensions of (ref-obj-of r),
        ;; using the (specs-mask-of r) to lengthen the expanded specs
        ;; to (length (dimensions-of (ref-obj-of r))).  Then do 
        ;; margin-of-margin, which does not require any knowledge of 
        ;; dimensions.  In fact, we cannot calculate the final dimensions
        ;; until we have the result of margin-of-margin !!
        ;;
        (let* ((temp-specs (expand-margin-specs r specifiers))                  
               (specs (margin-of-margin temp-specs r))
               (dim (compute-margin-dim (dimensions-of (ref-obj-of r)) specs)))
          (finish-ref ref-r (ref-obj-of r) dim shape specs r-mask))  ; --> margin
        ;;
        ;; Otherwise r is an indices-ref, and so will be 
        ;; our returned ref-object. Its ref-object will be (ref-obj-of r).
        ;; Here we are really just doing a margin sel on r-specs; so, we use
        ;; (dimensions-of r) in compute-margin-dim.
        ;;
        (let* ((temp-specs (expand-margin-specs r specifiers))                  
               (specs (margin-of-indices temp-specs r))
               (dim (array-dimensions specs)))
          (finish-ref ref-r (ref-obj-of r) dim shape specs))))))     ; --> indices

(defun ref-by-indices (ref-r r specifiers shape)
  (if (direct-ref-p r)         
    ;;
    ;; simplest case;  just stuff the info into an indirect
    ;; ref-object.
    ;;
    (let* ((specs (indices-array (first specifiers)))
           (dim (array-dimensions specs)))
      (finish-ref ref-r r dim shape specs))                          ; --> indices
    ;;
    ;; Here r is an indirect-ref, so we want to figure out how
    ;; to configure a new indirect-ref which refers to the same
    ;; direct ref-obj as r, namely, (ref-obj-of r).
    ;;
    (let* ((r-specs (specs-of r))
           (temp-specs (indices-array (first specifiers)))
           (dim (array-dimensions temp-specs))
           (specs (if (margin-specs-p r-specs)
                    (indices-of-margin temp-specs r)
                    (indices-of-indices temp-specs r))))
      (finish-ref ref-r (ref-obj-of r) dim shape specs))))           ; --> indices

(defun finish-ref (ref-r ref-obj dim shape specs &optional (r-mask nil))
  (let* ((mask (get-mask dim shape r-mask))
         (dim (list-if-t dim mask)))
    #+:debug-ref
    (progn
      (format *quail-debug-io* "~&dimensions ~S" (setf (dimensions-of ref-r) dim))
      (format *quail-debug-io* "~&specs-mask ~S" (setf (specs-mask-of ref-r) mask))
      (format *quail-debug-io* "~&ref-obj ~S" (setf (ref-obj-of ref-r) ref-obj))
      (format *quail-debug-io* "~&specs ~S" (setf (specs-of ref-r) specs))
      (describe ref-r))
    (setf (dimensions-of ref-r) dim)
    (setf (specs-mask-of ref-r) mask)
    (setf (ref-obj-of ref-r) ref-obj)
    (setf (specs-of ref-r) specs))
  ref-r)                              ; useful when debugging to return this

;--------------------------------------------------------------------------------

;;;
;  Utility functions required by ref
;  

;          
;  margin-of-margin ... 
;

(defun margin-of-margin (new-specs r)
  (loop for margin-old in (specs-of r)
        as  margin-new in new-specs
        collect
            (cond ((eq margin-old t)
                     margin-new)
                  ((eq margin-new t)
                     margin-old)
                  ((eq :c (first margin-old))
                     (if (eq :c (first margin-new))
                              (complement-complement margin-old margin-new)
                              (complement-index margin-old margin-new)))
                  ((eq :c (first margin-new))
                     (index-complement margin-old margin-new))
                  (t (index-index margin-old margin-new)))))

(defun index-index (old-list new-list)
  (loop for i in new-list
        collect (elt old-list i)))

(defun index-complement (old-list new-list)
  (let ((return-list '()))
    (loop for comp-i in (cdr new-list)
          with index = 0
          do
          (setf return-list
                (append return-list
                        (subseq old-list index comp-i)))
          (setf index (+ comp-i 1))
          finally
          (setf return-list
                (append return-list (subseq old-list index))))
    return-list))

(defun complement-index (old-list new)
  (let* ((new-list (copy-list new)))
    (loop for index in new-list
          with comp-list = (cdr old-list)
          collect
          (or
           (loop for comp-i in comp-list
                 do
                 (cond
                  ((< index comp-i) (return index))
                  (T (incf index))))
           index))))
     
(defun complement-complement (old new)
  (let* ((old-list (copy-list old))
         (new-list (copy-list new))
         (additional-comps
          (loop for new-comp-i in (cdr new-list)
                with old-comps = (cdr old-list)
                collect
                (or (loop for old-comp-i in old-comps
                          do (cond
                              ((< new-comp-i old-comp-i) (return new-comp-i))
                              (T (incf new-comp-i))))
                    new-comp-i))))
    (append '(:c) (merge 'list
                         (cdr old-list)   
                         additional-comps
                         #'<))))

;
;  indices-of-margin
;

(defun indices-of-margin (new-specs r)
  ;; destroys new-specs !!
  (let ((old-specs (specs-of r))
        (old-mask (specs-mask-of r)))
    (loop for i
          from 0
          to (- (number-of-elements new-specs) 1)
          do (column-major-eset new-specs
                                i
                                (eref-transform-of-margin 
                                 (expand-list 
                                  (eref-true-index r
                                                   (column-major-eref new-specs i))
                                  old-mask
                                  0)
                                 old-specs))))
  new-specs)
                        
;
;  margin-of-indices
;

#|
(defun margin-of-indices (new-specs r)
  ;; a little computationally expensive, but easy.
  (apply #'sel (specs-of r) new-specs))
|#

(defun margin-of-indices (new-specs r)
  ;; conceptually (apply #'sel (specs-of r) new-specs)
  (let* ((old-specs (specs-of r))
         (old-dim (array-dimensions old-specs))
         (dim (compute-margin-dim old-dim new-specs))
         (specs (make-array dim)))
    (copy-kernel old-specs
                 specs
                 new-specs
                 (make-list (length old-dim) :initial-element t))
    specs))

;
;  indices-of-indices
;

(defun indices-of-indices (new-specs r)
  ;; destroys new-specs !!
  (let ((old-specs (specs-of r))
        (old-mask (specs-mask-of r)))
    (loop for i
          from 0
          to (- (number-of-elements new-specs) 1)
          do (column-major-eset new-specs
                                i
                                (eref-transform-of-indices 
                                 (expand-list 
                                  (eref-true-index r
                                                   (column-major-eref new-specs i))
                                  old-mask
                                  0)
                                 old-specs))))
  new-specs)

;
;  expand-margin-specs does the following to specs:
;
;     1.  if specs are longer than (dimensions-of r),
;           appends '(t) to the end of specs enough times so that the length of
;         specs is the number of dimensions of r
;         OR  uses  
;         OR  uses ref-true-args to reduce the length of specs to number of dims of r
#|  This has been removed to 'fix' a bug ... dga & rwo 91 11 29
;     2.  When r is indirect-ref-p and a ref-margin, all occurrences of t are 
;         changed to the corresponding element of the (specs-of r) reduced by
;         by taking only those for which (specs-mask-of r) are t.
|#
;     3.  if an element of specs is a single number n, it gets changed to (list n).
;     4.  if an element of specs is a list beginning with :c, the rest of the list
;         is sorted into ascending order.
;

(defun expand-margin-specs (r specs)
  (let* ((dim (dimensions-of r))
         (len-dim (length dim))
         (len-spec (length specs))
         (indirect-ref-p (indirect-ref-p r)))
    (if (> len-spec len-dim)
      (let ((true-specs (ref-true-args r specs len-dim len-spec)))
        (if (> (length true-specs) len-dim)
          (ref-err r specs)
          (setf specs true-specs)))
      (if (< len-spec len-dim)
        (setf specs (pad-list specs len-dim t))))
    (setf len-spec (length specs))
    (loop for i
          from 0
          to (- len-spec 1)
          do (let ((spec (elt specs i)))
               (setf (elt specs i) 
                     (cond ((listp spec)
                            (if (eq :c (first spec))
                              (append '(:c) 
                                      (remove-duplicates (sort (rest spec)
                                                               #'<)))
                              spec))
                           ((eq spec t)
                            t)
                           ((integerp spec) (list spec))
                           (t (quail-error "Invalid specifier for dimension ~S: ~S."
                                           i
                                           spec))))))
    (if (and indirect-ref-p (= len-spec len-dim))
      (setf specs (expand-list specs (specs-mask-of r) t)))
    specs))

#|
;; a working version which distinguished between rows and columns on margin refs  ...
;; too confusing due to incompatibilities with other cases

(defun expand-margin-specs (r specs)
  (let* ((dim (dimensions-of r))
         (len-dim (length dim))
         (len-spec (length specs))
         (indirect-ref-p (indirect-ref-p r))
         r-specs)
    (if (> len-spec len-dim)
      (if (and indirect-ref-p
               (margin-specs-p (setf r-specs (specs-of r)))
               (= (length specs) (length r-specs)))
        ;; then these are specs recognizing the shape of the ref-obj,
        ;; so try to use them
        nil
        ;; else there's no masking, maybe can do something.
        (let ((true-specs (ref-true-args r specs len-dim len-spec)))
          (if (> (length true-specs) len-dim)
            (ref-err r specs)
            (setf specs true-specs)))
        )
      ;; else try <
      (if (< len-spec len-dim)
        (setf specs (pad-list specs len-dim t))))
    (setf len-spec (length specs))
    (loop for i
          from 0
          to (- len-spec 1)
          do (let ((spec (elt specs i)))
               (setf (elt specs i) 
                     (cond ((listp spec)
                            (if (eq :c (first spec))
                              (append '(:c) 
                                      (remove-duplicates (sort (rest spec)
                                                               #'<)))
                              spec))
                           ((eq spec t)
                            t)
                           ((integerp spec) (list spec))
                           (t (quail-error "Invalid specifier for dimension ~S: ~S."
                                           i
                                           spec))))))
    (if (and indirect-ref-p (= len-spec len-dim))
      (setf specs (expand-list specs (specs-mask-of r) t)))
    specs))
|#

;
;  compute-margin-dim
;

(defun compute-margin-dim (total-dim specs)
  (loop for s in specs
        as  d in total-dim
        collect (cond ((eq s t) d)
                      ((eq (first s) :c) (- d (length (rest s))))
                      (t (length s)))))

;
;  indices-array
;

(defgeneric indices-array (indices &key instance elements))

(defun indices-array-kernel (indices instance elements)
  (let ((needs-consing-p 
         (not (consp (column-major-eref indices 0)))))                           
    (if (or instance elements needs-consing-p)
      (let* ((dim (array-dimensions indices))
             (targ (if instance (make-array dim) indices))
             (new-value-fcn (if needs-consing-p
                              #'list
                              (if elements #'copy-list #'identity))))
        (loop for i from 0 to (1- (number-of-elements indices))
              do (column-major-eset targ i (funcall new-value-fcn
                                                    (column-major-eref indices i))))
        targ)
      indices)))

;; this method is useful if the user calls indices-array directly

(defmethod indices-array ((indices list) &key (instance t) (elements nil))
  ;; we get a new instance regardless
  (declare (ignore instance))
  (if (not (consp (first indices)))
    (setf indices (mapcar #'(lambda (ind) (list ind)) indices))
    (if elements
      (setf indices (mapcar #'copy-list indices))))
  (apply #'vector indices))

(defmethod indices-array ((indices array) &key (instance nil) (elements nil))
  (indices-array-kernel indices instance elements))

(defmethod indices-array ((indices dimensioned-ref-object) 
                          &key (instance nil) (elements nil))
  (declare (ignore instance))
  (indices-array-kernel indices t elements))

;; a more useful indices-array maker, for users

(defun ind (indices-list &key (element-copy nil))
  "Builds an array of indices from the given list of indices. ~
   Each element is copied if keyword argument element-copy ~
   is non-NIL."
  (let* ((d1 (nested-list-dimensions indices-list))
         (dim (butlast d1)))
    (indices-array-kernel (make-array dim :initial-contents indices-list)
                          :elements element-copy)))

;
;  get-mask
;

(defun get-mask (dim shape dim-mask)
  (let* ((dim (if dim-mask (list-if-t dim dim-mask) dim))
         (shape (if (eq shape t)
                        (pad-list '() (length dim) t)
                        (pad-list shape (length dim) nil)))
         (mask (mapcar #'(lambda (x y)
                           (or x (/= y 1)))
                       shape
                       dim)))
    (expand-list mask dim-mask nil)))

;--------------------------------------------------------------------------------

;;;
;  DEFMETHODs of ref
;

(defmethod ref ((self dimensioned-ref-object) &rest args)
  (if (indirect-ref-p self)
    (ref-kernel (ref-instantiate self) self args)
    (missing-method 'ref self)))

(defmethod ref ((self number) &rest args)
  (ref-true-args self args 0 (length args))
  ;; if no error ...
  self)

(defmethod ref ((self symbol) &rest args)
  (ref-true-args self args 0 (length args))
  ;; if no error ...
  self)

(defmethod ref ((self character) &rest args)
  (ref-true-args self args 0 (length args))
  ;; if no error ...
  self)

(defmethod ref ((self sequence) &rest args)
  (ref-kernel (ref-instantiate self) self args))

;;;  array also handles the string case

(defmethod ref ((self array) &rest args)
  (ref-kernel (ref-instantiate self) self args))

;--------------------------------------------------------------------------------

;
;  ref-list
;     For any dimensioned ref-object.

(defun ref-list (r)
  (let* ((dim (dimensions-of r))
         (len (length dim)))
    (ref-list-recursion r
                        dim
                        (make-sequence 'list len)
                        len
                        0)))

(defun ref-list-recursion (r dim current terminus split)
  (if (eq split terminus)
    (apply #'eref r current)
    (let ((d-end (- (elt dim split) 1)))
      (loop for d
            from 0
            to d-end
            collect (progn
                      (setf (elt current split) d)
                      (ref-list-recursion r
                                          dim
                                          current
                                          terminus 
                                          (1+ split)))))))

(defun ref-print (r &optional (stream *quail-terminal-io*))
  (let* ((dim (dimensions-of r))
         (len (length dim)))
    (ref-print-recursion r
                        dim
                        (make-sequence 'list len)
                        len
                        0
                        stream)))

(defun ref-print-recursion (r dim current terminus split stream
                              &rest keys &key (elt-format "~S") (sep " ") (left "(") (right ")"))
 #-:sbcl-linux(declare (special *print-length*))
  (if (eq split terminus)
    (format stream elt-format (apply #'eref r current))
    (let ((d-end (- (elt dim split) 1))
          (ellipsis nil))
      (when (and *print-length*
                 (>= d-end *print-length*))
        (setf d-end (- *print-length* 1))
        (setf ellipsis t))
      (if left (format stream left))
      (loop for d
            from 0
            to d-end
            collect (progn
                      (setf (elt current split) d)
                      (apply #'ref-print-recursion
                             r
                             dim
                             current
                             terminus 
                             (1+ split)
                             stream
                             keys)
                      (if (and sep (< d d-end))
                        (format stream sep))))
      (if ellipsis
        (format stream " ..."))
      (if right (format stream right))))
  )



#|
#+:sbcl-linux(defun ref-print-recursion (r dim current terminus split stream
                              &rest keys &key (elt-format "~S") (sep " ") (left "(") (right ")"))
 ;(declare (special *print-length*))
  (if (eq split terminus)
    (format stream elt-format (apply #'eref r current))
    (let ((d-end (- (elt dim split) 1))
          (ellipsis nil))
      (when (and *print-length*
                 (>= d-end *print-length*))
        (setf d-end (- *print-length* 1))
        (setf ellipsis t))
      (if left (format stream left))
      (loop for d
            from 0
            to d-end
            collect (progn
                      (setf (elt current split) d)
                      (apply #'ref-print-recursion
                             r
                             dim
                             current
                             terminus 
                             (1+ split)
                             stream
                             keys)
                      (if (and sep (< d d-end))
                        (format stream sep))))
      (if ellipsis
        (format stream " ..."))
      (if right (format stream right))))
  )
|#


