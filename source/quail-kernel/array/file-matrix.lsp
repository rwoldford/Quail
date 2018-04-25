;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               file-matrix.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1993.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(file-matrix)))

;------------------------------------------------------------------------------

;;;
;  DEFCLASS of file-matrix
;

;;  Class file-matrix keeps its contents in the slot ref-contents, as a quail-file.

(defclass file-matrix (matrix quail-open-state-mixin faster-eref-open-mixin)
  (rowlen))

(push-extension-class 'file-matrix)

;------------------------------------------------------------------------------

;;;
;  return-class structure (see z-return-class.lisp)
;

(put-return-class 'file-matrix
                  'file-matrix
                  '(matrix array vector cons  number integer fixnum float
                    rational complex symbol))

;------------------------------------------------------------------------------


;;;
;  initialize-contents methods for file-matrix
;

(defmethod initialize-contents ((self file-matrix) (init (eql :empty))
                                &rest initargs &key (file ""))
  ;;
  ;;  Supercedes the primary method for ref-array and num-array
  ;;  which is called by initialize-instance :after.  This is
  ;;  usually from a make-instance call in make-dimensioned-result,
  ;;  in which case file will be "".
  ;;
  ;;  To avoid creating too many file-matrix instances while doing 
  ;;  computations, if the requested matrix is small enough and
  ;;  (string= (namestring file) ""),  the class of self is
  ;;  changed to matrix (dangerous), and the appropriate initialize-contents
  ;;  is performed.
  ;;
  ;;  The concept of "small enough" is determined by the variable
  ;;  *min-file-matrix-elements* (defined below).
  ;;
  ;;  Note that copy-dispatch for file-matrix provides the same
  ;;  functionality for sel.
  ;;
  (declare (special *min-file-matrix-elements*))
  (declare (ignore initargs))
  (if (and (< (apply #'* (dimensions-of self)) *min-file-matrix-elements*)
           (string= (namestring file) ""))
    ;;
    ;;  Change to class matrix
    ;;
    (progn
      (change-class self 'matrix)          ; dangerous, but seems to work ...
      (initialize-contents self :empty))   ; primary method returns matrix self
    ;;
    ;;  Proceed as class file-matrix
    ;;
    (progn
      (initialize-file-matrix self file)
      (fill-file-matrix self :default))))

(defmethod initialize-contents ((self file-matrix) (init null)
                                &rest initargs 
                                &key initial-element
                                (file ""))
  (declare (ignore initargs))
  (setf initial-element (or initial-element :default))
  (initialize-file-matrix self file)
  (fill-file-matrix self initial-element))

(defmethod initialize-contents ((self file-matrix) (init scan-env)
                                &key (file ""))
  (declare (special *width-of-an-entry*))
  (declare (ignore initargs))
  (with-slots (dimensions
               (quail-file ref-contents)
               rowlen) self
    (if (> (count t dimensions) 1)
      (quail-error "~&The dimensions specification ~S is ambiguous for scanning ... 
                    ~&there can be at most one ~S in the dimensions list."
                   dimensions
                   t))
    (setf file (quail-file-pathname file :prefix "file-matrix-"))
    (setf quail-file (make-instance 'quail-file :pathname file))
    (unwind-protect
      (quail-file-initial-open quail-file)    ;; has side effect quail-open-object !!
      (quail-close-preserve-object quail-file))
    (with-open-quail-objects (init quail-file) quail-file
      ;;  basically loop over scan-1 doing
      ;;  a (quail-file-write quail-file (file-matrix-get-format thing) thing),
      ;;  then pad with nan if need be.
      (let ((not-eof t)
            (input nil)
            (count 0))
        (loop while not-eof
              do (progn
                   (setf input (scan-1 init))
                   (if (eq input *eof*)
                     (setf not-eof nil)
                     (progn
                       (quail-file-write quail-file 
                                         (file-matrix-get-format input)
                                         input)
                       (incf count)))))
        (let* ((marginal-size (apply #'* (substitute 1 t dimensions)))
               (rem (rem count marginal-size)))
          (if (zerop rem)
            (setf dimensions (substitute (floor count
                                                marginal-size)
                                         t
                                         dimensions))
            (progn
              (setf dimensions (substitute (ceiling count
                                                    marginal-size)
                                           t
                                           dimensions))
              (loop for i from 1 to rem
                    do (quail-file-write quail-file
                                         (file-matrix-get-format nan)
                                         nan))                
              (warn "~&Scanned array ~S did not conform 
                     exactly to specified dimensions."
                    self))))))
    (let ((second-dim (case (length dimensions)
                        (2 (second dimensions))
                        (t 1))))
      (setf rowlen (+ 1 (* second-dim *width-of-an-entry*))))))
                   
(defmethod initialize-contents ((self file-matrix) (init t) &rest initargs &key)
  (declare (ignore initargs))
  (format *quail-terminal-io* "Error: Invalid initial-contents ~S or 
                         missing initialize-contents method.~%~%"
                        init)
  (missing-method 'initialize-contents self init))

;------------------------------------------------------------------------------

;;;
;  Important variables
;

(defvar *file-matrix-default-element*)
(setf *file-matrix-default-element* 0.0)

(defvar *width-of-an-entry* 26)
(defvar *decimal-places* 15)

(defvar *min-file-matrix-elements*)
(setf *min-file-matrix-elements* 10)

(defvar *float-format*)
(setf *float-format* (format nil " ~A~S,~SG"
                                 "~"
                                 (- *width-of-an-entry* 1)
                                 *decimal-places*))

(defvar *fixnum-format*)
(setf *fixnum-format* (format nil " ~A~SG"
                                   "~"
                                   (- *width-of-an-entry* 1)))

;  Formatting for symbols is intended for NaN, +infinity, -infinity
;  This assumes that the symbol with one whitespace character will fit
;  into *width-of-an-entry* characters.

(defvar *symbol-format*)
(setf *symbol-format* (format nil " ~A~S@S"
                                  "~"
                                   (- *width-of-an-entry* 1)))

(defmacro file-matrix-get-format (item)
  `(cond ((fixnump ,item)
            *fixnum-format*)
         ((symbolp ,item)
            *symbol-format*)
         (t
            *float-format*)))

;;;
;  Open/close methods -- note that file-matrix is NOT a subclass of quail-open-mixin.
;

(defmethod quail-open-object ((self file-matrix))
  (if (direct-ref-p self)
    (quail-open-object (ref-contents-of self))
    (quail-open-object (ref-contents-of (ref-obj-of self)))))

(defmethod quail-close-destroy-object ((self file-matrix))
  (if (direct-ref-p self)
    (quail-close-destroy-object (ref-contents-of self))
    (quail-close-destroy-object (ref-contents-of (ref-obj-of self)))))

(defmethod quail-close-preserve-object ((self file-matrix))
  (if (direct-ref-p self)
    (quail-close-preserve-object (ref-contents-of self))
    (quail-close-preserve-object (ref-contents-of (ref-obj-of self)))))

(defmethod quail-close-physically-object ((self file-matrix))
  (if (direct-ref-p self)
    (quail-close-physically-object (ref-contents-of self))
    (quail-close-physically-object (ref-contents-of (ref-obj-of self)))))

;;;
;  Utility functions
;

(defun initialize-file-matrix (the-file-matrix init-file) 
  (declare (special *width-of-an-entry*))
  (with-slots (dimensions
               (quail-file ref-contents)
               rowlen) the-file-matrix
    (setf init-file (quail-file-pathname init-file :prefix "file-matrix-"))
    (setf quail-file (make-instance 'quail-file :pathname init-file))
    (let ((second-dim (case (length dimensions)
                        (2 (second dimensions))
                        (t 1))))
      (setf rowlen (+ 1 (* second-dim *width-of-an-entry*))))))

(defun fill-file-matrix (the-file-matrix initial-element)
  (declare (special *width-of-an-entry*))
  (with-slots (dimensions
               (quail-file ref-contents)
               rowlen) the-file-matrix
    (unwind-protect
      (quail-file-initial-open quail-file)    ;; has side effect quail-open-object !!
      (quail-close-preserve-object quail-file))
    (with-open-quail-objects (quail-file) quail-file
      (let* ((file-size (quail-file-length quail-file))
             (dims (case (length dimensions)
                     (0 '(1 1))
                     (1 (append dimensions '(1)))
                     (2 dimensions)))
             (dim-size (* rowlen (first dims))))
        (cond ((eq 0 file-size)
               (let* ((initial-element
                       (if (eq initial-element :default)
                         *file-matrix-default-element*
                         initial-element))
                      (form-str
                       (format nil "~A~A~A~%"
                                   "~{" 
                                   (file-matrix-get-format initial-element)
                                   "~}"))
                      (init-row 
                       (make-sequence 'list (second dims)
                                      :initial-element initial-element))
                      (formatted-row (format nil form-str init-row)))
                 (loop for i from 0 to (- (first dims) 1)
                       do (quail-file-write quail-file "~A" formatted-row))))
              ((> file-size dim-size)
                (quail-error "Given file ~S is too large for given dimensions ~S 
                        ~&Requires ~S, actual length ~S."
                       (pathname-of quail-file)
                       dimensions
                       dim-size
                       file-size))
              ((< file-size dim-size)
                (quail-error "Given file ~S is too small for given dimensions ~S 
                        ~&Requires ~S, actual length ~S."
                       (pathname-of quail-file)
                       dimensions
                       dim-size
                       file-size))
              ((= file-size dim-size)
                (if (and (not (eq initial-element :default))
                         (quail-y-or-n-p 
                          "Given file ~S already exists, 
                           ~&but initial-element ~S has been provided... 
                           ~&Re-initialize with this initial-element?"
                          (pathname-of quail-file)
                          initial-element))
                  (progn
                    (quail-file-close-and-delete quail-file)
                    (fill-file-matrix the-file-matrix initial-element)))))))))

(defun file-matrix-subscript (fm subscripts)
  (with-slots (dimensions) fm
    (let* ((len-dim (length dimensions))
           (first-sub (case len-dim
                        (0 0)
                        (t (first subscripts))))
           (second-sub (case len-dim
                         (2 (second subscripts))
                         (t 0))))
      (list first-sub second-sub))))

(defun file-matrix-index (fm &rest subscripts)
  (declare (special *width-of-an-entry*))
  (with-slots (rowlen) fm
    (let* ((subs (file-matrix-subscript fm subscripts)))
      (+ (* (first subs) rowlen)
         (* (second subs) *width-of-an-entry*)))))

;;;  The next two functions are for direct-ref file-matrix instances only !!
;;;  The eref and (setf eref) _methods_ take care of indirect refs.

(defun file-matrix-eref (fm &rest subscripts)
  (with-slots ((quail-file ref-contents)) fm
    (let* ((subs (file-matrix-subscript fm subscripts))
           (index (apply #'file-matrix-index fm subs)))
      (quail-file-read-at-index quail-file index))))

(defun file-matrix-setf-eref (new-value fm &rest subscripts)
  (with-slots ((quail-file ref-contents)) fm
    (let* ((subs (file-matrix-subscript fm subscripts))
           (index (apply #'file-matrix-index fm subs)))
      (quail-file-write-at-index quail-file
                                 index
                                 (file-matrix-get-format new-value)
                                 new-value)))
  new-value)

;------------------------------------------------------------------------------

;;;
;  DEFMETHODs for file-matrix
;

(defmethod eref ((self file-matrix) &rest subscripts)
  (setf subscripts (eref-true-index self subscripts))
  (if (direct-ref-p self)
    (apply #'file-matrix-eref self subscripts)
    (apply #'file-matrix-eref (ref-obj-of self)
                              (eref-transform self subscripts))))

(defmethod (setf eref) (new-value (self file-matrix) &rest subscripts)
  (setf subscripts (eref-true-index self subscripts))
  (if (direct-ref-p self)
    (apply #'file-matrix-setf-eref new-value self subscripts)
    (apply #'file-matrix-setf-eref new-value
                                   (ref-obj-of self)
                                   (eref-transform self subscripts))))

#|
;;  I don't think these are needed, since the with-open-quail-objects affects
;;  only stuff in copy-dispatch which has a with-open-quail-objects around it 
;;  anyway.

(defmethod (setf ref) (new-value (self file-matrix) &rest specifiers)
  (with-open-quail-objects (self) file-matrix
    (setf-ref-kernel new-value self specifiers)))

(defmethod sel ((self file-matrix) &rest args)
  (with-open-quail-objects (self) file-matrix
    (sel-dim-ref-object self args)))
|#

(defmethod copy-dispatch ((proto file-matrix) ref-r)
  (declare (special *min-file-matrix-elements*))
  (let* ((dim (dimensions-of ref-r))
         (ref-obj (ref-obj-of ref-r))
         (specs (specs-of ref-r))
         (specs-mask (specs-mask-of ref-r)))
    (if dim
      (progn
        (convert-dim-ref-object ref-r)   ;; now ref-r is direct-ref
        ;;
        ;;  See the note in (initialize-contents file-matrix :empty)
        ;;  about this following mess.
        ;;
        (if (< (apply #'* dim) *min-file-matrix-elements*)
          ;;
          ;;  Change to class matrix
          ;;
          (progn
            (change-class ref-r 'matrix)
            (initialize-contents ref-r :empty))
          ;;
          ;;  Proceed as class file-matrix
          ;;
          (let* ((pathname (pathname-of ref-obj))
                 (name (pathname-name pathname))
                 (dir (pathname-directory pathname))
                 (file (merge-pathnames dir
                                        (concatenate 'string
                                                     name
                                                     (get-unique-filename "-"
                                                                          dir)))))
            (initialize-contents ref-r nil :file file)))
        (with-open-quail-objects (ref-obj ref-r) file-matrix
          (copy-kernel ref-obj ref-r specs specs-mask))
        ref-r)
      (eref ref-r))))        ; handles 0-dim case

#|
;; contents-of is gone everywhere, at least for now

(defmethod contents-of ((self file-matrix))
  (missing-method 'contents-of self))

;;; This is what this might look like if it wasn't such a bad idea ...

(defmethod contents-of ((self file-matrix))
  (let* ((dim (dimensions-of self))
         originally-open)
    (if dim
      (let* ((mat (make-instance 'matrix :dimensions dim)))
        (with-open-quail-objects (self) file-matrix
          (copy-kernel self mat (specs-of self) (specs-mask-of self)))
        mat)
      (eref self))))

|#

(defmethod pathname-of ((fm file-matrix))
  (if (direct-ref-p fm)
    (pathname-of (ref-contents-of fm))
    (pathname-of (ref-contents-of (ref-obj-of fm)))))

#|
(defmethod describe ((fm file-matrix))
  (format t "~&#<FILE-MATRIX ~S> is an instance of class ~S with slots: 
                  ~{~&~A~3,8@T~S~}"
                  (system-get-pointer fm)
                  (class-of fm)
                  (list
                  "CONTENTS" (contents-of fm)
                  "DIMENSIONS" (dimensions-of fm)))
  (values))
|#

(defmethod print-object ((fm file-matrix) stream)
  (with-slots ((quail-file ref-contents) dimensions) fm
    (cond ((eq dimensions :none)
             (format stream "#<empty ~A ~S>"
                     (class-name (class-of fm))
                     (system-get-pointer fm)))
          ((or (empty-ro-p fm) (eq quail-file :empty))
             (format stream "<empty ~S with dims ~S>"
                     (class-name (class-of fm))
                     dimensions))
          (t (let* ((ro (ref-obj-of fm))
                    (ro (if (eq ro :none) fm ro))
                    (zf (if (eq ro :none) quail-file (ref-contents-of ro))))
               (format stream "#<~A from ~S, with dims ~S>"
                       (class-name (class-of ro))
                       (namestring (pathname-of zf))
                       dimensions)))))
  fm)





