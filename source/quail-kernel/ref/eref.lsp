;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               eref.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1991.
;;;     R.W. Oldford 1992.
;;;     Greg Anglin 1993.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          eref
;;;          eref-transform
;;;          eref-transform-of-margin
;;;          eref-transform-of-indices
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(eref *setf-eref*)))

;--------------------------------------------------------------------------------

;;;
;  DEFGENERICs of eref 
; 

(defgeneric eref (r &rest index)
    (:documentation
  "Returns element of r specified by index.")
)

(defgeneric (setf eref) (new-value a &rest index)
    (:documentation
  "Puts new-value in place (eref a index).
   new-value is returned by all (setf eref) methods.")
)

;--------------------------------------------------------------------------------

;;;  This code allows us to do (apply #'(setf eref) ...) using the expression
;;;  (apply *setf-eref* ...)

(defvar *setf-eref*
  "The value of this symbol allows us to do (apply #'(setf eref) ...) ~
   using the expression (apply *setf-eref* ...).")

(setf *setf-eref*
      #+:cl-2 #'(setf eref)
      #+:pcl #'pcl::setf\ quail-kernel\ eref)

;--------------------------------------------------------------------------------

;;;
;  Useful macros
;

; this should really handle a dim-ref-obj pointing to a list, too.

(defmacro margin-specs-p (specs)
  `(or (listp ,specs) (symbolp ,specs) (numberp ,specs)))

(defmacro indices-specs-p (specs)
  `(not (margin-specs-p ,specs)))

;;;
;  Utility functions for eref methods
;

;
; eref-true-index
;   allows objects with dimensions-rank < 2 to be eref'd with extraneous args, 
;   if the additional args are zeroes.

(defun eref-err (r args)
  (quail-error "Improper arguments ~S provided to EREF for object ~S ~
                        with dimensions ~S."
                       args
                       r
                       (dimensions-of r)))

(defun eref-true-index (r index &optional dim-rank-r dim-rank-index)
  (let* ((dim-rank-r (or dim-rank-r (length (dimensions-of r))))
         (dim-rank-index (or dim-rank-index (length index))))
    (if (>= dim-rank-r dim-rank-index)
      ;; leaves the method to deal with any problems if >.
      index
      (let ((true-index (remove 0 index :count (- dim-rank-index dim-rank-r))))
        (if (and (< dim-rank-r 2)
                 (eql (length true-index) dim-rank-r))
          true-index
          (eref-err r index))))))
                     
;
; eref-transform
;   transforms index to indirect-ref dimensioned-ref-object r to index
;   to (ref-obj-of r)
;   assumes that index is already a true-index in the sense of eref-true-index

(defun eref-transform (r index)         
  (let* ((specs (specs-of r))
         (specs-mask (specs-mask-of r))
         (index (expand-list index specs-mask 0)))
    (if (margin-specs-p specs)
      (eref-transform-of-margin index specs)
      (eref-transform-of-indices index specs))))

(defun eref-transform-of-margin (index specs)
  (loop for i in index
        as  s in specs
        collect (cond ((eq s t) i)
                      ((eq (first s) :c) 
                       (let ((comp-list (rest s)))
                         (or (loop for comp-i in comp-list
                                   do (cond ((< i comp-i)
                                             (return i))
                                            (t (incf i))))
                             i)))
                      (t (elt s i)))))

(defun eref-transform-of-indices (index specs)
  (apply #'aref specs index))

;;;
;  DEFMETHODs of eref
;

(defmethod eref ((self t) &rest index)
  (declare (ignore index))
  (missing-method 'eref self))

;  the following case is only called in practice for dimensioned-ref-objects which have
;  a ref-obj belonging to one of the basic classes, or accidently when a eref method 
;  is left undefined for an object inheriting from dimensioned-ref-object.  Since this 
;  slows down eref for indirect refs to basic classes, such refs might better be 
;  avoided by using sel unless indirect reference is absolutely necessary.

(defmethod eref ((self dimensioned-ref-object) &rest index)
  (if (indirect-ref-p self)
    (apply #'eref (ref-obj-of self) (eref-transform self (eref-true-index self index)))
    (missing-method 'eref self)))

(defmethod eref ((self number) &rest index)
  (eref-true-index self index 0 0)
  ;; if no error, then ...
  self)

(defmethod eref ((self symbol) &rest index)
  (eref-true-index self index 0 0)
  ;; if no error, then ...
  self)

(defmethod eref ((self character) &rest index)
  (eref-true-index self index 0 0)
  ;; if no error, then ...
  self)

(defmethod eref ((self sequence) &rest index)
  ;; let elt worry about out-of-bounds ...
  (elt self (first (eref-true-index self index 1))))

#|
;; old functionality which eref'd some nested lists with more than one
;; index  ... not used anymore, I hope.  dga 93 03 26.

(defmethod eref ((self sequence) &rest index)
  (if (>= (first index)
          (length self))
    (quail-error "Index ~d out of bounds for sequence ~s -- in EREF."
                 (first index)
                 self)
    (if (/= (length index) 1)
      (let ((result (elt self (first index))))
        ;; use 'list to dodge the list of strings case
        (if (typep result 'list)
          (apply #'eref result (rest index))
          (elt self (first (eref-true-index self index 1)))))
      (elt self (first index)))))
|#

(defmethod eref ((self array) &rest index)
  (apply #'aref self (eref-true-index self index)))

;;;
;  DEFMETHODs of (setf eref)
;

(defmethod (setf eref) (new-value (self t) &rest index)
  (declare (ignore index new-value))
  (missing-method "(setf eref)" self))

;  see above comment for (defmethod eref (self dimensioned-ref-object) ...)

(defmethod (setf eref) (new-value (self dimensioned-ref-object) &rest index)
  (declare (special *setf-eref*))
  (if (indirect-ref-p self)
    (apply *setf-eref* 
           new-value 
           (ref-obj-of self)
           (eref-transform self (eref-true-index self index)))
    (missing-method 'eref self)))

(defmethod (setf eref) (new-value (self sequence) &rest index)
  ;; let elt worry about out-of-bounds ...
  (setf (elt self (first (eref-true-index self index 1))) new-value))

#|
;; old functionality which eref'd some nested lists with more than one
;; index  ... not used anymore, I hope.  dga 93 03 26.

(defmethod (setf eref) (new-value (self sequence) &rest index)
  (if (>= (first index)
          (length self))
    (quail-error "Index ~d out of bounds for sequence ~s -- in (SETF EREF)."
                 (first true-index)
                 self)
    (if (/= (length index) 1)
      (let ((result (elt self (first index))))
        ;; use 'list to dodge the list of strings case
        (if (typep result 'list)
          (apply *setf-eref* new-value result (rest index))
          (setf (elt self (first (eref-true-index self index 1))) new-value)))
      (setf (elt self (first index)) new-value))))
|#

(defmethod (setf eref) (new-value (self array) &rest index)
  (setf (apply #'aref self (eref-true-index self index)) new-value))

;;; 
;  Function eref-mask and (setf eref-mask)
;

(defun eref-mask (r &rest args)
  (multiple-value-bind (index mask)
                       (interpret args '(:mask))
    (apply #'eref r (if mask
                       (list-if-t index mask :pad-with t)
                       index))))



