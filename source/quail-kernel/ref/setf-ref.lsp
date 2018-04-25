;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               setf-ref.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          setf-ref-kernel
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*setf-ref* setf-ref-kernel)))

;--------------------------------------------------------------------------------

(defgeneric (setf ref) (new-value r &rest specifiers)
    (:documentation
  "Puts new-value in place (ref r specifiers). new-value must be an object ~
   itself accepted by ref (although not necessarily of the same class as r), ~
   and be dimensioned appropriately for specifiers.  (setf ref) DOES NOT ~
   accept :shape in the ref portion of the form.  ~
   new-value is returned by all (setf ref) methods.")
)

;--------------------------------------------------------------------------------

;;;  This code allows us to do (apply #'(setf ref) ...) using the expression
;;;  (apply *setf-ref* ...)

(defvar *setf-ref*)
(setf *setf-ref*
      #+:cl-2 #'(setf ref)
      #-:cl-2 #'pcl::setf\ quail-kernel\ ref)

;--------------------------------------------------------------------------------

;;;
;  Kernel functions required by the (setf ref) defmethods.
;

(defgeneric setf-ref-kernel (new-value targ)
  )

;
;  (setf-ref-kernel new-value target) is required for same reason as ref-kernel.
;

;
;  May have problems with something like
;
;         (setf (ref a ...) (ref a ...)).
;
;  Should deal with this by doing copy-ref first on new-value if the ultimate
;  ref-object is the same for new-value and r.
;

(defmethod setf-ref-kernel (new-value (targ t))
  (missing-method 'setf-ref-kernel new-value targ))

(defmethod setf-ref-kernel (new-value (targ dimensioned-ref-object))
  (declare (special *setf-eref*))
  (let* ((targ-dim (dimensions-of targ))
         (src-dim (dimensions-of new-value)))
    (multiple-value-bind (conformable dim targ-mask src-mask) 
                         (conform targ-dim src-dim)
      (if (not conformable)
        (quail-error "Referenced portion of object ~S has dimensions ~S, ~
                ~&but new value ~S provided has dimensions ~S."
               targ
               targ-dim
               new-value
               src-dim))
      (let* ((num-dims (length dim))
             (current (make-sequence 'list num-dims :initial-element 0))
             index)
        (loop for d
              from 0
              to (- (apply #'* dim) 1)
              do (progn 
                   (setf index (list-if-t current targ-mask))
                   (apply *setf-eref*
                          (apply #'eref-mask 
                                 new-value
                                 (append current 
                                         (list :mask src-mask)))
                          targ
                          index)
                   (setf current (row-major-next-subscript current dim)))))))
  new-value)

;--------------------------------------------------------------------------------

;;;
;  DEFMETHODs of (setf ref)
;

(defmethod (setf ref) (new-value (self sequence) &rest specifiers)
  (setf-ref-kernel new-value (apply #'ref self specifiers)))

(defmethod (setf ref) (new-value (self array) &rest specifiers)
  (setf-ref-kernel new-value (apply #'ref self specifiers)))

(defmethod (setf ref) (new-value (self dimensioned-ref-object) &rest specifiers)
  (setf-ref-kernel new-value (apply #'ref self specifiers)))

