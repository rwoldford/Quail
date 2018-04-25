;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               ones-array.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     Greg Anglin  1993?
;;;     R.W. Oldford 1994.
;;;
;;;--------------------------------------------------------------------------------

(in-package :qk)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(ones-array ones-matrix ones)))

(defclass ones-array (num-array)
  ()
  (:documentation "An array of 1s. (:see-also (ones-matrix :class) ~
                   (ones :function)) ")
  )

(defmethod initialize-instance :after ((r ones-array) &rest initargs)
  (declare (ignore initargs))
  (setf (ref-contents-of r) 1))

(defmethod eref ((self ones-array) &rest index)
  (eref-true-index self index)
  ;; if no error ...
  1)

(defmethod (setf eref) (new-value (self ones-array) &rest index)
  (declare (ignore new-value index))
  (quail-error "~&Can't setf an element of an array of class ~S."
               (class-name (class-of self))))

(defclass ones-matrix (matrix ones-array)
  ()
  (:documentation "A matrix of 1s. ~
                   (:see-also (ones-array :class) ~
                   (ones :function)) ")
  )


(defmethod initialize-instance :after ((r ones-matrix) &rest initargs)
  (declare (ignore initargs))
  (setf (ref-contents-of r) 1))

(defmethod eref ((self ones-matrix) &rest index)
  (eref-true-index self index)
  ;; if no error ...
  1)

(defmethod (setf eref) (new-value (self ones-matrix) &rest index)
  (declare (ignore new-value index))
  (quail-error "~&Can't setf an element of an array of class ~S."
               (class-name (class-of self))))

(defun ones (n &rest other-dimensions)
  "Create an array full of ones whose elements are constant. ~
   (:required~
   (:arg n The first dimension of the array.)) ~
   (:rest ~
   (:arg other-dimensions NIL The remaining dimensions of the array ~
   of ones.))~
   (:elaboration This is an array of constants. Its contents cannot be changed ~
   and every element is the fixnum 1.)"
  (cond
   ((null other-dimensions)
    (make-instance 'ones-matrix :dimensions (list n)))
   ((= 1 (length other-dimensions))
    (make-instance 'ones-matrix :dimensions (list n (first other-dimensions))))
   (T
    (make-instance 'ones-array :dimensions (append (list n) other-dimensions)))
   )
  )

(put-return-class 'num-array 'ones-array
                  '(ones-array matrix num-array
                    array vector cons  number integer fixnum float
                    rational complex symbol))

(put-return-class 'num-array 'ones-matrix
                  '(ones-array num-array))

(put-return-class 'matrix 'ones-matrix
                  '(matrix array vector cons  number integer fixnum float
                    rational complex symbol))
