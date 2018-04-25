;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            triangular.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991.
;;;     R.W. Oldford 1994.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;
(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(upper-triangle-of backsolve)))

(defgeneric upper-triangle-of (X)
  (:documentation "Returns an upper triangular square matrix whose ~
                   non-zero elements have been copied from the upper ~
                   triangular portion of X.")
  )

(defmethod upper-triangle-of ((X matrix))
  (let* ((size (ncols X))
         (result (sel X (iseq size) (iseq size))))
    (loop
      for i from 0 below size
      do
      (loop
        for j from 0 below i
        do
        (setf (eref result i j) 0)))
    result))
                

(defun backsolve (x y &key transpose upper)
  "Solves triangular linear systems of the form xb = y or zb = y where ~
   z is the transpose of x.  Returns b. ~&~
   By default, x is lower triangular and is not ~
   transposed.  If transpose is non-nil, the transpose of x is used.  If ~
   upper is non-nil, x is taken to be upper triangular.  Elements of x ~
   outside of the triangular portion of interest are ASSUMED to be zero, ~
   regardless of their actual values."
  (let* ((job 00)
         (n (first (dimensions-of x)))
         (b (sel y))
         (info (array 0 :dimensions '(1))))
    (if transpose (incf job 10))
    (if upper (incf job 1))
    (dtrsl x n b job info)
    b))
        
           
#|
;-----

(defclass upper-triangular-matrix (matrix)
  ())


(put-return-class 'matrix
                  'upper-triangular-matrix
                  '(matrix array vector cons integer rational float symbol))

(defmethod make-upper-triangular-matrix ((self matrix))
  (change-class self 'upper-triangular-matrix))

;-----

(defmethod eref :around ((self upper-triangular-matrix) &rest index)
  (let ((i (first index))
        (j (second index)))
    (if (> i j) 0 (call-next-method))))

(defmethod (setf eref) :around
           (new-value (self upper-triangular-matrix) &rest index)
  (let ((i (first index))
        (j (second index)))
    (if (> i j) 0 (call-next-method))))

;-----

(defmethod ref :around ((self dimensioned-ref-object) &rest args)
  (multiple-value-bind (subscripts shape)
                       (interpret args '(:shape))
    (if (null subscripts) 
      self
      (call-next-method))))
|#
