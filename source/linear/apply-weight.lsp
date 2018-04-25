;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               apply-weight.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(apply-weight remove-weight)))

;-----------------------------------------------------------------------------
;METHOD: apply-weight
;-----------------------------------------------------------------------------

(defmethod apply-weight ((weight null) (x matrix))
  x)

(defmethod-multi-one-body apply-weight ((weight (matrix sequence))
                                        (x matrix))
  
  (cond ((mat2dp weight)
         (let ((weight (upper-triangle-of (cholesky-of weight)))
               (result (array 0 :dimensions (dimensions-of x)))
               (n (nrows x))
               (p (ncols x))
               )
           (with-cl-functions (*)
             (loop for i from 0 below n
                   do
                   (loop for j from 0 below p
                         do
                         (setf (eref result i j)
                               (loop for k from i below n
                                     sum
                                     (* (eref weight i k)
                                        (eref x k j))))
                         )
                   ))
           result))
        (t (* (sqrt weight) x)))
  )

#|
(defmethod-multi-one-body apply-weight ((x matrix) (weight (vector sequence)))
  (let ((weight (matrix-sqrt weight)))
    (cond ((mat1dp weight) (* weight x))
          ((mat2dp weight) (.* weight x))
          (t (quail-error "Weight is not of dimension 1 or 2.")))))

(defmethod-multi-one-body apply-weight :before ((x matrix)
                                                (weight (matrix sequence)))
  (cond ((null (dimensions-of weight)) nil)
        ((mat1dp weight)
         (unless (= (first (dimensions-of x))
                    (first (dimensions-of weight)))
           (quail-error "Weights do not match number of observations."))
         )
        ((mat2dp weight)
         (unless (= (first (dimensions-of x))
                    (first (dimensions-of weight)))
           (quail-error "Weights do not match number of observations.")
         (unless (apply #'= (dimensions-of weight))
           (quail-error "Weight matrix not square.")))
        (t (quail-error "Weight is not of dimension 1 or 2."))))
|#


;;;-----------------------------------------------------------------------------
;;;  METHOD: remove-weight
;;;-----------------------------------------------------------------------------

(defmethod remove-weight ((weight null) (x matrix))
  x)

(defmethod-multi-one-body remove-weight ((weight (matrix sequence))
                                         (x matrix))
  "Removes the sqrt or matrix sqrt of weight from x."
  (let ((result (sel x)))
    (cond
     ((mat2dp weight)
      (with-slots
        ((cholesky-a a))
        (cholesky-of weight)
        (dtrls cholesky-a result 01)
        result))
     (T
      (loop
        for i from 0 below (nrows result)
        do
        (loop for j from 0 to (ncols result)
              with div = (cl::sqrt (eref weight i))
              do
              (setf (eref result i j)
                    (/ (eref x i j) div))
              )
        )
      result)))
  )
