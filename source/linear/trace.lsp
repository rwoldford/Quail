;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            trace.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;-----------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(trace-of)))


(defgeneric trace-of (A)
  (:documentation "Returns the trace of the square matrix A. ~
                   Simply sums the diagonal elements."))

(defmethod trace-of ((A T))
  (let ((first-dim (or (first (dimensions-of A)) 1))
        (second-dim (or (second (dimensions-of A)) 1))
        )
    (cond
     ((= first-dim second-dim)
      (if (= first-dim 1)
        (eref A 0)
        (loop for i from 0 below first-dim
              sum (eref A i i)))
      )
     (T
      (quail-error
       "Matrix is ~s by ~s! Trace-of requires a square matrix; ~s is not."
       first-dim second-dim A)))))

(defmethod trace-of ((A identity-matrix))
  (nrows A))
