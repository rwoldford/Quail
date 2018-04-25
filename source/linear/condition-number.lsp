;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          condition-number.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(condition-number-of)))


(defgeneric condition-number-of (thing)
  (:documentation
   "Returns the condition number of its argument; ~
   That is the ratio of its largest singular value to its smallest.~
   For some methods, this will be only a numerical approximation."
                  ))

(defmethod condition-number-of ((X identity-matrix))
  1
  )

(defmethod condition-number-of ((X inverse-matrix))
  (inverse (condition-number-of (inverse X)))
  )

(defmethod condition-number-of ((X T))
  "Returns the condition number of its argument; ~
   That is the ratio of its largest singular value to its smallest.~
   (:see-also (singular-values-of :generic-function) ~
   (sv-decomposition :topic) )"
  (let ((svs (singular-values-of X)))
    (/ (max svs) (min svs)))
  )

(defmethod condition-number-of ((lu lu-decomposition))
  "Returns the condition number as estimated by the lu-decomposition ~
   itself.~
   (:see-also (singular-values-of :generic-function) ~
   (sv-decomposition :topic) )"
  (cond
   ((q::rcond-of lu) (/ (q::rcond-of lu)))
   (T (warn "This LU decomposition has no condition number information.~&~
             NaN returned.")
      NaN)
   ))

(defmethod condition-number-of ((c cholesky-decomposition))
  "Returns the condition number as estimated by the cholesky-decomposition ~
   itself.~
   (:see-also (singular-values-of :generic-function) ~
   (sv-decomposition :topic) )"
  (cond
   ((q::rcond-of c)
    (/ (q::rcond-of c)))
   (T (warn "This cholesky decomposition has no condition number information.~&~
             Try an unpivoted version.~&~
             NaN returned.")
      NaN)
   ))

