;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            determinant.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(determinant)))


(defgeneric determinant (thing)
  (:documentation
   "Returns the determinant of its argument."
                  ))

(defmethod determinant ((X identity-matrix))
  (declare (ignore X))
  1)

(defmethod determinant ((X inverse-matrix))
  (inverse (determinant (inverse x))))

(defmethod determinant ((X matrix))
  "Returns the determinant of the square matrix X. ~
   Depends on an lu decomposition of X, followed by ~
   a call to the LINPACK routine dgedi."
  (let ((first-dim (or (first (dimensions-of X)) 1))
        (second-dim (or (second (dimensions-of X)) 1))
        )
    (cond
     ((= first-dim second-dim)
      (let ((lu (lud-of X)))
        (if (= 1.0 (+ 1.0 (eref (rcond-of lu))))
          0
          (let* ((a (lu-of lu))
                 (n (first (dimensions-of a)))
                 (ipvt (ipvt-of lu))
                 (det (array 0.0D0 :dimensions (list 2)))
                 (work (array 0.0D0 :dimensions (list n)))
                 (job 10))
            (dgedi a n ipvt det work job)
            (cl::* (eref det 0)
                          (cl:expt 10 (eref det 1))))))
      )
     (T
      (quail-error
       "Matrix is ~s by ~s! Determinant requires a square matrix; ~s is not."
       first-dim second-dim X)))))

(defmethod determinant ((LU lu-decomposition))
  "Returns the determinant from the lu decomposition, LU, by ~
   a call to the LINPACK routine dgedi."
  (if (= 1.0 (+ 1.0 (eref (rcond-of lu))))
    0
    (let* ((a (lu-of lu))
           (n (first (dimensions-of a)))
           (ipvt (ipvt-of lu))
           (det (array 0.0D0 :dimensions (list 2)))
           (work (array 0.0D0 :dimensions (list n)))
           (job 10))
      (dgedi a n ipvt det work job)
      (cl::* (eref det 0)
                    (cl:expt 10 (eref det 1))))))


(defmethod determinant ((c cholesky-decomposition))
  "Returns the determinant from the Cholesky decomposition, c, by ~
   a call to the LINPACK routine dpodi, if necessary."
  (let* ((a (upper-triangle-of c))
         (n (nrows a)))
    (if (or (and (null (jpvt-of c))
                 (zerop (info-of c)))
            (and (jpvt-of c)
                 (= (info-of c) n)))
      (let ((det (array 0.0D0 :dimensions (list 2)))
            (job 10))
        (dpodi a n det job)
        (cl::* (eref det 0)
                      (cl:expt 10 (eref det 1))))
      0)
    )
  )

(defmethod determinant ((svd sv-decomposition))
  "Returns the determinant from the singular value decomposition, svd, ~
   of a square matrix."
  (let ((u (left-singular-vectors-of svd)))
    (if (= (nrows u) (ncols u))
      (reduce-slices #'* (singular-values-of svd)
                     :initial-value 1.0)
      (quail-error
       "Can't determine a determinant from the singular value ~
        decomposition of a non-square matrix.  svd = ~s"
       svd))))
