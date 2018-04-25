;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               solve.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;-----------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(solve)))

;-------------------------------------------------------------------------------
; GENERIC FUNCTION: solve
;-------------------------------------------------------------------------------

(defgeneric solve (x y &key &allow-other-keys)        
  (:documentation
   "Consider the linear system Xb = y. ~
    Given both X and y, solve calculates and returns the solution b. ~
    If X is given, but y is null or an identity matrix, ~
    then solve returns the inverse of X. ~
    (:see-also (inverse :generic-function) (backsolve :function) ~
    (matrix-decompositions :topic))"
   ))
  
(defmethod solve ((x t) y &key &allow-other-keys)
  (missing-method 'solve x y))

(defmethod solve :before ((x matrix) y &key &allow-other-keys)
  (if y
    (unless (= (nrows X) (nrows y)) 
      (quail-error "Number of rows of ~s does not equal number of ~
                    rows of ~s." x y))
    ) 
  )


(defmethod solve :around ((x lu-decomposition) (y identity-matrix)
                          &key &allow-other-keys)
  (if (= (nrows (lu-of x))
         (nrows y))
    (inverse x)
    (quail-error "Solve: Dimensions don't match ~&~
                  ~5Tx = ~s ~&~
                  ~5Ty = ~s."
                 x y)))


(defmethod solve :around ((x lu-decomposition) (y (eql NIL)) &key &allow-other-keys)
  (inverse x))

(defmethod solve ((lu lu-decomposition) y &key &allow-other-keys)
  "Given an lu-decomposition lu and y, solve returns b such that LUb = y. ~
   If y is not given, then solve calls inverse on lu. ~
   (:see-also lud-of inverse)"
  (if (= 1.0 (+ 1.0 (eref (rcond-of lu))))
    (quail-error "Matrix is singular to working precision.")
    (let* ((a (lu-of lu))
           (ipvt (ipvt-of lu))
           (b (sel y))
           (job 0))
      (dgels a ipvt b job)
      b))
  )

(defmethod solve ((chol cholesky-decomposition) y &key &allow-other-keys)
  "Given a Cholesky-decomposition chol and y, solve returns b such that ~
   A .* tp(A) b = y. ~
   If y is not given, then solve calls inverse on chol. ~
   (:see-also cholesky-of inverse)"
  (if (and (rcond-of chol)
           (= 1.0 (+ 1.0 (eref (rcond-of chol)))))
    (quail-error "Matrix is singular to working precision.")
    (if (jpvt-of chol)
      ;; a pivoted cholesky
      (quail-error
       "Solve not yet implemented for a pivoted Cholesky decomposition.~
        ~&Try using an unpivoted decomposition first.")
      (let* ((a (upper-triangle-of chol)))
        (if y
          (backsolve a (backsolve a y :upper T)
                     :upper T
                     :transpose T)
          (inverse chol)))
      )))

(defmethod solve :around ((x matrix) (y identity-matrix)
                          &key
                          &allow-other-keys)
  (if (= (nrows x)
         (nrows y))
    (inverse x)
    (quail-error "Solve: Dimensions don't match ~&~
                  ~5Tx = ~s ~&~
                  ~5Ty = ~s."
                 x y)))


(defmethod solve :around ((x matrix) (y (eql NIL))
                          &key 
                          &allow-other-keys)
  (inverse x))

(defmethod solve ((x matrix) y &rest keyword-args &key (qr? NIL) &allow-other-keys)
  "Given X and y, solve returns two values, the first of which is ~
   b such that Xb = y. ~
   First if x is square and qr? NIL, ~
   an lu-decomposition is used to solve the system ~
   and the second value returned is NIL.~
   In the non-square case, the least squares solution (unpivoted) is found ~
   via a QR decomposition of X and returned as the first value.  ~
   The second value will be an instance of qr-solution which contains other ~
   information such as the residuals, etc. ~
   (:key ~
   (:arg qr? NIL If non-NIL, the solution is forced to be via a QR ~
   decomposition.  In this case the keyword arguments from qrd-of and ~
   solve from a qr-solution will be relevant.) ~
   )~
   (:rest (:arg keyword-args These are keyword arguments that might be ~
   used in the non-square case and are passed directly on to both ~
   qrd-of and solve with a qr-decomposition. ) ~
   )~
   (:see-also (solve :generic-function) (lud-of :generic-function) ~
   (qrd-of :generic-function) (qr-solution :class))"
  (if (and (not qr?) (= (nrows x) (ncols x)))
    (values (solve (lud-of x) y) NIL)
    (let* ((qrd (apply #'qrd-of X keyword-args))
           (pivot (pivoted-p qrd))
           (jpvt (jpvt-of qrd))
           coef
           qrs)
      (multiple-value-setq (coef qrs)
        (apply #'solve qrd y keyword-args))
      (if (and coef pivot)
        (setf coef (unpivot-in-place (sel coef) jpvt)))
      (values coef qrs)
      )
    )
  )
