;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Beta functions
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.

(in-package :quail-user)

;;;
;;; In this file we consider the special functions:
;;;
;;;      beta                          ... the complete beta function B(a,b)
;;;      incomplete-beta               ... the incomplete beta function Ix(a,b)
;;;
;;;  See also the overview file for other special functions.

(edit-file "eg:Mathematics;Special-Functions;overview.lsp")


;;;------------------------------------------------------------------------------
;;;
;;; Complete Beta Function
;;;
;;;        B(a,b)  = Gamma(a) * Gamma(b) / Gamma(a+b)
;;;
;;;
;;;------------------------------------------------------------------------------

(beta 10 11)

(exp (- (+ (log-gamma 10) (log-gamma 11))
        (log-gamma 21)))

;;;----------------------------------------------------------------------------------
;;;
;;;  Incomplete beta function Ix(a,b) is the value of the cumulative distribution
;;;  function of a beta(a,b) random variable evaluated at the point x.
;;;
;;;                          x
;;;                  1      /   a-1      b-1
;;;  Ix(a,b)  =  --------  /   t    (1-t)    dt           (a,b > 0)
;;;               B(a,b)  /
;;;                       0
;;;
;;;  0 < x <= 1
;;;
;;;-----------------------------------------------------------------------------------
;;;
;;;  Call
;;;
;;; (incomplete-beta a b x :max-iterations 100 :epsilon 3.0D-7)
;;;
;;;  Returns the value of the cumulative distribution function of a beta(a,b) random 
;;;  variable evaluated at the point x.
;;;  That is the value of the incomplete beta function Ix(a,b).

(incomplete-beta 1 1 0.5)
(dist-beta 0.5 :a 1 :b 1)
(quantile-beta .5 :a 1 :b 1)

(incomplete-beta 2 3 0.40)
(dist-beta 0.40 :a 2 :b 3)
(quantile-beta .52 :a 2 :b 3)
