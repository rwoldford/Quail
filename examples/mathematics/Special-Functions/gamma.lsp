;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Gamma functions
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.

(in-package :quail-user)

;;;
;;; In this file we consider the special functions:
;;;
;;;      gamma                         ... the complete gamma function
;;;      log-gamma                     ... the natural log of the gamma function
;;;      incomplete-gamma              ... the incomplete-gamma function
;;;      incomplete-gamma-complement   ... the complement of the incomplete-gamma
;;;
;;;  See also the overview file for other special functions.

(edit-file "eg:Mathematics;Special-Functions;overview.lsp")

;;;------------------------------------------------------------------------------
;;;
;;; Complete Gamma Function
;;;
;;; 
;;;                            +infinity
;;;                           /
;;;             Gamma(a) =   /   exp(-t) t^(a-1) dt
;;;                         /
;;;                        0
;;;
;;;  for arbitrary a > 0.
;;;
;;;  Some properties are
;;;
;;;     Gamma(n) = (n-1) * Gamma(n-1)
;;;     Gamma(1) = 1
;;;     Gamma(n+1) = n! for non-negative integer n 
;;;     Gamma(1/2) = sqrt(pi)

(gamma 4)
(factorial 3)
(expt (gamma 1/2) 2)
pi

;;; Natural log of the gamma function.  It's usually more stable
;;; to work with the log of the gamma function.
;;;

(log-gamma 4)

;;;------------------------------------------------------------------------------
;;;
;;; Incomplete Gamma Function
;;;
;;; Implementation relies on both a series expansion and a continued
;;; fraction expansion for the incomplete gamma and its complement,
;;; respectively.
;;;
;;; Here we call
;;;                                    x
;;;                         1        /
;;;             P(a,x) = --------   /   exp(-t) t^(a-1) dt
;;;                      gamma(a)  /
;;;                                0
;;;
;;; the incomplete gamma function.  This is just the cumulative
;;; distribution function of a gamma random variable with parameter a.
;;; The notation follows that of Numerical Recipes.
;;; 
;;; The complement of P is Q(a,x) = 1 - P(a,x)
;;;
;;; There exists a series expansion for P(a,x)*gamma(a)
;;; And a continued-fraction expansion for Q(a,x)*gamma(a)
;;; 
;;; Most importantly, their regions of convergence are complementary
;;; so an accurate incomplete gamma function is had by using
;;; each where appropriate.
;;;
;;;---------------------------------------------------------------------------------
;;;
;;;  Call
;;;
;;; (incomplete-gamma  a x :epsilon 1.0D-7 :max-iterations 100)
;;;
;;;  Returns two values.  The first is the incomplete gamma function P(a,x)
;;;  defined to be the cumulative distribution function of a gamma random variable
;;;  with shape parameter a. 
;;;  The second is log-gamma(a) + log (the first) and is 
;;;  sometimes denoted as the natural log of lower-case-gamma(a,x).
;;;
;;;  Epsilon and max-iterations are convergence parameters.

(incomplete-gamma 3 6)

(+ (log-gamma 3) (log (incomplete-gamma 3 6)))

;;;  Call
;;;
;;; (incomplete-gamma-complement a x :epsilon 1.0D-7 :max-iterations 100)
;;;
;;;  Returns two values.  The first is the complement of the incomplete gamma function,
;;;  namely 1 - P(a,x), where P(a,x) is the incomplete gamma function that is
;;;  defined to be the same as the cumulative distribution function of a gamma 
;;;  random variable with shape parameter a.  
;;;  The second is gamma(a) * (1 - P(a,x)) and is sometimes denoted as
;;;  upper-case-gamma(a,x).
;;;
;;;  As before, epsilon and max-iterations are convergence parameters.
  
(incomplete-gamma-complement 3 6)
(* (gamma 3) (incomplete-gamma-complement 3 6))

;;; compare
(incomplete-gamma 3 6)
(- 1 (incomplete-gamma-complement 3 6))


