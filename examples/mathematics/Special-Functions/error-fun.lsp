;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Error function
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.

(in-package :quail-user)

;;;
;;; In this file we consider the special functions:
;;;
;;;      error-function                ... the error function erf(x)
;;;      error-function-complement     ... the complement erfc(x) of the error
;;;                                        function 1 - erf(x)
;;;
;;;  See also the overview file for other special functions.

(edit-file "eg:Mathematics;Special-Functions;overview.lsp")

;;;---------------------------------------------------------------------------------
;;;
;;;  The Error Function
;;;
;;;---------------------------------------------------------------------------------
;;;
;;;
;;;                         x     2
;;;               2        /    -t
;;;   erf(x) = --------   /   e     dt        
;;;            sqrt(pi)  /
;;;                     0
;;;
;;;  Some properties:
;;;
;;;   erf(0) = 0                erfc(0) = 1
;;;   erf(infinity) = 1         erfc(infinity) = 0
;;;   erf(-x)  = -erf(x)        erfc(-x) =  2 - erfc(x)
;;;  
;;;
;;;  These are related to the incomplete gamma functions as
;;;
;;;       erf(x)  = P(1/2, x^2)      x >= 0
;;;       erfc(x) = Q(1/2, x^2)      x >= 0
;;;
;;;  and to Gaussian(0, 1) or Normal(0,1) probabilities as
;;;
;;;  Prob(|z| < a)  = erf(a / sqrt(2))    where z ~ N(0,1)
;;;
;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Call
;;; 
;;;  (error-function x :max-iterations 100 :epsilon 1.0D-7)
;;;
;;;  Returns the error function erf(x).
;;;
;;; Call
;;; 
;;;  (error-function-complement x :max-iterations 100 :epsilon 1.0D-7)
;;;
;;;  Returns the error function erfc(x) = 1 - erf(x).
;;;

(error-function 0)

;;;
;;;  And for very large values of x (note that + or -infinity are not acceptable.
;;;

(error-function 1000000)

;;; Other properties

(error-function 3)
(error-function -3)

(error-function-complement 0)
(error-function-complement 1000000)
(error-function-complement -3)
(- 2 (error-function-complement 3))

;;; relation to incomplete gamma funs.

(error-function 3)
(incomplete-gamma 0.5 9)

(error-function-complement 3)
(incomplete-gamma-complement 0.5 9)

;;; and to the gaussian calculations

(- (dist-gaussian 1.645) (dist-gaussian -1.645))

(error-function (/ 1.645 (sqrt 2)))

