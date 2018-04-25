;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           error-function.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;-------------------------------------------------------------------------------
;;;
;;;


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(error-function error-function-complement)))

;;;---------------------------------------------------------------------------------
;;;
;;;  The Error Function
;;;
;;;---------------------------------------------------------------------------------

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
;;;  and to Gaussian or Normal(0,1) probabilities as
;;;
;;;  Prob(|z| < a)  = erf(a / sqrt(2))    where z ~ N(0,1)
;;;

(defun error-function 
       (x &key (max-iterations 100) (epsilon 1.0D-7))
  "Returns the error function erf(x). Calculated by incomplete-gamma.~
   (:required (:arg x A real valued number)) ~
   (:key ~
   (:arg max-iterations 100 Maximum number of iterations before ~
   divergence of the incomplete-gamma is declared.) ~
   (:arg epsilon 1.0D-7 Convergence criterion for incomplete gamma calculation.) ~
   ) ~
   (:see-also (error-function-complement :function) ~
   (incomplete-gamma :function) ~
   ) ~
   (:examples ~
   (:files (Error function ~
   eg:Mathematics;Special-Functions;error-fun.lisp ~
   ) ~
   ) ~
   )"
  (declare (type number x)
           (type fixnum max-iterations)
           (type float epsilon)
           (optimize  (speed 3)
                      (safety 3)))
  (cond
   ((= x 0) 0)
   ((< x 0) (- (incomplete-gamma 0.5 (* x x)
                                 :epsilon epsilon
                                 :max-iterations max-iterations)))
   (T (values (incomplete-gamma 0.5 (* x x)
                                :epsilon epsilon
                                :max-iterations max-iterations)))))

(defun error-function-complement 
       (x &key (max-iterations 100) (epsilon 1.0D-7))
  "Returns the error function erfc(x) = 1 - erf(x).  ~
   Calculated by incomplete-gamma.~
   (:required (:arg x A real valued number)) ~
   (:key ~
   (:arg max-iterations 100 Maximum number of iterations before ~
   divergence of the incomplete-gamma is declared.) ~
   (:arg epsilon 1.0D-7 Convergence criterion for incomplete gamma calculation.) ~
   ) ~
   (:see-also (error-function :function) ~
   (incomplete-gamma :function) ~
   (incomplete-gamma-complement :function) ~
   ) ~
   (:examples ~
   (:files (Error function ~
   eg:Mathematics;Special-Functions;error-fun.lisp ~
   ) ~
   ) ~
   )"
  (declare (type number x)
           (type fixnum max-iterations)
           (type float epsilon)
           (optimize  (speed 3)
                      (safety 3)))
  (cond
   ((= x 0) 1)
   ((< x 0) (+ 1.0 (incomplete-gamma 0.5 (* x x)
                                     :epsilon epsilon
                                     :max-iterations max-iterations)))
   (T (values (incomplete-gamma-complement 0.5 (* x x)
                                   :epsilon epsilon
                                   :max-iterations max-iterations)))))
