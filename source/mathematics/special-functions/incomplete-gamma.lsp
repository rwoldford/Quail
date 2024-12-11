;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               incomplete-gamma.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Dan 
;;;     R.W. Oldford 1992.
;;;
;;;
;;;-------------------------------------------------------------------------------
;;;
;;; This file contains the CL implementation of the special function:
;;; incomplete gamma.
;;;
;;; Code is a hand-coded translation of fortran code from Numerical Recipes
;;; Chapter 6, by Press, Flannery, Teukolsky, and Vetterling (1986).
;;;
;;;-------------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(incomplete-gamma incomplete-gamma-complement)))


;;;------------------------------------------------------------------------------
;;;
;;; Incomplete Gamma Function
;;;
;;; Implementation relies on both a series expansion and a continued
;;; fraction expansion for the incomplete gamma and its complement,
;;; respectively.
;;;
;;; Here (as in NR) we call
;;;                                    x
;;;                         1        /
;;;             P(a,x) = --------   /   exp(-t) t^(a-1) dt
;;;                      gamma(a)  /
;;;                                0
;;;
;;; the incomplete gamma function.  This is just the cumulative
;;; distribution function of a gamma random variable with parameter a.
;;; 
;;; The complement of P is Q(a,x) = 1 - P(a,x)
;;;
;;; There exists a series expansion for P(a,x)*gamma(a)
;;; And a continued-fraction expansion for Q(a,x)*gamma(a)
;;; 
;;; Most importantly, their regions of convergence are complementary
;;; so an accurate incomplete gamma function can be had by using
;;; each where appropriate.
;;;
;;;---------------------------------------------------------------------------------

(proclaim '(sb-ext:maybe-inline incomplete-gamma)) ;24NOV2024
(defun incomplete-gamma
       (a x &key (epsilon 1.0D-7) (max-iterations 100))
  "Returns two values.  The first is the incomplete gamma function P(a,x) ~
   defined to be the cumulative ~
   distribution function of a gamma random variable with shape parameter a.  ~
   The second is log-gamma(a) + log (P(a,x)) and is ~
   sometimes denoted as the natural log of lower-case-gamma(a,x)."

  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  
  
  (cond
   ((or (not (numberp x))
        (< x 0.0))
    (error "The value of the second argument, x, must be a number greater than or ~
            equal to 0, not ~s!"
           x))
   ((or (not (numberp a))
        (<= a 0.0))
    (error "The parameter gamma shape-parameter, a, must be a number greater than 0, ~
            not ~s!"
           a)))
  
  (if (= x 0.0)
    (values 0.0 q::-infinity)
    (if (< x (+ a 1))
      ;; Use the series representation.
      (let ((results
             (multiple-value-list (gamma-series-approximation
                                   a x
                                   :epsilon epsilon
                                   :max-iterations max-iterations))))
        (values
         (first results)
         (+ (log (first results)) (second results))
         ))
      
      ;; Else use the continued-fraction representation of the complementary gamma.
      (let* ((results
              (multiple-value-list (gamma-continued-fraction-approximation
                                    a x
                                    :epsilon epsilon
                                    :max-iterations max-iterations)))
             (inc-gamma-1 (- 1.0 (first results)))
             (inc-gamma-2  (+  (log inc-gamma-1) (second results))
                           )
             )
        
        (values inc-gamma-1 inc-gamma-2)))))

(defun incomplete-gamma-complement
       (a x &key (epsilon 1.0D-7) (max-iterations 100))
  "Returns two values.  The first is the complement of the incomplete gamma function, ~
   namely 1 - P(a,x), where P(a,x) is the incomplete gamma function that is ~
   defined to be the same as the cumulative ~
   distribution function of a gamma random variable with shape parameter a.  ~
   The second is gamma(a) * (1 - P(a,x))  and is ~
   sometimes denoted as upper-case-gamma(a,x)."
  

  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  
  (cond
   ((or (not (numberp x))
        (< x 0.0))
    (error "The value of the second argument, x, must be a number greater than or ~
            equal to 0, not ~s!"
           x))
   ((or (not (numberp a))
        (<= a 0.0))
    (error "The parameter gamma shape-parameter, a, must be a number greater than 0, ~
            not ~s!"
           a)))
  
  (if (< x (+ a 1))
    ;; Use the series representation for the incomplete gamma and complement.
    (let* ((results
            (multiple-value-list (gamma-series-approximation
                                               a x
                                               :epsilon epsilon
                                               :max-iterations max-iterations)))
           (gamma-1 (- 1.0 (first results)))
           (gamma-2 (* gamma-1 (exp (second results)))))
      (values gamma-1 gamma-2))
      
    ;; Else use the continued-fraction representation of the complementary gamma.
    (let ((results
           (multiple-value-list (gamma-continued-fraction-approximation
                                               a x
                                               :epsilon epsilon
                                               :max-iterations max-iterations))))
      (values
       (first results)
       (* (first results) (exp (second results)))))))


(defun gamma-series-approximation (a x &key (max-iterations 100)
                                            (epsilon 3.0D-7))
  "Returns two values.  The first is the incomplete gamma function, ~
   defined to be the same as the cumulative ~
   distribution function of a gamma random variable with shape parameter a.  ~
   Evaluation is via its series representation.  Converges rapidly ~
   when x < a + 1. ~
   The second is the natural log of gamma(a)."
  
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )

  (let ((ln-gamma-a (log-gamma a))
        (gamma-series 0.0))
    (if (or (not (numberp x))
            (< x 0))
      (error "The value of the second argument, x, must be a number greater than or ~
                   equal to 0, not ~s!"
                  x))
    (if (not (zerop x))
      (let* ((sum (/ 1.0 a))
             (delta sum)
             (ap a))
        (loop for iteration-no from 1 by 1
              until (< (abs delta) (* epsilon (abs sum)))
              do 
              (setf ap (+ ap 1))
              (setf delta (/ (* delta x) ap))
              (setf sum (+ sum delta))
              (when (> iteration-no max-iterations)
                (error "Either the first argument, a = ~s is too large ~
                        or the max-iterations = ~s is too small."
                       a max-iterations)))
        (setf gamma-series (* sum
                              (exp (- (* a (log x)) x ln-gamma-a))))))
    (values gamma-series ln-gamma-a)))



(defun gamma-continued-fraction-approximation
       (a x &key (max-iterations 100) (epsilon 3.0D-7))
  "Returns two values.  The first is the complement of the incomplete gamma function, ~
   1 - P(a,x), where P(a,x) is defined to be the same as the cumulative ~
   distribution function of a gamma random variable with shape parameter a.  ~
   Evaluation is via a continued fraction expansion.  Converges rapidly when ~
   x > a + 1.  ~
   The second is the natural log of gamma(a)."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  
  (let ((ln-gamma-a (log-gamma a))
        (gamma-cf 1.0))
    
    ;; Error checking
    (if (or (not (numberp x))
            (< x 0))
      (error "The value of the second argument, x, must be a number greater than or ~
                   equal to 0, not ~s!"
                  x))
    
    ;; Now the approximation.
    (if (not (zerop x))
      
      ;; otherwise we do the continued fraction expansion.
      (let*
        ;; First a parameter to denote the standard to test against for convergence
        ((gold 0.0)
         ;; Successive approximants, gn  in the continued fraction expansion
         ;; are evaluated by using two 3 term recurrence relations (see num recipes)
         (gn gold)
         (a0 1.0)
         (a1 x)
         (b0 0.0)
         (b1 1.0)
         ;; the renormalization factor that prevents overflow of the partial
         ;; numerators and partial denominators.
         (factor 1.0))
        
        (let (n-a n*fac)
          (loop for n from 1 by 1
              do
              ;; Check on iteration count.
              (if (> n max-iterations)
                (error "Either the first argument, a = ~s is too large ~
                        or the max-iterations = ~s is too small."
                       a max-iterations))
              
              ;; One step of the recurrence
              (setf n-a (- n a))
              (setf a0 (* (+ a1 (* a0 n-a)) factor))
              (setf b0 (* (+ b1 (* b0 n-a)) factor))
              
              ;; Next step of recurrence
              (setf n*fac (* n factor))
              (setf a1 (+ (* x a0) (* n*fac a1)))
              (setf b1 (+ (* x b0) (* n*fac b1)))
              
              ;; Should we renormalize?
              (when (not (zerop a1))
                (setf factor (/ 1.0 a1))
                (setf gn (* b1 factor))
                
                ;; Check convergence
                (when (< (abs (/ (- gn gold) gn))
                         epsilon)
                  ;; Converged, so put factors in front
                  (setf gamma-cf (* (exp (- (* a (log x)) 
                                            x
                                            ln-gamma-a))
                                    gn))
                  ;; Return from loop
                  (return))
                
                ;; Not converged, so update the gold standard.
                (setf gold gn))
              )
          )
        )
      )
    ;; we're done 
    (values gamma-cf ln-gamma-a)
    )
  )


#|
(defun incomplete-gamma-cf
       (alpha x  &key (max-iterations 100) (epsilon 1.0D-7))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (flet ((den-fun (y j)
          (declare (type number y) (type integer j))
          (cond
           ((= j 0) 0.0)
           ((oddp j) y)
           ((evenp j) 1)))
         (num-fun (y j)
          (declare (ignore y) (type integer j))
          (cond 
           ((= j 1) 1.0)
           ((oddp j) (/ (- j 1) 2))
           )
          ((evenp j) (- (+ 1 (/ (- j 2) 2)) alpha)
           )))
  (funcall 
   (continued-fraction-eval (function num-fun) (function den-fun))
     x :max-iterations max-iterations :epsilon epsilon)))
  

(defun incomplete-gamma (alpha x &key (max-iterations 100) (epsilon 1.0D-7))
  "The incomplete gamma function.  This is just the cumulative ~
   distribution function of a gamma random variable with parameter alpha."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  
  (let((ans 0)                                                                                                                
       (old-ans 0))
    (cond 
     ((< x 0) 0.0)
     ((< x (+ 1 alpha))
      (/ (* (exp (- 0 x))
            (expt x alpha)
            (loop for n from 0 by 1
                  do (setf ans (+ old-ans (/ (* (exp (log-gamma alpha))
                                                (expt x n))
                                             (exp (log-gamma (+ alpha 1 n))))))                                                  
                  (if (< (abs (- ans old-ans))
                         epsilon )
                    (return ans))
                  (if (> n max-iterations)
                    (error "Failed to converge"))
                  (setf old-ans ans)))
         (exp (log-gamma alpha))))
     ((>= x (+ 1 alpha))
      (- 1 (/ (* (incomplete-gamma-cf alpha x
                                      :epsilon epsilon
                                      :max-iterations max-iterations)
                 (exp (- 0 x))
                 (expt x alpha))
              (exp (log-gamma alpha)))))
     )))
|#
    
  
