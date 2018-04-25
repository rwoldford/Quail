;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               special-functions.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;
;;;-------------------------------------------------------------------------------
;;;
;;; This file contains the CL implementation of some special functions:
;;; log-gamma, factorial, gamma, beta, incomplete beta
;;;
;;; Code is a hand-coded translation of fortran code from Numerical Recipes
;;; Chapter 6, by Press, Flannery, Teukolsky, and Vetterling (1986).
;;;
;;;-------------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (export '(log-gamma fact log-n! choose beta gamma-inc)))




;;;------------------------------------------------------------------------------
;;;
;;; LOG-GAMMA
;;;
;;;------------------------------------------------------------------------------

(defun log-gamma (x)
  (declare (special pi))
  "Returns the natural logarithm og the gamma finction evaluated at x>0.  ~
   If 0<x<1, the reflection formula is used. Source: Numerical Recipes."
  (cond
   ((not (numberp x)) (quail-error "~s is not a number!" x))
   ((<= x 0) (quail-error "Argument must be positive: ~s" x))
   ((< x 1) (-  (+ (log pi) (log (- 1 x)))                ; Reflection formula
                (+ (log (sin (* pi (- 1 x))))             ; Eqn (6.1.4) in NR
                   (log-gamma (- 2 x)))))
   (t
    (let* ((coefs (make-array 6
                    :element-type 'double-float
                    :initial-contents
                    '(76.18009173D0 -86.50532033D0 24.01409822D0
                      -1.231739516D0 0.120858003D-2 -0.536382D-5)))
          (stp 2.50662827465D0)
          (z (- x 1D0))
          (temp1 (+ z 5.5D0))
          (temp (- (* (+ z 0.5D0)
                      (log temp1))
                   temp1)))
      (+ temp
         (log (* stp
                 (+ 1D0
                    (loop for i from 0 to 5
                       sum (progn ()
                              (setf z (+ z 1D0))
                              (/ (aref coefs i) z)))))))))))


;;;-------------------------------------------------------------------------------
;;;
;;; Factorial
;;; 
;;; Sets up a global-table to cache results <- not implemented yet!
;;;
;;;-------------------------------------------------------------------------------

#-:sbcl-linux(defconstant *table-of-factorials*
  (make-array 33 :initial-element 1))

#+:sbcl-linux(qk::define-constant *table-of-factorials*
  (make-array 33 :initial-element 1))

(defun fact (n &key (integer-arithmetic nil)
                 (max-for-integers 32))
  "Returns n! Source: Numerical Recipes."
  (if (or (not (integerp n)) (minusp n))
    (quail-error "Argument must be a non-negative integer: ~s" n)
    (labels ((int-arith-fact (k)
                (let ((result 1))
                  (loop for i from 1 to k
                     do (setf result (* i result)))
                  result)))
      (if (or integer-arithmetic (<= n max-for-integers))
        (int-arith-fact n)
        (exp (log-gamma (+ n 1)))))))


;;;-------------------------------------------------------------------------------
;;;
;;; Log-Factorial
;;;
;;; Sets up a table to cache results.
;;;
;;;-------------------------------------------------------------------------------


#-:sbcl-linux(defconstant *table-of-log-factorials*
  (make-array 100 :initial-element -1))

#+:sbcl-linux(qk::define-constant *table-of-log-factorials*
  (make-array 100 :initial-element -1))

(defun log-n! (n)
  "Returns log(n!) Source: Numerical Recipes."

  (if (or (not (integerp n)) (minusp n))
    (quail-error "Argument must be a non-negative integer: ~s" n))

  (if (< n (length *table-of-log-factorials*))
    (let ((table-value (aref *table-of-log-factorials* n)))
      (if (< table-value 0)
        (setf (aref *table-of-log-factorials* n) (log-gamma (+ n 1))))
      (aref *table-of-log-factorials* n))
    (log-gamma (+ n 1))))


;;;------------------------------------------------------------------------------
;;;
;;; Choose n k
;;;
;;; The binomial coefficient.
;;;
;;;------------------------------------------------------------------------------


(defun choose (n k)

  "Returns the binomial coefficient nCk or n!/(k! (n-k)!).  ~
   For small values it returns an integer, otherwise a floating point."

  (if (or (not (integerp n)) (minusp n))
    (quail-error "Argument must be a non-negative integer: ~s" n))
  
  (if (or (not (integerp k)) (minusp k) (> k n))
    (error
     "Second argument, ~s , must be a non-negative integer <= ~s" k n))
  
  (let ((result (exp (- (log-n! n) (log-n! k) (log-n! (- n k))))))
    (if (<= result most-positive-fixnum)
      (setf result (round result)))
    result))

 

;;;------------------------------------------------------------------------------
;;;
;;; Complete Beta Function
;;;
;;;        Beta(a,b)  = Gamma(a) * Gamma(b) / Gamma(a+b)
;;;
;;;------------------------------------------------------------------------------

(defun beta (a b)
  
  "Returns the value of the complete beta function B(a,b)."
  
  (exp (- (+ (log-gamma a) (log-gamma b))
          (log-gamma (+ a b)))))



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
;;; The incomplete gamma function.  This is just the cumulative
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

(defun gamma-inc (a x)
  ())

    
  
