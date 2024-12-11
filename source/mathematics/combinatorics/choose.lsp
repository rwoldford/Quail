;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               choose.lisp                              
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
;;; This file contains the CL implementation of the binomial coefficient
;;;
;;;                          n!
;;;  (choose n k)  <=>  ------------  .
;;;                       k! (n-k)!
;;;
;;;-------------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(choose)))


;;;------------------------------------------------------------------------------
;;;
;;; Choose n k
;;;
;;; The binomial coefficient.
;;;
;;;------------------------------------------------------------------------------

(proclaim '(sb-ext:maybe-inline choose)) ;24NOV2024
(defun choose (n k)
  
  "Returns the binomial coefficient nCk or n!/(k! (n-k)!).  ~
   For small values it returns an integer, otherwise a floating point."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline log-n!))
  
  (if (or (not (integerp n)) (minusp n))
    (quail-error "Argument must be a non-negative integer: ~s" n))
  
  (if (or (not (integerp k)) (minusp k) (> k n))
    (error
     "Second argument, ~s , must be a non-negative integer <= ~s" k n))
  
  (let ((result (exp (- (log-n! n) (log-n! k) (log-n! (- n k))))))
    (if (<= result most-positive-fixnum)
      (setf result (round result)))
    result))
