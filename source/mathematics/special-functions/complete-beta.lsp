;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               complete-beta.lisp                              
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
;;; This file contains the CL implementation of some special functions:
;;; log-gamma, factorial, gamma, beta, incomplete beta
;;;
;;; Code is a hand-coded translation of fortran code from Numerical Recipes
;;; Chapter 6, by Press, Flannery, Teukolsky, and Vetterling (1986).
;;;
;;;-------------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(beta)))

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
