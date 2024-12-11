;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               factorial.lisp                              
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
;;; factorial, log-n!
;;;
;;; Code is a hand-coded translation of fortran code from Numerical Recipes
;;; Chapter 6, by Press, Flannery, Teukolsky, and Vetterling (1986).
;;;
;;;-------------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(factorial log-n!)))


;;;-------------------------------------------------------------------------------
;;;
;;; Factorial
;;; 
;;; Sets up a global-table to cache results <- not implemented yet!
;;;
;;;-------------------------------------------------------------------------------

#-:sbcl-linux(defconstant +table-of-factorials+
  (make-array 33 :initial-element 1))

#+:sbcl-linux(qk::define-constant +table-of-factorials+
  (make-array 33 :initial-element 1))

(defun factorial (n &key
                    (integer-arithmetic? nil)
                    (max-for-integers 32))
  "Returns the integer n! This is calculated using integers by ~
   forming the multiplication.  ~
   The answer will be exact and will not overflow.  ~
   Alternatively, the log-gamma function ~
   is called and exponentiated giving only an approximate answer.~
   (:required (:arg n A positive integer.)) ~
   (:key ~
   (:arg integer-arithmetic? NIL If non-NIL integer-arithmetic will ~
   be used and because of bignums will not overflow.)  ~
   (:arg max-for-integers 32 If the argument n exceeds this value and ~
   integer-arithmetic? is NIL then the floating point approximation will be ~
   used.  Otherwise exact integer arithmetic is used.) ~
   )~
   (:see-also log-gamma log-n! choose)~
   "
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline log-gamma))
  (if (or (not (integerp n)) (minusp n))
    (quail-error "Argument must be a non-negative integer: ~s" n)
    (labels ((int-arith-fact (k)
               (let ((result 1))
                 (loop for i from 1 to k
                       do (setf result (* i result)))
                 result)))
      (if (or integer-arithmetic? (<= n max-for-integers))
        (int-arith-fact n)
        (exp (log-gamma (+ n 1)))))))


;;;-------------------------------------------------------------------------------
;;;
;;; Log-Factorial
;;;
;;; Sets up a table to cache results.
;;;
;;;-------------------------------------------------------------------------------


#-:sbcl-linux(defconstant +table-of-log-factorials+
  (make-array 100 :initial-element -1))

#+:sbcl-linux(qk::define-constant +table-of-log-factorials+
  (make-array 100 :initial-element -1))

(proclaim '(sb-ext:maybe-inline log-n!)) ;24NOV2024
(defun log-n! (n)
  "Returns log(n!) Source: Numerical Recipes. ~
   (:see-also log-gamma n!)"
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline log-gamma))
  (if (or (not (integerp n)) (minusp n))
    (quail-error "Argument must be a non-negative integer: ~s" n))
  
  (if (< n (length +table-of-log-factorials+))
    (let ((table-value (aref +table-of-log-factorials+ n)))
      (if (< table-value 0)
        (setf (aref +table-of-log-factorials+ n) (log-gamma (+ n 1))))
      (aref +table-of-log-factorials+ n))
    (log-gamma (+ n 1))))

