;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;               Some simple counting tools
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.

(in-package :quail-user)

;;;
;;;
;;;     In this file, the following functions are treated:
;;;
;;;      choose
;;;      factorial
;;;      log-n!
;;;     


;;;
;;;  (choose n k) -- the simple nCk ... the number of ways of choosing k
;;;                  items from n

(choose 6 4)
(choose 10 1)

;;;
;;;  (factorial n)  --- compute n! for positive integers n.

(factorial 6)

;;;
;;; That was computed exactly using integer arithmetic.
;;; The following is not

(factorial 33)

;;; but uses the log-gamma function instead and its answer only approximate.
;;; Exact values can be produced by either forcing integer-arithmetic as in

(factorial 33 :integer-arithmetic? T)

;;; or by changing the cut off above which the log-gamma function is used,

(factorial 33 :max-for-integers 40)

;;;
;;; The natural logarithm of n!

(log-n! 10)
(factorial 10)
(exp (log-n! 10))

