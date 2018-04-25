;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;               Factoring integers and related tools
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.

(in-package :quail-user)

;;;
;;;
;;;     In this file, the following functions are treated:
;;;
;;;      coprimep
;;;      dividesp
;;;      factor
;;;      smallest-divisor
;;;
;;;   as well as the common lisp functions
;;;    
;;;      gcd
;;;      lcm
;;;      mod
;;;      rem
;;;     


;;;
;;;  (coprimep n k) -- predicate test whether positive integers n and k
;;;                    share any common divisors.

(coprimep 10 15)
(coprimep 7 7)
(coprimep 5 7)

;;;
;;;  (dividesp n k) -- predicate test whether the positive integer n 
;;;                    divides evenly into the positive integer k.

(dividesp 10 15)
(dividesp 5 15)
(dividesp 15 5)
(dividesp 7 7)
(dividesp 5 7)

;;;
;;;  (factor n) -- factor the natural number n into its prime factors

(factor 6)

(factor 314159265)

;;;
;;;  (smallest-divisor n) -- finds and returns the smallest divisor of n
;;;                          that is greater than or equal to the value
;;;                          of the keyword argument from

(smallest-divisor 6)
(smallest-divisor 6 :from 2)
(smallest-divisor 6 :from 3)
(smallest-divisor 6 :from 4)
(smallest-divisor 6 :from 1)


;;;
;;;  (gcd k m n) -- finds the greatest common divisor of the
;;;                 integers k m n

(gcd 10 15 20)
(gcd -10 -15 -20)

;;;
;;;  (lcm k m n) -- finds the least positive common multiple of the
;;;                 integers k m n

(lcm 10 15 20)
(lcm -10 -15 -20)

;;;
;;;  (mod n m) --  n mod m ... computes the modulus of n with respect to
;;;                the base divisor m.
;;;

(mod 10 15)
(mod 15 10)
(mod -15 10)

;;; positive base:

(loop for i from -5 to 5
      do (format *quail-terminal-io*
                 "~& (mod ~s -2) = ~s " i (mod i 2)))

;;; negative base:

(loop for i from -5 to 5
      do (format *quail-terminal-io*
                 "~& (mod ~s -2) = ~s " i (mod i -2)))


;;;
;;;  (rem n m) --  computes the remainder of n after division by m.
;;;

(rem 10 15)
(rem 15 10)
(rem -15 10)
