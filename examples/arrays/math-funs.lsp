;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Mathematical functions
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.

(in-package :quail-user)

(setf a (array '((1.2 3 4.5) (6.7 8.9 0.1))
               :dimensions '(2 3)))

;;;
;;; In addition to the usual arithmetic operators
;;; All of the Common-Lisp mathematical functions have been extended
;;; to work on arrays.
;;; One notable exception is boole
;;;

;;; The functions which have been extended may be on the following symbols:

*logical-numerical-ops*

*numerical-type-coercion-functions*

*extended-simple-functions*

;;;
;;;  The functions listed on these symbols are as described in
;;;  the Common Lisp Manual.  They now take arrays and lists as arguments
;;;  as well.
;;;
;;;  Some examples

(loop for fn in *logical-numerical-ops*
      do (format *terminal-io* "~&(~s  ....)" fn))

(<- x (array '(0 0 1 1)))
(<- y (array '(0 1 0 1)))

(logior  x y)
(logxor  x y)
(logand  x y)
(logeqv  x y)
(lognand  x y)
(lognor  x y)
(logandc1  x y)
(logandc2  x y)
(logorc1  x y)
(logorc2  x y)
(lognot  x)
(logtest  x y)
(logbitp  x y)
(ash  x y)
(logcount  x)
(integer-length x)


(loop for fn in *numerical-type-coercion-functions*
      do (format *terminal-io* "~&(~s  a)" fn))


(float  a)
(rational  a)
(rationalize  a)
(complex  a)



(loop for fn in *extended-simple-functions*
      do (format *terminal-io* "~&(~s  a)" fn))

(exp  a)
(sqrt  a)
(isqrt  (ceiling a))
(abs  (- a))
(phase  a)
(signum  a)
(sin  a)
(cos  a)
(tan   a)
(cis   a)
(asin   a)
(acos   a)
(atan   a)
(sinh   a)
(cosh   a)
(tanh   a)
(asinh   a)
(acosh   a)
(atanh   a)
(numerator   (rationalize a))
(denominator   (rationalize a))
(realpart   (complex a))
(imagpart   a)
(floor   a)
(ceiling   a)
(truncate   a)
(round  a)
(ffloor   a)
(fceiling   a)
(ftruncate   a)
(fround   a)
(expt   a a)
(log   a)
(rem   a a)
(mod   a 1)
(gcd   (floor a) (ceiling a))
(lcm   (floor a) (ceiling a))
(conjugate   (sqrt (- a)))




