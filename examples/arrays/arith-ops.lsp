;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Arithmetic operations on arrays
;;;
;;;
;;;  Authors:
;;;     D.G. Anglin 1994.

(in-package :quail-user)

(setf a (array '((1 3 5) (6 8 10))))

(setf b (array '((101.2 103.0 104.5) (206.7 208.9 200.1))))

(setf c (array '((100.0) (200.0))))

(setf d '(0 10 20 30 40 50 60 70 80))

(setf g (array '(1 2 3 4 5) :dimensions '(5 2 3) :fill :col))

;------------------------------------------------------------------------------

;;  Element-wise mathematical operations.

;;  +, -, *, / operate element-wise in extended real numbers (ie. numbers plus
;;  the special symbols NAN [Not A Number], INFINITY, +INFINITY, -INFINITY).
;;  These operations are all n-ary.

;;  Each operation attempts to find the most reasonable meaning of the operator
;;  in a given context.

;;  a and b have exactly the same shape, so they are added element-wise

(+ a b)

;; similarly for other operators

(* a b)

(* d d d)

;;  a has two rows, so the 1d object '(20 30) is treated as a column, and subtracted
;;  from each column

(- a '(20 30))

;;  a has three columns, so the 1d object '(100 200 300) is treated as a row, and
;;  each row is divided by it element-wise.  In an ambiguous case, a 1d object is
;;  a column.

(/ a '(100 200 300))

;;  the - and / operators can accept unary args

(- d)

;; we work in extended numbers ... illegal mathematical operations in the reals
;; will often produce successful results in the extended reals

(/ d)

(setf (eref b 0 1) nan)

b

(/ b a)

;; element-wise mathematical operators work on num-arrays as well

(* g (- 1 g))

;;;
;;;  Extending the functionality.
;;;
;;;  Each of the four operators + - / *  
;;;  are implemented using the generic functions
;;;  plus-object minus-object divides-object and times-object respectively.
;;;  Each takes two arguments representing the two operands of the mathematical
;;;  function.
;;;  To introduce these functions for new data types where + - / *
;;;  might have some meaning, you will need to implement the corresponding 
;;;  plus-object ... etc. for the class of arguments it is expected to receive.
;;;  If this new + etc. might be used as a unary function, then you will also need
;;;  to implement a plus-object etc whose first argument is :identity
;;;  and whose second argument is an object of the new class.

;;; The logical predicates > >= < <= = have not been implemented for arrays.
;;; However their implementation would proceed in exactly the same way by implementing
;;; methods
;;; greater-than-object greater-than-equals-object
;;; less-than-object less-than-equals-object and equals-object 

;------------------------------------------------------------------------------

;;  Min and Max.

(min 3 a)

(min (* 25 a) b)

(min +infinity 5 0.7 2000)

(max 5 nan)

(min -infinity NaN)

(max -infinity NaN)

;;;  These can be extended in the same way as other operators by
;;;  defining appropriate methods for min-object and max-object
;------------------------------------------------------------------------------

;;  Basic behaviour of the transpose function tp.

(setf at (tp a))

(tp (array '(3 4 5)))

;------------------------------------------------------------------------------

;;  Matrix multiplication and dot product
;;  ( A heuristic for remembering this operator is to think "dot" "product" )

(.* at a)

(.* at b)

;; this is the dot-product operator for 1d things of same size

(.* d d)

;; 1d objects are ALWAYS columns to .*

(setf f (array '((1 2) (3 4))))

(.* f '(10 20))

;; (.* '(10 20) f)    ;; an error

(.* (tp '(10 20)) f)

;------------------------------------------------------------------------------

;;  Advanced behavior of the transpose function tp.

(setf gt (tp g))

;; by default, the dimensions are the reverse of the original dimensions

(dimensions-of gt)

;; an equivalent specification

(setf gt (tp g :perm '(2 1 0)))

;; a different array

(setf gt2 (tp g :perm '(2 0 1)))

(dimensions-of gt2)



