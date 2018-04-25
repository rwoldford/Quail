;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        Element-wise predicate tests 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;   
;;;   In this file, we illustrate the use of the following functions
;;;   for refable objects:
;;;
;;;    =, <, <=, >, >=
;;;
;;;   And then show how these may be extended to new situations by writing
;;;   methods for the following generic functions:
;;;
;;;    equals-object, less-than-object, less-than-equals-object, 
;;;    greater-than-object, greater-than-equals-object, 
;;;    test-elements.
;;;
;;;
;;;
;;;   First, some arrays
;;;

(<- a (array (iseq 12) :dimensions '(4 3)))

(<- b (array (random-discrete-uniform :n 12 :from 0 :to 11)
             :dimensions '(4 3)))

(<- c (sel b))

(setf (ref c 0 T) '(100 200 300))


;;;
;;;  Equality testing.
;;;  Numerical equality is tested with the = predicate.
;;;  Returns T if the two arguments are numerically equal as in

(= 1 1.0)

;;;  or for multiple arguments:

(= 1 1.0 1e0 10/10)

;;;
;;;  This has been extended to handle arguments of arbitrary dimension
;;;  as in 

(= '(1 2 3) '(1.0 2.0 3.0) '(1/1 20/10 30/10))

;;;  or

(= '(1 2 3) '(2 2 3) '(3 2 1))

;;;  or

(= c b c)

;;;  You will notice that in each of these cases two values are returned.
;;;  The first is the logical result, T or NIL, of the predicate test.
;;;  The second is an object whose elements are T or NIL as corresponding
;;;  elements of the arguments satisfy the = predicate.
;;;
;;;  Both values can be captured using standard Common Lisp multiple-value
;;;  functions as for example in multiple-value-list

(<- results (multiple-value-list (= b c)))
(<- result1 (first results))
(<- result2 (second results))
result1
result2

;;;  If not all arguments have the same dimension then, as with the map-element
;;;  function, some values are reused.
;;;  For example,

(= 1 '(1 1 1))

;;;  returns NIL as its first value because the two arguments have
;;;  different numbers of elements.  In the second value the result
;;;  of comparing the value of the first argument with each element of the
;;;  second is recorded.
;;;  As with the map-element, in constructing the second value, elements
;;;  are re-used until all elements of the argument with the most elements
;;;  are exhausted.  The order of use is the same as map-element with
;;;  its order arg being 0.
;;;  The dimensions of the arguments must be commensurate.
;;;  (= '(1 2 3) '(1 2 3 4)) will fail but the following will not:

(= 1 b)

(= '(100 200 300)  c)

(= '(100 200 300 400)  c)

;;;  The ``shape'' of the second value is that of the argument having
;;;  the most elements.
;;;

(= 1 2 3 4 5 6 7)

;;;
;;;  Other numerical predicates operate in an analogous way.
;;;
;;;  <  ... Tests whether arguments are in increasing order.
;;;

(< 1 2 3 4)
(< 1 2 4 3)

