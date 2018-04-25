;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          eigen.lsp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Carsten Whimster 1997.
;;;
;;;-----------------------------------------------------------------------------

;;; This is a file demonstrating the use of the eigen value decomposition.
;;;
;;; Note that the array A must be real symmetric.

(<- A (array '((1.0  2.0  3.0)
               (2.0  3.0  5.0)
               (3.0  5.0  4.0))))

(<- V (eigen-vectors-of A))

;;; The eigen values and vectors are now cached on A, as these were automatically
;;; calculated above. Consequently the following two operations work on already
;;; existing data, and don't use any computational resources.

(<- D (eigen-values-of A))
(.* V (diagonal D) (tp V))      ;; should return A
(inspect (eigen-of A))
(inverse (eigen-of A))

;;; Now we can reference it as often as we like without redoing the calculations.

;;; Here is a larger example which demonstrates the accuracy for ill conditioned
;;; matrices. First we set the random seed so that our results are repeatable. In
;;; this example, the eigen values are actually the numbers that are sent in, so
;;; we can compare the output to the input for accuracy. Note that the sequence
;;; is not important here.

(setf (qk::current-value-of
       qk::*default-random-number-generator*) 616053555)

(setf pre1 (diagonal '(100000 100000 100000 100000 100000
                         100000      1      1      1      1)))
(setf pre2 (array (random-gaussian :n 100) :dimensions '(10 10)))
(setf pre3 (right-singular-vectors-of pre2))
(setf B (.* pre3 pre1 (tp pre3)))

(setf eigen (eigen-of B))
(eigen-values-of eigen)

;;; Here is the output (note that it is sorted in descending order):
;;;
;;; #1m(100000.00000000022 100000.00000000017
;;;     100000.00000000007 100000.00000000006
;;;      99999.99999999999 99999.99999999997
;;;     1.0000000000147449 1.0000000000099358
;;;     0.9999999999945548 0.9999999999911166)

;;; Finally, we can access the function behind the eigen decomposition directly.
;;; This returns the array A, the vector of eigen values, and the matrix of eigen
;;; vectors.

(<- D (array 0.0 :dimensions '(3)))
(<- V (array 0.0 :dimensions '(3 3)))
