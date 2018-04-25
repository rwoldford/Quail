;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   intro.lsp
;;;
;;;
;;;  Authors:
;;;     D.G. Anglin 1994.

(in-package q-user)

;;;
;;;---------------------------------------------------------------------------
;;;; array:  the basic mechanism for creating arrays

;;; By default, array creates NUM-ARRAYS, which can hold extended numbers
;;; (ie. numbers plus the special symbols NAN [Not A Number], INFINITY,
;;; +INFINITY, -INFINITY).  If the array has <= 2 dimension axes, then
;;; an object of class MATRIX (a subclass of NUM-ARRAY) is created.

;;; a 2x3 array

(setf a (array '((100 101 102) (110 111 112))))

;;; a 3-vector

(setf b (array '(28 7 32)))

;;; a 3x1 matrix

(setf c (array '((10) (20) (30))))

;;; a 1x5 matrix

(setf d (array '(9 8 7 6 5) :dimensions '(1 5)))

;;; a 4x5x6 array of 7's

(setf e (array 7 :dimensions '(4 5 6)))

;;; arrays with elements which are NOT extended numbers should be declared
;;; to have class REF-ARRAY (so-called because it supports the function REF,
;;; which we will see shortly).

(setf days (array '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
                  :class 'ref-array))

;;; it is possible to create 0-dimensional arrays

(setf f (array 5 :dimensions '()))   ;;; ie.  :dimensions NIL

;;; more advanced uses of array in a later examples file ...

;;;---------------------------------------------------------------------------
;;;; dimensions-of

(dimensions-of a)

(dimensions-of b)

(dimensions-of c)

(dimensions-of d)

(dimensions-of e)

;;; applies to regular lisp objects, too

(dimensions-of (vector 1 2 3 4 5 6 7 8))

(dimensions-of 5)

(dimensions-of "foo")


;;;---------------------------------------------------------------------------
;;;; number-of-dimensions

(number-of-dimensions a)

(number-of-dimensions b)

(number-of-dimensions c)

(number-of-dimensions d)

(number-of-dimensions e)

;;; applies to regular lisp objects, too

(number-of-dimensions (vector 1 2 3 4 5 6 7 8))

(number-of-dimensions 5)

(number-of-dimensions "foo")

;;;---------------------------------------------------------------------------
;;;; number-of-elements

(number-of-elements a)

(number-of-elements b)

(number-of-elements c)

(number-of-elements d)

(number-of-elements e)

;;; applies to regular lisp objects, too

(number-of-elements (vector 1 2 3 4 5 6 7 8))

(number-of-elements 5)

(number-of-elements "foo")

;;;---------------------------------------------------------------------------
;;;; matrix-dimensions-of

(matrix-dimensions-of a)

;;; note that 1d things are, by default, columns ...

(matrix-dimensions-of b)

(matrix-dimensions-of c)

(matrix-dimensions-of d)

;;; the next is not an error, but is just the same as dimensions-of
;;; for arrays with more than 2 dimensions

(matrix-dimensions-of e)

(matrix-dimensions-of 5)

(matrix-dimensions-of "foo")

;;;---------------------------------------------------------------------------
;;;; eref:  array element access  ... NOTE EVERYTHING IS ZERO-BASED !!

;;;; eref stands for "element reference"

(eref b 0)

;; (eref b 3)      ;; an error .. only 0..2 are legitimate indices

(eref a 0 2)

;;; 1-dimensional objects like b can treated as 2-dimensional

(eref b 1 0)

(eref b 0 1)

;; (eref b 1 1)   ;; an error ...

;;; c and d however, were _defined_ to be 2-dimensional

(eref c 1 0)

;; (eref c 0 1)   ;;; an error ..

(eref d 0 4)

;; (eref d 4 0)   ;;; an error ..

;;; eref can also be used to access regular lisp objects

(eref (make-array '(4) :initial-contents '(0 1 2 3)) 2)

(eref '(100 200 300) 0)

(eref "foo" 1)

(eref 5)         ;;; just returns the number

(eref 'foo)      ;;; just returns the symbol

(eref f)         ;;; f is a 0-dimensional object, returns its contents

;;; can even get silly if you like ...

(eref "foo" 0 2)

;;; 0-dimensional things can be accessed with as many zero args as you like  ..

(eref :keyword 0 0 0 0 0)

;;; ... same for 1-dimensional ...

(eref b 0 0 0 0)

;;;; ... but it's an error beyond that (ambiguous)

;; (eref d 0 3 0)  ;;; error  .. could mean 0 3 or 3 0 ...

;;;---------------------------------------------------------------------------
;;;; (setf eref):  setting array elements

;;;; eref is an acceptable location specifier for setf
;;;; note that setf always returns the new-value, not the updated instance

(setf (eref a 1 1) 3000)
a

;;; 0-dimensional arrays can (setf eref)'d as well ... 

(setf (eref f) 17)

;;; but not 0-dimensional symbols and numbers

;; (setf (eref 5) 12)  ;;; an error

;;; note that strings *do* have dimensions for (setf eref), as for eref

(setf q "Quail")
(setf (eref q 3) #\y)
q

;;; to apply a setf operator, do
;;;
;;;    (apply #'(setf eref) new-value instance args)
;;;

(apply #'(setf eref) 1.23456 a '(0 1))
a

;;; similarly for funcall

(funcall #'(setf eref) 98765 a 1 2)
a


