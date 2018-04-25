;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Simple iteration macros for slices                             
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
;;;;;;;;;;;;;;;;;;;;;
;;;


(in-package :quail-user)

(setf a (array '((1.2 3 4.5) (6.7 8.9 0.1))
                :dimensions '(2 3)))

(setf b (array (seq 1 24) :dimensions '(2 3 4)))  ;; 2 rows, 3 cols, 4 layers

;;;   
;;;   In this file, we illustrate the use of the following iteration
;;;   macros for refable objects:
;;;       ... doslices
;;;           collect-slices
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  DOSLICES
;;;
;;;
;;;  The general syntax of doslices macro is
;;;
;;;  (doslices (slice-name refable-object slices return-form order)
;;;                                     ;;^^^^^^^^^^^^^^^^^^^^^^^^
;;;                                     ;;last three are optional
;;;      body-form-1 body-form-2 .... body-form-n)
;;;
;;;  slice-name is bound to the current slice of the refable-object
;;;  and the body-forms 1 to n are executed in sequence at each iteration.
;;;  Each slice is the sub array of refable object corresponding to a single
;;;  fixed value for all dimensions identified by the list slices.
;;;  If slices is omitted or eq :elements then iteration is over all elements
;;;  and slice-name refers to the actual element as opposed to a ref to it.
;;;  If slices is NIL, then a single iteration is done where the slice-name 
;;;  is bound to a ref of the refable-object.
;;;  Upon completion slice-name is bound to NIL and return-form is evaluated
;;;  and returned as the value of the doslices macro.
;;;  Default return form is NIL.
;;;  Order of iteration over the slices is either :row or :column major
;;;  depending on the value of order.  Default is :row.


;;;  For example suppose we want to print slices of
;;;  a multi-way array.
;;;  Then we could write

(defun print-slices (thing  &optional (slices :elements) (order :row))
  "Prints each slice in thing as determined by the fixed dimensions listed ~
   in slices.  Slices are printed in either :row or :column major order ~
   depending on the value of the default argument order."
  (doslices (piece thing slices NIL order)
    (print piece)))

;;;
;;; Print all elements of a
;;;

(print-slices a )

;;;
;;; Print (ref a)
;;;

(print-slices a  NIL)

;;;
;;; Print the rows of a
;;;

(print-slices a '(0))
;;;
;;; Print the columns of a
;;;

(print-slices a '(1))


;;;
;;; Print ref of every element of a
;;;

(print-slices a '(0 1))

;;;
;;; Print the layers of b
;;;

(print-slices b '(2))

;;;
;;; Print the columns of b
;;;
;;; first in row-major-order
(print-slices b '(1 2) :row)

;;; And then in column major order
(print-slices b '(1 2) :column)

;;; Default is row major order
(print-slices b '(1 2))

;;;
;;; Print the rows of b
;;;

(print-slices b '(0 2))

;;; Print all elements of b

(print-slices b )

;;;
;;; Or how about finding the slice with the largest mean?
;;;

(defun biggest-mean (thing slices)
  (let ((result (row-major-ref-slice thing slices 0)))
    ;; Got the starting value now iterate
    ;; Note that the return form is result
    (doslices (x thing slices result)
      (if (> (mean x) (mean result))
        (<- result x)))))

;;;
;;; Row with largest mean
;;;
(biggest-mean b '(0 2))
;;;
;;; Column with largest mean
;;;
(biggest-mean b '(1 2))
;;;
;;; Layer with largest mean
;;;
(biggest-mean b '(2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  COLLECT-SLICES
;;;
;;;  This is almost the same as doslices except that it has no return form
;;;  and instead returns the list of values of the last form executed in 
;;;  the body at each iteration.
;;;  The general syntax of collect-slices macro is
;;;
;;;  (collect-slices (slice-name refable-object slices order)
;;;                                           ;;^^^^^^^^^^^^
;;;                                           ;;last two are optional
;;;      body-form-1 body-form-2 .... body-form-n)
;;;
;;;  slice-name is bound to the current slice of the refable-object
;;;  and the body-forms 1 to n are executed in sequence at each iteration.
;;;  Each slice is the sub array of refable object corresponding to a single
;;;  fixed value for all dimensions identified by the list slices.
;;;  If slices is omitted or eq :elements then iteration is over all elements
;;;  and slice-name refers to the actual element as opposed to a ref to it.
;;;  If slices is NIL, then a single iteration is done where the slice-name 
;;;  is bound to a ref of the refable-object.
;;;  Collect-slices returns a list of length (number-of-slices refable-object slices)
;;;  containing, in the order of iteration,
;;;  the values of the last form evaluated at each iteration.
;;;  Order of iteration over the slices is either :row or :column major
;;;  depending on the value of order.  Default is :row.

;;;
;;;
;;;  Gather up the means of the slices into a list
;;;

(defun collect-means (thing slices &optional (order :row))
  (collect-slices (x thing slices order)
    (mean x)))


;;;
;;; Row means
;;;
(collect-means b '(0 2))
(collect-means b '(0 2) :column)
;;;
;;; Column means
;;;
(collect-means b '(1 2))
(collect-means b '(1 2) :column)
;;;
;;; Layer means
;;;
(collect-means b '(2))
(collect-means b '(2) :column)  ;; identical because there is only 1 dimension
                                ;; in slices to iterate over.

;;;
;;;  Element means
;;;
(collect-means b :elements)
;;; or
(collect-means b '(0 1 2))
