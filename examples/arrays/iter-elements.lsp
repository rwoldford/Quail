;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        General Iteration over elements                 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SEE ALSO doslices and collect-slices

(in-package :quail-user)

(setf a (array '((1.2 3 4.5) (6.7 8.9 0.1))
                :dimensions '(2 3)))

(setf b (seq 10 -10 -1))
(setf small-b (ref b '(:c 0 1 2)))        ;; all but the first three elements of b

(setf c (array (random-uniform :n 4) :dimensions '(2 2)))

(setf d (sel a))  ;; a copy of a

(setf e (array (seq 1 24) :dimensions '(2 3 4)))  ;; 2 rows, 3 cols, 4 layers

;;;   
;;;   In this file, we illustrate the use of the following
;;;   functions for arrays:
;;;       ... column-major-eref
;;;           column-major-list-elements
;;;           column-major-set-elements
;;;           column-major-ref-slice
;;;           row-major-eref
;;;           row-major-list-elements
;;;           row-major-set-elements
;;;           row-major-ref-slice
;;;
;;; We have already illustrated some of these.
;;; They are especially useful to iterate over the elements of an array.
;;;
;;; For example, to access the elements of an arbitrary array in 
;;; row-major-order, that is last index changing fastest and the first slowest,
;;; we have:

(defun print-in-row-order (a)
  "This prints out all the elements of the array a in row major order"
  (let ((n (number-of-elements a)))
    (format *quail-terminal-io* "~&In row-major order,")
    (loop for i from 0 to (- n 1)
          do (format *quail-terminal-io*
                     "~&Element ~s = ~s"
                     i
                     (row-major-eref a i)))))

(print-in-row-order a)
(print-in-row-order b)

;;; Or in either row or column major order

(defun print-elements (a &key (order :row))
  "This prints out all the elements of the array a in either row or~
   column major order.  Which depends on the keyword"
  (let ((n (number-of-elements a)))
    (cond
     ((eq order :row)
      (format *quail-terminal-io* "~&In row-major order,")
      (loop for i from 0 to (- n 1)
            do (format *quail-terminal-io*
                       "~&Element ~s = ~s"
                       i
                       (row-major-eref a i))))
     ((eq order :column)
      (format *quail-terminal-io* "~&In column-major order,")
      (loop for i from 0 to (- n 1)
            do (format *quail-terminal-io*
                       "~&Element ~s = ~s"
                       i
                       (column-major-eref a i))))
     )))

(print-elements a :order :row)
(print-elements a :order :column)
(print-elements b :order :row)
(print-elements b :order :column)


;;;
;;;  Both column-major-eref and row-major-eref can be used
;;;  with setf

(setf (column-major-eref a 0) 9999)
a
;;;
;;;
;;;  It is often required that we manipulate whole slices of a
;;;  refable quantity.
;;;
;;;  To that end, we provide the functions
;;;
;;;   number-of-slices
;;;   column-major-ref-slice
;;;   row-major-ref-slice
;;;

;;;
;;;  For example suppose we want to know the mean of several slices of
;;;  a mult-way array.
;;;  Then we could write

(defun slice-means (thing slices &optional (order :row))
  "Calculates and returns a list of the means of slices of thing. ~
   where slices is a list of those dimensions in thing which ~
   define a slice.  All other dimensions determine the interior ~
   contents of each slice.  Order is an optional argument ~
   which determines the order in which the slices are traversed.~
   The default value of order is :row, it can also be :column for ~
   column major order instead of row major order."

  (if (numberp slices) (setf slices (list slices)))   ; this allows us to be a bit sloppy
                                                      ; in specifying slices
  (let ((n (number-of-slices thing slices)))
    (cond
     ((eq order :row)
      (loop for i from 0 to (- n 1)
            collect (mean (row-major-ref-slice thing slices i))))
     ((eq order :column)
      (loop for i from 0 to (- n 1)
            collect (mean (column-major-ref-slice thing slices i)))))))

;;;
;;; Row means of a are

(slice-means a 0)

;;;
;;; Column means of a are

(slice-means a 1)

;;;
;;; Column means of a in column major order over the slices are

(slice-means a 1 :column)

;;;
;;; No difference because the slices were only over 1 fixed dimension
;;;

(dimensions-of e)

;;;
;;;  `layer' means of e are
;;;
(slice-means e 2)

;;;
;;;  Column means of e for every layer
;;;

(slice-means e '(1 2))

;;;
;;; Like row-... and column-major-eref, row-... and column-major-ref-slice
;;; can be used with setf, or <-, as in

(setf (column-major-ref-slice e '(2) 0)       ;; set the first (0) layer (slices = '(2) )
                                              ;; of e to be full of 99s
      (array 99 :dimensions '(2 3)))

(ref e t t 0)                                 ;; all rows all columns first layer

e

;;;
;;;  Be careful these are genuine refs, NOT COPIES.
;;;
;;;  Example:  put slice of an array with the largest mean at the end
;;;  by pushing it down through the array one slice at a time.
;;;

(defun push-big-mean-down (x &optional slices)
  ;; First we will allow slices to be a number. If it is we turn it
  ;; into a list and carry on.
  (if (not (listp slices)) (setf slices (list slices)))

  (let (swap)                                  ; swap is the temporary variable
                                               ; to be used for the swap.
    (loop for i from 1 to (- (number-of-slices x slices) 1)
          when (> (mean (column-major-ref-slice x slices (- i 1)))
                  (mean (column-major-ref-slice x slices i)))
            ;;
            ;; Then we want to  swap slices
          do
            (<- swap 
                (sel (column-major-ref-slice x slices i)))
            ;;   ^^^ note we must COPY the slice to be overwritten
            ;; hence the SEL. Otherwise you will lose it!
            (<- (column-major-ref-slice x slices i)
                (column-major-ref-slice x slices (- i 1)))
            (<- (column-major-ref-slice x slices (- i 1))
                swap)))
  x)
            
a
;; Push the column with the largest mean to the end of a
(push-big-mean-down a 1)

e
;;; Last layer of e 
(ref e t t 3)
;; Push the layer with the largest mean to the end of e
(push-big-mean-down e 2)
;;; New last layer of e 
(ref e t t 3)
   

;;;;;;;;;;;;;;;;;
;;;
;;;                    slices = :element
;;;
;;;  There is an important special case for the slices argument that
;;;  makes column-major-ref-slice and row-major-ref-slice behave
;;;  exactly as their *-major-eref counterparts.
;;;  Namely, if given the keyword :elements as the value of the
;;;  slices argument then they are identical to their eref counterparts.
;;;
;;;  This means that the elements can be accessed and set directly with
;;;  column- and row-major-ref-slice.
;;;  This is very convenient for the definition of higher level mapping
;;;  functions like sort, reduce-slices, etc.
;;;  
;;;  Alternatively, if all dimensions are specified as the slices
;;;  argument then a reference to each element is returned.
;;;  The distinction is illustrated below:

(column-major-ref-slice a :elements 0)

;;;  returns the element, whereas if all dimensions define a slice, as in

(column-major-ref-slice a (iseq (number-of-dimensions a)) 0)

;;;  then a zero-dimensional reference to the element is returned.
;;;
;;; Either can be used with setf and will achieve the same result

(setf (column-major-ref-slice a :elements 0) 99)
(eref a 0 0)

(setf (column-major-ref-slice a (iseq (number-of-dimensions a)) 0) 999)
(eref a 0 0)





;;;;;;;;;;;;;;;
;;;  
;;;  Occasionally you will want a list of all the elements in a ref'able thing.
;;;
;;;  This can be had in column- or row-major order respectively with the
;;;  functions:
;;;           column-major-list-elements
;;;           row-major-list-elements
;;;  These are sometimes handy for the loop macro which can loop over lists
;;;  as part of its arguments as in

(loop for thing in (list  "a" 1 "v") do (print thing))

a

(column-major-list-elements a)
(row-major-list-elements a)
;;;
;;; or
;;;

(loop for x in (column-major-list-elements a) sum x)

(row-major-list-elements "abcdefghij")

;;;  
;;;  SImilarly, you might have a list of elements which you would like to use to fill
;;;  your ref'able structure with, as in:

(column-major-set-elements a  '(1 2 3 4 5 6))
;;;
;;; or
;;;
(row-major-set-elements a  '(1 2 3 4 5 6))

;;;
;;; The principal value of these last 4 functions is to make use
;;; of the many procedures available in Lisp that operate on lists
