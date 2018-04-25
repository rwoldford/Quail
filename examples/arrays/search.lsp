;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        Searching refable objects
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;   
;;;   In this file, we illustrate the use of the following map functions
;;;   macros for refable objects:
;;;
;;;       ... find-slices         ... Finds all slices in ref-object which
;;;                                   match item when the function test
;;;                                   is applied to item and the slice of ref-object.
;;;                                   It returns a list of the slices found.
;;;       ... find-slices-if      ... Finds every slice in ref-object which returns
;;;                                   non-NIL when the function pred is applied 
;;;                                   to it.
;;;                                   It returns a list of the slices found.
;;;       ... count-slices        ... Counts all slices in ref-object which match 
;;;                                   item when the function test is applied to
;;;                                   item and the slice of ref-object.
;;;                                   It returns the number of slices found.
;;;       ... count-slices-if     ... Counts every slice in ref-object which
;;;                                   returns non-NIL when a function pred is
;;;                                   applied to it.
;;;                                   It returns the number of slices found.
;;;       ... slice-positions     ... Finds all slices in ref-object which match
;;;                                   item when the function test is applied to
;;;                                   item and the slice of ref-object.
;;;                                   It returns a list of the positions of the
;;;                                   slices found.
;;;       ... slice-positions-if  ... Finds the position of every slice in
;;;                                   ref-object which returns non-NIL when the
;;;                                   function pred is applied to it.
;;;                                   It returns a list of the positions of the
;;;                                   slices found.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :quail-user)

;;;
;;; Some arrays to work with.
;;;


(setf a (array  (iseq 6) :dimensions '(2 3)))

(setf b (array (iseq 1 24) :dimensions '(2 3 4)))  ;; 2 rows, 3 cols, 4 layers

(setf c (ref a 0))

(setf d (sel a))
(setf (eref d 0 0) NaN)

(setf e (sel b))
(setf (eref e  0 0 0) NaN)

;;;
;;; And the following print function may be helpful to see what the
;;; pieces.
;;;

(defun print-slices (obj &key 
                         (slices :elements)
                         (order :row))
  (let ((i 0))
    (format *quail-terminal-io* "~&~% Order is ~s~%" order)
    (doslices (slice obj slices 'done order)
      (format *quail-terminal-io* "~& Slice ~s is ~s" i slice)
      (incf i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  find-slices
;;;
;;; (find-slices item ref-object
;;;              :order :row
;;;              :slices :elements
;;;              :test #'ref-eq)
;;;
;;;  Finds all slices in ref-object which
;;;  match item when the function test
;;;  is applied to item and the slice of ref-object.
;;;  It returns a list of the slices found.

;;;
;;;  Look for elements

(find-slices 3 a)
(find-slices 9999 a)
(find-slices NaN d)

(find-slices 7 a :test #'>)
(find-slices 7 a :test #'> :order :column)

;;;  Look for larger slices

(find-slices c a :slices 0)

;;;  which is there because c is a ref of the first row of a
;;;  and so satisfies the test ref-eq.
;;;  But it is NOT the identical structure
;;;  and so fails on the eq test.

(find-slices c a :slices 0 :test #'eq)

;;;  Note that numbers are eq so

(find-slices 3 a :test #'eq)

;;;  works.
;;;
;;;  Neither is c a column of a.

(find-slices c a :slices 1)

;;;
;;;  And more general structures can be searched
;;;

(find-slices (ref e 0 0 t) e :slices '(0 1))

;;;  will find a slice
;;;  but a copy of that slice will not be found!

(find-slices (sel e 0 0 t) e :slices '(0 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  find-slices-if
;;;
;;; (find-slices-if pred ref-object
;;;                :order :row
;;;                :slices :elements)
;;;
;;;  Finds every slices in ref-object which
;;;  returns non-NIL when the predicate function pred is called on it.
;;;  It returns a list of the slices found.

;;;
;;;  Look for elements

(find-slices-if #'(lambda (x) (< x 3))
                a)

(find-slices-if #'(lambda (x) (< x 7))
                e)

(find-slices-if #'(lambda (x) (< x 7))
                e 
                :order :column)

(find-slices-if #'(lambda (x) (eq x NaN)) a)

(find-slices-if #'(lambda (x) (eq x NaN)) d)

;;;  Note that eq had to be used in last two searches.
;;;  Because NaN is special and may result from +infinity/-infinity or
;;;  0/0 or -0/0 , etc. it is dangerous to say that two NaNs are =
;;;  Therefore (= NaN NaN) always returns NIL.
;;;  And so will

(find-slices-if #'(lambda (x) (= x NaN)) d)

;;;
;;;  We might write a function then that checks for NaNs
;;;  as follows:

(defun find-NaNs (thing)
  (find-slices-if #'(lambda (x) (eq x NaN))
                  thing))

(find-NaNs a)
(find-NaNs b)
(find-NaNs c)
(find-NaNs d)
(find-NaNs e)

;;;
;;;  And it is a small step from there to one that checks
;;;  arbitrary slices for NaNs and illustrates a lot of what
;;;  we have learned so far.
;;;

(defun find-NaNs
       (thing &key (slices :elements) (order :row))
  (when (numberp slices) (setf slices (list slices)))
  (case order
    (:row
     (loop for i from 0 to (- (number-of-slices thing slices) 1)
           when
           (find-slices-if
            #'(lambda (x) (eq x NaN))
            (row-major-ref-slice thing slices i))
           collect
           (row-major-ref-slice thing slices i)))
    (:column
     (loop for i from 0 to (- (number-of-slices thing slices) 1)
           when
           (find-slices-if 
            #'(lambda (x) (eq x NaN))
            (column-major-ref-slice thing slices i))
           collect
           (column-major-ref-slice thing slices i)))
    ))

;;;
;;;  As before
;;;

(find-NaNs a)
(find-NaNs b)
(find-NaNs c)
(find-NaNs d)
(find-NaNs e)

;;;
;;; But we can now find say all the rows of e that contain NaN
;;;

(find-nans e :slices '(0 2))

;;; All row layers

(find-nans e :slices 0)

;;; All columns

(find-nans e :slices '(1 2))

;;; All layers

(find-nans e :slices 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  count-slices
;;;
;;; (count-slices item ref-object
;;;              :slices :elements
;;;              :test #'ref-eq)
;;;
;;;  Counts all slices in ref-object which match 
;;;  item when the function test is applied to
;;;  item and the slice of ref-object.
;;;  It returns the number of slices found.

;;;
;;;  Counting elements

(count-slices 3 a)
(count-slices 9999 a)
(count-slices NaN d)

(count-slices 7 a :test #'>)

;;;  Look for larger slices

(count-slices c a :slices 0)

;;;  which is there because c is a ref of the first row of a
;;;  and so satisfies the test ref-eq.

(count-slices c a :slices 0 :test #'eq)

;;;  Neither is c a column of a.

(count-slices c a :slices 1)

;;;
;;;  And more general structures can be searched
;;;

(count-slices (ref e 0 0 t) e :slices '(0 1))

;;;  will count a slice but not its copy.

(count-slices (sel e 0 0 t) e :slices '(0 1))

(count-slices NaN a :test #'eq)
(count-slices NaN b :test #'eq)
(count-slices NaN c :test #'eq)
(count-slices NaN d :test #'eq)
(count-slices NaN e :test #'eq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  count-slices-if
;;;
;;; (count-slices-if pred ref-object
;;;                :slices :elements)
;;;
;;;  Counts every slice in ref-object which returns non-NIL
;;;  when the function pred is applied to it.
;;;  It returns the number of slices found.
;;;
;;;
;;;  Count elements

(count-slices-if #'(lambda (x) (< x 3))
                a)

(count-slices-if #'(lambda (x) (< x 7))
                e)

(count-slices-if #'numberp e)

(count-slices-if #'(lambda (x) (eq x NaN)) e)

(count-slices-if #'(lambda (x) (eq x NaN)) d)

;;;
;;; And similarly for higher dimensionsal structures
;;;

(count-slices-if #'find-nans e :slices 0)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;              slice-positions
;;;
;;;
;;;
;;; (slice-positions item ref-object
;;;              :order :row
;;;              :slices :elements
;;;              :test #'ref-eq)
;;;
;;;  Finds all slices in ref-object which match
;;;  item when the function test is applied to
;;;  item and the slice of ref-object.
;;;  It returns a list of the positions of the slices found.
;;;
;;;  Look for elements

(slice-positions 3 a)
(print-slices a)
(slice-positions 9999 a)
(slice-positions NaN d)

(slice-positions 7 e :test #'>)
(print-slices e)
(slice-positions 7 e :test #'> :order :column)
(print-slices e :order :column)

;;;  Positions of larger slices

(slice-positions c a :slices 0)

(slice-positions (ref e 0 0 t) e :slices '(0 1))


;;;
;;;  Find-slices could have been implemented using slice-positions
;;;

(loop for i in (slice-positions 7 e
                                :test #'>
                                :order :column 
                                :slices :elements)
      collect
      (column-major-ref-slice e :elements i))
;;;
;;; is the same as 
;;;

(find-slices 7 e
             :test #'>
             :order :column 
             :slices :elements)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  slice-positions-if
;;;
;;; (slice-positions-if
;;;                 pred ref-object
;;;                :order :row
;;;                :slices :elements)
;;;
;;;  Finds the position of every slice in
;;;  ref-object which returns non-NIL when the
;;;  function pred is applied to it.
;;;  It returns a list of the positions of the slices found.
;;;

(slice-positions-if #'(lambda (x) (< x 3))
                a)

(slice-positions-if #'(lambda (x) (< x 7))
                e)

(slice-positions-if #'(lambda (x) (< x 7))
                e 
                :order :column)

(slice-positions-if #'(lambda (x) (eq x NaN)) a)

(slice-positions-if #'(lambda (x) (eq x NaN)) d)
