;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               count-slices.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(count-slices count-slices-if)))

;--------------------------------------------------------------------------------

(defun count-slices (item ref-object
                         &key
                         (slices :elements)
                         (test #'ref-eq))
  "Counts all slices in ref-object which match item when the function test ~
   is applied to item and the slice of ref-object.~
   It returns the number of slices found.~
   (:see-also count-slices-if count doslices collect-slices)~
   (:required ~
   (:arg item The item to be compared to each slice in ref-object.)~
   (:arg ref-object The source of the slices which will be compared to item.) ~
   ) ~
   (:key ~
   (:arg slices :elements ~
   The integer or list of fixed dimensions that identifies a slice.~
   For example if slices is 0 then a slice is defined to be the set of all ~
   elements of a ref-object having a common value of the zeroth index. ~
   There will be as many slices as the size of the zeroth dimension of the ~
   ref-object. ~
   Similarly, if slices is '(0 1) then a slice of a ref-object ~
   is the set of all elements ~
   having common zeroth and first index and there will be as many slices ~
   as there are pairs of zeroth and first indices.  ~
   If NIL then the whole of the ref-object is taken to be the slice. ~
   Slices may also be the keyword :elements in which case the elements ~
   are accessed directly as opposed to a ref of them.) ~
   (:arg test #'ref-eq The function to be called on item and slice.  If it returns ~
   non-NIL then that slice is counted.)~
   )~
   "
  (when (numberp slices) (setf slices (list slices)))
  (let ((count 0))
    (doslices (slice ref-object slices)
      (if (funcall test item slice)
        (incf count)))
    count))
    
(defun count-slices-if (pred ref-object
                         &key
                         (slices :elements))
  "Counts every slice in ref-object which returns non-NIL ~
   when the function pred is applied to it.~
   It returns the number of slices found.~
   (:see-also count-slices count-if doslices collect-slices)~
   (:required ~
   (:arg pred The predicate function to be applied to each slice.) ~
   (:arg ref-object The source of the slices which will be compared to item.) ~
   ) ~
   (:key ~
   (:arg slices :elements ~
   The integer or list of fixed dimensions that identifies a slice.~
   For example if slices is 0 then a slice is defined to be the set of all ~
   elements of a ref-object having a common value of the zeroth index. ~
   There will be as many slices as the size of the zeroth dimension of the ~
   ref-object. ~
   Similarly, if slices is '(0 1) then a slice of a ref-object ~
   is the set of all elements ~
   having common zeroth and first index and there will be as many slices ~
   as there are pairs of zeroth and first indices.  ~
   If NIL then the whole of the ref-object is taken to be the slice. ~
   Slices may also be the keyword :elements in which case the elements ~
   are accessed directly as opposed to a ref of them.) ~
   )~
   "
  (when (numberp slices) (setf slices (list slices)))
  (let ((count 0))
    (doslices (slice ref-object slices)
      (if (funcall pred slice)
        (incf count)))
    count)
  )


    

