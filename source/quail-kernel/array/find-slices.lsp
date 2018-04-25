;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               find-slices.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(find-slices find-slices-if)))

;--------------------------------------------------------------------------------

(defun find-slices (item ref-object
                         &key
                         (order :row)
                         (slices :elements)
                         (test #'ref-eq))
  "Finds all slices in ref-object which match item when the function test ~
   is applied to item and the slice of ref-object.~
   It returns a list of the slices found.~
   (:see-also find-slices-if find doslices collect-slices)~
   (:required ~
   (:arg item The item to be compared to each slice in ref-object.)~
   (:arg ref-object The source of the slices which will be compared to item.) ~
   ) ~
   (:key ~
   (:arg order :row The order in which the slices are to be searched. ~
   Column-major by specifying :column, row-major by specifying :row.)~
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
   non-NIL then that slice is returned.)~
   )~
   (:returns The list of slices which were found in the order they were found.) ~
   "
  (if (eq order :col) (setf order :column))
  (when (numberp slices) (setf slices (list slices)))
  (cond
   ((eq order :column)
    (let (slice)
      (loop for i from 0 to (- (number-of-slices ref-object slices) 1)
            when
            (progn (setf slice (column-major-ref-slice ref-object slices i))
                   (funcall test item slice))
            collect slice)))
   ((eq order :row)
    (let (slice)
      (loop for i from 0 to (- (number-of-slices ref-object slices) 1)
            when
            (progn (setf slice (row-major-ref-slice ref-object slices i))
                   (funcall test item slice))
            collect slice)))
   (T
    (quail-error "Illegal order given to find-slices -- ~s ~%~
                  Order must be one of (:column :row)." order))))



(defun find-slices-if (pred ref-object
                         &key
                         (order :row)
                         (slices :elements))
  "Finds every slice in ref-object which returns non-NIL ~
   when the function pred is applied to it. ~
   It returns a list of the slices found. ~
   (:see-also find-slices find-if doslices collect-slices)~
   (:required ~
   (:arg pred The predicate function to be applied to each slice.) ~
   (:arg ref-object The source of the slices which will be compared to item.) ~
   ) ~
   (:key ~
   (:arg order :row The order in which the slices are to be searched. ~
   Column-major by specifying :column, row-major by specifying :row.)~
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
   (:returns The list of slices which were found in the order they were found.) ~
   "
  (if (eq order :col) (setf order :column))
  (when (numberp slices) (setf slices (list slices)))
  (cond
   ((eq order :column)
    (let (slice)
      (loop for i from 0 to (- (number-of-slices ref-object slices) 1)
            when
            (progn (setf slice (column-major-ref-slice ref-object slices i))
                   (funcall pred slice))
            collect slice)))
   ((eq order :row)
    (let (slice)
      (loop for i from 0 to (- (number-of-slices ref-object slices) 1)
            when
            (progn (setf slice (row-major-ref-slice ref-object slices i))
                   (funcall pred slice))
            collect slice)))
   (T
    (quail-error "Illegal order given to find-slices-if -- ~s ~%~
                  Order must be one of (:column :row)." order))))


    

