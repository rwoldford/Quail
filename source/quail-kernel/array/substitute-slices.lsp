;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             substitute-slices.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(substitute-slices substitute-slices-if)))

;--------------------------------------------------------------------------------

(defun substitute-slices (old-slice new-slice ref-object
                                    &key
                                    (slices :elements)
                                    (test #'ref-eq))
  "Returns a copy of ref-object with slices that matched ~
   old-slice according to the test function test replaced by new-slice.~
   It is a non-destructive operation.  If no slices matched it returns a~
   copy of ref-object.~
   (:see-also substitute-slices-if substitute doslices collect-slices)~
   (:required ~
   (:arg old-slice The old-slice to be compared to each slice in ref-object.)~
   (:arg new-slice The new-slice to replace each slice matching old-slice ~
   in ref-object. Note that its dimensions must match those of old-slice. ~
   at least to the extent that this is required by ref itself.)~
   (:arg ref-object The source of the slices which will be compared to old-slice.) ~
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
   (:arg test #'ref-eq The function of two arguments ~
   to be called on old-slice and each slice.  ~
   If it returns ~
   non-NIL then that slice is replaced by new-slice, otherwise it remains.)~
   )~
   "
  (when (numberp slices) (setf slices (list slices)))
  (let* (result)
    ;; Make the copy

    (setf result (sel ref-object))
    (loop for i from 0 to (- (number-of-slices result slices) 1)
          when
          (funcall test old-slice (column-major-ref-slice ref-object slices i))
          do
          (setf (column-major-ref-slice result slices i)
                new-slice))
    result))
    
(defun substitute-slices-if (new-slice pred ref-object
                         &key
                         (slices :elements))
  "Returns a copy of ref-object with slices that returned non-NIL when tested ~
   by the predicate function pred replaced by new-slice.~
   It is a non-destructive operation.  If no slices matched it returns a~
   copy of ref-object.~
   (:see-also substitute-slices-if substitute doslices collect-slices)~
   (:required ~
   (:arg new-slice The new-slice to replace each slice in ref-object satisfying ~
   pred. Note that its dimensions must match those of the slice being replaced, ~
   At least to the extent that this is required by ref itself.)~
   (:arg pred The predicate function which tests each slice in ref-object.  ~
   If it returns non-NIL that slice is to be replaced, if NIL it is not.)~
   (:arg ref-object The source of the slices.) ~
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
  (let* (result)
    ;; Make the copy

    (setf result (sel ref-object))
    (loop for i from 0 to (- (number-of-slices result slices) 1)
          when
          (funcall pred (column-major-ref-slice ref-object slices i))
          do
          (setf (column-major-ref-slice result slices i)
                new-slice))
    result))


    

