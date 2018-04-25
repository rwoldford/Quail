;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             replace-slices.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(replace-slices)))

;--------------------------------------------------------------------------------

(defun replace-slices (ref-object
                       &key
                       (slices :elements)
                       (function #'identity)
                       (copy? T))
  "Replaces every slice of ref-object by the result of calling function ~
   on that slice.  This means that function ~
   must return an object of identical dimensions to the original slice. ~
   By default, replace-slices is not destructive to the original ref-object.~
   If copy? is NIL then replace-slices will operate directly on the ref-object.  ~
   Returns the ref-object, or copy, with slices replaced.  ~
   (:see-also substitute-slices substitute-slices-if doslices collect-slices)~
   (:required ~
   (:arg ref-object The source whose slices will be replaced.) ~
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
   (:arg function #'identity A function of one argument ~
   to be called on each slice.  The slice is replaced by the result, so function must ~
   return an object having the same dimensions as the original slice.) ~
   (:arg copy? T If NIL, then the slices of ref-object are replaced directly.  ~
   Otherwise a copy is made first.) ~
   )~
   "
  (when (numberp slices) (setf slices (list slices)))
  (if copy? (setf ref-object (sel ref-object)))
  (loop for i from 0 to (- (number-of-slices ref-object slices) 1)
          do
          (setf (column-major-ref-slice ref-object slices i)
                (funcall function (column-major-ref-slice ref-object slices i))))
  ref-object)
