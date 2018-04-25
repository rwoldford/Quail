;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               map-slices.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(map-slices)))

;--------------------------------------------------------------------------------

(defun map-slices (fun slices order ref-object &rest more-ref-objects)
  "The function must take as many arguments as there are ref-objects ~
   provided.  The result is a list such that element j of the list ~
   is the result of applying fun to the j'th slice of each of the argument ~
   ref-objects. Note that therefore the slices argument must be sensible ~
   for all argument ref-objects. ~
   (:see-also map-element doslices collect-slices)~
   (:required ~
   (:arg fun The function to be applied to slices of all ref-objects.~
   It must accept as many arguments as there are ref-objects in the arguments.) ~
   (:arg slices The integer or list of fixed dimensions that identifies a slice.~
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
   are accessed directly as opposed to a ref of them.  In this special ~
   case, map-slices will be much like map-element but not nearly as clever.)~
   (:arg order The order of iteration over the slices -- :column for ~
   column major order, :row for row-major order.)~
   (:arg ref-object The source of the slices which will be the first argument ~
   to fun.)~
   ) ~
   (:rest more-ref-objects The remaining ref-objects.) ~
   (:returns A list such that element j of the list ~
   is the result of applying fun to the j'th slice of each of the argument ~
   ref-objects. The length of the list will be as long as the minimum ~
   number-of-slices in any of the ref-objects.)~
   "
  (when (numberp slices) (setf slices (list slices)))
  (let* ((size (if more-ref-objects
                 (reduce #'(lambda (x y)
                             (min x (number-of-slices y slices)))
                         more-ref-objects
                         :initial-value (number-of-slices ref-object slices))
                 (number-of-slices ref-object slices))))
    (cond
     ((or (eq order :column) (eq order :col))
      (if more-ref-objects
        (loop for i from 0 to (- size 1)
              collect
              (apply fun
                     (column-major-ref-slice ref-object slices i)
                     (loop for obj in more-ref-objects
                           collect
                           (column-major-ref-slice obj slices i))))
        (loop for i from 0 to (- size 1)
              collect
              (funcall fun (column-major-ref-slice ref-object slices i)))
        ))
     ((eq order :row)
      (if more-ref-objects
        (loop for i from 0 to (- size 1)
              collect
              (apply fun
                     (row-major-ref-slice ref-object slices i)
                     (loop for obj in more-ref-objects
                           collect
                           (row-major-ref-slice obj slices i))))
        (loop for i from 0 to (- size 1)
              collect
              (funcall fun (row-major-ref-slice ref-object slices i)))
        ))
     (T (quail-error "Illegal order for Map-slices ~s. ~%~
                      Must be either :row or :column." order)))))
    

