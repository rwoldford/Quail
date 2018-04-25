;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           order.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1992.
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(order)))
  
(defun order (object positions 
                     &key 
                     (slices :elements)
                     (copy? NIL)
                     (type :exterior)
                     (order :row))
  "Repositions the sub-arrays of the fixed-dimensions of object into an order  ~
   determined by the second argument in conjunction with the information ~
   given by the keyword arguments slices types and order.  ~
   (:see-also sort sort-position ranks indices) ~
   (:required ~
   (:arg object The object to be sorted.  Typically a ref-object or sequence.) ~
   (:arg positions A list of indices of the elements or slices in the desired order.~
   For example, the first element in positions is the index of the slice of ~
   object which will be in the first position of the ordered object.) ~
   ) ~
   (:key ~
   (:arg slices NIL A list specifying which slice dimensions are ~
   to be fixed -- if null, no dimensions are fixed.)  ~
   (:arg copy? NIL Unless this flag is non-NIL, the ordering ~
   will be destructive to the original object.  ~
   If non-NIL, the object is copied before ordering takes place.) ~
   (:arg type :exterior
   The keyword type specifies whether the ordering is to be done between
   slices, i.e. :exterior, or within slices, i.e. :interior.)~
   (:arg order :row
   The major ordering in which the slices and their interiors are to ~
   be indexed.  :row means row major ordering and the last index changes ~
   fastest.  :column means column major order with the first index changing ~
   most rapidly.)~
   )"
  (when (numberp slices) (setf slices (list slices)))
  (if copy? (setf object (sel object)))
  (cond
   ((eq order :row)
    (cond
     ((eq type :exterior)
      (loop for thing in
            (loop for i in positions collect
                  (sel (row-major-ref-slice object slices i)))
            as i from 0 do
            (setf (row-major-ref-slice object slices i) thing)))
     ((eq type :interior)
      (loop for indices in positions as i from 0
            do
            (order
             (row-major-ref-slice object slices i)
             indices
             :slices :elements
             :type :exterior))
      )))
   ((or (eq order :column) (eq order :col))
    (cond
     ((eq type :exterior)
      (loop for thing in
            (loop for i in positions collect
                  (sel (column-major-ref-slice object slices i)))
            as i from 0 do
            (setf (column-major-ref-slice object slices i) thing)))
     ((eq type :interior)
      (loop for indices in positions as i from 0
            do
            (order
             (column-major-ref-slice object slices i)
             indices
             :slices :elements
             :type :exterior
             :order :column))
      )))
   )
  object)
