;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               collapse.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(collapse)))

;--------------------------------------------------------------------------------

(defun collapse (fun ref-object
                     &key
                     (slices NIL)
                     (order  :row))
  "Calls the function fun on each slice of ref-object having indices ~
   in the dimensions identified by the list slices.  ~
   Collapse tries to return an object like ref-object but having fewer dimensions. ~
   The dimensions of the returned object will match those of ref-object as identified ~
   by slices.  For example if ref-object is a 2 by 3 by 4 by 5  array and slices is (0 1), ~
   then the returned object will be a 2 by 3 array.  ~
   Each element of the returned object will be the result of calling fun on ~
   the corresponding slice.  In our example, the 0 0 element of the returned object ~
   will be the result of calling fun on (ref ref-object 0 0 T T).  ~
   The elements of the returned object are filled in the same order that the slices ~
   of ref-object are traversed: either :row or :column major order.~
   (:required ~
   (:arg fun A function of one argument, namely a single slice.)~
   (:arg ref-object The refable object over which slicing is to take place.) ~
   )~
   (:key ~
   (:arg slices NIL The slices over which the collapse is to take place. ~
   One way to think of it is that the slices identify the dimensions of ref-object ~
   that are to be preserved.  The value of slices is typically ~
   an integer or list of fixed dimensions.  ~
   For example if slices is 0 then a slice is defined to be the set of all ~
   elements of a ref-object having a common value of the zeroth index. ~
   There will then be as many slices as the size of the zeroth dimension of the ~
   ref-object. ~
   Similarly, if slices is '(0 1) then a slice of a ref-object ~
   is the set of all elements ~
   having common zeroth and first index and there will be as many slices ~
   as there are pairs of zeroth and first indices.  ~
   If NIL then the whole of the ref-object is taken to be the slice. ~
   Slices may also be the keyword :elements in which case the elements ~
   are accessed directly as opposed to a ref of them.  In this special case, ~
   the elements themselves are collapsed according to fun.) ~
   (:arg order :row The order in which iteration is to take place over slices.) ~
   )~
   (:returns A ref-object of the same class as the ~
   argument ref-object but with dimensions the same as its slices.)~
   (:see-also reduce-slices doslices map-slices)~
   "
  (if (eq order :col) (setf order :column))
  (when (numberp slices) (setf slices (list slices)))
  (cond
   ((null slices) 
    ;; The whole thing is the slice
    (funcall fun ref-object))
   (T
    ;;try and make a structure of the same type as ref-object
    ;; and the same dimsneions as a single slice and stuff the results
    ;; in the cells.
    ;; The way this is now achieved is by copying a  single slice
    ;; and filling it.
    ;; There has to be a more efficient way to do this
    ;; ******* rwo
    (let (result)
      (if (listp slices)
        ;; then sort out the right size to return
        (let ((complement-dims (iseq (length (dimensions-of ref-object)))))
          (loop
            for i in slices
            do (setf complement-dims (remove  i complement-dims)))
          (setf result
                (sel (column-major-ref-slice
                      ref-object
                      complement-dims
                      0)))
          )
        (setf result (sel ref-object)))
      (cond
       ;;row-order
       ((eq order :row)
        (loop
          for i from 0
          to (- (number-of-slices ref-object slices) 1)
          do
          (setf (row-major-eref result i)
                (funcall fun
                         (row-major-ref-slice ref-object slices i)))
          )
        result)
       ;; column-order
       ((eq order :column)
        (loop
          for i from 0
          to (- (number-of-slices ref-object slices) 1)
          do
          (setf (column-major-eref result i)
                (funcall fun
                         (column-major-ref-slice ref-object slices i)))
          )
        result)
       (T
        (quail-error "Illegal order given to collapse -- ~s ~%~
                      Order must be one of (:column :row)." order))))
    )))


