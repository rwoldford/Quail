;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               sweep.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;    R.W. Oldford 1994
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (export '(sweep)))

(defun sweep (fun ref-object
                  &key
                  (slices NIL)
                  (order :row)
                  (copy? NIL)
                  (broom #'-))
  "Sweeps the result of applying fun from each slice of ref-object using ~
   the function broom.  ~
   This is done by calling the function fun on each slice to get a result ~
   for that slice.  Then that slice is replaced by the result of ~
   calling broom on slice and result. an on subtracting the ~
   the result from that slice. ~
   Returns the swept ref-object, or if copy? is non-NIL the swept copy of ~
   ref-object and a list of the values swept from each slice. ~
   (:required ~
   (:arg fun A unary function that can be applied to an arbitrary slice of ~
   a ref-object.) ~
   (:arg ref-object The refable object whose slices will be swept.) ~
   )~
   (:key ~
   (:arg slices NIL ~
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
   (:arg order :row The order in which iteration is to take place over ~
   the slices.) ~
   (:arg copy? NIL A logical flag indicating whether a copy of ref-object is to ~
   be swept rather than the original ref-object.) ~
   (:arg broom #'- A function of two arguments to be called on the slice and the ~
   result.) ~
   )~
   (:returns Sweep returns two values.  The first is either the original or ~
   a copy of the ref-object after sweeping.  The second is a refable object ~
   like ref-object whose elements are the items ~
   swept from each slice. The dimensions of this last item are determined to match ~
   those of the set of all slices.)~
   (:see-also collapse map-slices doslices collect-slices row-major-ops ~
   column-major-ops)~
   "
  
  (if (eq order :col) (setf order :column))
  (when (numberp slices) (setf slices (list slices)))
  (if copy? (setf ref-object (sel ref-object)))
  (let (swept-value swept-values)
    (if (listp slices)
      ;; then sort out the right size to return
      (let ((complement-dims (iseq (length (dimensions-of ref-object)))))
        (loop
          for i in slices
          do (setf complement-dims (remove  i complement-dims)))
        (setf swept-values
              (sel (column-major-ref-slice
                    ref-object
                    complement-dims
                    0)))
        )
      (setf swept-values
            (make-sequence 'list (number-of-slices ref-object slices))))
    
    (cond
     ((eq order :row)
      (loop
        for i from 0
        to (- (number-of-slices ref-object slices) 1)
        as slice = (row-major-ref-slice ref-object slices i)
        do
        (setf swept-value (funcall fun slice))
        (setf (row-major-ref-slice ref-object slices i)
              (funcall broom slice swept-value))
        (setf (row-major-eref swept-values i) swept-value)
        ))
     ((eq order :column)
      (loop
        for i from 0
        to (- (number-of-slices ref-object slices) 1)
        as slice = (column-major-ref-slice ref-object slices i)
        do
        (setf swept-value (funcall fun slice))
        (setf (column-major-ref-slice ref-object slices i)
              (funcall broom slice swept-value))
        (setf (column-major-eref swept-values i) swept-value)
        ))
     (T
      (quail-error "Illegal order given to sweep -- ~s ~%~
                    Order must be one of (:column :row)." order)))
    (values ref-object swept-values))
  )
