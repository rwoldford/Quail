;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           sort.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1992.
;;;     R.W. Oldford 1992, 1994.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(sort)))

(defun sort (object predicate 
                    &key 
                    (key      nil)
                    (slices   :elements)
                    (type     :exterior)
                    (copy?     nil)
                    (stable?   nil))
  
  "Sorts the sub-arrays of slices of ref-object into an order  ~
   determined by predicate. For multi-dimensional objects, ~
   the sorted results are stored in row-major order (last index changing ~
   fastest).~
   (:elaboration The arguments predicate and key are to be interpreted ~
   exactly as the same named arguments in the Common Lisp sort function.  ~
   The argument stable? is NIL (the default) or non-NIL depending on whether ~
   it is desirable that stability be guaranteed in the sense of the Common ~
   Lisp function stable-sort. ~
   If the argument combination permits it the sort will follow ~
   the Common Lisp standard.) ~
   (:see-also sort-object ~
   basic-sort-by-eref cl::sort cl::stable-sort ~
   sort-position ranks order) ~
   (:required ~
   (:arg object The object to be sorted.  Typically a ref-object or sequence.) ~
   (:arg predicate A function of two arguments which returns non-NIL if and only ~
   if the first argument should be considered as preceding the second.))~
   (:key ~
   (:arg key NIL If non-NIL this is a function that when applied to an element ~
   will return ~
   the key for that element.  The keys are then compared by the predicate.) ~
   (:arg slices NIL If non-NIL, slices should be an index or list of indices ~
   that identifies ~
   where in the object the sorting is to take place.  For example if slices ~
   is 0 then a slice is defined to be the set of all elements having a common ~
   zeroth index.  If we also have an :interior type then this says that the elements ~
   should be sorted within rows of the object. ~
   If slices is the list (1 2) then a slice is the set of all elements who ~
   share the same 1 and 2 indices. ~
   In any case the sorting applies over all possible slices. ~
   If slices is not supplied then it is assumed that every element of object will ~
   be a slice.
   If slices is NIL then only one slices is available, namely the object itself.)~
   (:arg type :exterior Two types of sort are possible: :interior and :exterior. ~
   The interior sort sorts the elements within the specified slices and ~
   the predicate should be a function that compares elements of slices.
   The exterior sort sorts the slices and not the elements within each one and ~
   the predicate should be a function that compares slices.) ~
   (:arg copy? NIL Unless this flag is non-NIL, the sort will be destructive to the ~
   original object.  If non-NIL, the object is copied before sorting takes place. ~
   Note that for :interior type sorts this copying does not apply recursively to copy the ~
   elements of the original object.  So interior sorts with null slices will typically ~
   be destructive to the original elements.) ~
   (:arg stable? NIL If NIL, the sorting operation is not guaranteed to be stable. ~
   If elements x and y are considered to be equal by the predicate as in ~
   the predicate returning NIL when presented with x y in either order, ~
   then there is no guarantee that the elements will stay in their original order ~
   in object. ~
   If stable? is non-NIL then the original order will be preserved but the sort ~
   will be slower.) ~
   )~
   "
  (when (numberp slices) (setf slices (list slices)))
  (sort-object (if copy? (sel object) object)
               predicate
               key
               slices
               type
               stable?)
  )

