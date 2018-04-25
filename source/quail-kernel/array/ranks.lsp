;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           ranks.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(ranks)))


(defun ranks (object predicate 
                     &key 
                     (key      nil)
                     (slices   :elements)
                     (type     :exterior))
  
  "Ranks contents of object from smallest to largest according to the predicate ~
   function~
   (smallest has rank 0, increasing by 1 thereafter).  The value returned is a list ~
   of the ranks or a list of lists of ranks (for interior ranks on slices). ~
   (:elaboration The arguments predicate and key are to be interpreted ~
   exactly as the same named arguments in the Common Lisp sort function.)  ~
   (:see-also sort sort-position order) ~
   (:required ~
   (:arg object The object to be ranked.  Typically object or sequence.) ~
   (:arg predicate A function of two arguments which returns non-NIL if and only ~
   if the first argument should be considered as preceding the second.))~
   (:key ~
   (:arg key NIL If non-NIL this is a function that when applied to an element ~
   will return ~
   the key for that element.  The keys are then compared by the predicate.) ~
   (:arg slices :elements ~
   The integer or list of fixed dimensions that identifies a slice.~
   For example if slices is 0 then a slice is defined to be the set of all ~
   elements of object having a common value of the zeroth index. ~
   There will be as many slices as the size of the zeroth dimension of the ~
   object. ~
   Similarly, if slices is '(0 1) then a slice of object ~
   is the set of all elements ~
   having common zeroth and first index and there will be as many slices ~
   as there are pairs of zeroth and first indices.  ~
   If NIL then the whole of the object is taken to be the slice. ~
   Slices may also be the keyword :elements in which case the elements ~
   are accessed directly as opposed to a ref of them.)~
   (:arg type :interior Two types of ranking are possible: :interior and :exterior. ~
   The interior ranking ranks the elements within the specified slices and ~
   the predicate should be a function that compares elements of slices.
   The exterior ranking ranks the slices and not the elements within each one and ~
   the predicate should be a function that compares slices.) ~
   )"
  
  (cond ((eql type :exterior)
         (sort-position 
          (sort-position object predicate :key key :slices slices :type :exterior)
          #'<))
        ((eql type :interior)
         (mapcar #'(lambda (x) 
                     (sort-position x #'<))
                 (sort-position object predicate 
                                  :key key :slices slices :type :interior)))))
