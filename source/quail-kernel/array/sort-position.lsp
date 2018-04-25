;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           sort-position.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1992.
;;;     R.W. Oldford 1992, 1994
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(sort-position)))


(defun sort-position (object predicate 
                             &key 
                             (key      nil)
                             (slices   :elements)
                             (type     :exterior))
  
  "Returns the position of the smallest element, ranked by predicate, ~
   followed by the position of the next smallest element, etc., all ~
   collected into a list.~
   (:required ~
   (:arg object The object whose contents are to have their sorted ~
   position identified) ~
   (:arg predicate A function of two arguments which returns non-NIL if and only ~
   if the first argument should be considered as preceding the second.))~
   (:key ~
   (:arg key NIL If non-NIL this is a function that when applied to an element ~
   will return ~
   the key for that element.  The keys are then compared by the predicate.) ~
   (:arg type :exterior Two types of sort-position ~
   are possible: :interior and :exterior. ~
   The interior one identifies the order of ~
   the elements within the specified slices and ~
   the predicate should be a function that compares elements of slices.
   The exterior one identifies the order of ~
   the slices and not the elements within each one and ~
   the predicate should be a function that compares slices.) ~
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
   )~
   (:returns A list, or list of lists, of sort-positions.)"
  
  
  (cond ((eql type :exterior)
         (if key
           (mapcar #'car
                   (sort 
                    (let ((i -1))
                      (collect-slices (slice object slices)
                        (cons (incf i)
                              (funcall key slice))))
                    predicate
                    :key #'cdr))
           (mapcar #'car
                   (sort 
                    (let ((i -1))
                      (collect-slices (slice object slices)
                        (cons (incf i)
                              slice)))
                    predicate
                    :key #'cdr))
           )
         )
        ((eql type :interior)
         (collect-slices (slice object slices)
           (sort-position slice predicate :slices :elements :key key :type :exterior)
           ))))
