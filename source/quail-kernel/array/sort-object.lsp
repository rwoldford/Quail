;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           sort-object.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(basic-sort-by-eref sort-object)))

;;;----------------------------------------------------------------------------
;;;
;;;  The following is just what it says
;;;
;;;  Should anyone write a new sort function that uses eref to access the
;;;  elements of its argument, they should call it basic-sort-by-eref.
;;;  
;;;  N.B. The present version uses the system defined sort.
;;;
;;;----------------------------------------------------------------------------

(defun basic-sort-by-eref (object predicate
                                  &key key stable?)
  "This is the basic sort function for ref'able objects.  ~
   It is always destructive -- resetting the elements of its first argument. ~
   (:see-also sort sort-position ranks order eref) ~
   (:required ~
   (:arg object The object to be sorted.  Something whose elements can be ~
   accessed by the eref function.) ~
   (:arg predicate A function of two arguments which returns non-NIL if and only ~
   if the first argument should be considered as preceding the second.))~
   (:key ~
   (:arg key NIL If non-NIL this is a function that when applied to an element ~
   will return ~
   the key for that element.  The keys are then compared by the predicate.) ~
   (:arg stable? NIL If NIL, the sorting operation is not guaranteed to be stable. ~
   If elements x and y are considered to be equal by the predicate as in ~
   the predicate returning NIL when presented with x y in either order, ~
   then there is no guarantee that the elements will stay in their original order ~
   in object. ~
   If stable? is non-NIL then in the case of ties the original order will be ~
   preserved but the sort will be slower.) ~
   )~
   (:elaboration Should anyone write a new sort function that uses eref to ~
   access the ~
   elements of its argument, they should call it basic-sort-by-eref. ~%~
   N.B. The present version uses the Common Lisp system defined sort.)"
  (cond
   (key
    (cond
     (stable?
      (row-major-set-elements object
                              (cl:stable-sort
                               (row-major-list-elements object) predicate :key key)))
     (T
      (row-major-set-elements object
                              (cl:sort
                               (row-major-list-elements object) predicate :key key)))))
   (T
    (cond
     (stable?
      (row-major-set-elements object
                              (cl:stable-sort
                               (row-major-list-elements object) predicate)))
     (T
      (row-major-set-elements object
                              (cl:sort
                               (row-major-list-elements object) predicate)))))))

(defgeneric sort-object (object
                         predicate
                         key
                         slices
                         type
                         stable?)
  (:documentation
   "Just like sort, except that keyword arguments are converted to required ~
    arguments.  This helps with argument typing. And the sort is always destructive ~
    to the object."))

(defmethod sort-object (object
                        predicate
                        key
                        slices
                        type
                        stable?)
  "Missing method for all unclassified cases."
  (missing-method 'sort-object 
                  object
                  predicate
                  key
                  slices
                  type
                  stable?)
  )




(defmethod-multi sort-object ((object (symbol number character))
                              (predicate function)
                              key
                              slices
                              type
                              stable?)
  "Returns object since sorting doesn't quite make sense here."
  (declare (ignore predicate key slices type stable?))
  object)



;;; This appears to be covered by the order sort-position as well!
(defmethod-multi sort-object ((object (dimensioned-ref-object sequence array))
                              (predicate function)
                              key
                              (slices  T) ;;(eql :elements))
                              (type (eql :interior ))
                              stable?)
  "Implements the interior sort as an exterior sort for every slice. "
  (doslices (slice object slices object)
    (sort-object slice predicate key :elements :exterior stable?)
    ))



(defmethod sort-object ((object sequence)
                        (predicate function)
                        key
                        (slices (eql :elements))
                        (type (eql :exterior))
                        stable?)
  "In this situation the usual Common Lisp sort function applies."
  (declare (ignore slices type))
  (if key
    (if stable?
      (cl:stable-sort object predicate :key key)
      (cl:sort        object predicate :key key))
    (if stable?
      (cl:stable-sort object predicate)
      (cl:sort        object predicate)))
  )

(defmethod-multi sort-object
  ((object (T sequence dimensioned-ref-object array))
   (predicate function)
   key
   (slices null)
   (type (eql :exterior))
   stable?)
  (declare (ignore predicate key slices type stable?))
  object)


(defmethod-multi sort-object
  ((object (dimensioned-ref-object array))
   (predicate function)
   key
   (slices (eql :elements))
   (type (eql :exterior))
   stable?)
  "In this situation the basic-sort-by-eref is used."
  (declare (ignore slices type))
  (basic-sort-by-eref object predicate :key key :stable? stable?))

(defmethod-multi sort-object ((object (dimensioned-ref-object sequence array))
                              (predicate function)
                              key
                              (slices list)
                              (type (eql :exterior))
                              stable?)
  (declare (ignore stable?))
  (order object (sort-position object predicate 
                               :key key 
                               :slices slices 
                               :type :exterior)
         :slices slices 
         :type :exterior
         :copy? NIL))
