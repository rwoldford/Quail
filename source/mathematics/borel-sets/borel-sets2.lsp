;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               borel-sets2.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1989, 1990, 1992
;;;     N. Wiebe 1998
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                             BOREL SETS 2
;;;
;;;
;;;  Classes:
;;;
;;;         Countable-Union
;;;         Countable-Intersection
;;;         Explicit-Finite-Union
;;;         Explicit-Finite-Intersection
;;;         Countable-Set
;;;         Explicit-Finite-Set
;;;
;;;  Global Constants:
;;;
;;;         *max-reasonable-number-of-iterations*
;;;
;;;  Global Variables:
;;;
;;;         *default-union-sort*
;;;         *default-set-less-than-predicate*
;;;
;;;----------------------------------------------------------------------------
;;;
;;;  Countable Unions of sets
;;;
;;;----------------------------------------------------------------------------

(defclass countable-union
  (countable-collection predicate-set)
  (
   (defining-predicate
    :initarg :defining-predicate
    :type function
    :initform
    #'(lambda (union member)
        (declare (special *max-reasonable-number-of-iterations*))
          (do
            ((index (count-start-of union)
                    (next-index union index))
             (counter 1 (+ counter 1))
             (big-count *max-reasonable-number-of-iterations*))
             ;; end-test
             ((count-endedp union index))
           
            ;; Now the action
            (cond
             ((> counter big-count)
              (if (quail-yes-or-no-p
                   "Have tested ~s sets already.  Should testing continue?"
                   (- counter 1))
                (setf big-count
                      (+ big-count
                         (loop
                           with answer = NIL
                           until (and answer (integerp answer) (>= answer 0))
                           do
                           (setf answer
                               (quail-query "How many more sets should be tested? ")
                               )
                           finally (return answer))))
                (cond
                 ((quail-yes-or-no-p
                   "Assume value ~%~
                    ~s ~%~
                    is in the set? ~%~
                    ~s ? " member union)
                  (return T))
                 ((quail-yes-or-no-p
                   "Assume value ~%~
                    ~s ~%~
                    is *NOT* in the set? ~%~
                    ~s ? " member union)
                  (return NIL))
                 (T (quail-error "Can't tell whether the value ~%~
                             ~s ~%~
                             is in the set ~%~
                             ~s ~%~
                             or not!" member union)))))
             ((op-memberp member (element union index)) (return T)))))
    )
   )
  (:documentation
   "The set representing the union of a countable (possibly :+infinite) number ~%~
    of sets. ~%~
    Each set in the union is indexed over a contiguous subset of ~%~
    the extended integers: ~%~
    [-infinity ,...,-1,0, 1, ... +infinity]~%~
    and is accessed by applying the element function to the union and ~%~
    the desired index."))

(defmethod combine-collections ((c2 borel-set) (c1 countable-collection))
  (combine-collections c1 c2))

(defmethod combine-collections
  ((c1 countable-collection) (c2 borel-set))

  (let* ((c1-cfun (contains-set-p-function-of c1))
         (c1-ifun (element-function-of c1))
         (ifp-1 (index-from-position-function-of c1))
         (new-begin  0)
         (new-end (number-of-sets-in c1)))
    
    (make-instance (type-of c1)
        :from new-begin :to new-end
        :contains-set-p-function
        (function
           (lambda (collection set)
             (or (funcall c1-cfun collection set)
                 (same-set-p c2 set))))
        :disjoint?
        (cond  ((not-disjoint-collection-p c1) NIL)
               ((and (disjoint-collection-p c1) (disjoint-sets-p c1 c2)) T)
               (T :unknown))
        :element-function
        (function
           (lambda (collection index)
             (cond
              
              ((zerop index)       ; borel-set first
               c2)

              (T
               (funcall c1-ifun                        ; All the rest later
                             collection                     
                             (funcall
                              ifp-1
                              (- index 1))))
             ))))))


;;;----------------------------------------------------------------------------
;;;
;;;  Countable Intersections of sets
;;;
;;;----------------------------------------------------------------------------

(defclass countable-intersection
  (countable-collection predicate-set)
  
  (
   (defining-predicate
    :initform
    #'(lambda (intersection member)
        (declare (special *max-reasonable-number-of-iterations*))

          (do
            ((index (count-start-of intersection)
                    (next-index intersection index))
             (counter 1 (+ counter 1))
             (big-count *max-reasonable-number-of-iterations*)
             (current-set NIL))
             ;; end-test
             ((count-endedp intersection index))
            
            ;; Now the action
            (setf current-set (element intersection index))
            (cond
             ((> counter big-count)
              (if (quail-yes-or-no-p
                   "Have tested ~s sets already.  Should testing continue?"
                   (- counter 1))
                (setf big-count
                      (+ big-count
                         (loop
                           with answer = NIL
                           until (and answer (integerp answer) (>= answer 0))
                           do
                           (setf answer
                               (quail-query "How many more sets should be tested? ")
                               )
                           finally (return answer))))
                (cond
                 ((quail-yes-or-no-p
                   "Assume value ~%~
                    ~s ~%~
                    is in the set? ~%~
                    ~s ? " member intersection)
                  (return T))
                 ((quail-yes-or-no-p
                   "Assume value ~%~
                    ~s ~%~
                    is *NOT* in the set? ~%~
                    ~s ? " member intersection)
                  (return NIL))
                 (T (quail-error "Can't tell whether the value ~%~
                             ~s ~%~
                             is in the set ~%~
                             ~s ~%~
                                  or not!" member intersection)))))
             ((not (memberp member current-set)) (return NIL)))))
    ))
  (:documentation
   "The set representing the intersection of a countable~%~
    (possibly :+infinite) number of sets. ~%~
    Each set in the intersection is indexed over a contiguous subset~%~
    of the extended integers: ~%~
    [-infinity ,...,-1,0, 1, ... +infinity]~%~
    and is accessed by applying the element function to the intersection and ~%~
    the desired index."))
 
(defmethod combine-collections ((c2 borel-set) (c1 countable-intersection))
  (combine-collections c1 c2))

(defmethod combine-collections
           ((c1 countable-intersection) (c2 borel-set))
  
  (let* ((c1-cfun (contains-set-p-function-of c1))
         (c1-ifun (element-function-of c1))
         (ifp-1 (index-from-position-function-of c1))
         (new-begin  0)
         (new-end (number-of-sets-in c1)))
    
    (make-instance (type-of c1)
      :from new-begin :to new-end
      :contains-set-p-function
      (function                          ;;is 'and' the only difference from the countable union
       (lambda (collection set)
         (and (funcall c1-cfun collection set)
              (same-set-p c2 set))))
      :disjoint?
      (if (not-disjoint-collection-p c1) NIL T)
      :element-function
      (function
       (lambda (collection index)
         (cond
          
          ((zerop index)       ; borel-set first
           c2)
          
          (T
           (funcall c1-ifun                        ; All the rest later
                    collection                     
                    (funcall
                     ifp-1
                     (- index 1))))
          ))))))




;;;----------------------------------------------------------------------------
;;;
;;;  Explicitly recorded finite unions of sets
;;;
;;;----------------------------------------------------------------------------

(defclass explicit-finite-union
  (explicit-finite-collection countable-union)
  (
   (contents
    :documentation
    "A common lisp sequence where the entire collection of sets in the union ~%~
     are stored.")
   )
  (:documentation
   "A finite union of sets (with sets stored explicitly, as opposed to ~%~
    implicitly within a countable-union."))


(defmethod initialize-instance :after ((self explicit-finite-union) &key)
  (declare (special *default-union-sort*))
  (if (and (eq NaN (infimum self))
           (eq NaN (supremum self)))
    (recalculate-bounds self))
  (if (not (sorted-collection-p self))
    (collection-sort self *default-union-sort*))
  (if (eq (closure-property-of self) :unknown)
    (determine-closure self))
)

(defmethod disjoint-collection-p ((self explicit-finite-union))
  (let ((result (disjointness-of self)))
    (if (eq result :unknown)
      (setf (disjointness-of self) (apply #'disjoint-sets-p (contents-of self)))
      result)))

(defmethod not-disjoint-collection-p ((self explicit-finite-union))
  (not (disjoint-collection-p self))) ;;works b/k if unknown gives an error
 

;;;----------------------------------------------------------------------------
;;;
;;;  Explicitly recorded finite intersection of sets
;;;
;;;----------------------------------------------------------------------------

(defclass explicit-finite-intersection
  (explicit-finite-collection countable-intersection)
  ((contents
    :documentation
    "A common lisp sequence where the entire collection of sets in the intersection ~%~
     are stored.")
  )
  (:documentation
   "A finite intersection of sets (with sets stored explicitly, as opposed to ~%~
    implicitly within a countable-intersection."))


(defmethod initialize-instance :after ((self explicit-finite-intersection) &key)
 (declare (special *default-set-less-than-predicate*))
   (if (not (sorted-collection-p self))
    (collection-sort self *default-set-less-than-predicate*))
  (if (eq (closure-property-of self) :unknown)
    (determine-closure self))                        ;try to determine closure here
  )



;;;----------------------------------------------------------------------------------
;;;
;;;  COUNTABLE-SET  
;;;
;;;  Has the property that its contents  extended-real valued and are denumerable.
;;;  Hence the new methods: element, cardinality
;;;
;;;----------------------------------------------------------------------------------

(defclass countable-set
  (countable-union)
  ((begin-index
    :initarg :begin-index)
   (end-index
    :initarg :end-index)
   (contains-set-p-function
    :initform
    #'(lambda (collection set)
        (declare (special *reasonable-number-of-iterations*))
        (if (extended-realp set)
          (do
            ((index (count-start-of collection)
                    (next-index collection index))
             (counter 1 (+ counter 1))
             (big-count *reasonable-number-of-iterations*))
            ;; end-test
            ((count-endedp collection index))
            
            ;; Now the action
            
            (cond
             ((> counter big-count)
              (if (quail-yes-or-no-p
                   "Have tested ~s sets already.  Should testing continue?"
                   (- counter 1))
                (setf big-count 
                      (+ big-count
                         (loop
                           with answer = NIL
                           until (and answer (integerp answer) (>= answer 0))
                           do
                           (setf answer
                                 (quail-query "How many more sets should be tested? ")
                                 )
                           finally (return answer))))
                (cond
                 ((quail-yes-or-no-p
                   "Assume set ~%~
                    ~s ~%~
                    is in the collection ~%~
                    ~s ? " set collection)                    (return T))
                 ((quail-yes-or-no-p
                   "Assume set ~%~
                    ~s ~%~
                    is *NOT* in the collection ~%~
                    ~s ? " set collection)                    (return NIL))
                 (T (quail-error "Can't tell whether the set ~%~
                                  ~s ~%~
                                  is in the collection ~%~
                                  ~s ~%~
                                  or not!" set collection)))))
             ((eq (element collection index) set) (return T))))
          NIL))
    
    :documentation
    "A function of two arguments which, when applied to the collection and any set, ~%~
     tests whether the specified set is in the collection.~%~
     Default function compares the given set to each set ~%~
     in the collection (ad nauseum)"))
  (:documentation
   "A countable union whose elements are extended-reals."))

#|
(defgeneric index-of (countable-set element)
  (:documentation
   "Returns index of the given element in the (countable) set.")
)
|#

;;;----------------------------------------------------------------------------------
;;;
;;;  explicit-finite-set
;;;
;;;  cardinality is never +infinity
;;;  Method for element must be specialized for any subclass.
;;;
;;;----------------------------------------------------------------------------------

(defclass explicit-finite-set
  (explicit-finite-union countable-set)
  ((begin-index
    :initarg :begin-index)
   (end-index
    :initarg :end-index)
   (closure
    :initform :closed)
   )
  (:documentation
   "A finite set whose elements are stored explicitly as a collection."))


(defmethod initialize-instance :after ((self explicit-finite-set) &key)
  (if (not (eq (closure-property-of self) :closed))
    (setf (closure-property-of self) :closed)))
  

(defmethod (setf closure-property-of)
  (new-value (self explicit-finite-set))
  (if (not (eq new-value :closed))
    (quail-error
     "Sorry.  All finite sets are closed. ~%~
      ~s is not an allowed value for an explicit-finite-set."
     new-value)
    (setf (slot-value self 'closure-property) :closed)))

(defmethod add-sets :before ((collection explicit-finite-set) &rest sets)
  (labels ((check (x &rest other-x)
             (if other-x
               (and (extended-realp x)
                    (apply #'check other-x))
               (extended-realp x))))
    (if (and sets
             (not (apply #'check sets)))
      (quail-error "An attempt was made to add a non-extended real ~s ~%~
                    to an explicit-finite-set (~s)." sets collection))))