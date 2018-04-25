;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               borel-sets.lisp
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
;;;                             BOREL SETS
;;;
;;;  In this file the classes representing Borel-Sets and operators on those
;;;  classes are defined.  Various specialized methods which implement the
;;;  operators are defined elsewhere.
;;;
;;;  Examples are given in "borel-examples.lisp".
;;;  Special instances and relates functions, constants, and variables
;;;  are in "borel-special.lisp".
;;;
;;;  In the sequel, the base set, of which every set defined here is a subset,
;;;  is the extended real line:
;;;
;;;                [ -infinity, +infinity ]
;;;
;;;  Other important notes: *********************************************************
;;;
;;;       1. Where sensible, all operators are n-ary for n >=1
;;;          Each operator will invoke a binary method having the same name as the
;;;          operator except prefixed by "op".  For example, set-union is the name of the
;;;          general n-ary union operator which calls the method op-set-union on two arguments.
;;;          The "op" operators are specialized to do the right things on the basis
;;;          of their argument types.
;;;
;;;       2. Any extended real number (extended-real-p => T) can be treated as if
;;;          it were either a countable-set or an interval.
;;;          In either case the set is closed and has the extended real number as
;;;          its single element.
;;;          ************************************************************************
;;;
;;;
;;;  Classes:
;;;
;;;         Borel-Set                (includes any extended-real-number
;;;                                   as a singleton finite-set/closed-interval)
;;;         Empty-Set
;;;         Interval
;;;         Complement-Set
;;;         Predicate-Set
;;;
;;;
;;;
;;;  Global constant:
;;;
;;;         *the-extended-real-line*  ... [ -infinity, +infinity ].
;;;
;;;
;;;----------------------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  borel-set
;;;


(defclass borel-set (quail-object)
  ((infimum :reader infimum :initarg :infimum :initform NaN
            :documentation "The infimum of the set wrt the extended reals.")
   (supremum :reader supremum :initarg :supremum :initform NaN
             :documentation "The supremum of the set wrt the extended reals.")
   (closure-property :reader closure-property-of :initarg :closure-property :initform :unknown
            :documentation "Records what is known about the closure-property of this set."))
  (:documentation "An arbitrary Borel set on the extended real line.")
  )


(defmethod initialize-instance :after ((self borel-set) &key)
  (let ((inf (infimum self))
        (sup (supremum self)))
    (if (and (> inf sup) (not (empty-set-p self)))
      (quail-error "An attempt was made to create an instance of the class ~s~%~
             that had an infimum (~s) larger than its supremum (~s)"
             (class-name (class-of self)) inf sup))))

(defmethod (setf infimum) (new-value (self borel-set))
  (if (not (or (extended-realp new-value)
               (eq new-value NaN)))
    (quail-error
     "Illegal value ~s. ~%~
      Infimum must be an extended-real or NaN"
     new-value)
    (if (> new-value (supremum self))
      (quail-error
       "Illegal value ~s. ~%~
        Infimum must not be > supremum = ~s"
       new-value (supremum self))
      (setf (slot-value self 'infimum) new-value))))

(defmethod (setf supremum) (new-value (self borel-set))
  (if (not (or (extended-realp new-value)
               (eq new-value NaN)))
    (quail-error
     "Illegal value ~s. ~%~
      Supremum must be an extended-real or NaN"
     new-value)
    (if (< new-value (infimum self))
      (quail-error
       "Illegal value ~s. ~%~
        Supremum must not be > infimum = ~s"
       new-value (infimum self))
      (setf (slot-value self 'supremum) new-value))))

(defmethod (setf closure-property-of) (new-value (self borel-set))
  (if (not (or (eq new-value :closed)
               (eq new-value :neither)
               (eq new-value :open)
               (eq new-value :both)
               (eq new-value :unknown)
               (eq new-value :left-closed)
               (eq new-value :right-closed)
               (eq new-value :left)
               (eq new-value :right)              
               (eq new-value :left-open)
               (eq new-value :right-open)
               (eq new-value :closed-left)
               (eq new-value :closed-right)
               (eq new-value :open-left)
               (eq new-value :open-right)))
    (quail-error
     "~s is not a defined closure-property"
     new-value)
    (setf (slot-value self 'closure-property) new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Plus the generic functions
;;;  determine-closure
;;;  recalculate-bounds
;;;  cardinality
;;;


(defgeneric determine-closure (borel-set)
  (:documentation
   "Tries to determine closure, then sets the closure-property accordingly.~%~
    N.B. This includes setting it to :unknown if it fails.")
  )

(defgeneric recalculate-bounds (borel-set)
  (:documentation
   "Attempts to determine and set the infimum and supremum slots of the ~%~
    set.~%~
    N.B. This includes setting it to :unknown if it fails.")
  )
(defgeneric cardinality (borel-set)
  (:documentation
   "The cardinality of the given borel-set.")
)


;;;-----------------------------------------------------------------------------------
;;;
;;;  The generic "op-" functions.
;;;
;;;-----------------------------------------------------------------------------------

(defgeneric op-borel-set-p (borel-set)
  (:documentation
   "Returns T or NIL depending on whether or not the argument is a borel-set.")
  )

(defgeneric op-set-complement (borel-set1 borel-set2)
  (:documentation
   "Returns the complement of its first argument in its second.")
  )
   
(defgeneric op-set-union (borel-set1 borel-set2)
  (:documentation
   "Returns the union of two borel-sets.")
  )
   
(defgeneric op-set-intersection (borel-set1 borel-set2)
  (:documentation
   "Returns the intersection of two borel-sets")
  )
   
(defgeneric op-memberp (borel-set number)
  (:documentation
   "Tests whether the number is a member of the given borel-set.")
  )

(defgeneric op-insidep (borel-set1 borel-set2)
  (:documentation
   "Tests whether the first borel-set is a subset of the second.")
  )
   
(defgeneric op-containsp (borel-set1 borel-set2)
  (:documentation
   "Tests whether the second borel-set is a subset of the first.")
  )
   
(defgeneric op-empty-set-p (borel-set)
  (:documentation
   "Tests whether this borel-set is empty (T) or not (NIL).")
  )
   
(defgeneric op-closed-set-p (borel-set)
  (:documentation
   "Tests whether this borel-set is KNOWN to be closed." )
  )
   
(defgeneric op-open-set-p (borel-set)
  (:documentation
   "Tests whether this borel-set is KNOWN to be open.")
  )
   
(defgeneric op-neither-closure-p (borel-set)
  (:documentation
   "Tests whether this borel-set is KNOWN to be closed.")
  )

(defgeneric op-set-difference (borel-set1 borel-set2)
  (:documentation
   "Returns the set difference S1 - S2 of its two arguments S1 and S2")
  )

(defgeneric op-disjoint-sets-p (set1 set2)
  (:documentation
   "Checks to see if set1 and set2 are disjoint."))


;;;----------------------------------------------------------------------------------
;;;
;;; EMPTY-SET
;;;
;;;----------------------------------------------------------------------------------

(defclass empty-set (borel-set)
  ((infimum :allocation :class
            :documentation
            "Infimum of the empty set is +infinity.")
   (supremum :allocation :class
             :documentation
             "Supremum of the empty set is -infinity.")
   (closure-property :initform :both :allocation :class
            :documentation "The empty set is both open and closed."))
  (:documentation "A borel-set having no elements.")
  )

(defmethod initialize-instance :after ((self empty-set) &key)
  (setf (infimum self) +infinity)
  (setf (supremum self) -infinity)
   self)

(defmethod (setf infimum) (new-value (self empty-set))
  (if (not (eq new-value +infinity))
    (quail-error
     "Illegal value ~s. ~%~
      Infimum of the empty-set is +infinity"
     new-value)
    (setf (slot-value self 'infimum) new-value)))

(defmethod (setf supremum) (new-value (self empty-set))
  (if (not (eq new-value -infinity))
    (quail-error
     "Illegal value ~s. ~%~
      'supremum of the empty-set is -infinity"
     new-value)
    (setf (slot-value self 'supremum) new-value)))


;;;----------------------------------------------------------------------------
;;;
;;; INTERVALS on the real-line.
;;;
;;;----------------------------------------------------------------------------

(defclass interval

  (borel-set)

  ((infimum :initform -infinity)
   (supremum :initform +infinity)
   (closure-property :initform :open
            :documentation
            "Closure-property of an interval is by default :open. ~%~
             Other possibilities are: ~%~
             :left  (i.e. closed left, open right) ~%~
             :right (i.e. closed right, open left) ~%~
             :closed, :open, and :both (only for an extended real line).")

   )
  (:documentation "An arbitrary interval on the extended real line.")
  )


(defmethod initialize-instance :after ((self interval) &key)
  (if (and (= (infimum self) (supremum self))
           (not (eq (closure-property-of self) :closed)))
    (quail-error "An interval having equal infimum and supremum (here ~s) ~
            must be closed on both ends.~%This one's closure-property was ~s."
           (infimum self) (closure-property-of self))))

(defmethod (setf infimum) :before (new-value (self interval))
  (if (and (= new-value (supremum self))
           (not (eq (closure-property-of self) :closed)))
    (quail-error "An interval having equal infimum and supremum (here ~s) ~
                  must be closed on both ends.~%This one's closure-property was ~s."
                 new-value (closure-property-of self))
    new-value))

(defmethod (setf supremum) :before (new-value (self interval))
  (if (and (= new-value (infimum self))
           (not (eq (closure-property-of self) :closed)))
    (quail-error "An interval having equal infimum and supremum (here ~s) ~
                  must be closed on both ends.~%This one's closure-property was ~s."
                 new-value (closure-property-of self))
    new-value))

(defmethod (setf closure-property-of) :before (new-value (self interval))
  (if (and (= (infimum self) (supremum self))
           (not (eq new-value :closed)))
    (quail-error "An interval having equal infimum and supremum (here ~s) ~
                  must be closed on both ends."
                 (infimum self))
    new-value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Some generic functions peculiar to intervals
;;;

(defgeneric left-open-p (interval)
  (:documentation
   "Tests whether the interval is open on the left.")
  )

(defgeneric left-closed-p (interval)
  (:documentation
   "Tests whether the interval is closed on the left.")
  )

(defgeneric right-open-p (interval)
  (:documentation
   "Tests whether the interval is open on the right.")
  )

(defgeneric right-closed-p (interval)
  (:documentation
   "Tests whether the interval is closed on the right.")
  )

(defgeneric separatep (interval1 interval2)
  (:documentation
   "Tests whether two intervals have null intersection.")
  )

;;;----------------------------------------------------------------------------
;;;
;;;  Complement Sets
;;;
;;;----------------------------------------------------------------------------

(defclass complement-set (borel-set)
  ((set
    :initarg :set
    :accessor set-of
    :documentation
    "The set of which the complement was taken with respect to ~%~
     the complement-set.")
   (wrt
    :initarg :wrt
    :initform *the-extended-real-line*
    :accessor complement-wrt
    :documentation
    "The default value is the extended real line.")
   )
  (:documentation
   "The complement of a set with respect to  ~%~
    the complement-set."))

#|
(defmethod infimum ((self complement-set))
  (let ((inf1 (infimum (set-of self)))
        (inf2 (infimum (comparison-set-of self)))
        (sup1 (supremum (set-of self))))
    (cond ((or (< inf2 inf1)
               (> inf2 sup1)
               (and (= inf1 inf2)
                    (open-left (set-of self))
                    (closed-left (comparison-set-of self))))
           inf2)
          ((
            
|#

;;;----------------------------------------------------------------------------
;;;  
;;;  Predicate-Sets
;;;
;;;  Set defined where inclusion is determined by a predicate function p.
;;;
;;;  i.e.    { x : p(x) = T , x real in [-infinity, +infinity] }
;;; 
;;;  is called a predicate-set here.
;;;
;;;----------------------------------------------------------------------------

(defclass predicate-set
  (borel-set)
  ((defining-predicate
     :accessor defining-predicate-of
     :type function
     :initarg :defining-predicate
;;;     :initform #'extended-reals-defining-predicate
     :initform #'extended-realp
     :documentation
     "A function of two arguments: the set and the test-element.~%~
      When applied to the predicate-set and an extended-real~%~
      it returns T if the value is in the set and NIL otherwise."))
  (:documentation
   "Set defined where inclusion is determined by a predicate function p. ~%~%~
    i.e.    { x : p(x) = T , x real in [-infinity, +infinity] } ~%~%~
    is called a predicate-set here. ~%~
    The role of p() is played by a function attached to the slot ~%~
    called defining-property."))


