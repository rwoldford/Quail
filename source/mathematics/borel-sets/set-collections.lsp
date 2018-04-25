;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               set-collections.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1990, 1992
;;;     N. Wiebe 1998
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                             COLLECTIONS OF SETS
;;;
;;;  In this file the classes representing arbitrary collections of sets and operators
;;;  on those classes are defined.  Various specialized methods which implement the
;;;  operators are defined elsewhere.
;;;
;;;  These collections are most interesting to us when their members must be borel-sets.
;;;  (See Borel-Sets.lisp for class definitions on Borel-Sets.)
;;;
;;;
;;;  Classes:
;;;
;;;
;;;    1. set-collection                ... The base class.
;;;
;;;                  Operators:   contains-set-p
;;;                               disjoint-collection-p
;;;                               not-disjoint-collection-p
;;;                               combine-collections.
;;;
;;;
;;;    2. countable-collection          ... A countable set-collection with each set is
;;;                                         indexed by an integer.
;;;
;;;                  Additional
;;;                  Operators:   begin-index-of
;;;                               end-index-of
;;;                               number-of-sets-in
;;;                               finite-collection-p
;;;                               element
;;;                               element-function-of
;;;                               index.
;;;
;;;                                     
;;;    3. explicit-finite-collection    ... A finite countable-collection whose sets
;;;                                         are stored explicitly in a sequence.
;;;                                         Members can be accessed by index from 0 to
;;;                                         N-1.
;;;
;;;                  Additional
;;;                  Operators:   add-sets              - add sets to the collection
;;;                               remove-sets           - remove ...
;;;                               remove-sets-if        - first arg is a predicate
;;;                               delete-sets           - destructive remove
;;;                               delete-sets-if        - ibid
;;;                               collection-sort       - order sets according to some
;;;                                                       pairwise "strictly less than"
;;;                                                       predicate for two sets.
;;;                               safe-collection-sort  - non-destructive sort
;;;                               sorted-collection-p    - test if sorted
;;;                               reset-end-index       - handy to reset count
;;;                                    
;;;
;;;                                     
;;;
;;;  Wouldn't it be neat if we also had:
;;;
;;;    4. algebra                       ... A collection of subsets of a base set that is
;;;                                         closed under complement and finite union or 
;;;                                         intersection (choose one, the other follows
;;;                                         by De Morgan).
;;;  and
;;;    5. sigma-algebra                 ... An algebra closed under countable union and
;;;                                         intersection.
;;;
;;;  Unfortunately, contains-set-p is a tad difficult to implement for algebras. :-)
;;;  (Though not impossible to rig up a "close-enough" approximation assuming there
;;;   are generators defined for the algebra and all sets to be tested for inclusion
;;;   are constructed directly from the generators using complement, union and
;;;   intersection operators as defined by us. ... rwo)
;;;
;;;----------------------------------------------------------------------------------


;;;----------------------------------------------------------------------------------
;;;
;;;  Arbitrary collection of sets
;;;
;;;----------------------------------------------------------------------------------

(defclass set-collection (quail-object)
  ((contains-set-p-function
          :accessor contains-set-p-function-of
          :initarg :contains-set-p-function
          :initform #'(lambda (self set)
                        (quail-yes-or-no-p
                         "Does the collection: ~%~
                          ~s ~%~
                          contain the set: ~%~
                          ~s ?"
                         self set))
          :type function
          :documentation
          "A function of two arguments which, when applied to the collection and any set, ~%~
           tests whether the specified set is in the collection.")

   (disjointness
    :initarg :disjoint?
    :initform :unknown
    :accessor disjointness-of
    :documentation
    "A slot (T, NIL, or :unknown) indicating whether the sets in the collection are known ~%~
     a priori to be disjoint (i.e. empty intersections), not disjoint, or unknown (the default)."))


  (:documentation
   "The class representing an arbitrary collection of sets. ~%~
    Membership in the collection is determined by the function~%~
    found on the contains-set-p-function slot of the collection.  The ~%~
    default is that all borel-sets are in the collection."))



(defgeneric contains-set-p (set-collection set)
  (:documentation
   "Test whether the collection has the given set as a member.")
  )

(defgeneric disjoint-collection-p (set-collection)
  (:documentation
   "Test whether the sets of this collection are all disjoint.")
  )

(defgeneric not-disjoint-collection-p (set-collection)
  (:documentation
   "Test whether the sets of this collection are not all disjoint.")
  )

(defgeneric combine-collections (set-collection1 set-collection2)
  (:documentation
   "Combine two collections into a single one. ~%~
    N.B.  No guarantee a set does not appear more than once.")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The methods
;;;


(defmethod contains-set-p ((collection set-collection)
                          set)
  (funcall (contains-set-p-function-of collection) collection set))

(defmethod disjoint-collection-p ((collection set-collection))
  (let ((result (disjointness-of collection)))
    (if (eq result :unknown)
      NIL
      result)))

(defmethod not-disjoint-collection-p ((collection set-collection))
  (let ((result (disjointness-of collection)))
    (if (eq result :unknown)
      NIL
      (not result))))
  
(defmethod combine-collections
  ((c1 set-collection) (c2 set-collection))
  (let ((new-collection
         (make-instance 'set-collection))
        (c1-fun (contains-set-p-function-of c1))
        (c2-fun (contains-set-p-function-of c2)))
    
    (setf (contains-set-p-function-of new-collection)
          (function
           (lambda (collection set)
              (or (funcall c1-fun collection set)
                  (funcall c2-fun collection set)))))
    
    (setf (disjointness-of new-collection)
          (if  (and (not-disjoint-collection-p c1)
                    (not-disjoint-collection-p c2))
            NIL
            :unknown))
    
    new-collection))



;;;----------------------------------------------------------------------------------
;;;
;;;  Countable collection of sets
;;;
;;;----------------------------------------------------------------------------------

  
(defclass countable-collection
  (set-collection)

  ((begin-index
    :initarg :from
    :initform 0
    :reader begin-index-of
    :documentation
    "The starting index of the collection. Must be an extended integer.  ~%~
     Default is 0.)")

   (end-index
    :initarg :to
    :initform +infinity
    :reader end-index-of
    :documentation
    "The last index of the collection. Must be an extended integer.  ~%~
     Default is +infinity.")

   (count-start
    :accessor count-start-of
    :type function
    :documentation
    "The integer index where complete enumeration of the collection ~%~
     begins.")

   (count-end-test
    :accessor count-end-test-of
    :type function
    :documentation
    "A function of two arguments: the countable-collection and an extended-integer ~%~
     representing the current index in enumeration of the collection. ~%~
     When applied, it produces T if the set has been completely enumerated ~%~
     and NIL otherwise.")

   (count-increment-function
    :accessor count-increment-function-of
    :type function
    :documentation
    "A function of two arguments: the countable-collection and a single ~%~
     extended-integer index representing the current index in the count. ~%~
     When applied, it produces the next index in the counting sequence. ~%~
     A sensible default will be produced based on the values of ~%~
     the local slots begin-index, end-index, count-start, and count-end.")
   
   (index-from-position-function
    :accessor index-from-position-function-of
    :type function
    :documentation
    "A function of two arguments: the countable-collection and a single ~%~
     non-negative extended-integer representing the 0-based position that ~~
     the desired index hold in the enumeration order of the collection.")
 
   (position-from-index-function
    :accessor position-from-index-function-of
    :type function
    :documentation
    "A function of two arguments: the countable-collection and a single ~%~
     extended-integer representing the index of the set of interest in ~%~
     the collection. ~%~
     Returns a non-negative extended-integer giving the 0-based position that ~~
     the desired set has in the enumeration order of the collection.")

   (element-function
    :accessor element-function-of
    :initarg :element-function
    :initform #'(lambda (collection index)
                  (declare (ignore index))
                  (quail-error
                   "No element-function has been defined for this collection: ~%~
                    ~s ."
                   collection))
    :type function
    :documentation
    "A function of two arguments.  The first argument is the entire collection. ~%~
     The second is an integer, i say, representing the index of the set to be retrieved. ~%~
     It must lie between the begin-index-of and end-index-of the collection (inclusive).  ~%~
     Function must return the set i of the collection.")
   
   (index-function
    :accessor index-function-of
    :initarg :index-function
    :initform
    #'(lambda (collection set)
        (declare (special *reasonable-number-of-iterations*))
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
                         (setf answer (quail-query "How many more sets should be tested? "))
                         finally (return answer))))
              (return NIL)))
           ((same-set-p (element collection index) set) (return index)))))

    :documentation
    "A function of two arguments.  The first argument is the entire collection. ~%~
     The second is a set whose index in the collection is to be retrieved. ~%~
     The function returns this index or NIL if set was not found.")


   (contains-set-p-function
          :initform
          #'(lambda (collection set)
              (declare (special *reasonable-number-of-iterations*))
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
                  ((same-set-p (element collection index) set) (return T)))))

          :documentation
          "A function of two arguments which, when applied to the collection and any set, ~%~
           tests whether the specified set is in the collection.~%~
           Default function compares the given set to each set ~%~
           in the collection (ad nauseum)"))

  (:documentation
   "The collection of a countable number of sets. ~%~
    Each set in the collection is indexed by an integer between the begin-index ~%~
    and the end-index (inclusively).~%~
    A set is accessed by applying the element function to the collection and ~%~
    the desired index.  This in turn calls the function stored on the slot  ~%~
    called element-function."))


(defmethod initialize-instance :after ((self countable-collection) &key)
  (let ((last (end-index-of self))
        (first (begin-index-of self)))
    (if (not (and (extended-integerp first)
                  (extended-integerp last)))
      (quail-error "Can't define a collection indexed from ~s to ~s.~%~
               The begin and end indices must be extended-integers." first last)
      (set-count-controls self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  SETF METHODS
;;;  to check values on slots begin- and end-index.
;;;


(defmethod (setf begin-index-of) (new-value (collection countable-collection))
  (if (extended-integerp new-value)
    (progn
      (setf (slot-value collection 'begin-index) new-value)
      (set-count-controls collection)
      new-value)
    (quail-error "New-value must be an extended-integer, it cannot be ~s."
             new-value)))


(defmethod (setf end-index-of) (new-value (collection countable-collection))
  (if (extended-integerp new-value)
    (progn
      (setf (slot-value collection 'end-index) new-value)
      (set-count-controls collection)
      new-value)
    (quail-error "New-value must be an extended-integer, it cannot be ~s."
             new-value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  New operators
;;;


(defgeneric element (countable-collection integer)
   (:documentation
   "Returns the set in the countable collection having index i, where~%~
    i is an integer between the begin-index and the end-index (inclusive).")
  )  

(defgeneric index-of (countable-collection set)
  (:documentation
   "Returns the index in the countable collection of the specified set. ~%~
    If set is not found, NIL is returned.")
  )

(defgeneric number-of-sets-in (countable-collection)
  (:documentation
   "Returns the number of sets in the countable collection.")
  )


(defgeneric finite-collection-p (countable-collection)
  (:documentation
   "Tests whether the number of sets in the countable collection is finite.")
  )


(defgeneric set-count-controls (countable-collection)
  (:documentation
   "Sets the count control slots: count-start, count-increment-function, and ~%~
    count-end-test.  Based on the current values of begin-index and end-index.")
  )


(defgeneric next-index (countable-collection current-index)
  (:documentation
   "Returns the next-index after the current-index. ~%~
    Used in complete enumeration of a set.")
  )

(defgeneric count-endedp (countable-collection current-index)
  (:documentation
   "Used in complete enumeration of a set.  ~%~
    End test for the enumeration.")
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The methods for countable-collection
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  The following method does not work for
;;;  combinations of countable- and explicit-finite-collections
;;;  Beats me why..  methinks a bug in MACL ... :-) rwo

(defmethod combine-collections
  ((c1 countable-collection) (c2 countable-collection))
  (let* ((c1-cfun (contains-set-p-function-of c1))
         (c2-cfun (contains-set-p-function-of c2))
         (c1-ifun (element-function-of c1))
         (c2-ifun (element-function-of c2))
         (ifp-1 (index-from-position-function-of c1))
         (ifp-2 (index-from-position-function-of c2))
         (n1 (number-of-sets-in c1))
         (n2 (number-of-sets-in c2))
         (small-n (min n1 n2))
         (new-begin  0)
         (new-end (+ n1 n2 -1))
         (small-c (if (<= n1 n2)
                   1
                   2)))
    
    (make-instance (cond 
                    ((subtypep (type-of c1) (type-of c2))
                     (type-of c2))
                    ((subtypep (type-of c2) (type-of c1))
                     (type-of c1))
                    (T 'countable-collection))
      :from new-begin :to new-end
      :contains-set-p-function
        (function
           (lambda (collection set)
             (or (funcall c1-cfun collection set)
                 (funcall c2-cfun collection set))))
        :disjoint?
        (if  (and (not-disjoint-collection-p c1)
                  (not-disjoint-collection-p c2))
            NIL
            :unknown)
        :element-function
        (function
           (lambda (collection index)
             (cond
              ((eq index +infinity)                       ; Are we over-reaching?
               (if (eq small-n +infinity)
                 (quail-error
                  "Set with index +infinity is not well-defined for ~%~
                   the collection: ~s " collection)
                 (case small-c
                   (1 (funcall c2-ifun
                               c2
                               (funcall ifp-2 (/ index 2)))) ;always +infinity anyway
                   (2 (funcall c1-ifun            ;isn't it the other way around?
                               c1
                               (funcall ifp-1 (/ index 2)))))))
              
              ((and (evenp index)
                    (< (/ index 2) small-n))        ; First evens go to the
               (case small-c                                ; smallest collection.
                 (1 (funcall c1-ifun
                             c1 
                             (funcall ifp-1 (/ index 2))
                             ))
                 (2 (funcall c2-ifun
                             c2
                             (funcall ifp-2 (/ index 2))))))

              ((and (oddp index)                            ; First odds go to the
                    (< (/                           ; larger collection.
                               (- index 1)
                                  2)
                           small-n))
               (case small-c
                 (1 (funcall c2-ifun                        ; Note switch here.
                             c2
                             (funcall ifp-2
                                      (/
                                        (- index 1)
                                           2))))
                 (2 (funcall c1-ifun
                             c1
                             (funcall ifp-1
                                      (/
                                        (- index 1)
                                           2))))))

              (T
               (case small-c
                 (1 (funcall c2-ifun                        ; All the rest go to the
                             c2                     ; larger collection.
                             (funcall
                              ifp-2
                              (- index small-n))))
                 (2 (funcall c1-ifun
                             c1
                             (funcall
                              ifp-1 
                              (- index small-n))
                             )))))                          ; end cond
             )))))

;;;;;;;;;;
;;;


(defmethod number-of-sets-in ((collection countable-collection))
  (let* ((b-i (begin-index-of collection))
         (e-i (end-index-of collection))
         (max-i (max b-i e-i))
         (min-i (min b-i e-i)))
    (+ 1 (- max-i min-i))))


(defmethod finite-collection-p ((self countable-collection))
  (< (number-of-sets-in self) +infinity))


(defmethod element ((self countable-collection) index)
  (if (extended-integerp index)
    (if (or (> index
                   (max (end-index-of self)
                            (begin-index-of self)))
            (< index
                   (min (end-index-of self)
                            (begin-index-of self))))
      (quail-error "Index (~s) is out of range. ~%~
               All indices must be between ~s and ~s inclusively."
              index (begin-index-of self) (end-index-of self))
      (funcall (element-function-of self) self index))
    (quail-error "Illegal index: ~s ~%Must be an extended-integer."
            index)))


(defmethod index-of ((collection countable-collection) set)
  (funcall (index-function-of collection) collection set))


(defmethod contains-set-p ((collection countable-collection) set)
  (funcall (contains-set-p-function-of collection) collection set))

(defmethod next-index
  ((collection countable-collection) current-index)
  (if (extended-integerp current-index)
    (funcall
     (count-increment-function-of collection)
     collection current-index)
    (quail-error "Illegal argument: ~s ~%~
             Current index must be an extended-integer."
            current-index)))
  

(defmethod count-endedp ((collection countable-collection) index)
  (if (extended-integerp index)
    (funcall (count-end-test-of collection) collection index)
    (quail-error "Illegal argument: ~s ~%~
             Index must be an extended-integer."
            index)))

(defmethod disjoint-collection-p ((collection countable-collection))
  (let ((result (disjointness-of collection)))
    (if (eq result :unknown)
      NIL
      result)))

(defmethod not-disjoint-collection-p ((collection countable-collection))
  (let ((result (disjointness-of collection)))
    (if (eq result :unknown)
      NIL
      (not result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The following is a critically
;;;  important method.  In the future,
;;;  most of the functions in it should be defined
;;;  once, stored in a class-shared hash-table and
;;;  looked up when needed! ... rwo
;;;

 
(defmethod set-count-controls ((collection countable-collection))

  (let* ((begin-index (begin-index-of collection))
         (end-index (end-index-of collection))
         (max-index (max begin-index end-index))
         (min-index (min begin-index end-index)))

   (cond

     ((and (= min-index -infinity)
           (= max-index +infinity))
      (setf (count-start-of collection) 0)
      (setf (count-end-test-of collection)
            #'(lambda (collection current-index)
                (declare (ignore collection current-index))
                NIL))
      (setf (count-increment-function-of collection)
            #'(lambda (collection current-index)
                (declare (ignore collection))
                (cond
                 ((= current-index 0) 1)
                 ((> current-index 0) (* -1 current-index))
                 ((< current-index 0)
                  (+ 1 (* -1 current-index))))))
      (setf (index-from-position-function-of collection)
            #'(lambda (position)
                (cond
                 ((or (not (extended-integerp position))
                      (< position 0))
                  (quail-error
                   "No such position exists in the enumeration: ~s" position))
                 ((= position +infinity)
                  (quail-error
                   "This position is not well defined in the current enumeration: ~s"
                   position))
                 ((zerop position) 0)
                 ((evenp position) (* -1 (/ position 2)))
                 (T (/ (+ position 1) 2)))))
      (setf (position-from-index-function-of collection)
            #'(lambda (index)
                (cond
                 ((not (extended-integerp index))
                  (quail-error
                   "Illegal index, ~s, must be an extended-integer." index))
                 ((or (= index +infinity)
                      (= index -infinity))
                  (quail-error
                   "This index does not have a well defined position in the ~
                    current enumeration:~%~s"
                   index))
                 ((zerop index) 0)
                 ((< index 0) (* -2 index))
                 (T (- (* index 2) 1))))))
                 
     
     
     ((and (= min-index -infinity)
           (> max-index -infinity))
      (setf (count-start-of collection) max-index)
      (setf (count-end-test-of collection)
            #'(lambda (collection current-index)
                (declare (ignore collection current-index))
                NIL))
      (setf (count-increment-function-of collection)
            #'(lambda (collection current-index)
                (declare (ignore collection))
                (- current-index 1)))
      (setf (index-from-position-function-of collection)
            #'(lambda (position)
                (cond
                 ((or (not (extended-integerp position))
                      (< position 0))
                  (quail-error
                   "No such position exists in the enumeration: ~s" position))
                 (T (- max-index position)))))
      (setf (position-from-index-function-of collection)
            #'(lambda (index)
                (cond
                 ((not (extended-integerp index))
                  (quail-error
                   "Illegal index, ~s, must be an extended-integer." index))
                 ((> index max-index)
                  (quail-error
                   "Index out of range: ~s.~%~
                    Must be an extended-integer <= ~s." index max-index))
                 ((= index -infinity) +infinity)
                 (T (- index max-index))))))
     
     
     ((and (= max-index +infinity)
           (< min-index +infinity))
      (setf (count-start-of collection) min-index)
      (setf (count-end-test-of collection)
            #'(lambda (collection current-index)
                (declare (ignore collection current-index))
                NIL))
      (setf (count-increment-function-of collection)
            #'(lambda (collection current-index)
                (declare (ignore collection))
                (+ current-index 1)))
      (setf (index-from-position-function-of collection)
            #'(lambda (position)
                (cond
                 ((or (not (extended-integerp position))
                      (< position 0))
                  (quail-error
                   "No such position exists in the enumeration: ~s" position))
                 (T (+ position min-index)))))
      (setf (position-from-index-function-of collection)
            #'(lambda (index)
                (cond
                 ((not (extended-integerp index))
                  (quail-error
                   "Illegal index, ~s, must be an extended-integer." index))
                 ((< index min-index)
                  (quail-error
                   "Index out of range: ~s.~%~
                    Must be an extended-integer >= ~s." index min-index))
                 ((= index +infinity) +infinity)
                 (T (- index min-index))))))
     

     ((and (> min-index -infinity)
           (< max-index +infinity))
      (setf (count-start-of collection) min-index)
      (setf (count-end-test-of collection)
            #'(lambda (collection current-index)
                (> current-index
                       (max (begin-index-of collection)
                                (end-index-of collection)))))
      (setf (count-increment-function-of collection)
            #'(lambda (collection current-index)
                (declare (ignore collection))
                (+ current-index 1)))
      (setf (index-from-position-function-of collection)
            #'(lambda (position)
                (cond
                 ((or (not (extended-integerp position))
                      (< position 0)
                      (> position (- max-index min-index)))
                  (quail-error
                   "No such position exists in the enumeration: ~s" position))
                 (T (+ position min-index)))))
      (setf (position-from-index-function-of collection)
            #'(lambda (index)
                (cond
                 ((not (extended-integerp index))
                  (quail-error
                   "Illegal index, ~s, must be an extended-integer." index))
                 ((or (< index min-index)
                      (> index max-index))
                  (quail-error
                   "Index out of range: ~s.~%~
                    Must be an extended-integer in [~s,~s]." index min-index max-index))
                 (T (- index min-index))))))
     
      
     ((= begin-index end-index)
      (setf (count-start-of collection) begin-index)
      (setf (count-end-test-of collection)
            #'(lambda (collection current-index)
                (/= current-index
                        (begin-index-of collection))))
      (setf (count-increment-function-of collection)
            (if  (or (= begin-index +infinity)
                     (= begin-index -infinity))
              #'(lambda (collection current-index)
                  (if (= current-index (begin-index-of collection))
                    ;; any extended-integer /= begin-index will do
                    0
                    (begin-index-of collection)))
              #'(lambda (collection current-index)
                  (if (= current-index (begin-index-of collection))
                    ;; any extended-integer /= begin-index will do
                    +infinity
                    (begin-index-of collection)))))
      (setf (index-from-position-function-of collection)
            #'(lambda (position)
                (cond
                 ((or (not (extended-integerp position))
                      (/= position 0))
                  (quail-error
                   "Only position 0 exists in this enumeration, not ~s" position))
                 (T 0))))
      (setf (position-from-index-function-of collection)
            #'(lambda (index)
                (cond
                 ((not (extended-integerp index))
                  (quail-error
                   "Illegal index, ~s, must be an extended-integer." index))
                 ((/= index begin-index)
                  (quail-error
                   "Index out of range: ~s.~%~
                    Must be an extended-integer in [~s,~s]." index min-index max-index))
                 (T 0)))))
     ))
  T)




;;;----------------------------------------------------------------------------------
;;;
;;;  Finite collections of sets whose contents are stored explicitly
;;;  on a contents slot (as opposed to implicitly as above).
;;;
;;;----------------------------------------------------------------------------------

  
(defclass explicit-finite-collection
  (countable-collection)

  ((contents
    :initform NIL
    :initarg :contents
    :reader contents-of
    :documentation
    "The entire contents of the collection explicitly constructed and ~%~
     stored in a sequence.")
   
   (set-less-than-predicate
    :initform NIL
    :initarg :set-less-than-predicate
    :reader set-less-than-predicate-of
    :documentation
    "Function of two arguments (set1 set2) used to order the contents ~%~
     of the collection.")
   
   ;; Now modifications of inherited slots.

   (begin-index
    :initform 0
    :reader begin-index-of
    :documentation
    "The starting index of the collection. ~%~
     Always 0 for this kind of collection.")

   (end-index
    :initform -1
    :reader end-index-of
    :documentation
    "The last index of the collection.  ALWAYS equal to ~%~
     (- (length (contents-of collection)) 1 ) .")

   (element-function
    :initform #'(lambda (collection index)
                  (elt (contents-of collection) index))
    :initarg :element-function-overruled-by
    :documentation
    "A function of two arguments.  The first argument is the entire collection. ~%~
     The second is a non-negative integer, i say, representing the zero-based ~%~
     index of the set to be retrieved. ~%~
     Function is expected to return the i'th set in the collection. ~%~
     Default should not need to be changed but can be overruled at ~%~
     initialization with initarg :element-function-overruled-by .")

   (contains-set-p-function
    :initarg  :contains-set-p-function-overruled-by
    :initform
     #'(lambda (collection set)
         (find set (contents-of collection) :test #'same-set-p))
     :documentation
     "A function of two arguments which, when applied to the collection  ~%~
      and any set, tests whether the specified set is in the collection. ~%~
      Default should not need to be changed but can be overruled at ~%~
      initialization with initarg :contains-set-p-function-overruled-by ."))

  (:documentation
   "The collection of a finite number of sets. ~%~
    All sets in the collection are instantiated and explicitly stored
    in the contents-of this collection. ~%~
    Each set in the collection is indexed by an integer between the begin-index, 0, ~%~
    and the end-index (inclusively).~%~
    A set is accessed by applying the element function to the collection and ~%~
    the desired index.  This in turn calls the function stored on the slot  ~%~
    called element-function."))


(defmethod initialize-instance :after
  ((self explicit-finite-collection) &key)
  (if (set-less-than-predicate-of self)
    ;; Then sort the contents.
    ;; The sort automatically resets the end-index.
    (collection-sort self (set-less-than-predicate-of self))
    ;; Else make sure the size of the collection is correct.
    (reset-end-index self)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  SETF METHODS
;;;  
;;;


(defmethod (setf begin-index-of)
  (new-value (collection explicit-finite-collection))
  
  (if (and (integerp new-value) (zerop new-value))
    
    (setf (slot-value collection 'begin-index) new-value)
    
    (quail-error "This kind of collection must have begin-index = 0.")))


(defmethod (setf end-index-of)
  (new-value (collection explicit-finite-collection))
  
  (if (and (integerp new-value)
           (= new-value 
              (- (length (contents-of collection)) 1))) 
    
    (setf (slot-value collection 'end-index) new-value)
    
    (quail-error "This kind of collection must have end-index = ~s."
            (- (length (contents-of collection)) 1))))



(defmethod set-count-controls
  ((collection explicit-finite-collection))
  (let ((n-1 (- (number-of-sets-in collection) 1)))
    (setf (count-start-of collection) 0)
    (setf (count-end-test-of collection)
          #'(lambda (collection current-index)
                  (> current-index
                         (end-index-of collection))))
    (setf (count-increment-function-of collection)
          #'(lambda (collection current-index)
              (declare (ignore collection))
              (+ current-index 1)))
    (setf (index-from-position-function-of collection)
          #'(lambda (position)
              (cond
               ((or (not (extended-integerp position))
                    (< position 0)
                    (> position n-1))
                    (quail-error
                     "No such position exists in the enumeration: ~s" position))
               (T position))))
    (setf (position-from-index-function-of collection)
          #'(lambda (index)
              (cond
               ((or (not (extended-integerp index))
                    (< index 0)
                    (> index n-1))
                    (quail-error
                     "No such index exists in the enumeration: ~s" index))
               (T index)))))
  T)
     
      
(defmethod number-of-sets-in ((collection explicit-finite-collection))
  (let* ((b-i (begin-index-of collection))
         (e-i (end-index-of collection))
         (max-i (max b-i e-i))
         (min-i (min b-i e-i)))
    (if (contents-of collection)
      (+ 1 (- max-i min-i))
      0)))

     
(defmethod (setf contents-of)
  (new-value (collection explicit-finite-collection))
  
  (if (typep new-value 'sequence) 
    
    (let ()
      (setf (slot-value collection 'contents) new-value)
      (reset-end-index collection)
      new-value)
    
    (quail-error "New-value for contents, ~s, is not of type sequence."
            new-value)))


(defmethod (setf set-less-than-predicate-of)
  (new-value (collection explicit-finite-collection))
  
  (cond
   ((null new-value)
    (setf (slot-value collection 'set-less-than-predicate) new-value))
   ((typep new-value 'function)
    (setf (slot-value collection 'set-less-than-predicate) new-value))
   ((and (typep new-value 'list)
         (eq (car new-value) 'lambda))
    (setf (slot-value collection 'set-less-than-predicate)
          (function new-value)))
   ((symbolp new-value)
    (cond ((or (special-form-p new-value)
               (macro-function new-value))
           (setf (slot-value collection 'set-less-than-predicate)
                 (eval
                  `(function
                    (lambda (s1 s2)
                     (funcall ,new-value s1 s2))))))
          ((fboundp new-value)
           (setf (slot-value collection 'set-less-than-predicate)
                 (symbol-function new-value)))))
   (T (quail-error "Not a function: ~s."
                 new-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Overrides of some inherited methods follow.
;;;


(defmethod combine-collections
  ((c1 explicit-finite-collection) (c2  explicit-finite-collection))
  
  (let 
    (new-contents new-collection sort-fun)
    
    (setf sort-fun (or (set-less-than-predicate-of c1)
                       (set-less-than-predicate-of c2)))
    
    (setf new-contents
          (let ((cc1 (copy-seq (contents-of c1)))
                (cc2 (copy-seq (contents-of c2))))
            (delete-duplicates
             (concatenate (type-of-seq cc1) cc1 cc2)      ; :after initialize-instance
             :test #'same-set-p)))                         ;  sorts if it's necessary
    
    (setf new-collection
          (make-instance 'explicit-finite-collection
                         :contains-set-p-function
                         #'(lambda (collection set)
                             (find set (contents-of collection)
                                   :test #'same-set-p))
                         :disjoint?
                         (if (and (not-disjoint-collection-p c1)
                                  (not-disjoint-collection-p c2))
                           NIL
                           :unknown)
                         :element-function
                         #'(lambda (collection index)
                             (elt (contents-of collection) index))
                         :set-less-than-predicate
                         sort-fun
                         :contents
                         new-contents))
    new-collection))

  

(defmethod index-of ((collection explicit-finite-collection) set)
  (position set (contents-of collection) :test #'same-set-p))
  
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some defgenerics for new operators on
;;; explicit-finite-collections
;;;

(defgeneric reset-end-index (explicit-finite-collection)
  (:documentation
   "Resets the end-index-of the collection.")
  )


(defgeneric sorted-collection-p (explicit-finite-collection)
  (:documentation
   "Are the contents of the collection sorted?")
  )

(defgeneric add-sets (explicit-finite-collection &rest sets)
  (:documentation
   "Adds the specified sets to the collection (if not already in the collection).")
  )

(defgeneric remove-sets (explicit-finite-collection &rest sets)
  (:documentation
   "Removes the specified sets from the collection (if in the collection).")
  )

(defgeneric remove-sets-if (explicit-finite-collection test)
  (:documentation
   "Removes those sets from the collection for which application ~%~
    of the test predicate returns T.")
  )

(defgeneric collection-sort
  (explicit-finite-collection &optional less-than-predicate)
  (:documentation
   "Destructively rearranges the order of the sets in the collection~%~
    according to the less-than-predicate if supplied. ~%~
    If predicate is not supplied, then ~%~
    (set-less-than-predicate-of collection) is used. ~%~")
  )

(defgeneric delete-sets (explicit-finite-collection &rest sets)
  (:documentation
   "Deletes the specified set from the collection (if in the collection).~%~
    A destructive version of remove-a-set.")
  )

(defgeneric delete-sets-if (explicit-finite-collection test)
  (:documentation
   "Deletes those sets from the collection for which application ~%~
    of the test predicate returns T.~%~
    A destructive version of remove-sets-if.")
  )

(defgeneric safe-collection-sort
  (explicit-finite-collection &optional less-than-predicate)
  (:documentation
   "Rearranges the order of the sets in the collection~%~
    according to the less-than-predicate if supplied. ~%~
    If predicate is not supplied, then ~%~
    (set-less-than-predicate-of collection) is used. ~%~
    (Non-destructive version of collection-sort.")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; And their methods
;;;

(defmethod reset-end-index ((collection explicit-finite-collection))
  (setf (end-index-of collection)
        (- (length (contents-of collection)) 1)))

(defmethod sorted-collection-p
  ((collection explicit-finite-collection))
  (if (set-less-than-predicate-of collection)
    T
    NIL))

(defmethod add-sets 
  ((collection explicit-finite-collection) &rest sets)
  
  (if sets
    (let ((base-seq (contents-of collection)))
      
      (setf (contents-of collection)
            
            (delete-duplicates
             (if (sorted-collection-p collection)
              (merge (type-of-seq base-seq)
                     base-seq
                     sets
                     (set-less-than-predicate-of collection))
              (concatenate (type-of-seq base-seq)
                     base-seq
                     sets))
             :test #'same-set-p))
      collection)))
  
(defmethod remove-sets
  ((collection explicit-finite-collection) &rest sets)

  (loop for set in sets
     with base-seq = (contents-of collection)
     do
     (setf base-seq
              (remove set base-seq))
     finally
     (setf (contents-of (copy-set collection)) base-seq)) collection)
        

(defmethod remove-sets-if
  ((collection explicit-finite-collection) test)

  (setf (contents-of collection)
        (remove-if test (contents-of (copy-set collection)))) collection)
  

(defmethod collection-sort
  ((collection explicit-finite-collection)
   &optional (less-than-predicate (set-less-than-predicate-of collection)))
  
  (setf (contents-of collection)
        (sort (contents-of collection) less-than-predicate))
  (setf (set-less-than-predicate-of collection)
        less-than-predicate)
  collection)

(defmethod delete-sets
  ((collection explicit-finite-collection) &rest sets)
  
  (loop for set in sets
     with base-seq = (contents-of collection)
     do
     (setf base-seq
              (delete set base-seq))
     finally
     (setf (contents-of collection) base-seq)) collection)


(defmethod delete-sets-if
  ((collection explicit-finite-collection) test)

  (setf (contents-of collection)
        (delete-if test (contents-of collection))) collection)

(defmethod safe-collection-sort
  ((collection explicit-finite-collection)
   &optional (less-than-predicate (set-less-than-predicate-of collection)))
  
  (let ((copy-of-contents
         (copy-seq (contents-of collection))))
    (setf (contents-of collection)
          (sort copy-of-contents less-than-predicate))
    (setf (set-less-than-predicate-of collection)
          less-than-predicate)
    collection))

