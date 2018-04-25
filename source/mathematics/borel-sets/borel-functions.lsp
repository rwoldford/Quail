;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               borel-functions.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1989, 1990, 1992
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                             BOREL FUNCTIONS
;;;
;;;
;;;  Basic operations:
;;;
;;;         borel-set-p           ... test whether the argument is KNOWN to be a borel-set.
;;;         set-complement        ... set complement.
;;;         set-union             ... union.
;;;         set-intersection      ... intersection.
;;;         set-difference-p      ... difference in sets (same as binary complement).
;;;         sym-difference        ... symmetric difference of sets.
;;;         memberp               ... membership testing.
;;;         insidep               ... subset test.
;;;         containsp             ... subset test in reverse order.
;;;         same-set-p            ... test whether sets have the same contents.
;;;         disjoint-sets-p       ... test whether sets are disjoint.
;;;         empty-set-p           ... test whether sets are empty.
;;;         closed-set-p               ... test whether set is KNOWN to be closed.
;;;         open-set-p                 ... test whether set is KNOWN to be open.
;;;         neither-closure-p     ... test whether it is KNOWN that the set
;;;                                   is neither open nor closed.
;;;         closure-property-of   ... get what is known about the closure-property
;;;                                   of the set.
;;;                                   Returns an informative keyword (e.g. :open,
;;;                                   :closed, :neither, :both, :unknown, :left-closed,
;;;                                   :right-closed, and possibly others).
;;;         determine-closure     ... tries to determine closure *and will set* the
;;;                                   closure-property accordingly.
;;;         recalculate-bounds    ... attempts to determine, *and will set* the
;;;                                   infimum and supremum slots of the set.
;;;         cardinality           ... number of elements in a countable set
;;;                                   i.e. one of 0,1,2,... +infinity or NaN if
;;;                                   unknown.
;;;
;;;
;;;  Global constants:
;;;
;;;         *the-empty-set*           ... Set of NO ELEMENTS.
;;;         *the-extended-real-line*  ... [ -infinity, +infinity ].
;;;
;;;
;;;----------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  A predicate for testing whether things are borel-sets
;;;

(defun borel-set-p (thing &rest other-things)

  "Tests whether all arguments are borel-sets."

  (if other-things
    (and (op-borel-set-p thing)
         (apply #'borel-set-p other-things))
    (op-borel-set-p thing)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  closure-property testing:
;;;  
;;;  open-set-p                  - returns non-NIL if the set is *KNOWN*
;;;                           to be open otherwise NIL.
;;;  closed-set-p                - returns non-NIL if the set is *KNOWN*
;;;                           to be closed otherwise NIL.
;;;
;;; IMPLICATION: (not (open-set-p foo)) *DOES NOT IMPLY* (closed-set-p foo)
;;;              and vice versa.
;;;
;;;  neither-closure-p   - returns non-NIL if the set is *KNOWN*
;;;                           to be neither open nor closed.
;;;


(defun open-set-p (set1 &rest other-sets)

  "Test whether all the given sets are KNOWN to be open.~%~
   N.B. Returning NIL does *NOT* imply that some set is closed!"

  (if other-sets
    (and (op-open-set-p set1)
         (apply #'open-set-p other-sets))
    (op-open-set-p set1)))

(defun closed-set-p (set1 &rest other-sets)

  "Test whether all the given sets are KNOWN to be closed. ~%~
   N.B. Returning NIL does *NOT* imply that some set is open!"

  (if other-sets
    (and (op-closed-set-p set1)
         (apply #'closed-set-p other-sets))
    (op-closed-set-p set1)))
       
(defun neither-closure-p (set1 &rest other-sets)

  "Test whether all the given sets are KNOWN to be neither ~%~
   open nor closed.~%~
   N.B. Returning NIL does *NOT* imply that some set is either~%~
   open or closed!"

  (if other-sets
    (and (op-neither-closure-p set1)
         (apply #'neither-closure-p other-sets))
    (op-neither-closure-p set1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Testing for the empty set.
;;;

(defun empty-set-p (set1 &rest other-sets)
  (if other-sets
    (and (op-empty-set-p set1)
         (apply #'empty-set-p other-sets))
    (op-empty-set-p set1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The membership operator:  memberp
;;;
(defun memberp (value set)
  (if (extended-realp value)
    (op-memberp value set)
    (quail-error
     "~s cannot be a member of any Borel set! ~%~
      It must be an extended-real."
     value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The set-union operator: set-union
;;;
;;;  N.B.  If given a single argument, set-union 
;;;        returns *the-extended-real-line* as if union was taken
;;;        with respect to *the-extended-real-line*.
;;;

(defun set-union (set1 &rest other-sets)
  (declare (special *the-extended-real-line*))

  "Returns the union of all the given sets. ~
   If given a single argument, set-union returns *the-extended-real-line*~
   as if the union was taken with respect to *the-extended-real-line*.  ~
   (:see-also op-set-union)"

  (if other-sets
    (let ((rest-of-them (rest other-sets)))
      (if rest-of-them
        (apply #'set-union
           (op-set-union set1 (first other-sets))
           rest-of-them)
        (op-set-union set1 (first other-sets))))
    *the-extended-real-line*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The set-intersection operator
;;;
;;;  N.B.  If given a single argument, set-intersection 
;;;        returns its argument as if the 
;;;        intersection was taken
;;;        with respect to *the-extended-real-line*.
;;;

(defun set-intersection (set1 &rest other-sets)
  
  "Returns the intersection of all the given sets. ~%~
   If given a single argument, set-intersection returns its argument as if ~%~
   the intersection was taken with respect to *the-extended-real-line*.  ~
   (:see-also op-set-intersection)"
  (if other-sets
    (apply #'set-intersection
           (op-set-intersection set1 (first other-sets))
           (rest other-sets))
    set1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The subset predicate: insidep
;;;
;;;  N.B.
;;;     1. If given a single argument, insidep 
;;;        returns T as if the 
;;;        subset predicate was testing whether the
;;;        argument was a subset of *the-extended-real-line*.
;;;     2. As an n-ary predicate it tests for
;;;        monotone subsetting of its arguments.
;;;        (monotone non-decreasing sets)
;;;

(defun insidep (set1 &rest other-sets)
  (declare (special *the-extended-real-line*))

  "1. Tests for monotone subsetting of its arguments.~%~
      ~3,0T(I.e. monotone non-decreasing sets.) ~%~
   2. If given a single argument, insidep returns T~%~
      ~3,0Tas if the subset predicate was testing whether the argument~%~
      ~3,0Twas a subset of *the-extended-real-line*."

  (if other-sets
    
    (let ((next-set (first other-sets))
          (rest-of-them (rest other-sets)))
      (if (or (eq set1 next-set) (op-insidep set1 next-set))
        (if rest-of-them
          (apply #'insidep next-set rest-of-them)
          next-set)))
    T))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The converse, the contains predicate: containsp
;;;
;;;  N.B.
;;;     1. If given a single argument, containsp tests
;;;        whether its argument contains (i.e. IS)
;;;        *the-extended-real-line*.
;;;     2. As an n-ary predicate it tests for
;;;        monotone subsetting of its arguments.
;;;        (monotone non-decreasing sets)
;;;     3. If T it returns the largest set (i.e.
;;;        right-most argument.
;;;

(defun containsp (set1 &rest other-sets)
  (declare (special *the-extended-real-line*))

  "1. Tests for monotone containment of its arguments.~%~
      ~3,0T(I.e. monotone non-increasing sets.) ~%~
   2. If given a single argument, containsp tests whether its argument~%~
      ~3,0Tcontains *the-extended-real-line*."

  (if other-sets
    
    (let ((next-set (first other-sets))
          (rest-of-them (rest other-sets)))
      (if (op-containsp set1 next-set)
        (if rest-of-them
          (apply #'containsp next-set rest-of-them)
          T)))
    (if (op-containsp set1 *the-extended-real-line*)
      T
      NIL)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  same-set-p
;;;
;;;  ... Testing whether the contents of many sets are the same.
;;;


(defun same-set-p (set1 &rest other-sets)
  (declare (special *the-extended-real-line*))
  "Tests whether the given sets all have the same contents. ~%~
   If a single argument is given, testing is done against ~%~
   *the-extended-real-line*."
  
  (if other-sets
    
    (let ((set2 (first other-sets))
          (rest-of-them (rest other-sets)))
      (if (or (eq set1 set2)
              (and (op-insidep set1 set2)
                   (op-insidep set2 set1)))
        (if (null rest-of-them)
          T
          (apply #'same-set-p set2 rest-of-them)))) 
    
    (and (op-insidep set1 *the-extended-real-line*)
         (op-insidep *the-extended-real-line* set1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  disjoint-sets-p
;;;
;;;  ... Testing whether the sets have null intersection.
;;;

(defun disjoint-sets-p (set1 &rest other-sets)
  "Tests whether the given sets all null intersection. ~%~
   If a single argument is given, NIL is returned as if ~%~
   testing were done against *the-extended-real-line*."
  (if other-sets
    (let ((set2 (first other-sets))
          (rest-of-them (rest other-sets)))
      (if (op-disjoint-sets-p set1 set2)
        (if (null rest-of-them)
          T
          (apply #'disjoint-sets-p set2 rest-of-them))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The set-complement operator
;;;
;;;  N.B.
;;;     1. If given a single argument, set-complement assumes 
;;;        you mean complement with respect to the base set
;;;        (i.e. wrt *the-extended-real-line*).
;;;     2. As a binary operator it returns the complement of
;;;        its first argument in its second.
;;;     3. As an n-ary operator, it accumulates complements
;;;        left to right.  That is
;;;        (set-complement S1 S2 S3 S4)
;;;        behaves the same as
;;;        (set-complement
;;;            (set-complement
;;;                (set-complement S1 S2)
;;;                S3)
;;;            S4)
;;;

(defun set-complement (set1 &rest other-sets)
  (declare (special *the-extended-real-line*))

  "1. If given a single argument, set-complement assumes~%~
      ~3,0Tyou mean complement with respect to the base set ~%~
      ~3,0T(i.e. wrt *the-extended-real-line*). ~%~
   2. As a binary operator it returns the complement of~%~
      ~3,0Tits first argument in its second.~%~
   3. As an n-ary operator, it accumulates complements left to right.~%~
      ~3,0TThat is~~%~~
      ~3,0T(set-complement S1 S2 S3 S4)~~%~~
      ~3,0Tbehaves the same as~~%~~
      ~3,0T(set-complement~%~
      ~3,0T    (set-complement~%~
      ~3,0T        (set-complement S1 S2)~%~
      ~3,0T        S3)~%~
      ~3,0T    S4)."


  (if other-sets
    
    (let ((next-set (first other-sets))
          (rest-of-them (rest other-sets)))
      
      (if rest-of-them
        
          (apply #'set-complement 
                 (op-set-complement set1 next-set)
                 rest-of-them)
          
          (op-set-complement set1 next-set)))
    
    (op-set-complement set1 *the-extended-real-line*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  Symmetric difference as an n-ary operator
;;;
;;;  As with other operators if only a single argument is given,
;;;  it is assumed that the second argument is *the-extended-real-line*.
;;;

(defun sym-difference  (set1 &rest other-sets)
  (declare (special *the-extended-real-line*))

  "Returns the symmetric difference of all the given sets.~%~
   If a single argument is given, the sym-difference  is applied to the~%~
   given set and *the-extended-real-line*.  ~
   (:see-also set-difference-p)"


  (if other-sets
    (op-set-complement
     (apply #'set-intersection set1 other-sets)
     (apply #'set-union set1 other-sets))
    (op-set-complement set1 *the-extended-real-line*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Set difference as an n-ary operator.
;;;  If only one argument is given, *the-empty-set* is returned
;;;  as the result.  That is it is assumed that one is trying
;;;  to remove *the-extended-real-line* from the first set.
;;;
;;;


(defun set-difference-p (set1 &rest other-sets)
  (declare (special *the-empty-set*))

  "Returns the set difference of its arguments S1, S2, ..., Sn as~%~
   S1 - S2 - ... - Sn.~%~
   If only one argument is given, *the-empty-set* is returned.~%~
   That is, it is assumed that one is trying to remove~%~
   *the-extended-real-line* from the first set."


  (if other-sets
    (let ((next-set (first other-sets))
          (rest-of-them (rest other-sets)))
      (if rest-of-them
        (apply #'set-difference-p
           (op-set-difference set1 next-set)
           rest-of-them)
        (op-set-difference set1 next-set)))
    *the-empty-set*))

