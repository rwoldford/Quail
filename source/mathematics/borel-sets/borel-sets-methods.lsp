;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               Borel-Sets-methods.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1989, 1990
;;;     N. Wiebe 1997, 1998
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                             BOREL SETS METHODS
;;;
;;;  In this file the methods which implement operators on borel-sets
;;;  are defined.  Methods which implement the operators on argument combinations
;;;  of different classes are defined elsewhere.
;;;
;;;
;;;  Methods implemented in this file are as indicated in the
;;;  following table:
;;;
;;;      Method                 
;;;                             
;;;      Name                   |       Argument       |
;;;  ___________________________|______________________|
;;;                             |                      |                      
;;;                             |                      |                      
;;;  op-borel-set-p             |      borel-set       |                      
;;;                             |  (extended-realp)    |                      
;;;                             |                      |                            
;;;  op-empty-set-p             |      borel-set       |                      
;;;                             |    complement-set    |                      
;;;                             |    countable-set     |                      
;;;                             |      empty-set       |                      
;;;                             |  (extended-realp)    |                      
;;;                             |                      |                      
;;;  op-closed-set-p            |      borel-set       |                      
;;;                             |  (extended-realp)    |                      
;;;                             |                      |                      
;;;  op-open-set-p              |      borel-set       |                      
;;;                             |  (extended-realp)    |                      
;;;                             |                      |                      
;;;  op-neither-closure-p       |      borel-set       |                      
;;;                             |      interval        |                      
;;;                             |  (extended-realp)    |                      
;;;                             |                      |                           
;;;  cardinality                |      borel-set       |                      
;;;                             |    countable-set     |                      
;;;                             |      empty-set       |                      
;;;                             |  (extended-realp)    |                      
;;;                             |                      |                      
;;;  ___________________________|______________________|
;;;
;;;
;;;  Global constants:
;;;
;;;         *max-reasonable-number-of-iterations*
;;;         *big-set-count*           ... Large positive integer representing
;;;                                       the number of elements in a countable
;;;                                       set that we are willing to check before
;;;                                       asking the user whether we should continue.
;;;
;;;-------------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Making each extended real appear as if it were a singleton set.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod infimum ((thing t))
  (if (extended-realp thing)
    thing
    (missing-method 'infimum thing)))

(defmethod supremum ((thing t))
  (if (extended-realp thing)
    thing
    (missing-method 'supremum thing)))

(defmethod closure-property-of ((thing t))
  (if (extended-realp thing)
    :closed
    (missing-method 'infimum thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-borel-set-p  - predicate test whether we have a borel-set or not.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod op-borel-set-p ((thing borel-set))
  (declare (ignore thing))
  T)

(defmethod op-borel-set-p ((thing t))               ; include extended-real nos.
  (extended-realp thing))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-empty-set-p  - Tests whether the set is empty.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod op-empty-set-p (set)
  (if (extended-realp set)
    NIL
    (missing-method 'op-empty-set-p set)))

(defmethod op-empty-set-p ((set borel-set))
  (quail-error
   "Not enough information to determine whether the given set: ~%~
    ~s ~%~
    is empty or not." set))

(defmethod op-empty-set-p ((set set-collection))
  (quail-error
   "Not enough information to determine whether the given set: ~%~
    ~s ~%~
    is empty or not." set))
                                            
(defmethod op-empty-set-p ((set empty-set))
  T)

(defmethod op-empty-set-p ((set interval))
  NIL)

(defmethod op-empty-set-p ((set complement-set))
  (op-insidep (complement-wrt set) (set-of set)))

(defmethod op-empty-set-p ((set predicate-set))
    (if (open-set-p set)
      (= (infimum set) (supremum set))     
      
      (if (closed-set-p set)
                                                          ; Is this mathematically
        NIL                                               ; correct to always return
                                                          ; NIL in this case?
        (quail-error
         "Not enough information to determine whether the given set: ~%~
          ~s ~%~
          is empty or not." set))))

(defmethod op-empty-set-p ((set explicit-finite-collection))  ;; or just efu
  (let ((result T))
    (loop for item in (contents-of set) do
          (when (not (op-empty-set-p item))
            (return) (setf result NIL))) result))

(defmethod op-empty-set-p ((set countable-collection))  ;; or just countable-union
  (declare (special *max-reasonable-number-of-iterations*))
  (let ((result T))
    (do
      ((index (count-start-of set)
              (next-index set index))
       (counter 1 (+ counter 1))
       (big-count *max-reasonable-number-of-iterations*))
      ;; end-test
      ((count-endedp set index) result)
      
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
             "Assume set ~s is empty?" set)
            (return T))
           ((quail-yes-or-no-p
             "Assume set ~s ~%~
              is *NOT* empty?" set)
            (progn (setf result NIL) (return)))
           (T (quail-error "Can't tell whether set ~s ~%~
                            empty or not!" set)))))
       ((not (op-empty-set-p (element set index))) (progn (setf result NIL)
                                                          (return))))) result))


(defmethod op-empty-set-p ((set countable-intersection))
  (disjoint-collection-p set))

(defmethod op-empty-set-p ((set countable-set))
  (let ((num-elts (cardinality set)))
    (if (numberp num-elts)
      (= num-elts 0)
      NIL)))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-closed-set-p  - Test whether the set is KNOWN to be closed.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod op-closed-set-p (set)
  (if (extended-realp set)
    T
    (missing-method 'op-closed-set-p set)))

(defmethod op-closed-set-p ((set borel-set))
  (or (eq (closure-property-of set) :closed)
      (eq (closure-property-of set) :both)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-open-set-p  - Test whether the set is KNOWN to be open.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod op-open-set-p (set)
  (if (extended-realp set)
    NIL
    (missing-method 'op-open-set-p set)))

(defmethod op-open-set-p ((set borel-set))
  (or (eq (closure-property-of set) :open)
      (eq (closure-property-of set) :both)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-neither-closure-p  - Test whether the set is KNOWN to be both
;;;                          not closed and not open.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod op-neither-closure-p (set)
  (if (extended-realp set)
    NIL
    (missing-method 'op-neither-closure-p set)))

(defmethod op-neither-closure-p ((set borel-set))
  (or (eq (closure-property-of set) :neither)
      (eq (closure-property-of set) :unknown)))

(defmethod op-neither-closure-p ((set interval))
  (case (closure-property-of set)
    (:left-closed T)
    (:right-closed T)
    (:left-open T)
    (:right-open T)
    (:closed-left T)
    (:closed-right T)
    (:open-left T)
    (:open-right T)
    (:neither T)
    (:left T)
    (:right T)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; determine-closure    - Try to determine the closure property of a set 
;;;                        and then set it accordingly (in most cases).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod determine-closure ((set explicit-finite-intersection))
  (let ((new-closure (closure-property-of (first (contents-of set)))))
    (loop for item in (rest (contents-of set)) do
          (when (not (eq new-closure (closure-property-of item)))
            (setf new-closure :unknown)
            (return)))
    (setf (closure-property-of set) 
          (if (eq new-closure (or :closed :open))
            new-closure :unknown))))


(defmethod determine-closure ((set explicit-finite-union))
  (let ((new-closure (closure-property-of (first (contents-of set)))))
    (loop for item in (rest (contents-of set)) do
          (when (not (eq new-closure (closure-property-of item)))
            (setf new-closure :unknown)
            (return)))
    (setf (closure-property-of set) new-closure)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Related closure methods that are specific to intervals
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod left-open-p (set)
  (if (extended-realp set)
    NIL
    (missing-method 'left-open-p set)))

(defmethod right-open-p (set)
  (if (extended-realp set)
    NIL
    (missing-method 'right-open-p set)))

(defmethod left-closed-p (set)
  (if (extended-realp set)
    T
    (missing-method 'left-closed-p set)))

(defmethod right-closed-p (set)
  (if (extended-realp set)
    T
    (missing-method 'right-closed-p set)))

(defmethod left-open-p ((set interval))
  (case (closure-property-of set)
    (:open  T)
    (:right-closed T)
    (:left-open T)
    (:closed-right T)
    (:open-left T)
    (:right T)))

(defmethod right-open-p ((set interval))
  (case (closure-property-of set)
    (:open  T)
    (:left-closed T)
    (:right-open T)
    (:closed-left T)
    (:open-right T)
    (:left T)))

(defmethod left-closed-p ((set interval))
  (case (closure-property-of set)
    (:closed  T)
    (:left-closed T)
    (:right-open T)
    (:closed-left T)
    (:open-right T)
    (:left T)))


(defmethod right-closed-p ((set interval))
  (case (closure-property-of set)
    (:closed  T)
    (:right-closed T)
    (:left-open T)
    (:closed-right T)
    (:open-left T)
    (:right T)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Recalculate bounds (and set in most cases).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod recalculate-bounds (thing)
  (unless
    (extended-realp thing)
    (missing-method 'recalculate-bounds thing)))

(defmethod recalculate-bounds ((set borel-set))
  (when
    (quail-yes-or-no-p
     "Sorry, don't even know how to try recalculating ~%~
      the bounds of~% ~s. ~%Set to :unknown ?")
    (setf (infimum set) :unknown)
    (setf (supremum set) :unknown)))

(defmethod recalculate-bounds ((self countable-union))
  (declare (special *max-reasonable-number-of-iterations*))
  (let ((new-inf +infinity)
        (new-sup -infinity))
    (do
      ((index (count-start-of self)
              (next-index self index))
       (counter 1 (+ counter 1))
       (big-count *max-reasonable-number-of-iterations*)
       (current-set NIL))
      ;; end-test
      ((or (count-endedp self index)
           (and (eq new-inf NaN)
                (eq new-sup NaN))))
      
      ;; Now the action
      (setf current-set (element self index))
      (if (> counter big-count)
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
                           (quail-query "How many more sets should be tested? "))
                     finally (return answer))))
          (progn
            (if 
              (not (quail-yes-or-no-p "Assume value of infimum is ~s ? " new-inf))
              (setf new-inf NaN))
            (if 
              (not (quail-yes-or-no-p "Assume value of supremum is ~s ? " new-sup))
              (setf new-sup NaN)))))
        (unless (eq new-inf NaN)
          (setf new-inf (min new-inf (infimum current-set))))
        (unless (eq new-sup NaN)
          (setf new-sup (max new-sup (supremum current-set)))))

    (setf (infimum self) new-inf)
    (setf (supremum self) new-sup) (list new-inf new-sup)))

(defmethod recalculate-bounds ((self explicit-finite-union))
  (setf (infimum self) (apply #'min (mapcar #'infimum (contents-of self))))
  (setf (supremum self) (apply #'max (mapcar #'supremum (contents-of self)))))

(defmethod recalculate-bounds ((self explicit-finite-set))
  (setf (infimum self) (apply #'min (contents-of self)))
  (setf (supremum self) (apply #'max (contents-of self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  cardinality  - Number of elements in a countable set.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cardinality (set)
  (if (extended-realp set)
    1
    (missing-method 'cardinality set)))

(defmethod cardinality ((set borel-set))
  (if (empty-set-p set)                                    ; cardinality is not
    0                                                 ; sensible in general
    NaN))                                            ; therefore given here
                                                      ; as NaN, i.e. unknown.

(defmethod cardinality ((set empty-set))
  0)


(defmethod cardinality ((self countable-set))
  (number-of-sets-in self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Functions peculiar to intervals
;;;  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod separatep ((set1 interval) (set2 interval))
;; separate and not touching
  (let ((min1 (infimum set1))
        (max1 (supremum set1))
        (min2 (infimum set2))
        (max2 (supremum set2)))
    
    (or (> min1 max2)                 ; clear separation
        (> min2 max1)
        (and (= min1 max2)            ; share end-points
             (and (left-open-p set1)      ; as long as both
                  (right-open-p set2)))   ; are open
        (and (= max1 min2)             ; i.e., they are not touching
             (and (right-open-p set1)
                  (left-open-p set2))))))


(defun determine-interval-closure (left-closed? right-closed?)
  "Determines appropriate closure property for an interval having ~%~
   given values of left-closed? (T or NIL) and right-closed? (T or NIL)."
  (cond ((and left-closed? right-closed?) :closed)
        (left-closed? :left)
        (right-closed? :right)
        (T :open)))


