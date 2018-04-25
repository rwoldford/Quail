;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               insidep-methods.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1989, 1990
;;;     N. Wiebe 1998
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                             INSIDE PREDICATE METHODS
;;;
;;;      Method                 |                Argument type                |
;;;                             |_____________________________________________|
;;;      Name                   |      Argument 1      |      Argument 2      |
;;;  ___________________________|______________________|______________________|
;;;                             |                      |                      |
;;;                             |                      |                      |
;;;  op-insidep                 |      borel-set       |      borel-set       |
;;;                             |      borel-set       |    complement-set    |
;;;                             |      borel-set       |      empty-set       |
;;;                             |      borel-set       | explicit-finite-inter|
;;;                             |      borel-set       | explicit-finite-union|
;;;                             |      borel-set       |   set-collection     |
;;;                             |      borel-set       |  (extended-realp)    |
;;;                             |   complement-set     |      borel-set       |
;;;                             |    countable-set     |    countable-set     |
;;;                             |      empty-set       |      borel-set       |
;;;                             |      empty-set       |      empty-set       |
;;;                             |explicit-finite-inter |      borel-set       |
;;;                             |explicit-finite-inter |explicit-finite-inter |
;;;                             | explicit-finite-set  |      borel-set       |
;;;                             | explicit-finite-set  | explicit-finite-set  |
;;;                             | explicit-finite-set  |      interval        |
;;;                             | explicit-finite-union|      borel-set       |
;;;                             | explicit-finite-union| explicit-finite-union|
;;;                             |      interval        |      interval        |
;;;                             |      interval        |  (extended-realp)    |
;;;                             |  (extended-realp)    |      borel-set       |
;;;                             |  (extended-realp)    |  (extended-realp)    |
;;;                             |                      |                      |
;;;  op-containsp               |  same as above       |  same as above       |
;;;                             |                      |                      |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-insidep  - Subset binary operator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod op-insidep (set1 set2)
  (if (and (extended-realp set1)
           (extended-realp set2))
    (= set1 set2)
    (missing-method 'op-insidep  set1 set2)))

(defmethod op-insidep (set1 (set2 borel-set))
  (if (extended-realp set1)
    (memberp set1 set2)
    (missing-method 'op-insidep  set1 set2)))

(defmethod op-insidep ((set1 borel-set) set2)
  (if (extended-realp set2)
    NIL
    (missing-method 'op-insidep  set1 set2)))

(defmethod op-insidep :around ((set1 interval) set2)
  (if (= (infimum set1) (supremum set1))  ;singleton, would have to be closed
    (op-insidep (infimum set1) set2)
    (call-next-method set1 set2)))

(defmethod op-insidep ((set1 borel-set) (set2 borel-set))
  (unless (eq set1 set2)
    (quail-error
     "Not enough information to determine whether ~s ~%~
      is inside ~s." set1 set2)))

(defmethod op-insidep  ((set1 empty-set) (set2 borel-set))
  set2)

(defmethod op-insidep  ((set1 borel-set) (set2 empty-set))
  NIL)

(defmethod op-insidep  ((set1 empty-set) (set2 empty-set))
  T)

(defmethod op-insidep :around ((set1 complement-set) (set2 borel-set))
  (cond ((empty-set-p (set-of set1))
         (op-insidep (complement-wrt set1) set2))
        ((empty-set-p (complement-wrt set1))
         T)
        (T
         (call-next-method set1 set2))))

(defmethod op-insidep ((set1 complement-set) (set2 borel-set))
  (or (op-insidep (complement-wrt set1) set2)
      (let ((set (op-set-complement (op-set-intersection (complement-wrt set1) set2) 
                                    (complement-wrt set1))))
        (op-insidep set (set-of set1)))))

(defmethod op-insidep :around ((set1 borel-set) (set2 complement-set))
  (cond ((empty-set-p (set-of set2))
         (op-insidep set1 (complement-wrt set2)))
        ((empty-set-p (complement-wrt set2))
         NIL)
        (T
         (call-next-method set1 set2))))

(defmethod op-insidep ((set1 borel-set) (set2 complement-set))
  (and (op-insidep set1 (complement-wrt set2))
       (op-disjoint-sets-p set1 (set-of set2))))


(defmethod op-insidep  ((set1 interval) (set2 interval))
  
   (let* ((min1 (infimum set1))
          (max1 (supremum set1))
          (min-in? (memberp min1 set2))
          (max-in? (memberp max1 set2)))
     
     (or
      
      (and min-in? max-in?)              ; both end-points are in
      
      (and min-in?                       ; left-side is in,
           (= max1 (supremum set2))  ; equal & open right-sides
           (right-open-p set1)
           (right-open-p set2))
      
      (and max-in?                       ; right-side is in,
           (= min1 (infimum set2))   ; equal & open left-sides
           (left-open-p set1)
           (left-open-p set2))

      (and (= min1 (infimum set2))    ;equal bounds and set1 open
           (= max1 (supremum set2))
           (open-set-p set1)))))

;;; b/k EFS are always ordered
(defmethod op-insidep ((set1 explicit-finite-set) (set2 interval))
  (and (< (infimum set2) (infimum set1))
       (> (supremum set2) (supremum set1))))


(defmethod op-insidep  ((set1 explicit-finite-set) (set2 explicit-finite-set))

  (if (or (> (infimum set2) (infimum set1))
          (> (supremum set1) (supremum set2)))
    
    ;; then answer is false.
    
    NIL
    
    ;; else do more work.
    
    (if (and (listp (contents-of set1)) (listp (contents-of set2)))
      
      ;; if both are lists then we can use subsetp

      (subsetp (contents-of set1) (contents-of set2))
      
      ;; else check out the cardinality of the intersection.
      
      (= (cardinality (op-set-intersection set1 set2)) (cardinality set1))))
  )


(defmethod op-insidep ((set1 explicit-finite-set) (set2 borel-set))
  (let ((result T))
    (loop for item in (contents-of set1) do
          (when (not (memberp item set2))
            (progn (setf result NIL)
                   (return)))) result))

(defmethod op-insidep  ((set1 countable-set) (set2 countable-set))
  
  (declare (special *big-set-count*))

  (if (or (> (infimum set2) (infimum set1))
          (> (supremum set1) (supremum set2)))
    
    ;; then answer is false.
    
    NIL
    
    ;; else check out the cardinality of the intersection.
      
      (= (cardinality (op-set-intersection set1 set2)) (cardinality set1))))

(defmethod op-insidep ((set1 explicit-finite-union) (set2 explicit-finite-union))
  (let ((result T))
    (loop for item in (contents-of set1) do
          (unless (op-insidep item set2)
            (setf result NIL))) result))

(defmethod op-insidep ((set1 borel-set) (set2 explicit-finite-union))
  (loop for item in (contents-of set2) do
        (when (op-insidep set1 item)
          (return T))))

(defmethod op-insidep ((set1 explicit-finite-union) (set2 borel-set))
  (let ((result T))
    (loop for item in (contents-of set1) do
          (unless (op-insidep item set2)
            (setf result NIL))) result))

(defmethod op-insidep ((set1 borel-set) (set2 explicit-finite-intersection))
  (let ((result T))
    (loop for item in (contents-of set2) do
          (unless (op-insidep set1 item)
            (quail-error
             "Not enough information to determine whether ~s ~%~
              is inside ~s." set1 set2))) result))

(defmethod op-insidep ((set1 explicit-finite-intersection) (set2 explicit-finite-intersection))
  (let ((result T))
    (loop for item in (contents-of set2) do
          (unless (op-insidep set1 item)
            (quail-error
             "Not enough information to determine whether ~s ~%~
              is inside ~s." set1 set2))) result))

(defmethod op-insidep ((set1 explicit-finite-intersection) (set2 borel-set))
  (let ((result T))
    (loop for item in (contents-of set1) do
          (unless (op-insidep item set2)
            (quail-error
             "Not enough information to determine whether ~s ~%~
              is inside ~s." set1 set2))) result))

(defmethod op-insidep ((set1 borel-set) (set2 countable-collection))
  (contains-set-p set2 set1))

(defmethod op-insidep ((set1 countable-collection) (set2 borel-set))
  (declare (special *reasonable-number-of-iterations*))
  (do
    ((index (count-start-of set1)
            (next-index set1 index))
     (counter 1 (+ counter 1))
     (big-count *reasonable-number-of-iterations*)
     (result T))
    ;; end-test
    ((count-endedp set1 index) result)
    
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
           "Assume collection ~%~
            ~s ~%~
            is in the set ~%~
            ~s ? " set1 set2)                    (return T))
         ((quail-yes-or-no-p
           "Assume collection ~%~
            ~s ~%~
            is *NOT* in the set ~%~
            ~s ? " set1 set2)                    (progn (setf result NIL)
                                                        (return NIL)))
         (T (quail-error "Can't tell whether the collection ~%~
                          ~s ~%~
                          is in the set ~%~
                          ~s ~%~
                          or not!" set1 set2)))))
     ((not (op-insidep (element set1 index) set2)) (progn (setf result NIL)
                                                          (return NIL))))))

(defmethod op-insidep (set1 (set2 countable-set))
  (if (extended-realp set1)
    (contains-set-p set2 set1)
    NIL))

(defmethod op-insidep ((set1 countable-set) set2)
  (if (extended-realp set2)
    (and (= (number-of-sets-in set1) 1)
         (= set2 (element set1 (count-start-of set1))))
    NIL))

(defmethod op-insidep ((set1 borel-set) (set2 countable-set))
  (if (extended-realp set1)
    (contains-set-p set2 set1)
    NIL))

(defmethod op-inside :around ((set1 countable-intersection) (set2 borel-set))
  (if (call-next-method set1 set2)
    set2
    (quail-error "Can't tell whether the collection ~%~
                  ~s is in the set ~s ~%~
                  or not!" set1 set2)))
  
(defmethod op-insidep ((set1 borel-set) (set2 countable-intersection))
  (declare (special *reasonable-number-of-iterations*))
  (do
    ((index (count-start-of set2)
            (next-index set2 index))
     (counter 1 (+ counter 1))
     (big-count *reasonable-number-of-iterations*)
     (result T))
    ;; end-test
    ((count-endedp set2 index) result)
    
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
            ~s ? " set1 set2)                    (return T))
         ((quail-yes-or-no-p
           "Assume set ~%~
            ~s ~%~
            is *NOT* in the collection ~%~
            ~s ? " set1 set2)                    (progn (setf result NIL)
                                                        (return NIL)))
         (T (quail-error "Can't tell whether the set ~%~
                          ~s ~%~
                          is in the collection ~%~
                          ~s ~%~
                          or not!" set1 set2)))))
     ((not (op-insidep set1 (element set2 index))) (progn (setf result NIL)
                                                          (return NIL))))))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-containsp  - Set containment (reverse of subset binary operator)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod op-containsp (set1 set2)
  (op-insidep set2 set1))


#| This needs to be fixed.

(defmethod op-insidep  ((set1 countably-infinite-increasing-set)
                      (set2 countably-infinite-increasing-set))
  
  (declare (special *big-set-count*))
  
  (let ((inf1 (infimum set1))
        (inf2 (infimum set2))
        (sup1 (supremum set1))
        (sup2 (supremum set2)))
    
    (if (or (> sup1 sup2)
            (< inf1 inf2))
      
      ;; then answer is false.
    
      NIL
      
      ;; else more work needs to be done.
      
      (loop
        with x-i = (element set1 0)
        and big-count = *big-set-count*
        
        for i from 0 by 1
         as j from (index-of set2 x-i) by 1
           do
            (cond
             ((not j) (return NIL))
             ((or (>= i big-count) (>= j big-count))
                 (if
                   (quail-y-or-n-p
                     "Have found that ~A elements of the first set (~S)~%~
                      are contained in the second set (~S).~%~
                      And have checked ~A elements of the second set.~%~
                      Assume first set is inside the second?  Otherwise I'll keep checking."
                     i set1 set2 j)
                   (return T)
                   (setf big-count (+ big-count (/ *big-set-count* 2)))))
             
            (if (>
            (setf answer (and answer
                              (memberp (element set1 i) set2)))
            (when (null answer) (return)))
            
|#

