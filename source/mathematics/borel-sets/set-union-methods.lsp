;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               set-union-methods.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1989, 1990
;;;     N. Wiebe 1997, 1998
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                             SET UNION METHODS
;;;
;;;      Method                 |                Argument type                |
;;;                             |_____________________________________________|
;;;      Name                   |      Argument 1      |      Argument 2      |
;;;  ___________________________|______________________|______________________|
;;;                             |                      |                      |
;;;                             |                      |                      |
;;;  op-set-union               |      borel-set       |      borel-set       |
;;;                             |      borel-set       |      empty-set       |
;;;                             |      borel-set       | explicit-finite-set  |
;;;                             |      borel-set       | explicit-finite-union|
;;;                             |      borel-set       |   (extended-realp)   |
;;;                             |    complement-set    |    complement-set    |
;;;                             |    complement-set    |   (extended-realp)   |
;;;                             |    complement-set    |      interval        |
;;;                             |    complement-set    |    predicate-set     |
;;;                             |    countable-union   |      borel-set       |
;;;                             |    countable-union   |    countable-union   |
;;;                             |      empty-set       |      borel-set       |
;;;                             |      empty-set       | explicit-finite-union| why
;;;                             |      empty-set       |   (extended-realp)   |
;;;                             | explicit-finite-set  |      borel-set       |
;;;                             | explicit-finite-set  | explicit-finite-set  |
;;;                             | explicit-finite-set  |      interval        |
;;;                             | explicit-finite-set  |   (extended-realp)   |
;;;                             | explicit-finite-union|      borel-set       |
;;;                             | explicit-finite-union|      empty-set       |
;;;                             | explicit-finite-union| explicit-finite-union|
;;;                             | explicit-finite-union|   (extended-realp)   |
;;;                             |      interval        |    complement-set    |
;;;                             |      interval        | explicit-finite-set  |
;;;                             |      interval        |      interval        |
;;;                             |      interval        |    predicate-set     |
;;;                             |    predicate-set     |    complement-set    |
;;;                             |    predicate-set     |      interval        |
;;;                             |    predicate-set     |    predicate-set     |
;;;                             |  (extended-realp)    |      borel-set       |
;;;                             |  (extended-realp)    |    complement-set    |
;;;                             |  (extended-realp)    |      empty-set       |
;;;                             |  (extended-realp)    |explicit-finite-set   |
;;;                             |  (extended-realp)    |explicit-finite-union |
;;;                             |  (extended-realp)    |   (extended-realp)   |




(in-package :quail)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-set-union  - Set Union binary operator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod op-set-union (set1 set2)
  (if (and (extended-realp set1)
           (extended-realp set2))
    (if (= set1 set2)
      set1
      (make-instance 'explicit-finite-set
                     :contents (list set1 set2)))
    (missing-method 'op-set-union set1 set2)))

(defmethod op-set-union ((set1 borel-set) (set2 borel-set))
  (declare (special *default-set-less-than-predicate*))
  (make-instance 'explicit-finite-union
    :contents
    (list set1 set2)
    :set-less-than-predicate *default-set-less-than-predicate*))

(defmethod op-set-union (set1 (set2 borel-set))
  (op-set-union set2 set1))

(defmethod op-set-union ((set1 borel-set) set2)
  (declare (special *default-set-less-than-predicate*))
  (if (extended-realp set2)
    (if (op-memberp set2 set1)
      set1
      (make-instance 'explicit-finite-union
        :contents (list set1 set2)
        :set-less-than-predicate *default-set-less-than-predicate*
        :disjoint? T))
    (missing-method 'op-set-union set1 set2)))

(defmethod op-set-union ((set1 empty-set) set2)
  (if (extended-realp set2)
    set2
    (missing-method 'op-set-union set1 set2)))

(defmethod op-set-union (set1 (set2 empty-set))
  (op-set-union set2 set1))

(defmethod op-set-union ((set1 empty-set) (set2 borel-set))
  set2)
  
(defmethod op-set-union ((set1 borel-set) (set2 empty-set))
  set1)

(defmethod op-set-union (set1 (set2 complement-set))
  (if (extended-realp set1)
    (if (same-set-p set1 (set-of set2))
      (complement-wrt set2)
      (make-instance 'explicit-finite-union
        :contents (list set1 set2)))
    (missing-method 'op-set-union set1 set2)))
      
(defmethod op-set-union ((set1 complement-set) set2)
  (op-set-union set2 set1))
    
;;;;;;;;;;;;;;;;;;;
;;;
;;; union of intervals
;;;


(defmethod op-set-union ((set1 interval) (set2 interval))
  
  (let* (new-set
         (min1 (infimum set1))
         (min2 (infimum set2))
         (max1 (supremum set1))
         (max2 (supremum set2))
         (new-inf (min min1 min2))
         (new-sup (max max1 max2)))
    
    (if (separatep set1 set2)
      
      ;; then union is union of two disjoint intervals
      
      (setf
       new-set
       (make-instance 'explicit-finite-union
                      :contents (list set1 set2)
                      :infimum new-inf :supremum new-sup))
      
      ;; otherwise it's another interval and closure-property needs to be
      ;; figured out.
      
      (let (new-closure-property
            (left-closed? NIL)
            (right-closed? NIL)
            (left-set  (if (<= min1 min2) set1 set2))
            (right-set (if (>= max1 max2) set1 set2)))
        
        ;; 
        ;; First the left side:
        
        (if
          
          (or (left-closed-p left-set)
              (and (= min1 min2)
                   (or (left-closed-p set1)
                       (left-closed-p set2))))
          
          ;; then it's closed on the left
          
          (setf left-closed? T)
          
          ;; otherwise it's open on the left,
          ;; left-closed? remains nil
          
          )
        
        ;; Now the right side:
        
        (if
          
          (or (right-closed-p right-set)
              (and (= max1 max2)
                   (or (right-closed-p set1)
                       (right-closed-p set2))))
          
          ;; then it's closed on the right
          
          (setf right-closed? T)
          
          ;; otherwise it's open on the right,
          ;; right-closed? remains nil
          
          )
        
        
        ;; Which yields the closure-property:
        
        (setf new-closure-property
              (determine-interval-closure left-closed? right-closed?))
        
        
        
        ;; Finally make the appropriate interval
        
        (setf new-set
              (make-instance 'interval
               :closure-property new-closure-property
               :infimum new-inf
               :supremum new-sup))))

      (simplify-object new-set))
)

(defmethod op-set-union ((set1 explicit-finite-set) (set2 explicit-finite-set))
  (let ((new-contents
         (seq-union (contents-of set1) (contents-of set2)))
        (new-inf (min (infimum set1) (infimum set2)))
        (new-sup (max (supremum set1) (supremum set2))))
    
    (make-instance 'explicit-finite-set
                   :contents new-contents
                   :infimum new-inf
                   :supremum new-sup)))

(defmethod op-set-union ((set1 explicit-finite-set) set2)
  (if (extended-realp set2)
      (make-instance 'explicit-finite-set
        :contents (concatenate 'list (contents-of set1) (list set2)))
    (missing-method 'op-set-union set1 set2)))


(defmethod op-set-union (set1 (set2 explicit-finite-set))
  (op-set-union set2 set1))


(defmethod op-set-union ((set1 explicit-finite-union) (set2 borel-set))
  (if (member set2 (contents-of set1)  :test #'same-set-p) 
    set1
    (make-instance 'explicit-finite-union :contents 
                                    (concatenate 'list (contents-of set1) (list set2)))))

(defmethod op-set-union ((set1 explicit-finite-union) (set2 countable-union))
  (let ((new-collection (copy-set set2)))
    (loop for item in (contents-of set1) do
          (when (not (contains-set-p set2 item))
            (setf new-collection (combine-collections new-collection item)))) new-collection))

(defmethod op-set-union ((set1 explicit-finite-union) (set2 countable-set))
  (make-instance 'explicit-finite-union :contents 
                 (concatenate 'list (contents-of set1) (list set2))))

(defmethod op-set-union ((set1 borel-set) (set2 explicit-finite-union))
  (op-set-union set2 set1))

(defmethod op-set-union ((set1 borel-set) (set2 explicit-finite-set))
  (op-set-union set2 set1))

(defmethod op-set-union ((set1 explicit-finite-set) (set2 borel-set))
  (if (every #'(lambda(x) (memberp x set2)) (contents-of set1))
    set2
    (make-instance 'explicit-finite-union :contents 
                   (concatenate 'list (list set1 set2)))))

(defmethod op-set-union ((set1 explicit-finite-union) (set2 explicit-finite-union))
  (if (eq set1 set2)  ;;or op-same-set-p
    set1
    (simplify-object (make-instance 'explicit-finite-union :contents 
                                    (concatenate 'list (contents-of set1) (contents-of set2))))))


(defmethod op-set-union ((set1 explicit-finite-union) set2)
  (if (extended-realp set2)
    (if (memberp set2 set1)
      set1
      (make-instance 'explicit-finite-union 
        :contents (concatenate 'list (contents-of set1) (list set2))))
  (missing-method 'op-set-union set1 set2)))

(defmethod op-set-union (set1 (set2 explicit-finite-union))
  (op-set-union set2 set1))


;;; predicate-set, interval, and complement-sets

(defmethod op-set-union ((set1 complement-set) (set2 interval))
  (op-set-union (op-set-complement (set-of set1) 
                                   (op-set-union (complement-wrt set1) set2))
                (op-set-intersection (set-of set1) set2)))

(defmethod op-set-union ((set1 interval) (set2 complement-set))
  (op-set-union set2 set1))

(defmethod op-set-union ((set1 complement-set) (set2 complement-set))
  (if (not (op-empty-set-p (op-set-intersection (set-of set1) (complement-wrt set2))))
    (if (not (op-empty-set-p (op-set-intersection (set-of set2) (complement-wrt set1))))
      (op-set-union (complement-wrt set1) (complement-wrt set2))
      (op-set-complement (set-of set1) 
                         (op-set-union (complement-wrt set1) 
                                       (complement-wrt set2))))
    (op-set-complement (op-set-union (set-of set1) (set-of set2))
                       (op-set-union (complement-wrt set1) 
                                     (complement-wrt set2)))))


(defmethod op-set-union ((set1 predicate-set) (set2 predicate-set))
  (make-instance 'predicate-set :defining-predicate
                 #'(lambda (test-member)
                     (or (funcall (defining-predicate-of set1) test-member)
                         (funcall (defining-predicate-of set2) test-member)))
                 :infimum (min (infimum set1) (infimum set2))
                 :supremum (max (supremum set1) (supremum set2))))

(defmethod op-set-union ((set1 predicate-set) (set2 interval))
  (make-instance 'predicate-set :defining-predicate
                 #'(lambda (test-member)
                     (or (funcall (defining-predicate-of set1) test-member)
                         (memberp test-member set2)))))

(defmethod op-set-union ((set1 interval) (set2 predicate-set))
  (op-set-union set2 set1))

(defmethod op-set-union ((set1 predicate-set) (set2 complement-set))
  (make-instance 'predicate-set :defining-predicate
                 #'(lambda (test-member)
                     (or (funcall (defining-predicate-of set1) test-member)
                         (and (memberp  test-member (complement-wrt set2))
                              (not (memberp test-member (set-of set2))))))))

(defmethod op-set-union ((set1 complement-set) (set2 predicate-set))
  (op-set-union set2 set1))

;;; EFS

(defmethod op-set-union ((set1 explicit-finite-set) (set2 interval))
  (if (op-insidep set1 set2)
    set2
    (if (or (not (op-disjoint-sets-p set1 set2))
            (< (infimum set2) (supremum set1))
            (< (infimum set1) (supremum set2)))
      (let* ((contents (contents-of set1))
             (upper-set (remove-if #'(lambda (x) (if (<= x (supremum set2)) x))
                                   contents))
             (lower-set (remove-if #'(lambda (x) (if (>= x (infimum set2)) x))
                                   contents))
             (upper-set (if upper-set
                          (if (not (= (length upper-set) 1))
                            (make-instance 'explicit-finite-set :contents upper-set)
                            (elt upper-set 0))))
             (lower-set (if lower-set 
                          (if (not (= (length lower-set) 1))
                            (make-instance 'explicit-finite-set :contents lower-set)
                            (elt lower-set 0))))
             set2-copy)
        
        (labels ((check-boundaries (set1 set2)
                   (let (left-closed? right-closed? new-closure-property
                                      (set2-copy (copy-set set2)))
                     (if (and (left-open-p set2) (find (infimum set2) (contents-of set1)))
                       (setf left-closed? T)
                       (setf left-closed? (left-closed-p set2)))
                     (if (and (right-open-p set2) (find (supremum set2) (contents-of set1)))
                       (setf right-closed? T)
                       (setf right-closed? (right-closed-p set2)))        
                     (setf new-closure-property (determine-interval-closure left-closed? right-closed?))
                     
                     (setf (closure-property-of set2-copy) new-closure-property) set2-copy)))

          (setf set2-copy (check-boundaries set1 set2)))
          
          
          (cond ((and lower-set upper-set) 
                 (make-instance 'explicit-finite-union :contents (list lower-set set2-copy upper-set)))
                (lower-set
                 (make-instance 'explicit-finite-union :contents (list lower-set set2-copy)))
                (upper-set
                 (make-instance 'explicit-finite-union :contents (list set2-copy upper-set)))))
        
        (make-instance 'explicit-finite-union :contents (list set1 set2)))))
  

(defmethod op-set-union ((set1 interval) (set2 explicit-finite-set))
  (op-set-union set2 set1))

(defmethod op-set-union ((set1 countable-union) (set2 countable-union))
  (if (eq set1 set2)
    set1
    (combine-collections set1 set2)))

(defmethod op-set-union ((set1 countable-set) (set2 countable-set))
  (if (eq set1 set2)
    set1
    (combine-collections set1 set2)))  ;remove-duplicates?


(defmethod op-set-union ((set1 countable-union) (set2 borel-set))
  (if (contains-set-p set1 set2)
    set1              
    (combine-collections set1 set2)))

(defmethod op-set-union ((set1 borel-set) (set2 countable-union))
  (op-set-union set2 set1))

(defmethod op-set-union ((set1 countable-set) (set2 borel-set))
  (make-instance 'explicit-finite-union :contents (list set1 set2)))

(defmethod op-set-union ((set1 countable-set) (set2 empty-set))
  set1)

(defmethod op-set-union ((set1 borel-set) (set2 countable-set))
  (op-set-union set2 set1))

(defmethod op-set-union ((set1 countable-union) (set2 empty-set))
  set1)

(defmethod op-set-union ((set1 countable-intersection) (set2 borel-set))
  (if (eq set1 set2)
    set1
  (make-instance 'explicit-finite-union :contents (list set1 set2))))

(defmethod op-set-union ((set1 countable-intersection) (set2 empty-set))
  set1)

;;; not borel-sets ;;;
(defmethod op-set-union ((set1 set-collection) set2)
  (declare (special *default-set-less-than-predicate*))
  (if (extended-realp set2)
    (if (memberp set2 set1)
      set1
      (make-instance 'explicit-finite-union
        :contents (list set1 set2)
        :set-less-than-predicate *default-set-less-than-predicate*
        :disjoint? T))
    (missing-method 'op-set-union set1 set2)))

(defmethod op-set-union (set1 (set2 set-collection))
  (op-set-union set2 set1))

(defmethod op-set-union ((set1 set-collection) (set2 empty-set))
  set1)

(defmethod op-set-union ((set1 empty-set) (set2 set-collection))
  set2)

#| Finite-increasing-set as a class does not exist
(defmethod op-set-union ((set1 finite-increasing-set)
               (set2 finite-increasing-set))
  
  (let* ((new-contents (seq-union (contents-of set1)
                                  (contents-of set2)))
         (new-min (min (infimum set1) (infimum set2)))
         (new-max (max (supremum set1) (supremum set2))))
    
    (make-instance 'finite-increasing-set
                        :contents new-contents
                        :infimum new-min
                        :supremum new-max)))
|#


