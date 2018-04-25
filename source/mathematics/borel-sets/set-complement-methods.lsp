;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               set-complement-methods.lisp
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
;;;                             SET COMPLEMENT METHODS
;;;
;;;      Method                 |                Argument type                |
;;;                             |_____________________________________________|
;;;      Name                   |      Argument 1      |      Argument 2      |
;;;  ___________________________|______________________|______________________|
;;;                             |                      |                      |
;;;                             |                      |                      |
;;;  op-set-complement          |      borel-set       |      borel-set       |
;;;                             |      borel-set       |      empty-set       |
;;;                             |      borel-set       |  (extended-realp)    |
;;;                             |    complement-set    |      borel-set       |
;;;                             |    complement-set    |    complement-set    |
;;;                             |    complement-set    |      interval        |
;;;                             |    complement-set    |    predicate-set     |
;;;                             |      empty-set       |      borel-set       |
;;;                             | explicit-finite-set  |      borel-set       |
;;;                             | explicit-finite-set  | explicit-finite-set  |
;;;                             | explicit-finite-set  |      interval        |
;;;                             | explicit-finite-set  |  (extended-realp)    |
;;;                             | explicit-finite-union| explicit-finite-union|
;;;                             | explicit-finite-union|      interval        |
;;;                             |      interval        |    complement-set    |
;;;                             |      interval        | explicit-finite-union|
;;;                             |      interval        |      interval        |
;;;                             |      interval        |    predicate-set     |
;;;                             |    predicate-set     |      interval        |
;;;                             |    predicate-set     |    predicate-set     |
;;;                             |  (extended-realp)    |      borel-set       |
;;;                             |  (extended-realp)    |    complement-set    |
;;;                             |  (extended-realp)    | explicit-finite-set  |
;;;                             |  (extended-realp)    | explicit-finite-union|
;;;                             |  (extended-realp)    |  (extended-realp)    |
;;;  op-set-difference          |    same as above     |    same as above     |
;;;                             |                      |                      |


(in-package :quail)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-set-complement - Set complement as a binary operator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod op-set-complement (set1 set2)
(declare (special *the-empty-set*))
  (if (and (extended-realp set1) (extended-realp set2))
    (if (= set1 set2)
      *the-empty-set*
      set2)
    (missing-method 'op-set-complement set1 set2)))

(defmethod op-set-complement ((set1 borel-set) (set2 borel-set))
  (if (op-disjoint-sets-p set1 set2)
    set2
    (make-instance 'complement-set :set set1 :wrt set2)))
  

(defmethod op-set-complement ((set1 borel-set) set2)
(declare (special *the-empty-set*))
  (if (extended-realp set2)
    (if (memberp set2 set1)
      *the-empty-set*
      set2)
    (missing-method 'op-set-complement set1 set2)))

(defmethod op-set-complement (set1 (set2 borel-set))
  (if (extended-realp set1)
    (if (memberp set1 set2)
      (make-instance 'complement-set :set set1 :wrt set2)
      set2)
    (missing-method 'op-set-complement set1 set2)))

(defmethod op-set-complement (set1 (set2 complement-set))
  (if (extended-realp set1)
    (if (memberp set1 (set-of set2))
      set2
      (if (memberp set1 (complement-wrt set2))
        (if (extended-realp (set-of set2))
          (make-instance 'complement-set
            :set (make-instance 'explicit-finite-set :contents (list set1 (set-of set2)))
            :wrt (complement-wrt set2))
          (make-instance 'complement-set
            :set set1
            :wrt set2))))
    (missing-method 'op-set-complement set1 set2)))

;;; do I really want this?
(defmethod op-set-complement (set1 (set2 explicit-finite-union))
  (if (extended-realp set1)
    (if (memberp set1 set2)
      (if (member set1 (contents-of set2) :test #'same-set-p)
        (if (> (length (contents-of set2)) 2)
          (copy-set (remove-sets set2 set1))
          (elt (contents-of (remove-sets set2 set1)) 0))
        (make-instance 'complement-set :set set1 :wrt set2))
      set2)
    (missing-method 'op-set-complement set1 set2)))

(defmethod op-set-complement  ((set1 borel-set) (set2 empty-set))
  set2)

(defmethod op-set-complement  ((set1 empty-set) (set2 borel-set))
  set2)


(defmethod op-set-complement  ((set1 interval) (set2 interval))
  
  (declare (special *the-empty-set*))
  
  (let (new-set new-inf new-sup new-closure-property
                (left-closed? NIL)
                (right-closed? NIL)
                (min1 (infimum set1))
                (min2 (infimum set2))
                (max1 (supremum set1))
                (max2 (supremum set2)))
    (cond
     
     ((disjoint-sets-p set1 set2)               ; no intersection,
      (setf new-set set2))                ; op-set-complement  is all of set2
     
     ((op-insidep  set2 set1)
      (setf new-set *the-empty-set*))     ; op-set-complement  is empty
     
     ((and (<= min1 min2)             ; op-set-complement  is interval
           (not (and (left-closed-p set2)
                     (left-open-p set1)))
           (or (< max1 max2)             ; between max1 and max2
               (and (= max1 max2)
                    (right-open-p set1)
                    (right-closed-p set2))))
      
      (setf new-inf max1)
      (setf new-sup max2)
      (if (right-open-p set1) (setf left-closed? T))
      (if (right-closed-p set2) (setf right-closed? T))
      (setf new-closure-property
            (determine-interval-closure left-closed? right-closed?))
      (if (eq new-inf new-sup)
        (setf new-set new-inf)
        (setf new-set
              (make-instance
                'interval :infimum new-inf :supremum new-sup
                :closure-property new-closure-property))))
     
     ((and (or (> min1 min2)              ; op-set-complement  is interval
               (and (= min1 min2)
                    (left-open-p set1)
                    (left-closed-p set2)))
           (>= max1 max2)
           (not (and (right-closed-p set2)
                     (right-open-p set1))))            ; between min2 and min1
      
      (setf new-inf min2)
      (setf new-sup min1)
      (if (left-open-p set1) (setf right-closed? T))
      (if (left-closed-p set2) (setf left-closed? T))
      (setf new-closure-property
            (determine-interval-closure left-closed? right-closed?))
      (if (eq new-inf new-sup)
        (setf new-set new-inf)
        (setf new-set
              (make-instance
                'interval :infimum new-inf :supremum new-sup
                :closure-property new-closure-property))))
     
     (T                                   ; there are two intervals
      ; in the op-set-complement 
      ; the first from min2 to min1
      ; the second from max1 to max2
      
      (let (sub-int1 sub-int2)
        
        ;; work on first sub-interval
        
        (if (left-closed-p set2) (setf left-closed? T))
        (if (left-open-p set1) (setf right-closed? T))
        (setf new-closure-property
              (determine-interval-closure left-closed? right-closed?))
        (if (eq min1 min2)
          (setf sub-int1 min1)
          (setf sub-int1
                (make-instance 'interval
                  :infimum min2 :supremum min1
                  :closure-property new-closure-property)))
        
        ;; reinitialize closure-propertys
        
        (setf left-closed? NIL)
        (setf right-closed? NIL)
        
        ;; now work on second sub-interval
        
        (if (right-open-p set1) (setf left-closed? T))
        (if (right-closed-p set2) (setf right-closed? T))
        (setf new-closure-property
              (determine-interval-closure left-closed? right-closed?))
        (if (eq max1 max2)
          (setf sub-int2 max1)
          (setf sub-int2
                (make-instance 'interval
                  :infimum max1 :supremum max2
                  :closure-property new-closure-property)))
        
        ;; build the op-set-complement 
        (if (every #'extended-realp (list sub-int1 sub-int2))
          (setf new-set (make-instance 'explicit-finite-set
                          :contents (list sub-int1 sub-int2)))
          (setf new-set
                (make-instance 'explicit-finite-union
                  :infimum min2 :supremum max2
                  :contents (list sub-int1 sub-int2)
                  :disjoint? T
                  :closure-property
                  (if (and (closed-set-p sub-int1)
                           (closed-set-p sub-int2))
                    :closed
                    :neither)))) new-set))
     )
    new-set)
  )

(defmethod op-set-complement ((set1 complement-set)
                        (set2 borel-set))
  (op-set-union (op-set-intersection set2 (set-of set1))
             (op-set-complement (complement-wrt set1) set2)))

(defmethod op-set-complement
           ((set1 explicit-finite-set) (set2 explicit-finite-set))
  (declare (special *the-empty-set*))

    (let (new-set)
      
      (if (or (> (infimum set1) (supremum set2))
              (> (infimum set2) (supremum set1)))
        
        ;; then return set2
        
        (setf new-set set2)
        
        ;; else do more work
        
        (let ((contents
               (seq-complement (contents-of set1)
                               (contents-of set2))))
          (cond ((zerop (length contents))
                 (setf new-set *the-empty-set*))
                ((eq (length contents) 1)
                 (setf new-set (elt contents 0)))
                (T
                 (let ((new-min (elt contents 0))
                       (new-max (elt contents (- (length contents) 1))))
                   
                   (setf new-set
                         (make-instance
                           'explicit-finite-set
                           :contents contents
                           :infimum new-min
                           :supremum new-max)))))))
      
      new-set))

;;; should this be switched to borel-set??
(defmethod op-set-complement ((set1 explicit-finite-set) (set2 interval))
  (if (op-disjoint-sets-p set1 set2)
    set2
    (let ((new-contents (mapcan #'(lambda (x) (and (memberp x set2) (list x)))
                                (contents-of set1))))
    (cond 
     ((null new-contents) 
      set2)
     ((= (length new-contents) 1) 
      (progn (setf new-contents (elt new-contents 0))
             (make-instance 'complement-set :set new-contents :wrt set2)))
     (T
      (progn (setf new-contents (make-instance 'explicit-finite-set :contents new-contents))
             (make-instance 'complement-set :set new-contents :wrt set2)))))))

(defmethod op-set-complement ((set1 explicit-finite-set) (set2 borel-set))
  (declare (special *the-empty-set*))
  (if (empty-set-p set2)
    *the-empty-set*
    (make-instance 'complement-set :set set1 :wrt set2)))

(defmethod op-set-complement (set1 (set2 explicit-finite-set))
  (if (extended-realp set1)
    (if (member set1 (contents-of set2))
        (if (= (length (contents-of set2)) 2)
          (elt (contents-of (remove-sets set2 set1)) 0)
          (copy-set (remove-sets set2 set1)))
      set2)
    (missing-method 'op-set-complement set1 set2)))


(defmethod op-set-complement ((set1 explicit-finite-set) set2)
  (declare (special *the-empty-set*))
  (if (extended-realp set2)
    (if (member set2 (contents-of set1))
      *the-empty-set*
      set2)
    (missing-method 'op-set-complement set1 set2)))


;;; EFUs
(defmethod op-set-complement ((set1 explicit-finite-union) (set2 explicit-finite-union))
  (if (op-disjoint-sets-p set1 set2)
    set2
    (let (new-set)
      (loop for item in (contents-of set1) do
            (if new-set
              (setf new-set (op-set-intersection new-set
                                                 (op-set-complement item set2)))
              (setf new-set (op-set-complement item set2))))
    (if (null new-set)
      (setf new-set *the-empty-set*)) new-set)))
      
(defmethod op-set-complement ((set1 explicit-finite-union) (set2 interval))
  (let ((new-sets NIL))
    (loop for item in (contents-of set1) do
          (if new-sets
            (setf new-sets (set-intersection new-sets
                                             (set-complement item set2)))        
            (setf new-sets (set-complement item set2))))    
    new-sets))

(defmethod op-set-complement ((set1 predicate-set) (set2 explicit-finite-union))
  (if (member set1 (contents-of set2))
    (copy-set (remove-sets set2 set1))
    (make-instance 'complement-set :set set1 :wrt set2)))

(defmethod op-set-complement ((set1 explicit-finite-union) (set2 borel-set))
  (let ((new-sets NIL))
    (loop for item in (contents-of set1) do
          (if new-sets
            (setf new-sets (set-intersection new-sets
                                             (set-complement item set2)))        
            (setf new-sets (set-complement item set2))))    
    new-sets))

(defmethod op-set-complement ((set1 interval) (set2 explicit-finite-union))
  (let ((new-sets NIL))
    
    (loop for item in (contents-of set2) do
          (setf new-sets (append new-sets (list (op-set-complement set1 item)))))
    
    (setf new-sets (delete-if #'empty-set-p new-sets))
    
    (cond 
     ((null new-sets)
      (setf new-sets *the-empty-set*))
     ((= (length new-sets) 1)
      (setf new-sets (elt new-sets 0)))
     (T
      (if (every #'extended-realp new-sets)
        (setf new-sets (make-instance 'explicit-finite-set :contents new-sets))
      (setf new-sets (make-instance 'explicit-finite-union
                       :contents new-sets)))))
    new-sets))

;;; predicate-sets, intervals, complement-sets

(defmethod op-set-complement ((set1 complement-set) (set2 complement-set))
  (if (op-disjoint-sets-p set1 set2)       ;; does this make it faster or not really?
    set2
    (op-set-complement (set-of set2)
                       (op-set-union (op-set-complement (complement-wrt set1) (complement-wrt set2))
                                     (op-set-intersection (set-of set1) (complement-wrt set2))))))

(defmethod op-set-complement ((set1 interval) (set2 complement-set))
  (op-set-complement (op-set-union set1 (set-of set2))
                     (complement-wrt set2)))

(defmethod op-set-complement  ((set1 complement-set) (set2 interval))
  (op-set-union (op-set-complement (complement-wrt set1) set2)
                (set-intersection (set-of set1) set2)))


(defmethod op-set-complement ((set1 predicate-set) (set2 predicate-set))
  (declare (special *the-empty-set*))
  (if (eq set1 set2)
    *the-empty-set*
    (make-instance 'predicate-set :defining-predicate 
                   #'(lambda (test-member) 
                       (and (not (funcall (defining-predicate-of set1) test-member))
                            (funcall (defining-predicate-of set2) test-member))))))
                                                        
(defmethod op-set-complement ((set1 predicate-set) (set2 interval))
  (make-instance 'predicate-set :defining-predicate
                 #'(lambda (test-member)
                     (and (not (funcall (defining-predicate-of set1) test-member))
                          (op-memberp test-member set2)))))

(defmethod op-set-complement ((set1 interval) (set2 predicate-set))
  (make-instance 'predicate-set :defining-predicate
                 #'(lambda (test-member)
                     (and (not (op-memberp test-member set1))
                          (funcall (defining-predicate-of set2) test-member)))))

(defmethod op-set-complement ((set1 predicate-set) (set2 complement-set))
  (make-instance 'predicate-set :defining-predicate
                 #'(lambda (test-member)
                     (and (not (funcall (defining-predicate-of set1) test-member))
                          (op-memberp test-member set2)))))

(defmethod op-set-complement ((set1 complement-set) (set2 predicate-set))
  (make-instance 'predicate-set :defining-predicate
                 #'(lambda (test-member)
                     (and (not (op-memberp test-member set1))
                          (funcall (defining-predicate-of set2) test-member)))))


(defmethod op-set-complement ((set1 borel-set) (set2 countable-union))
  (make-instance 'complement-set :set set1 :wrt set2))

(defmethod op-set-complement ((set1 interval) (set2 countable-union))
  (if (containsp set2 set1)
    (make-instance 'complement-set :set set1 :wrt set2)
    set2))

(defmethod op-set-complement ((set1 countable-union) (set2 borel-set))
  (declare (special *the-empty-set*))
  (if (eq set1 set2)
    *the-empty-set*
    (if (contains-set-p set1 set2)   ;;third alt, don't know, gives error
      *the-empty-set*
      set2)))

(defmethod op-set-complement ((set1 interval) (set2 countable-set))
  (declare  (special *the-empty-set*))
  (if (eq set1 set2)
    *the-empty-set*
    (if (op-disjoint-sets-p set1 set2)
      set2
      (make-instance 'complement-set :set set1 :wrt set2))))

(defmethod op-set-complement ((set1 countable-set) (set2 borel-set))
  (declare (special *the-empty-set*))
  (if (eq set1 set2)
    *the-empty-set*
  (if (op-disjoint-sets-p set1 set2)
    set2
    (make-instance 'complement-set :set set1 :wrt set2))))

(defmethod op-set-complement ((set1 countable-union) (set2 empty-set))
  set2)

(defmethod op-set-complement ((set1 borel-set) (set2 countable-intersection))
  (declare (special *the-empty-set*))
  (if (eq set1 set2)
    *the-empty-set*
  (make-instance 'complement-set :set set1 :wrt set2)))

(defmethod op-set-complement ((set1 countable-intersection) (set2 borel-set))
  (declare (special *the-empty-set*))
  (if (eq set1 set2)
    *the-empty-set*
  (make-instance 'complement-set :set set1 :wrt set2)))
    
(defmethod op-set-complement ((set1 countable-intersection) (set2 empty-set))
  set2)
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-set-difference  - Set difference binary operator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod op-set-difference  (set1 set2)
  (op-set-complement set2 set1))

