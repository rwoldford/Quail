;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               set-intersection-methods.lisp
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
;;;                             SET INTERSECTION METHODS
;;;
;;;      Method                 |                Argument type                |
;;;                             |_____________________________________________|
;;;      Name                   |      Argument 1      |      Argument 2      |
;;;  ___________________________|______________________|______________________|
;;;                             |                      |                      |
;;;                             |                      |                      |
;;;  op-set-intersection        |      borel-set       |      borel-set       |
;;;                             |      borel-set       |    complement-set    |
;;;                             |      borel-set       |    countable-union   |
;;;                             |      borel-set       |      empty-set       |
;;;                             |      borel-set       |explicit-finite-inter |
;;;                             |      borel-set       | explicit-finite-set  |
;;;                             |      borel-set       | explicit-finite-union|
;;;                             |      borel-set       |     predicate-set    |
;;;                             |      borel-set       |   (extended-realp)   |
;;;                             |    complement-set    |      borel-set       |
;;;                             |    complement-set    |    complement-set    |
;;;                             |countable-intersection|countable-intersection|
;;;                             |    countable-union   |      borel-set       |
;;;                             |      empty-set       |      borel-set       |
;;;                             | explicit-finite-inter|      borel-set       |
;;;                             | explicit-finite-inter|explicit-finite-inter |
;;;                             | explicit-finite-set  |      borel-set       |
;;;                             | explicit-finite-set  | explicit-finite-set  |
;;;                             | explicit-finite-union|      borel-set       |
;;;                             | explicit-finite-union| explicit-finite-union|
;;;                             |      interval        |      interval        |
;;;                             |    predicate-set     |      borel-set       |
;;;                             |    predicate-set     |    predicate-set     |
;;;                             |  (extended-realp)    |      borel-set       |
;;;                             |  (extended-realp)    |  (extended-realp)    |


(in-package :quail)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-set-intersection  - Set Intersection binary operator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod op-set-intersection ((set1 borel-set) (set2 borel-set))
  (make-instance 'explicit-finite-intersection
                 :contents (list set1 set2)))

(defmethod op-set-intersection (set1 set2)
  (declare (special *the-empty-set*))
  (if (and (extended-realp set1)
           (extended-realp set2))
    (if (= set1 set2)
      set1
      *the-empty-set*)
    (missing-method 'op-set-intersection set1 set2)))

(defmethod op-set-intersection ((set1 borel-set) set2)
  (declare (special *the-empty-set*))
  (if (extended-realp set2)
    (if (memberp set2 set1)
      set2
      *the-empty-set*)
    (missing-method 'op-set-intersection set1 set2)))

(defmethod op-set-intersection (set1 (set2 borel-set))
  (op-set-intersection set2 set1))

(defmethod op-set-intersection ((set1 empty-set) (set2 borel-set))
  set1)
 
(defmethod op-set-intersection ((set1 borel-set) (set2 empty-set))
  set2)

;;; --------------------------
;;;
;;; Intersection of intervals
;;;
  
(defmethod op-set-intersection ((set1 interval) (set2 interval))
 
  (declare (special *the-empty-set*))
  
  (let (new-set)
    
    (if (separatep set1 set2)
      
      ;; then no intersection
      
      (setf new-set *the-empty-set*)
      
      ;; else sets overlap. More to sort out.
      
      (let* ((min1 (infimum set1))
             (max1 (supremum set1))
             (min2 (infimum set2))
             (max2 (supremum set2))
             (new-inf (max min1 min2))
             (new-sup (min max1 max2))
             (left-set  (if (<= min1 min2) set1 set2))
             (right-set (if (>= max1 max2) set1 set2))
             (left-closed? NIL)
             (right-closed? NIL)
             new-closure-property)
        
        ;; Only closure-property remains to be figured out.
        ;; 
        ;; First the left side:
        
        (if
          
          (or (and (> new-inf (infimum left-set))
                   (left-closed-p right-set))
              (and (= min1 min2)
                   (left-closed-p set1)
                   (left-closed-p set2)))
          
          ;; then it's closed on the left
          
          (setf left-closed? T)
          
          ;; otherwise it's open on the left,
          ;; left-closed? remains nil
          
          )
        
        ;; Now the right side:
        
        (if
          
          (or (and (< new-sup (supremum right-set))
                   (right-closed-p left-set))
              (and (= max1 max2)
                   (right-closed-p set1)
                   (right-closed-p set2)))
          
          ;; then it's closed on the right
          
          (setf right-closed? T)
          
          ;; otherwise it's open on the right,
          ;; right-closed? remains nil
          
          )
        
        ;; Which yields the closure-property:
        
        (setf new-closure-property
              (determine-interval-closure left-closed? right-closed?))
        
        
        ;; Finally make the appropriate set
        
        (if (not (= new-inf new-sup))
          (setf new-set
                (make-instance 'interval
                               :closure-property new-closure-property
                               :infimum new-inf
                               :supremum new-sup))
          (setf new-set
                new-inf))))

      new-set)
)


(defmethod op-set-intersection ((set1 explicit-finite-set) (set2 explicit-finite-set))
  (declare (special *the-empty-set*))
  (if (or (> (infimum set1) (supremum set2))
          (> (infimum set2) (supremum set1)))
    
    ;; then there is no intersection
    
    *the-empty-set*
    
    ;; else do more work
    
    (let* ((new-contents (seq-intersection (contents-of set1)
                                           (contents-of set2)))
           (new-len (length new-contents)))
      
      (cond ((zerop new-len) *the-empty-set*)
            ((= new-len 1) (elt new-contents 0))
            (T (make-instance 'explicit-finite-set
                 :contents new-contents
                 :infimum (seq-min new-contents)
                 :supremum (seq-max new-contents))))
      )))

(defmethod op-set-intersection ((set1 explicit-finite-set) (set2 borel-set))
  (op-set-intersection set2 set1))

(defmethod op-set-intersection ((set1 borel-set) (set2 explicit-finite-set))
  (declare (special *the-empty-set*))
  (let ((intersecting-set (mapcan #'(lambda (x) (and (memberp x set1) (list x)))
                                  (contents-of set2))))
    (cond ((null intersecting-set)
           *the-empty-set*)
          ((= (length intersecting-set) 1)
           (elt intersecting-set 0))
          (T
           (make-instance 'explicit-finite-set :contents intersecting-set)))))

(defmethod op-set-intersection ((set1 explicit-finite-set) (set2 countable-union))
  (op-set-intersection set2 set1))

(defmethod op-set-intersection ((set1 countable-union) (set2 explicit-finite-set))
  (declare (special *the-empty-set*))
  (let ((intersecting-set (mapcan #'(lambda (x) (and (memberp x set1) (list x)))
                                  (contents-of set2))))
    (cond ((null intersecting-set)
           *the-empty-set*)
          ((= (length intersecting-set) 1)
           (elt intersecting-set 0))
          (T
           (make-instance 'explicit-finite-set :contents intersecting-set)))))


#|        
(defmethod op-set-intersection ((set1 finite-increasing-set)
              (set2 finite-increasing-set))
  
  (declare (special *the-empty-set*))
  
  (let (new-set)
    
    (if (or (> (infimum set1) (supremum set2))
            (> (infimum set2) (supremum set1)))
      ;; then
      (setf new-set *the-empty-set*)
      ;; else
      (let ((contents (seq-intersection (contents-of set1)
                                        (contents-of set2))))
        (if (zerop (length contents))
          
          (setf new-set *the-empty-set*)
          
          (let ((new-min (elt contents 0))
                (new-max (elt contents (- (length contents) 1))))
            
            (setf new-set
              (make-instance
               'finite-increasing-set
               :contents contents
               :infimum new-min
               :supremum new-max))))))

      new-set))
|#


(defmethod op-set-intersection ((set1 explicit-finite-union) (set2 explicit-finite-union))
  (declare (special *the-empty-set*))
  (if (eq set1 set2)  ;;or op-same-setp
    set1      
    (if (op-disjoint-sets-p set1 set2)
      *the-empty-set*
      (let ((new-sets NIL) len)
         (loop for item1 in (contents-of set1) do
               (loop for item2 in (contents-of set2) do
                     (setf new-sets (append new-sets (list (op-set-intersection item1 item2))))))
         
         (setf new-sets (delete-if #'empty-set-p new-sets))
         (setf len (length new-sets))
         
         (if  (= len 1)
           (setf new-sets (elt new-sets 0))
           (setf new-sets (make-instance 'explicit-finite-union
                            :contents new-sets)))))))     

(defmethod op-set-intersection ((set1 borel-set) (set2 explicit-finite-union))
  (op-set-intersection set2 set1))

(defmethod op-set-intersection ((set1 explicit-finite-union) (set2 borel-set))
  (declare (special *the-empty-set*))
  (if (member set2 (contents-of set1) :test #'same-set-p)
    set2
    (if (op-disjoint-sets-p set1 set2)
      *the-empty-set*
      (let ((new-sets NIL) len)
         (loop for item in (contents-of set1) do
               (setf new-sets (append new-sets (list (op-set-intersection item set2)))))
         
         (setf new-sets (delete-if #'empty-set-p new-sets))
         (setf len (length new-sets))
         
         (if (= len 1)
           (setf new-sets (elt new-sets 0))
           (setf new-sets (make-instance 'explicit-finite-union
                            :contents new-sets)))))))

(defmethod op-set-intersection ((set1 explicit-finite-intersection) (set2 explicit-finite-intersection))
  (if (eq set1 set2)
    set1
    (make-instance 'explicit-finite-intersection :contents 
                   (concatenate 'list (contents-of set1) (contents-of set2)))))

(defmethod op-set-intersection ((set1 borel-set) (set2 explicit-finite-intersection))
  (op-set-intersection set2 set1))

(defmethod op-set-intersection ((set1 explicit-finite-intersection) (set2 borel-set))
  (if (member set2 (contents-of set1) :test #'same-set-p)
    set1
    (make-instance 'explicit-finite-intersection :contents
                   (concatenate 'list (contents-of set1) (list set2)))))

(defmethod op-set-intersection ((set1 explicit-finite-intersection) (set2 empty-set))
  set2)

(defmethod op-set-intersection ((set1 complement-set) (set2 borel-set))
  (op-set-complement (set-of set1) (op-set-intersection (complement-wrt set1) set2)))


(defmethod op-set-intersection ((set1 borel-set) (set2 complement-set))
  (op-set-intersection set2 set1))

(defmethod op-set-intersection ((set1 complement-set) (set2 complement-set))
  (if (same-set-p set1 set2)
    set1
  (op-set-complement (op-set-union (set-of set1) (set-of set2))
                     (op-set-intersection (complement-wrt set1) (complement-wrt set2)))))

(defmethod op-set-intersection ((set1 predicate-set) (set2 predicate-set))
  (if (eq set1 set2)
    set1
    (make-instance 'predicate-set :defining-predicate
                   #'(lambda (test-member)
                       (and (funcall (defining-predicate-of set1) test-member)
                            (funcall (defining-predicate-of set2) test-member))))))


(defmethod op-set-intersection ((set1 predicate-set) (set2 borel-set))
  (make-instance 'predicate-set :defining-predicate
                 #'(lambda (test-member)
                     (and (funcall (defining-predicate-of set1) test-member)
                          (memberp test-member set2)))))

(defmethod op-set-intersection ((set1 borel-set) (set2 predicate-set))
  (op-set-intersection set2 set1))


;; all these are done already using the defining-predicate slot
;; however the following one should be easily coded

(defmethod op-set-intersection ((set1 countable-intersection) (set2 countable-intersection))
  (combine-collections set1 set2))

;; is the following reasonable?
(defmethod op-set-intersection ((set1 countable-union) (set2 borel-set))
  (declare (special *the-empty-set*))
(print "big dummy")
  (if (contains-set-p set1 set2)
    set2
    *the-empty-set*))

(defmethod op-set-intersection ((set1 countable-set) (set2 borel-set))
  (op-set-intersection set2 set1))

;;I CAN WRITE A BETTER METHOD FOR THIS
(defmethod op-set-intersection ((set1 borel-set) (set2 countable-set))
  (declare (special *the-empty-set*))
  
  (if (op-disjoint-sets-p set1 set2)
    *the-empty-set*
    (make-instance 'explicit-finite-intersection :contents (list set1 set2))))
  

(defmethod op-set-intersection ((set1 borel-set) (set2 countable-union))
  (op-set-intersection set2 set1))

(defmethod op-set-intersection ((set1 countable-intersection) (set2 borel-set))
  (combine-collections set1 set2))

(defmethod op-set-intersection ((set1 countable-collection) (set2 empty-set))
  set1)

(defmethod op-set-intersection ((set1 countable-union) (set2 empty-set))
  set2)

(defmethod op-set-intersection ((set1 countable-intersection) (set2 empty-set))
  set2)

(defmethod op-set-intersection ((set1 countable-union) (set2 countable-union))
  (declare (special *the-empty-set*))

  (let ((new-sets NIL) len)
    (cond ((eq set1 set2) 
           (setf new-sets set1))
          ((finite-collection-p set1)
           (loop for item in (contents-of set1) do
                 (setf new-sets (append new-sets (list (op-set-intersection item set2)))))
           (setf new-sets (delete-if #'empty-set-p new-sets))
           (setf len (length new-sets))         
           (cond ((zerop len)
                  (setf new-sets *the-empty-set*))
                 ((= len 1)
                  (setf new-sets (elt new-sets 0)))
                 (T
                  (setf new-sets (make-instance 'explicit-finite-union
                                   :contents new-sets)))))
          ((finite-collection-p set2)
           (loop for item in (contents-of set2) do
                 (setf new-sets (append new-sets (list (op-set-intersection item set1)))))
           (setf new-sets (delete-if #'empty-set-p new-sets))
           (setf len (length new-sets))         
           (cond ((zerop len)
                  (setf new-sets *the-empty-set*))
                 ((= len 1)
                  (setf new-sets (elt new-sets 0)))
                 (T
                  (setf new-sets (make-instance 'explicit-finite-union
                                   :contents new-sets)))))
          (T (setf new-sets (make-instance 'explicit-finite-intersection
               :contents (list set1 set2))))) new-sets))
         
(defmethod op-set-intersection ((set1 countable-set) (set2 countable-set))
  (declare (special *the-empty-set*))
  (let ((new-sets NIL) len)
    (cond ((eq set1 set2) 
           (setf new-sets set1))
          ((finite-collection-p set1)
           (loop for item in (contents-of set1) do
                 (setf new-sets (append new-sets (list (op-set-intersection item set2)))))
           (setf new-sets (delete-if #'empty-set-p new-sets))
           (setf len (length new-sets))         
           (cond ((zerop len)
                  (setf new-sets *the-empty-set*))
                 ((= len 1)
                  (setf new-sets (elt new-sets 0)))
                 (T
                  (setf new-sets (make-instance 'explicit-finite-union
                                   :contents new-sets)))))
          ((finite-collection-p set2)
           (loop for item in (contents-of set2) do
                 (setf new-sets (append new-sets (list (op-set-intersection item set1)))))
           (setf new-sets (delete-if #'empty-set-p new-sets))
           (setf len (length new-sets))         
           (cond ((zerop len)
                  (setf new-sets *the-empty-set*))
                 ((= len 1)
                  (setf new-sets (elt new-sets 0)))
                 (T
                  (setf new-sets (make-instance 'explicit-finite-union
                                   :contents new-sets)))))
          (T (setf new-sets (make-instance 'explicit-finite-intersection
               :contents (list set1 set2))))) new-sets))
 