;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               disjoint-sets-p-methods.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;;;
;;;  Author:
;;;     N. Wiebe 1998
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                            DISJOINT SETS PREDICATE METHODS
;;;
;;;      Method                 |                Argument type                |
;;;                             |_____________________________________________|
;;;      Name                   |      Argument 1      |      Argument 2      |
;;;  ___________________________|______________________|______________________|
;;;                             |                      |                      |
;;;                             |                      |                      |
;;;  op-disjoint-sets-p         |      borel-set       |      borel-set       |
;;;                             |      borel-set       |    complement-set    |
;;;                             |      borel-set       | countable-collection |
;;;                             |      borel-set       |countable-intersection|
;;;                             |      borel-set       |   countable-set      |
;;;                             |      borel-set       |      empty-set       |
;;;                             |      borel-set       | explicit-finite-union|
;;;                             |      borel-set       |   (extended-realp)   |
;;;                             |    complement-set    |      borel-set       |
;;;                             | countable-collection |      borel-set       |
;;;                             |countable-intersection|      borel-set       |
;;;                             |   countable-set      |      borel-set       |
;;;                             |      empty-set       |      borel-set       |
;;;                             | explicit-finite-set  | explicit-finite-set  |
;;;                             | explicit-finite-set  |      interval        |
;;;                             | explicit-finite-union|      borel-set       |
;;;                             | explicit-finite-union| explicit-finite-union|
;;;                             |      interval        | explicit-finite-set  |
;;;                             |      interval        |      interval        |
;;;                             |  (extended-realp)    |      borel-set       |
;;;                             |  (extended-realp)    |  (extended-realp)    |


(in-package :quail)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-disjoint-sets-p  - disjoint sets predicate binary operator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          

(defmethod op-disjoint-sets-p ((set1 borel-set) (set2 borel-set))
    (quail-error "Not enough information to determine whether ~s ~%~
                  is disjoint from ~s." set1 set2))

(defmethod op-disjoint-sets-p (set1 set2)
  (if (and (extended-realp set1)
           (extended-realp set2))
    (if (= set1 set2)
      NIL T)
    (missing-method 'op-disjoint-sets-p set1 set2)))

(defmethod op-disjoint-sets-p ((set1 borel-set) set2)
  (if (extended-realp set2)
    (if (memberp set2 set1)
      NIL T)
    (missing-method 'op-disjoint-sets-p set1 set2)))

(defmethod op-disjoint-sets-p (set1 (set2 borel-set))
  (op-disjoint-sets-p set2 set1))

(defmethod op-disjoint-sets-p ((set1 empty-set) (set2 borel-set))
  T)
 
(defmethod op-disjoint-sets-p ((set1 borel-set) (set2 empty-set))
  T)
  
(defmethod op-disjoint-sets-p ((set1 interval) (set2 interval))
  (separatep set1 set2))
     
(defmethod op-disjoint-sets-p ((set1 explicit-finite-set) (set2 explicit-finite-set))
  (let* ((temp-set (concatenate 'list (contents-of set1) (contents-of set2)))
         (len (length temp-set))
         (temp-set (remove-duplicates temp-set))
         (len2 (length temp-set)))
    (= len len2)))

(defmethod op-disjoint-sets-p ((set1 explicit-finite-union) (set2 borel-set))
  (if (member set2 (contents-of set1))
    NIL
    (let ((info (mapcar #'(lambda (x) (op-disjoint-sets-p x set2)) (contents-of set1)))
          (result T))
        (loop for item in info do
              (when (not item) 
                (progn (setf result NIL)
                       (return)))) result)))

(defmethod op-disjoint-sets-p ((set1 borel-set) (set2 explicit-finite-union))
  (op-disjoint-sets-p set2 set1))

;;this isn't really necessary
(defmethod op-disjoint-sets-p ((set1 explicit-finite-union) (set2 explicit-finite-union))
  (if (eq set1 set2)  ;; or op-same-set-p
    NIL
    (let ((info (mapcar #'(lambda (x) (op-disjoint-sets-p x set2)) (contents-of set1)))
          (result T))
        (loop for item in info do
              (when (not item) 
                (progn (setf result NIL)
                       (return)))) result)))

(defmethod op-disjoint-sets-p ((set1 complement-set) (set2 borel-set))
  (if (op-disjoint-sets-p set2 (complement-wrt set1))
    T
    (op-insidep set2 (set-of set1))))

(defmethod op-disjoint-sets-p ((set1 borel-set) (set2 complement-set))
  (op-disjoint-sets-p set2 set1))

(defmethod op-disjoint-sets-p ((set1 explicit-finite-set) (set2 interval))
  (let ((result T))
    (loop for item in (contents-of set1) do
          (when (memberp item set2)
            (progn (setf result NIL)
                   (return)))) result))

(defmethod op-disjoint-sets-p ((set1 interval) (set2 explicit-finite-set))
  (op-disjoint-sets-p set2 set1))


(defmethod op-disjoint-sets-p ((set1 countable-collection) (set2 borel-set))
  (declare (special *max-reasonable-number-of-iterations*))
  (do
    ((index (count-start-of set1)
            (next-index set1 index))
     (counter 1 (+ counter 1))
     (big-count *max-reasonable-number-of-iterations*)
     (result T)
     (set NIL))
    ;; end-test
    ((or (count-endedp set1 index)
         (not result)) result)
    
    ;; Now the action
    (setf set (element set1 index))
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
            is disjoint from set ~%~
            ~s ? " set set2)
          (return T))
         ((quail-yes-or-no-p
           "Assume set ~%~
            ~s ~%~
            is *NOT* disjoint from set ~%~
            ~s ? " set set2)
          (return NIL))
         (T (quail-error "Can't tell whether set ~%~
                          ~s ~%~
                          is disjoint from set ~%~
                          ~s ~%~
                          or not!" set set2)))))
     ((not (op-disjoint-sets-p set set2)) (progn (setf result NIL)
                                                 (return)))))) 

(defmethod op-disjoint-sets-p ((set1 borel-set) (set2 countable-collection))
  (op-disjoint-sets-p set2 set1))
  
(defmethod op-disjoint-sets-p ((set1 countable-set) (set2 borel-set))
  (declare (special *max-reasonable-number-of-iterations*))
  (let ((result T))
  (do
    ((index (count-start-of set1)
            (next-index set1 index))
     (counter 1 (+ counter 1))
     (big-count *max-reasonable-number-of-iterations*)
     (item NIL))
    ;; end-test
    ((count-endedp set1 index) result)
    
    ;; Now the action
    (setf item (element set1 index))
    (cond
     ((> counter big-count)
      (if (quail-yes-or-no-p
           "Have tested ~s items already.  Should testing continue?"
           (- counter 1))
        (setf big-count
              (+ big-count
                 (loop
                   with answer = NIL
                   until (and answer (integerp answer) (>= answer 0))
                   do
                   (setf answer
                         (quail-query "How many more items should be tested? ")
                         )
                   finally (return answer))))
        (cond
         ((quail-yes-or-no-p
           "Assume ~s ~%~
            is disjoint from ~%~
            ~s ? " set1 set2)
          (return T))
         ((quail-yes-or-no-p
           "Assume ~s ~%~
            is *NOT* disjoint from ~%~
            ~s ? " set1 set2)
          (progn (setf result NIL) (return)))
         (T (quail-error "Can't tell whether ~%~
                          ~s ~%~
                          is disjoint from ~%~
                          ~s ~%~
                          or not!" set1 set2)))))
     ((memberp item set2) (progn (setf result NIL) (return))))) result))

(defmethod op-disjoint-sets-p ((set1 borel-set) (set2 countable-set))
  (op-disjoint-sets-p set2 set1))

(defmethod op-disjoint-sets-p ((set1 countable-intersection) (set2 borel-set))
  (quail-error "Not enough information to determine whether ~s ~%~
                is disjoint from ~s." set1 set2))

(defmethod op-disjoint-sets-p ((set1 borel-set) (set2 countable-intersection))
  (op-disjoint-sets-p set2 set1))
