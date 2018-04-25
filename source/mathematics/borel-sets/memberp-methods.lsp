;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               memberp-methods.lisp
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
;;;      Method                 |                Argument type                |
;;;                             |_____________________________________________|
;;;      Name                   |      Argument 1      |      Argument 2      |
;;;  ___________________________|______________________|______________________|
;;;                             |                      |                      |
;;;                             |                      |                      |      
;;;  op-memberp                 |   (extended-realp)   |      borel-set       |
;;;                             |   (extended-realp)   |   complement-set     |
;;;                             |   (extended-realp)   |   countable-set      |
;;;                             |   (extended-realp)   |      empty-set       |
;;;                             |   (extended-realp)   | explicit-finite-inter|
;;;                             |   (extended-realp)   | explicit-finite-set  |
;;;                             |   (extended-realp)   | explicit-finite-union|
;;;                             |   (extended-realp)   |      interval        |
;;;                             |   (extended-realp)   |   predicate-set      |
;;;                             |   (extended-realp)   |  (extended-realp)    |
;;;                             |                      |                      |
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; op-memberp  - Test whether an extended-real number
;;;             is an element of the set
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod op-memberp (member set)
  (if (and (extended-realp set)
           (extended-realp member))
    (= set member)
    (missing-method 'op-memberp member set)))

(defmethod op-memberp (member (set borel-set))
  (missing-method 'op-memberp member set))              ; must be specialized

(defmethod op-memberp (member (set empty-set))
  (declare (ignore member))
  NIL)

(defmethod op-memberp (number (set interval))
  
  (let ((min (infimum set))
        (max (supremum set)))
    
    (or
      (and (< min number)             ; number definitely in
           (> max number))            ; the interval
                                          ; 
                                          ; OR it
      (and (= min number)             ; equals lower end-point
           (left-closed-p set))           ; and interval is closed
                                          ; on left
                                          ; OR it 
      (and (= number max)             ; equals upper end-point
           (right-closed-p set)))         ; and interval is closed
                                          ; on right
    )
  )

(defmethod op-memberp (member (set predicate-set))
  (funcall (defining-predicate-of set) member))

(defmethod op-memberp (element (set explicit-finite-set))
  (member element (contents-of set)))

(defmethod op-memberp (member (set complement-set))
  (and (op-memberp member (complement-wrt set))
       (not (op-memberp member (set-of set)))))

(defmethod op-memberp (member (self explicit-finite-union))
  (loop for item in (contents-of self) do
        (when (op-memberp member item)
          (return T))))

(defmethod op-memberp (member (self explicit-finite-intersection))
  (let ((result T))
    (loop for item in (contents-of self) do
          (unless (op-memberp member item)
            (setf result NIL))) result))


(defmethod op-memberp (member (set countable-set))
  (if (defining-predicate-of set)
    (funcall (defining-predicate-of set) set member)
    (contains-set-p set member)))

(defmethod op-memberp (member (collection countable-union))
  (if (defining-predicate-of collection)
    (funcall (defining-predicate-of collection) collection member)
    (call-next-method member collection)))

(defmethod op-memberp (member (collection countable-intersection))
  (declare (special *reasonable-number-of-iterations*))
  (if (defining-predicate-of collection)
    (funcall (defining-predicate-of collection) collection member)
    (let ((result T))
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
             "Assume member ~%~
              ~s ~%~
              is in the collection ~%~
              ~s ? " member collection)                    (return T))
           ((quail-yes-or-no-p
             "Assume member ~%~
              ~s ~%~
              is *NOT* in the collection ~%~
              ~s ? " member collection)                    (return NIL))
           (T (quail-error "Can't tell whether the member ~%~
                            ~s ~%~
                            is in the collection ~%~
                            ~s ~%~
                            or not!" member member)))))
       ((unless (op-memberp member (element collection index)) (setf result NIL))))) result)))



;; not borel-sets, but collections of possible borel-sets so...

#|
(defmethod op-memberp (member (set set-collection))
  (quail-error
   "Not enough information to determine whether ~s ~%~
    is within the given set: ~s or not." member set))

(defmethod op-memberp (member (collection countable-collection))
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
           "Assume member ~%~
            ~s ~%~
            is in the collection ~%~
            ~s ? " member collection)                    (return T))
         ((quail-yes-or-no-p
           "Assume member ~%~
            ~s ~%~
            is *NOT* in the collection ~%~
            ~s ? " member collection)                    (return NIL))
         (T (quail-error "Can't tell whether the member ~%~
                          ~s ~%~
                          is in the collection ~%~
                          ~s ~%~
                          or not!" member member)))))
     ((if (op-memberp member (element collection index)) (return T))))))
|#