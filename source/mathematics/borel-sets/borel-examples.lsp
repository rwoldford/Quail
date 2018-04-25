;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               Borel-Examples.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1990.
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;                             EXAMPLES OF BOREL-SETS
;;;
;;;  In this file examples of familiar borel-sets are constructed.
;;;  Some of these have been constructed elsewhere and assigned to global
;;;  constants.
;;;
;;;----------------------------------------------------------------------------------


;;;----------------------------------------------------------------------------------
;;;
;;;  Some typical intervals
;;;
;;;----------------------------------------------------------------------------------


;;; [-1,1]

(setf a
      (make-instance 'interval
          :infimum -1
          :supremum 1
          :closure-property :closed))

;;; (-1,1)

(setf b
      (make-instance 'interval
          :infimum -1
          :supremum 1
          :closure-property :open))

;;; or (-1,1) again

(setf b
      (make-instance 'interval
          :infimum -1
          :supremum 1))                               ; default is open

;;; [0.5, 3)

(setf c
      (make-instance 'interval
          :infimum 0.5
          :supremum 3
          :closure-property :closed-left))

;;; [0.5,1)

(setf d
      (set-intersection b c))            ; by intersection

;;; 

;;;------------------------------------------------------------------------------------------
;;;
;;;  An explicit-finite-set
;;;
;;;------------------------------------------------------------------------------------------

;;; { 0, 1, 3,  2, 4, 10}

(setf ef1
      (make-instance 'explicit-finite-set
          :contents '(0 1 3 2 4 10)))

;;;----------------------------------------------------------------------------------
;;;
;;;  REALS
;;;
;;;----------------------------------------------------------------------------------

(setf real-nos
      (make-instance 'interval
            :infimum -infinity
            :supremum +infinity))

;;; or

(setf extended-reals
      (make-instance 'interval
            :infimum -infinity
            :supremum +infinity
            :closure-property :both))

;;; or

(setf extended-reals-too
      (make-instance 'predicate-set
            :closure-property :both
            :defining-predicate
            #'(lambda (set value)
                (declare (ignore set))
                (extended-realp value))))

           
;;;----------------------------------------------------------------------------------
;;;
;;;  The EXTENDED INTEGERS
;;;
;;;     {-infinity, ..., -2, -1, 0, 1, 2, ..., +infinity}
;;;
;;;----------------------------------------------------------------------------------


(setf the-extended-integers
      (make-instance 'countable-set
            :infimum -infinity
            :supremum +infinity
            :begin-index -infinity
            :end-index +infinity
            :closure-property :closed
            :element-function
            #'(lambda (set index)
                (declare (ignore set))
                index)
            :defining-predicate
            #'(lambda (set test-member)
                (declare (ignore set))
                (extended-integerp test-member))))
            
 

;;;-----------------------------------------------------------------------------------------
;;;
;;; The NATURAL-NUMBERS
;;;
;;; {0,1, 2, 3, 4, ...}  
;;; 
;;; An countably-infinite-increasing-set because they are COUNTED in increasing order.
;;;-----------------------------------------------------------------------------------------

(setf natural-numbers
      (make-instance 'countable-set
            :infimum 0
            :supremum +infinity
            :begin-index 0
            :end-index +infinity
            :element-function
            #'(lambda (set index)
                (declare (ignore set))
                index)
            :defining-predicate
            #'(lambda (set test-member)
                (declare (ignore set))
                (and (extended-integerp test-member)
                     (> test-member 0)))))
 

;;;-----------------------------------------------------------------------------------------
;;;
;;; The UNIQUE FIBONACCI NUMBERS
;;;
;;; {1, 2, 3, 5, 8, 13, ...}  
;;; 
;;; An countably-infinite-increasing-set because they are COUNTED in increasing order.
;;;-----------------------------------------------------------------------------------------

(setf unique-fibonacci-numbers
      (make-instance 'countable-set
            :infimum 1
            :supremum +infinity
            :begin-index 0
            :end-index +infinity
            :element-function
            #'(lambda (set index)
                (cond
                 ((= index 0) 1)
                 ((= index 1) 2)
                 ((= index +infinity)
                  +infinity)
                 (T
                  (+ (element set (- index 1))
                         (element set (- index 2))))))
            )
      )

;;;  or better

(setf unique-fibonacci-numbers
      (make-instance 'countable-set
            :infimum 1
            :supremum +infinity
            :begin-index 0
            :end-index +infinity
            :element-function
            #'(lambda (set index)
                (cond
                 ((= index +infinity)
                  +infinity)
                 (T
                  (+ (element set (- index 1))
                         (element set (- index 2))))))
            )
      )

;;;-----------------------------------------------------------------------------------------
;;;
;;; The Cantor Ternary Set
;;;
;;;-----------------------------------------------------------------------------------------

(setf cantor-set
      (make-instance 'predicate-set
        :closure-property :closed
        :defining-predicate
        #'(lambda (set value)
            (cond
             ((< 0 value) NIL)
             ((> value 1) NIL)
             (T (loop
                  for i from 1 by 1
                  with 
                  big-count = *max-reasonable-number-of-iterations* 
                  do
                  (if (= 1 (ternary-expansion value i))
                    (return NIL)
                    (if (> i big-count)
                      ;;   .    query user,
                      ;;   .    increase big-count,
                      ;;   .    et cetera.
                      ))))))
        )
      )

;;;------------------------------------------------------------------------------------------
;;;
;;;  An  F-SIGMA Set
;;;
;;;------------------------------------------------------------------------------------------

(setf open-3-4-as-F-sigma
      (make-instance 'countable-union
           :from 1 :to +infinity
           :element-function
           #'(lambda (union index)
               (make-instance 'interval
                   :closure-property :closed
                   :infimum
                   (+ 3 (/ 1 index))
                   :supremum
                   (- 4 (/ 1 index))))
          :closure-property :open)
      )

(describe (copy-set open-3-4-as-F-sigma))


;;;
;;; BTW 
;;;
;;;     (element open-3-4-as-F-sigma +infinity)
;;;
;;;               ==> [3,4]
;;;
;;;    ...  too bad,
;;;         close though!


;;;------------------------------------------------------------------------------------------
;;;
;;;  Similarly G-DELTA Sets could be defined.
;;;  (countable-unions of open sets)
;;;
;;;------------------------------------------------------------------------------------------