;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               illinois.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     St 440/840 Garth & John
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(illinois)))

(proclaim '(sb-ext:maybe-inline illinois)) ;24NOV2024
(defun illinois (funtn l r &key (max-iterations 1000) (epsilon .0000001) )

  "Takes a function and two points, one on the left of ~
   the root (l) and one on the right (r).  It returns an approximation ~
   of the root using the secant method with the illinois twist.  If ~
   it does not come up with an answer within the # ~
   of iterations it will return nil. ~
   Notice you can change the  ~
   epsilonerance and the number of iterations."
  
;*********************************************************************
  (let ( (p)
         (fl (funcall funtn l))
         (fr (funcall funtn r))
         (test 2))

;  Tests to see if the function is of negative slope
    (cond ((and (> fl 0) (< fr 0))
           (loop for i from 1 to max-iterations
                 do

;  Standart Secant method
                 (<- p (float (- r
                          (/ (* fr (- r l)) (- fr fl)))))
;  Tests if our calculated root is to the left of the root
                 (cond ((< (funcall funtn p) 0)
                        (if (<= (abs(- p r)) epsilon)
                          (return (float p)))
                        (<- r p)

;  This is the illinois part, if we move the same pt. twice in a row
;  we use the function divided by two
                        (if (= test 1)
                          (<- fr (/ (funcall funtn r) 2))
                          (<- fr (funcall funtn r)))
                        (<- test 1))

;  Tests if our calculated root is to the right of the root
                       ((> (funcall funtn p) 0)
                        (if (<= (abs(- p l)) epsilon)
                          (return (float p)))
                        (<- l p)

;  Again this is the illinois twist
                        (if (= test 0)
                          (<- fl (/ (funcall funtn l) 2))
                          (<- fl (funcall funtn l)))
                        (<- test 0))
                       ((= (funcall funtn p) 0)
                        (return (float p)))
                 )))

;  Tests to see if the function is of positive slope.  Once this is 
;  done the rest of the documentation as above applies
          ((and (< fl 0) (> fr 0))
           (loop for i from 1 to max-iterations
                 do

;  Standart Secant method
                 (<- p (float (- r
                          (/ (* fr (- r l)) (- fr fl)))))

;  Tests if our calculated root is to the right of the root
                 (cond ((> (funcall funtn p) 0)
                        (if (<= (abs(- p r)) epsilon)
                          (return (float p)))
                        (<- r p)

;  This is the illinois part, if we move the same pt. twice in a row
;  we use the function divided by two
                        (if (= test 1)
                          (<- fr (/ (funcall funtn r) 2))
                          (<- fr (funcall funtn r)))
                        (<- test 1))

;  Tests if our calculated root is to the left of the root
                       ((< (funcall funtn p) 0)
                        (if (<= (abs(- p l)) epsilon)
                          (return (float p)))
                        (<- l p)

;  Again this is the illinois twist
                        (if (= test 0)
                          (<- fl (/ (funcall funtn l) 2))
                          (<- fl (funcall funtn l)))
                        (<- test 0))
                       ((= (funcall funtn p) 0)
                        (return (float p)))
                 )))

         ((or (and (>= fl 0) (>= fr 0)) (and (<= fl 0) (<= fr 0)))
          (print "you have given an unacceptable set of points")
         )
   )))
