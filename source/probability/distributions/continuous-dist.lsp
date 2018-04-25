;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        continuous-dist.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;     R.W. Oldford 1993
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(continuous-dist)))

(defclass continuous-dist (prob-measure)
  ()
  (:documentation "A generic continuous probability distribution.")
)

(defmethod cdf-at ((cont continuous-dist) value)
  (cond 
   ((and (= (lower-bound-of cont) -INFINITY) (< (upper-bound-of cont) +INFINITY))
    (flet ((func (x) (pdf-at cont x)))
      (- 1 (extended-simpsons #'func (eref value 0) (eref (upper-bound-of cont) 0)
                                     :max-iterations 500))))
   ((> (lower-bound-of cont) -INFINITY)
    (flet ((func (x) (pdf-at cont x)))
      (extended-simpsons #'func (eref (lower-bound-of cont) 0) (eref value 0)
                                :max-iterations 500)))
   ;;  worst case, don't know anything, return NIL
   ((and (= (lower-bound-of cont) -INFINITY) (= (upper-bound-of cont) +INFINITY))
    (missing-method 'cdf-at cont (eref value 0))))
  )

;; return pdf-at value

(defmethod pdf-at ((cont continuous-dist) value)
  (labels ((func (x) (cdf-at cont x)))
    (let ((fprime (deriv #'func)))
      (funcall fprime value))))
               
;; return quantile-at value

(defmethod quantile-at ((cont continuous-dist) (p number) &key (start 0.5))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline newton illinois))
  (with-CL-functions (+ * / -)
    (if (and (> (lower-bound-of cont) -INFINITY)
             (< (upper-bound-of cont) +INFINITY))
      (flet ((g-fun (x) (- (cdf-at cont x) p)))
        (illinois #'g-fun
                  (eref (lower-bound-of cont) 0)
                  (eref (upper-bound-of cont) 0)))
      
      (flet ((g-fun (x) (- (cdf-at cont x) p))
             (g-prime (x) (pdf-at cont x)))
        (newton #'g-fun :start start :deriv #'g-prime))
      )))

(defmethod expectation ((dist continuous-dist) &optional g)
  (let ((l (lower-bound-of dist))
        (u (upper-bound-of dist)))
    (cond
     ((>= l u)  0)
     (T
      
      (if (= u +infinity)
        (setf u most-positive-double-float) )
      (if (= l -infinity)
        (setf l most-negative-double-float) )
      (if g
        (extended-simpsons
         (function (lambda (x)
                     (* (funcall g x) (pdf-at dist x))))
         l u)
        (extended-simpsons
         (function (lambda (x) (pdf-at dist x)))
         l u)))
     )
    ))
