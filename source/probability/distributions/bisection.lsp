;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               bisection.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 
;;; Fall 1992
;;; written by Wai Hong Choi 
;;; for R. W. Oldford

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '()))

;;;-----------------------------------------------------------------------------
;;;
;;;   BISECTION: Given prob, lower and upper, bisection gives F-inverse(prob) 
;;;              which should be an element of [lower upper].
;;;
;;;
;;;-----------------------------------------------------------------------------

(defmethod bisection ((distn prob-measure) prob
                      &key (lower nil) (upper nil) (epsilon 1.0d-10) (max-it 500))
  "Return one value. the value, inverse cdf at prob, is found using bisection method."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline same))
  (with-CL-functions (+ * / - exp expt sqrt > < =)
    (let ((m) (it 0))
      (if (equal lower nil) (setf lower (lower-bound-of distn))) 
      (if (equal upper nil) (setf upper (upper-bound-of distn)))
      (cond ((same distn lower prob :epsilon epsilon) lower)
            ((same distn upper prob :epsilon epsilon) upper)
            (T
             (loop
               (if (< it max-it) (setf it (+ 1 it)) (return "does not converge!"))
               (setf m (/ (+ upper lower) 2))
               (cond
                ((same distn m prob :epsilon epsilon) (return (float m)))
                ((<= prob (cdf-at distn m))
                 (setf upper m)
                 )
                (T
                 (setf lower m) 
                 ))
               ))
            ))))




(defmethod same ((distn prob-measure) x prob
                 &key (epsilon 1.0d-10))
  "Return t if prob is in interval (cdf(x-epsilon),cdf(x)]"
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (+ * / - exp expt sqrt > < =)
    (cond
     ((and (> prob (cdf-at distn (- x epsilon)))
           (<= prob (cdf-at distn x)))
      T)
     (T nil))
    ))




;;;------------------------------------------------------------------------------------
;;;
;;;     BISECTION FOR DISCRETE RANDOM VARIABLE X WHICH TAKES ON INTEGERS ONLY
;;;
;;;------------------------------------------------------------------------------------


(defmethod bisection ((distn discrete-dist) prob
                      &key (lower nil) (upper nil)
                      (epsilon 1.0d-10) (max-it 500))
  "Return an integer which is inverse (discrete) cdf at prob"
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline incomplete-gamma))
  (with-CL-functions (+ * / - exp expt sqrt > < => <=)
    (let ((m) (it 0))
      (if (not (numberp lower))
        (setf lower (lower-bound-of distn))
        (setf lower (floor lower)))
      (if (not (numberp upper))
        (setf upper (upper-bound-of distn))
        (setf upper (ceiling upper)))
      (cond 
       ((same distn lower prob :epsilon epsilon) lower)
       ((same distn upper prob :epsilon epsilon) upper)
       (T
        (loop
          (if (< it max-it) (setf it (+ 1 it)) (return "error"))
          (setf m (floor (/ (+ upper lower) 2)))
          (cond
           ((same distn m prob :epsilon epsilon) (return m))
           ((<= prob (cdf-at distn m))
            (setf upper m)
            )
           (T
            (setf lower m) 
            )
           )))
       ))))




(defmethod same ((distn discrete-dist) x prob &key (epsilon 1.0d-10))
  "Return t if prob in interval (cdf(x-1),cdf(x)]"
  (declare (ignore epsilon))
  (and (> prob (cdf-at distn (- x 1)))
         (<= prob (cdf-at distn x)))
  )
