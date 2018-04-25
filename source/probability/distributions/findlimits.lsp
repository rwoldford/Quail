;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               findlimits-lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Wai Hong Choi 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 
;;; Fall 1992
;;; written by Wai Hong Choi 
;;; for R. W. Oldford

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) 
    (export '()))
;;;-----------------------------------------------------------------------------
;;;
;;;   FIND-LIMITS : Given prob which is an element of (0,1), find-limits gives a 
;;;                 and b such that F-inverse(prob) is an element of [a.b].
;;; 
;;;-----------------------------------------------------------------------------


(defmethod find-limits ((distn prob-measure) prob
                        &key (start 0) (increment 5) (max-it 200))
  "Returns two values. the first is the lower limit and the second i~
   s the upper limit of an interval which contains inverse cdf at prob"
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - exp expt sqrt > < = /= )
    (let ((l)
          (u)
          (it 0))
      (if (minusp increment) (setf increment (abs increment)))
      (cond
       ((= increment 0) (quail-error "~s (increment) is not postive!" increment))
       ((and (numberp (lower-bound-of distn))
             (numberp (upper-bound-of distn)))
        (setf l (lower-bound-of distn))
        (setf u (upper-bound-of distn))
        (values l u))
       ((numberp (lower-bound-of distn))
        (find-upper-limit distn prob increment))
       ((numberp (upper-bound-of distn))
        (find-lower-limit distn prob increment))
       (T
        (setf l (- start increment))
        (setf u (+ start increment))
        (loop
          (if (< it max-it) (setf it (+ 1 it))
              (return "Please increase value of increment!"))
          (cond
           ((and (> prob (cdf-at distn l)) (<= prob (cdf-at distn u)))
            (return (values l u)))
           ((= prob (cdf-at distn l)) (setf u l) (setf l (- l increment)))
           ((< prob (cdf-at distn l))
            (return (find-lower-limit distn prob increment :upper l)))
           (T
            (return (find-upper-limit distn prob increment :lower u)))
           ))))
      )))





;;;-----------------------------------------------------------------------------
;;;
;;;     FIND-UPPER-LIMIT
;;;
;;;
;;;-----------------------------------------------------------------------------


(defmethod find-upper-limit((distn prob-measure) prob increment
                            &key (lower nil) (max-it 200))
  "Return a value: an upper limit of an interval containing inverse cdf at ~
   prob where a lower limit is given."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - exp expt sqrt > < = /= )
    (let ((upper) (it 0))
      (if (not lower) (setf lower (lower-bound-of distn)))
      (setf upper (+ lower increment))
      (loop      
        (if (< it max-it) (setf it (+ 1 it))
            (quail-error "Please increase value of increment!"))
        (cond
         ((<= prob (cdf-at distn upper)) (return (values lower upper)))
         (T 
          (setf lower upper)
          (setf upper (+ upper increment))))
        ))))



;;;-----------------------------------------------------------------------------
;;;
;;;     FIND-LOWER-LIMIT
;;;
;;;
;;;-----------------------------------------------------------------------------



(defmethod find-lower-limit((distn prob-measure) prob increment
                            &key (upper nil) (max-it 200))
  "Return a value: a lower limit of an interval containing inverse cdf ~
   at prob where an upper limit is given."
  
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - exp expt sqrt > < = /= )
    (let ((lower) (it 0))
      (if (not upper) (setf upper (upper-bound-of distn)))
      (setf lower (- upper increment))
      (loop
        (if (< it max-it) (setf it (+ 1 it))
            (quail-error "Please increase value of increment!"))
        (cond
         ((> prob (cdf-at distn lower)) (return (values lower upper)))
         (T 
          (setf upper lower)
          (setf lower (- lower increment))))
        ))))



