;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               cauchy.lisp                              
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
;;;
;;;--------------------------------------------------------------------------------
;;; 
;;; Fall 1992
;;; written by Rupa and Rita and Dianne and Ward
;;; for R. W. Oldford

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (export '(cauchy-dist density-cauchy  quantile-cauchy
          random-cauchy dist-cauchy)))

(defclass cauchy-dist (student)
  ((df :initform 1
            :reader df-of
            :allocation
            :class))
  (:documentation "The Cauchy distribution.")
  )
;;; All methods could be calculated directly.

(defmethod cdf-at ((C cauchy-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (+ * / - exp expt sqrt > < = /= )
    (+ .5 (* (/ 1 pi) (atan (/ (- x (location-of C)) (scale-of C)))))))

(defmethod pdf-at ((C cauchy-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (+ * / - exp expt sqrt > < = /= )
    (* (/ 1 pi) (/ 1 (+ 1 (expt (/ (- x (location-of C)) (scale-of C)) 2))))))

(defmethod random-value ((C cauchy-dist) &optional (n 1))
  (+ (* (scale-of C) (tan (* pi (- (random-uniform :n n) .5))))
     (location-of C)))

(defmethod quantile-at ((C cauchy-dist) (p number) &key start)
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (+ * / - exp expt sqrt > < = /= )
    (+ (* (scale-of C) (tan (* pi (- p .5)))) (location-of C))))



(defvar *cauchy*
  NIL
  "An instance of a cauchy-dist representing the location-scale ~
   cauchy distribution. ~
   This instance is used by the standard cauchy functions random-cauchy, ~
   quantile-cauchy, etc.")


(defun density-cauchy (x &key (location 0.0) (scale 1.0))
  "Returns the value of a location scale cauchy density at x. ~
   Location 0 and scale 1 define the standard cauchy distribution.  ~
   (:required ~
   (:arg x The point at which to evaluate the density.)) ~
   (:key ~
   (:arg location 0.0 The location of the cauchy density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.))"
  (declare (special *cauchy*))
  (let ((saved-l (location-of *cauchy*))
        (saved-s (scale-of *cauchy*))
        result)
    (setf (location-of *cauchy*) location)
    (setf (scale-of *cauchy*) scale)
    (setf result (pdf-at *cauchy* x))
    (setf (location-of *cauchy*) saved-l)
    (setf (scale-of *cauchy*) saved-s)
    result
    )
  )

(defun quantile-cauchy (p &key (location 0.0) (scale 1.0))
  
  "Returns the quantile at p of a location scale cauchy. ~
   Location 0 and scale 1 define the standard cauchy distribution.  ~
   (:required ~
   (:arg p The probability at which to evaluate the quantile.)) ~
   (:key ~
   (:arg location 0.0 The location of the cauchy density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.))"
  (declare (special *cauchy*))
  (let ((saved-l (location-of *cauchy*))
        (saved-s (scale-of *cauchy*))
        result)
    (setf (location-of *cauchy*) location)
    (setf (scale-of *cauchy*) scale)
    (setf result (quantile-at *cauchy* p))
    (setf (location-of *cauchy*) saved-l)
    (setf (scale-of *cauchy*) saved-s)
    result)
  )

(defun random-cauchy (&key (n 1) (location 0.0) (scale 1.0))
  "Returns n pseudo-random values from the location scale cauchy. ~
   Location 0 and scale 1 define the standard cauchy distribution.  ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg location 0.0 The location of the cauchy density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *cauchy*))
  (let ((saved-l (location-of *cauchy*))
        (saved-s (scale-of *cauchy*))
        result)
    (setf (location-of *cauchy*) location)
    (setf (scale-of *cauchy*) scale)
    (setf result (random-value *cauchy* n))
    (setf (location-of *cauchy*) saved-l)
    (setf (scale-of *cauchy*) saved-s)
    result)
  )

(defun dist-cauchy (x &key (location 0.0) (scale 1.0))
  "Calculates and returns the value of the distribution function at x ~
   for a location-scale cauchy. ~
   (i.e. prob(X <= x) .)  ~
   Location 0 and scale 1 define the standard cauchy distribution.  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg location 0.0 The location of the cauchy density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.)) "
  (declare (special *cauchy*))
  (let ((saved-l (location-of *cauchy*))
        (saved-s (scale-of *cauchy*))
        result)
    (setf (location-of *cauchy*) location)
    (setf (scale-of *cauchy*) scale)
    (setf result (cdf-at *cauchy* x))
    (setf (location-of *cauchy*) saved-l)
    (setf (scale-of *cauchy*) saved-s)
    result)
  )
