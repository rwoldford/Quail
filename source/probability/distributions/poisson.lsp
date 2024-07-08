;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               poisson.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;     Frankie and Joyce and Wai Hong
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(poisson-dist density-poisson  quantile-poisson
          random-poisson dist-poisson mean)))

(defclass poisson-dist (discrete-dist)
  ((lower-bound :reader lower-bound-of
                :initform 0
                :allocation :class)
   (upper-bound :reader upper-bound-of
                :initform +infinity
                :allocation :class)
   (mean :accessor mean
         :initarg :mean :initform 1))
  (:documentation "The poisson distribution with parameter mean")
)

(defmethod (setf upper-bound-of) (new-value (dist poisson-dist))
  (declare (ignorable dist)) ;(declare (ignore dist)) ; 31JUL2023
  (unless (= new-value infinity)
    (quail-error "Upper bound of a Poisson must be +infinity.  It cannot ~
                  be reset to ~s" new-value)))

(defmethod (setf lower-bound-of) (new-value (dist poisson-dist))
  (declare (ignorable dist)) ;(declare (ignore dist)) ; 31JUL2023
  (unless (= new-value 0)
    (quail-error "Lower bound of a Poisson must be 0.  It cannot ~
                  be reset to ~s" new-value)))

;;;-----------------------------------------------------------------------------
;;;
;;;     PDF-AT (POISSON)
;;;
;;;-----------------------------------------------------------------------------


(defmethod pdf-at ((dist poisson-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - expt exp)
    (let ((mean (eref (mean dist) 0)))
      (if (and (integerp x) (>= x 0))
        (/ (expt mean x) (* (exp mean) (factorial x)))
        0))))




;;;------------------------------------------------------------------------------
;;;
;;;     CDF-AT (POISSON) using incomplete gamma complement function, Q, where
;;;                      P(X < k) = Q(k, mean)
;;;
;;;------------------------------------------------------------------------------


(defmethod cdf-at ((dist poisson-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ floor)
    (let ((mean (eref (mean dist) 0)))
      (multiple-value-bind
        (value1 value2)
        (incomplete-gamma-complement (+ 1 (floor x)) mean)
        value2
        value1)
      )))





;;;-----------------------------------------------------------------------------
;;;
;;;     RANDOM-VALUE (POISSON) BASED ON POISSON PROCESS
;;;
;;;-----------------------------------------------------------------------------
#| Inherit from discrete

(defmethod random-value ((po poisson-dist) &optional (n 1))
  (flet
    ((generate-one ()
       (let ((u)
             (p 1)
             (rand 0)
             (c (/ 1 (exp (mean po)))))
         (loop
           (setf u (random-uniform))
           (setf p (* p u))
           (if (>= p c)
             (setf rand (+ rand 1))
             (return rand))
           ))))
    (if (= n 1)
      (generate-one)
      (array (loop for i from 1 to n collect (generate-one)))
      )
    ))
|#



;;;-----------------------------------------------------------------------------
;;;
;;;     QUANTILE-AT (POISSON)
;;;
;;;-----------------------------------------------------------------------------


(defmethod quantile-at ((dist poisson-dist) 
                        (prob number)
                        &key (start NIL))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (floor)
    (let ((m (floor (eref (mean dist) 0))))
      (multiple-value-bind (l u) (find-limits dist prob :increment m)
        (bisection dist prob :lower l :upper u)))))




(defvar *poisson*
  NIL
  "An instance of a poisson representing the poisson distribution. ~
   This instance is used by the standard poisson functions random-poisson, ~
   quantile-poisson, etc.")

(defun density-poisson (x &key (mean 1))
  "Returns the value of a  poisson probability at x. ~
   (:required ~
   (:arg x The point at which to evaluate the density, or probability.)) ~
   (:key ~
   (:arg mean 1 The mean of the poisson.))"
  (declare (special *poisson*))
  (let ((saved-mean (mean *poisson*))
        result)
    (setf (mean *poisson*) mean)
    (setf result (pdf-at *poisson* x))
    (setf (mean *poisson*) saved-mean)
    result
    )
  )

(defun quantile-poisson (prop &key (mean 1))
  
  "Returns the value of the prop'th poisson quantile. ~
   (:required ~
   (:arg prop The cumulative proportion at which to evaluate the quantile.)) ~
   (:key ~
   (:arg mean 1 The mean of the poisson.))"
  (declare (special *poisson*))
  (let ((saved-mean (mean *poisson*))
        result)
    (setf (mean *poisson*) mean)
    (setf result (quantile-at *poisson* prop))
    (setf (mean *poisson*) saved-mean)
    result))

(defun random-poisson (&key (n 1) (mean 1))
  "Returns n pseudo-random values from the poisson. ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg mean 1 The mean of the poisson.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *poisson*))
  (let ((saved-mean (mean *poisson*))
        result)
    (setf (mean *poisson*) mean)
    (setf result (random-value *poisson* n))
    (setf (mean *poisson*) saved-mean)
    result)
  )

(defun dist-poisson (x &key (mean 1))
  "Calculates and returns the value of the specified poisson distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg mean 1 The mean of the poisson.))"
  (declare (special *poisson*))
  (let ((saved-mean (mean *poisson*))
        result)
    (setf (mean *poisson*) mean)
    (setf result (cdf-at *poisson* x))
    (setf (mean *poisson*) saved-mean)
    result))
