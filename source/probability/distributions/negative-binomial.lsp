;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          negative-binomial.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;     by frankie (ID:89103738), Ward
;;;
;;;--------------------------------------------------------------------------------
;;; 
;;;  

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(negative-binomial number-of-successes prob-success
          density-negative-binomial  quantile-negative-binomial
          random-negative-binomial dist-negative-binomial)))

(defclass negative-binomial (discrete-dist)
  ((p :accessor p-of 
      :initarg :p
      :initform NIL)
   (lower-bound :accessor lower-bound-of 
                :initarg :successes
                :initform 1)
   (upper-bound :reader upper-bound-of
                :initform +INFINITY
                :allocation :class))
  (:documentation "The Negative Binomial distribution.")
  )

(defmethod initialize-instance :after ((self negative-binomial)
                                       &rest initargs
                                       &key (successes NIL))
  (declare (ignore initargs))
  (if (and successes (numberp successes) (> successes 0))
    (setf (number-of-successes self) successes))
  )

(defmethod number-of-successes ((dist negative-binomial))
  (lower-bound-of dist))

(defmethod (setf number-of-successes) (new-value (dist negative-binomial))
  (if (and (integerp new-value) (> new-value 0))
    (setf (lower-bound-of dist) new-value)
    (quail-error "Total number of successes must be an integer greater than zero, ~
                  not ~s ."
                 new-value)))

(defmethod (setf upper-bound-of) (new-value (dist negative-binomial))
  (unless (= new-value (upper-bound-of dist))
    (quail-error "Upper-bound is ~s for ~s, ~
                  not ~s ."
                 (upper-bound-of dist)
                 (class-name (class-of dist))
                 new-value))
  )

(defmethod prob-success ((dist negative-binomial))
  (p-of dist))

(defmethod (setf prob-success) (new-value (dist negative-binomial))
  (if (and (<= new-value 1.0) (>= new-value 0.0))
    (setf (p-of dist) new-value)
    (quail-error "Probability of success must be a number between 0.0 ~
                  and 1.0 inclusive, ~
                  not ~s ."
                 new-value)))

;;;------------------------------------------------------------------------
;  Negative-Binomial Distribution (Negative-Binomial (n,p))
;  
;  1) Parameter : p = the probability of success
;                 x  = the number of success
;
;  2) pdf : f(n) = /n-1\*p^x*(1-p)^(n-x)          , n >= x
;                  \x-1/ 
;
;  3) cdf :         / 0                           ,if x > a
;            F(a) = |
;                   \ SUM(from t = x to a){f(t)}  ,if x <= a
;                  
;;------------------------------------------------------------------------

(defmethod pdf-at ((dist negative-binomial) (n number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / -  expt sqrt >=)
    (let ((x (eref (number-of-successes dist) 0))
          (p (eref (p-of dist) 0)))
      (if (and (integerp n) (>= n x))
        (* (choose (- n 1) 
                   (- x 1))
           (expt p x)
           (expt (- 1 p) (- n x)))
        0))))

(defmethod cdf-at ((dist negative-binomial) (n number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / -  expt sqrt >=)
    (let ((x (eref (number-of-successes dist) 0))
          (p (eref (p-of dist) 0)))
      (if (>= (truncate n) x)
        (incomplete-beta x (truncate n) (- 1 p))
        0)
      )))

(defmethod quantile-at ((dist negative-binomial)
                        (prob number)
                        &key (start NIL))
  (declare (ignore start))
  (multiple-value-bind (l u) (find-limits dist prob)
    (bisection dist prob :lower l :upper u))
  )

(defmethod random-value ((dist negative-binomial) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let ((x (eref (number-of-successes dist) 0))
        (p (eref (p-of dist) 0))
        (sum 0))
    ; (loop for i from 1 to x do
    (do ((i 1 (incf i)))
        ((> i x))
      (setf sum
            (+ sum (random-geometric :p p :n n))))
    sum))


(defvar *negative-binomial*
  NIL
  "An instance of a negative-binomial representing the negative-binomial ~
   distribution. ~
   This instance is used by the standard functions random-negative-binomial, ~
   quantile-negative-binomial, etc.")

(defun density-negative-binomial (x &key (successes 1) (p 0.5))
  "Returns the value of a  negative-binomial probability at x. ~
   (:required ~
   (:arg x The point at which to evaluate the density, or probability.)) ~
   (:key ~
   (:arg successes 1 The total number of successful ~
   trials for the negative-binomial.) ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *negative-binomial*))
  (let ((saved-s (number-of-successes *negative-binomial*))
        (saved-p (prob-success *negative-binomial*))
        result)
    (setf (number-of-successes *negative-binomial*) successes)
    (setf (prob-success *negative-binomial*) p)
    (setf result (pdf-at *negative-binomial* x))
    (setf (number-of-successes *negative-binomial*) saved-s)
    (setf (prob-success *negative-binomial*) saved-p)
    result
    )
  )

(defun quantile-negative-binomial (prop &key (successes 1) (p 0.5))
  
  "Returns the value of the prop'th negative-binomial quantile. ~
   (:required ~
   (:arg prop The cumulative proportion at which to evaluate the quantile.)) ~
   (:key ~
   (:arg successes 1 The total number of successful ~
   trials for the negative-binomial.) ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *negative-binomial*))
  (let ((saved-s (number-of-successes *negative-binomial*))
        (saved-p (prob-success *negative-binomial*))
        result)
    (setf (number-of-successes *negative-binomial*) successes)
    (setf (prob-success *negative-binomial*) p)
    (setf result (quantile-at *negative-binomial* prop))
    (setf (number-of-successes *negative-binomial*) saved-s)
    (setf (prob-success *negative-binomial*) saved-p)
    result))

(defun random-negative-binomial (&key (n 1) (successes 1) (p 0.5))
  "Returns n pseudo-random values from the negative-binomial. ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg successes 1 The total number of successful ~
   trials for the negative-binomial.) ~
   (:arg p 0.5 The probability of success at each trial.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *negative-binomial*))
  (let ((saved-s (number-of-successes *negative-binomial*))
        (saved-p (prob-success *negative-binomial*))
        result)
    (setf (number-of-successes *negative-binomial*) successes)
    (setf (prob-success *negative-binomial*) p)
    (setf result (random-value *negative-binomial* n))
    (setf (number-of-successes *negative-binomial*) saved-s)
    (setf (prob-success *negative-binomial*) saved-p)
    result)
  )

(defun dist-negative-binomial (x &key (successes 1) (p 0.5))
  "Calculates and returns the value of the specified negative-binomial distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg successes 1 The total number of successful ~
   trials for the negative-binomial.) ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *negative-binomial*))
  (let ((saved-s (number-of-successes *negative-binomial*))
        (saved-p (prob-success *negative-binomial*))
        result)
    (setf (number-of-successes *negative-binomial*) successes)
    (setf (prob-success *negative-binomial*) p)
    (setf result (cdf-at *negative-binomial* x))
    (setf (number-of-successes *negative-binomial*) saved-s)
    (setf (prob-success *negative-binomial*) saved-p)
    result))


