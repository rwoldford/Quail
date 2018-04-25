;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               bernoulli.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;     Ward Quinlan and
;;;     Frankie and Joyce and Wai Hong
;;;     R.W. Oldford
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) 
     (export '(bernoulli  density-bernoulli  quantile-bernoulli
          random-bernoulli dist-bernoulli)))

(defclass bernoulli (binomial-dist)
  ((lower-bound :reader lower-bound-of
                :initform 0
                :allocation :class)
   (upper-bound :reader upper-bound-of
                :initform 1
                :allocation :class))
  (:documentation "The bernoulli distribution")
  )

(defmethod (setf number-of-trials) (new-value (dist bernoulli))
  (unless (= new-value 1)
    (quail-error "NUMBER-OF-TRIALS for class ~s must be 1."
                 (class-name (class-of dist)))))

(defmethod (setf upper-bound-of) (new-value (dist bernoulli))
  (unless (= new-value 0)
    (quail-error "UPPER-BOUND-OF for class ~s must be 1."
                 (class-name (class-of dist)))))

;;------------------------------------------------------------------
;;  Bernoulli Distribution (Bernoulli(p))         
;;                                                
;;  1. Parameter: p = probability of success.
;;  2. Range: x - integer
;;  3. pdf: f(x) = p         ,when x = 1
;;               = (1 - p)   ,when x = 0 
;;  4. cdf: F(x) = 0         ,when x < 0
;;               = (1 - p)   ,when x = 0
;;               = 1         ,when x >= 1
;;  5. r.v.: rv = 1          ,when r.v. of Unif[0,1] < p 
;;              = 0          ,otherwise
;;  6. quantile: F(x) = q
;;               x = 1       ,when q > p
;;               x = 0,      ,otherwise
;;------------------------------------------------------------------


(defmethod  pdf-at ((distribution bernoulli) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions ( = - )
    (let ((p (eref (p-of distribution) 0)))
      (cond ((= x 0)  (- 1 p))
            ((= x 1)  p)         
            (t  0))        
      )))

(defmethod  cdf-at ((distribution bernoulli)  (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions ( = - < >)
    (let ((p (eref (p-of distribution) 0)))
      (cond ((< x 0) 0)
            ((< x 1) (- 1 p))
            (t 1))  
      )))

(defmethod random-value ((distribution bernoulli) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (< =)
    (let ((p (p-of distribution)))
      (flet
        ((generate-one ()
           (if (< (random-uniform) p)
             1
             0)))
        (if (= n 1)
          (generate-one)
          (array (loop for i from 1 to n collect (generate-one)))
          )
        )
      ))
  )

(defmethod quantile-at ((distribution bernoulli) q &key start)
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (>)
    (if (> q (p-of distribution))
      1
      0)
    ))

(defmethod quantile-at ((be bernoulli) (prob number) &key (start 0))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (bisection be prob))


(defvar *bernoulli*
  NIL
  "An instance of a bernoulli representing the bernoulli distribution. ~
   This instance is used by the standard bernoulli functions random-bernoulli, ~
   quantile-bernoulli, etc.")

(defun density-bernoulli (x &key (p 0.5))
  "Returns the value of a  bernoulli probability at x. ~
   (:required ~
   (:arg x The point at which to evaluate the density, or probability.)) ~
   (:key ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *bernoulli*))
  (let ((saved-p (prob-success *bernoulli*))
        result)
    (setf (prob-success *bernoulli*) p)
    (setf result (pdf-at *bernoulli* x))
    (setf (prob-success *bernoulli*) saved-p)
    result
    )
  )

(defun quantile-bernoulli (prop &key (p 0.5))
  
  "Returns the value of the prop'th bernoulli quantile. ~
   (:required ~
   (:arg prop The cumulative proportion at which to evaluate the quantile.)) ~
   (:key ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *bernoulli*))
  (let ((saved-p (prob-success *bernoulli*))
        result)
    (setf (prob-success *bernoulli*) p)
    (setf result (quantile-at *bernoulli* prop))
    (setf (prob-success *bernoulli*) saved-p)
    result))

(defun random-bernoulli (&key (n 1) (p 0.5))
  "Returns n pseudo-random values from the bernoulli. ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg p 0.5 The probability of success at each trial.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *bernoulli*))
  (let ((saved-p (prob-success *bernoulli*))
        result)
    (setf (prob-success *bernoulli*) p)
    (setf result (random-value *bernoulli* n))
    (setf (prob-success *bernoulli*) saved-p)
    result)
  )

(defun dist-bernoulli (x &key  (p 0.5))
  "Calculates and returns the value of the specified bernoulli distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *bernoulli*))
  (let ((saved-p (prob-success *bernoulli*))
        result)
    (setf (prob-success *bernoulli*) p)
    (setf result (cdf-at *bernoulli* x))
    (setf (prob-success *bernoulli*) saved-p)
    result))
