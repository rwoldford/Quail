;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               binomial.lisp                              
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
;;;     R.W. Oldford 1995
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (export '(binomial-dist number-of-trials prob-success
          density-binomial  quantile-binomial
          random-binomial dist-binomial)))

(defclass binomial-dist (discrete-dist)
  ((lower-bound :accessor lower-bound-of
                :initform 0
                :allocation :class)
   (upper-bound :accessor upper-bound-of
                :initarg :upper-bound)
   (p :accessor p-of
      :initarg :p))
   (:documentation "The binomial distribution with parameters number-of-trials-of ~
                    being :upper-bound~
                    and the prob-success being :p.")
   )

(defmethod initialize-instance :after ((self binomial-dist)
                                       &rest initargs
                                       &key (n NIL))
  (declare (ignore initargs))
  (if (and n (numberp n) (> n 0))
    (setf (number-of-trials self) n))
  )

(defmethod number-of-trials ((dist binomial-dist))
  (upper-bound-of dist))

(defmethod (setf number-of-trials) (new-value (dist binomial-dist))
  (if (and (integerp new-value) (> new-value 0))
    (setf (upper-bound-of dist) new-value)
    (quail-error "Total number of trials must be an integer greater than zero, ~
                  not ~s ."
                 new-value)))

(defmethod prob-success ((dist binomial-dist))
  (p-of dist))

(defmethod (setf prob-success) (new-value (dist binomial-dist))
  (if (and (<= new-value 1.0) (>= new-value 0.0))
    (setf (p-of dist) new-value)
    (quail-error "Probability of success must be a number between 0.0 ~
                  and 1.0 inclusive, ~
                  not ~s ."
                 new-value)))


(defmethod (setf lower-bound-of) (new-value (dist binomial-dist))
  (unless (= new-value 0)
    (quail-error "LOWER-BOUND-OF for class ~s must be 0."
                 (class-name (class-of dist)))))

;;;-----------------------------------------------------------------------------
;;;
;;;     PDF-AT (binomial-dist)
;;;
;;;-----------------------------------------------------------------------------


(defmethod pdf-at ((bi binomial-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline choose))
  (multiple-value-bind
    (integer-x frac-x)
    (truncate x)
    (if (zerop frac-x)
      (with-CL-functions (+ * / - exp expt sqrt > < = /=)
        (let ((n (upper-bound-of bi))
              (p (p-of bi)))
          (* (choose n integer-x)
             (expt p integer-x)
             (expt (- 1 p) (- n integer-x)))))
      0)))




;;;-----------------------------------------------------------------------------
;;;
;;;     CDF-AT (binomial-dist) using incomplete beta function, I, where
;;;                       P(X >= k) = I(k, n-k+1, p)  
;;;
;;;-----------------------------------------------------------------------------


(defmethod cdf-at ((bi binomial-dist) x)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (let (intx)
    (cond
     ((not (numberp x)) (quail-error "~s is not a number!" x))
     ((minusp x) 0)
     ((>= x (upper-bound-of bi)) 1)
     (T
      (setf intx (floor x))
      (- 1 
         (incomplete-beta (+ 1 intx) (- (upper-bound-of bi) intx) (p-of bi))
         ))
     )))




;;;-----------------------------------------------------------------------------
;;;
;;;      RANDOM-VALUE (binomial-dist)  USING SUM OF BERNOULLI R.V.'S METHOD
;;;
;;;-----------------------------------------------------------------------------


(defmethod random-value ((bi binomial-dist) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let ((rand 0)
        (p (p-of bi)))
    ; (loop for i from 1 to (upper-bound-of bi) do
    (do ((i 1 (incf i)))
        ((> i (upper-bound-of bi)))
      (setf rand (+ rand (random-bernoulli :p p :n n))))
    rand
    ))




;;;-----------------------------------------------------------------------------
;;;
;;;     QUANTILE-AT (binomial-dist)
;;;
;;;-----------------------------------------------------------------------------


(defmethod quantile-at ((bi binomial-dist) (prob number) &key (start 0))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (bisection bi prob))


(defvar *binomial*
  NIL
  "An instance of a binomial representing the binomial distribution. ~
   This instance is used by the standard binomial functions random-binomial, ~
   quantile-binomial, etc.")

(defun density-binomial (x &key (total 1) (p 0.5))
  "Returns the value of a  binomial probability at x. ~
   (:required ~
   (:arg x The point at which to evaluate the density, or probability.)) ~
   (:key ~
   (:arg total 1 The total number of trials for the binomial.) ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *binomial*))
  (let ((saved-n (number-of-trials *binomial*))
        (saved-p (prob-success *binomial*))
        result)
    (setf (number-of-trials *binomial*) total)
    (setf (prob-success *binomial*) p)
    (setf result (pdf-at *binomial* x))
    (setf (number-of-trials *binomial*) saved-n)
    (setf (prob-success *binomial*) saved-p)
    result
    )
  )

(defun quantile-binomial (prop &key (total 1) (p 0.5))
  
  "Returns the value of the prop'th binomial quantiles. ~
   (:required ~
   (:arg prop The cumulative proportion at which to evaluate the quantile.)) ~
   (:key ~
   (:arg total 1 The total number of trials for the binomial.) ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *binomial*))
  (let ((saved-n (number-of-trials *binomial*))
        (saved-p (prob-success *binomial*))
        result)
    (setf (number-of-trials *binomial*) total)
    (setf (prob-success *binomial*) p)
    (setf result (quantile-at *binomial* prop))
    (setf (number-of-trials *binomial*) saved-n)
    (setf (prob-success *binomial*) saved-p)
    result))

(defun random-binomial (&key (n 1) (total 1) (p 0.5))
  "Returns n pseudo-random values from the binomial. ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg total 1 The total number of trials for the binomial.) ~
   (:arg p 0.5 The probability of success at each trial.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *binomial*))
  (let ((saved-n (number-of-trials *binomial*))
        (saved-p (prob-success *binomial*))
        result)
    (setf (number-of-trials *binomial*) total)
    (setf (prob-success *binomial*) p)
    (setf result (random-value *binomial* n))
    (setf (number-of-trials *binomial*) saved-n)
    (setf (prob-success *binomial*) saved-p)
    result)
  )

(defun dist-binomial (x &key (total 1) (p 0.5))
  "Calculates and returns the value of the specified binomial distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg total 1 The total number of trials for the binomial.) ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *binomial*))
  (let ((saved-n (number-of-trials *binomial*))
        (saved-p (prob-success *binomial*))
        result)
    (setf (number-of-trials *binomial*) total)
    (setf (prob-success *binomial*) p)
    (setf result (cdf-at *binomial* x))
    (setf (number-of-trials *binomial*) saved-n)
    (setf (prob-success *binomial*) saved-p)
    result))
