;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               beta.lisp                              
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
;;; Methods for The Continuous Beta Distribution
;;; Written by John  & Garth 
;;;
(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) 
     (export '(beta-dist density-beta  quantile-beta
          random-beta dist-beta)))

(defclass beta-dist (continuous-dist)
  ((shape1   :reader shape1-of
             :initarg :shape1)
   (shape2   :reader shape2-of
             :initarg :shape2)
   (1/beta   :accessor 1/beta
             :documentation "Just a holder of 1/beta(a,b).")
   (lower-bound :reader  lower-bound-of
                :initarg  :lower-bound
                :initform 0)
   (upper-bound  :reader  upper-bound-of
                 :initarg  :upper-bound
                 :initform 1)
   (gamma1 :accessor gamma1-of
           :initarg :gamma1-of
           :documentation "A gamma(shape1) distribution for random-values.")
   (gamma2 :accessor gamma2-of
           :initarg :gamma2-of
           :documentation "A gamma(shape2) distribution for random-values."))
  (:documentation "The Beta distribution with shape parameters 'shape1' ~
                   and 'shape2'. ")
  )

(defmethod (setf lower-bound-of)
           (new-value (self beta-dist))
  "Resetting of the lower bound is not allowed."
  (when (/= new-value 0)
    (quail-error "ERROR: lower-bound-of a beta distribution must be zero, ~
                  not ~s" new-value)
    )
  )

(defmethod (setf upper-bound-of)
           (new-value (self beta-dist))
  "Resetting of the upper bound is not allowed."
  (when (/= new-value 1)
    (quail-error "ERROR: upper-bound-of a beta distribution must be 1, ~
                  not ~s" new-value)
    )
  )

(defmethod (setf shape1-of)
           (new-value (self beta-dist))
  "Resetting the shape1 of the beta-dist."
  (with-CL-functions (= <= exp + -)
    (cond
     ((= new-value (shape1-of self)) new-value)
     ((<= new-value 0)
      (quail-error "Shape1 parameter of a beta must be greater than zero, not~
                    ~s ." new-value))
     (T
      (setf (shape-of (gamma1-of self)) new-value)
      (let ((b (shape2-of self)))
        (setf (1/beta self)
              (exp (- (log-gamma (+ new-value b))
                      (log-gamma new-value)
                      (log-gamma b)))))
      (setf (slot-value self 'shape1) new-value)
      )
     )
    )
  )

(defmethod (setf shape2-of)
           (new-value (self beta-dist))
  "Resetting the shape2 of the beta-dist."
  (with-CL-functions (= <= exp + -)
    (cond
     ((= new-value (shape2-of self)) new-value)
     ((<= new-value 0)
      (quail-error "Shape2 parameter of a beta must be greater than zero, not~
                    ~s ." new-value))
     (T
      (setf (shape-of (gamma2-of self)) new-value)
      (let ((a (shape1-of self)))
        (setf (1/beta self)
              (exp (- (log-gamma (+ a new-value))
                      (log-gamma a)
                      (log-gamma new-value)))))
      (setf (slot-value self 'shape2) new-value)
      )
     )
    )
  )
;;;********************************************************************
;                                THE BETA DISTRIBUTION
;   
;     B(shape1,shape2) = Gamma(shape1)*Gamma(shape2)/(Gamma(shape1+shape2)
;
;     pdf = B(shape1,shape2)^-1 * x^(shape1 - 1)*(1-x)^(shape2 - 1)
;
;     cdf: The Incomplete Beta Function
;
;     Random Values calculated with thanks to the guys who wrote Gamma
;
;;;**********************************************************************
;;;
;;;
;;; Initialize-instance :after sets up the B(shape1,shape2) for each instance
;;;

(defmethod initialize-instance :after ((distribution beta-dist) &key)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline log-gamma))
  (with-CL-functions (+ * / - exp expt sqrt > < =)
    (let ((a (shape1-of distribution))
          (b (shape2-of distribution)))
      (cond
       ((and (> a 0) (> b 0))
        (<- (1/beta distribution)
            ;;; (/ 1.0 (beta a b))
            (exp (- (log-gamma (+ a b))
                    (log-gamma a)
                    (log-gamma b))))
        (<-  (gamma1-of distribution)
             (make-instance 'gamma-dist :shape a))
        (<-  (gamma2-of distribution)
             (make-instance 'gamma-dist :shape b))
        )
       (T 
        
        (quail-error "shape1 = ~s and shape2 = ~s must be >0."
                     a
                     b))))
    ))


(defmethod cdf-at ((distribution beta-dist) x)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline incomplete-beta))
  (with-CL-functions (+ * / - exp expt sqrt > < =)
    (cond ((<= x 0) 0)
          ((>= x 1) 1)
          (T (incomplete-beta (shape1-of distribution) (shape2-of distribution) x))))
  )

;;; Pdf-at  Calculated Numerically
;;;

(defmethod pdf-at ((distribution beta-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (+ * / - exp expt sqrt > < =)
    (cond ((= 1.0 (shape1-of distribution) (shape2-of distribution))
           1.0)
          ((or (<= x 0) (>= x 1)) 0)
          (T (* (1/beta distribution)
                (expt x (- (shape1-of distribution) 1))
                (expt (- 1 x) (- (shape2-of distribution) 1)))))))
;;;
;;; Random-Value:  We all know that Gamma(shape1)/(Gamma(shape2)+Gamma(shape1))
;;; is distributed Beta(shape1,scale)... Soo Thats how we calculate are random values
;;; Notice if shape1 = scale then our random value from gamma1 = gamma2...
;;; this gives us a random value of 1/2 from the beta every time... Not too random
;;; thus the cond.
;;;

(defmethod random-value ((distribution beta-dist) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline incomplete-gamma))
  (with-CL-functions (+ * / - exp expt sqrt > < = /=)
    (let ((g1 (gamma1-of distribution))
          (g2 (gamma2-of distribution)))
      (if (> n 1)
        (array
         (loop for i from 1 to n
               collect
               (let ((u (random-value g1))
                     (v (random-value g2)))
                 (/ u (+ u v)))))
        (let ((u (random-value g1))
              (v (random-value g2)))
          (/ u (+ u v)))
        )
      )
    ))


(defmethod quantile-at ((distribution beta-dist) (p number) &key (start NIL))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline illinois))
  (with-CL-functions (+ * / - exp expt sqrt > < = /=)
    (let ((alpha (shape1-of distribution))
          (beta (shape2-of distribution))
          (step (/ (- (upper-bound-of distribution)
                      (lower-bound-of distribution))
                   10.0))
          bracket-point)
      (unless (numberp start)
        (let* ((approx-normal (quantile-gaussian p))
               (temp1 (/ 1.0 9.0 beta))
               (temp2 (+ 1.0 (- temp1) (* approx-normal (sqrt temp1))))
               (approx-chi (* 2.0 beta (expt temp2 3)))
               (temp3 (+ (* 4.0 alpha) (* 2.0 beta) -2.0)))
          (cond
           ((minusp approx-chi)
            (setf start (- 1.0
                           (expt (* beta (- 1.0 p)
                                    (beta alpha beta))
                                 (/ 1.0 beta)))))
           ((<= temp3 approx-chi)
            (setf start (expt (* alpha p (beta alpha beta))
                              (/ 1.0 beta))))
           (T (setf start (/ (- temp3 approx-chi) (+ temp3 approx-chi)))))))
      (flet ((G-fun (x) (- (cdf-at distribution x) p)))
        (cond
         ((minusp (G-fun start))
          (setf bracket-point (+ start step))
          (loop for i from 0.0 by step
                until (plusp (G-fun bracket-point))
                do (incf bracket-point step))
          (illinois #'G-fun start bracket-point))
         ((plusp (G-fun start))
          (setf bracket-point (- start step))
          (loop for i from 0.0 by step
                until (minusp (G-fun bracket-point))
                do (decf bracket-point step))
          (illinois #'G-fun bracket-point start))
         (T start))
        ))))
            


(defvar *beta-dist*
  NIL
  "An instance of a beta-dist representing the beta-dist distribution ~
   with shape parameters shape1 and shape2. ~
   This instance is used by the standard beta-dist functions random-beta, ~
   quantile-beta, etc.")

(defun density-beta (x &key (a 1) (b 1))
  "Returns the value of a beta-dist density at x. ~
   (:required ~
   (:arg x The point at which to evaluate the density.)) ~
   (:key ~
   (:arg a 1  The first shape parameter of the beta-dist distribution.) ~
   (:arg b 1  The second shape parameter of the beta-dist distribution.))"
  (declare (special *beta-dist*))
  (let ((saved-a (shape1-of *beta-dist*))
        (saved-b (shape2-of *beta-dist*))
        result)
    (setf (shape1-of *beta-dist*) a)
    (setf (shape2-of *beta-dist*) b)
    (setf result (pdf-at *beta-dist* x))
    (setf (shape1-of *beta-dist*) saved-a)
    (setf (shape2-of *beta-dist*) saved-b)
    result
    )
  )

(defun quantile-beta (p &key (a 1) (b 1))
  "Returns the value of a beta-dist quantile at p. ~
   (:required ~
   (:arg p The probability at which to evaluate the quantile.)) ~
   (:key ~
   (:arg a 1  The first shape parameter of the beta-dist distribution.) ~
   (:arg b 1  The second shape parameter of the beta-dist distribution.))"
  (declare (special *beta-dist*))
  (let ((saved-a (shape1-of *beta-dist*))
        (saved-b (shape2-of *beta-dist*))
        result)
    (setf (shape1-of *beta-dist*) a)
    (setf (shape2-of *beta-dist*) b)
    (setf result (quantile-at *beta-dist* p))
    (setf (shape1-of *beta-dist*) saved-a)
    (setf (shape2-of *beta-dist*) saved-b)
    result
    )
  )

(defun random-beta (&key (n 1) (a 1) (b 1))
  "Returns n pseudo-random values from the beta-dist with shape ~
   parameters a and b. ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg a 1  The first shape parameter of the beta-dist distribution.) ~
   (:arg b 1  The second shape parameter of the beta-dist distribution.))
   (:returns A vector of n pseudo-random values.)"
  (declare (special *beta-dist*))
  (let ((saved-a (shape1-of *beta-dist*))
        (saved-b (shape2-of *beta-dist*))
        result)
    (setf (shape1-of *beta-dist*) a)
    (setf (shape2-of *beta-dist*) b)
    (setf result (random-value *beta-dist* n))
    (setf (shape1-of *beta-dist*) saved-a)
    (setf (shape2-of *beta-dist*) saved-b)
    result
    )
  )

(defun dist-beta (x &key (a 1)(b 1))
  "Calculates and returns the value of the beta distribution function ~
   at x. (i.e. Prob(X <= x) .)  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg a 1  The first shape parameter of the beta-dist distribution.) ~
   (:arg b 1  The second shape parameter of the beta-dist distribution.)) "
  (declare (special *beta-dist*))
  (let ((saved-a (shape1-of *beta-dist*))
        (saved-b (shape2-of *beta-dist*))
        result)
    (setf (shape1-of *beta-dist*) a)
    (setf (shape2-of *beta-dist*) b)
    (setf result (cdf-at *beta-dist* x))
    (setf (shape1-of *beta-dist*) saved-a)
    (setf (shape2-of *beta-dist*) saved-b)
    result
    )
  )
