;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               K-dist.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1993
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(K-dist df-of density-K  quantile-K random-K dist-K)))

(defclass K-dist (continuous-dist)
  ((df :reader df-of
       :initarg :df))
  (:documentation "The K-dist distribution, with degrees of freedom = df. ~
                   (:elaboration ~
                   A K random-variable is simply a one to one transform of ~
                   a chi-squared random variable.  If x has a chi-squared distribution ~
                   on m degrees of freedom, then y = sqrt(x/m) has a K distribution ~
                   on m degrees of freedom. For gaussian data, the ratio of ~
                   the sample standard deviation to the true standard deviation ~
                   is distributed as a K random-variable.)")
  )

(defmethod initialize-instance :after ((self K-dist) &key)
  (setf (lower-bound-of self) 0.0))

(defmethod (setf df-of) (new-value (dist K-dist))
  (cond
   ((> new-value 0)
    (setf (slot-value dist 'df) new-value)
    new-value)
   (T (quail-error "~&Degrees of Freedom must be greater than zero, not = ~s"
                   new-value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  K DISTRIBUTION
;;;    This set of methods defines the probability density function (pdf), cumulative
;;;    density function (cdf), the quantile function, and a random value for the 
;;;    K distribution with parameter degrees of freedom (m). 
;;;    
;;;    A K random-variable is simply a one to one transform of
;;;    a chi-squared random variable.  If x has a chi-squared distribution
;;;    on m degrees of freedom, then y = sqrt(x/m) has a K distribution
;;;    on m degrees of freedom.
;;;    The methods depend on this close relation.
;;;                       (m/2)
;;;    pdf at x:         m                             (m - 1)   -(myy/2)
;;;                  ------------------------------ * y         e
;;;                       (m/2)-1
;;;                      2         * gamma (m/2)
;;;
;;;
;;;    cdf at x :    from the chi-squared  P(Y <= a) = P(Y^2 <= a^2)
;;;                                                  = P(m*Y^2 <= m*a^2)
;;;                                                  = P(x <= m*a^2)
;;;                     
;;;
;;;    random value :  from the chi-squared y = sqrt(x/m)
;;;
;;;    quantile at p : from the chi-squared  p = P(Y <= a)
;;;                                            = P(x <= ma^2)
;;;                    so if Qx is the p'th quantile of a chi-squared(m),
;;;                    then the p'th quantile "a" of the K(m) distribution is just
;;;                    a = sqrt(Qx/m)



;;;***************************

(defmethod pdf-at ((distribution K-dist) (y number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline log-gamma))
  (with-CL-functions (+ * / - exp log =)
    (if (= y 0.0)
      0.0
      (let* ((m (eref (df-of distribution) 0))
             (m/2 (/ m 2.0D0))
             (constant (- (* m/2 (log m))
                          (* (- m/2 1) (log 2.0D0))
                          (log-gamma m/2))))
        (exp (+ constant
                (* (- m 1.0) (log y))
                (- (* m/2 y y))))
        ))))


;;;***************************

(defmethod cdf-at ((distribution K-dist) (x number))
  "Derived from relation to Chi-squared."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline incomplete-gamma))
  (if (> x 0)
    (with-CL-functions (/ -)
      (let ((m (eref (df-of distribution) 0)))
        (dist-chi (* m  x x) :df m)))
    0))

;;;***************************

(defmethod random-value ((distribution K-dist) &optional (n 1))
  "Derived from relation to Chi-squared."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let* ((m (eref (df-of distribution) 0)))
      (sqrt (/ (random-chi :n n :df m) m ))))
      

;;;***************************
;;;

(defmethod quantile-at ((distribution K-dist)
                        (p number)
                        &key (start NIL))
  "Derived from relation to Chi-squared."
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let ((m (eref (df-of distribution) 0)))
    (sqrt (/ (quantile-chi p :df m) m))))
  


(defvar *K-dist*
  NIL
  "An instance of a K-dist representing the K-dist distribution ~
   with df degrees of freedom. ~
   This instance is used by the standard K-dist functions random-K, ~
   quantile-K, etc.")

(defun density-K (x &key (df 1))
  "Returns the value of a K-dist density at x. ~
   (:required ~
   (:arg x The point at which to evaluate the density.)) ~
   (:key ~
   (:arg df 1  The degrees of freedom. ~
   This is the parameter of the K-dist distribution.  ~
   The K-dist is slightly skewed right.  The smaller the degrees of fredom, ~
   the greater is this skewness.  As degrees of freedom increase the density ~
   becomes more symmetric and converges to the point mass at 1.0))"
  (declare (special *K-dist*))
  (let ((saved-df (df-of *K-dist*))
        result)
    (setf (df-of *K-dist*) df)
    (setf result (pdf-at *K-dist* x))
    (setf (df-of *K-dist*) saved-df)
    result
    )
  )

(defun quantile-K (p &key (df 1))
  
  "Returns the value of a K-dist quantile at p with df degrees of freedom. ~
   (:required ~
   (:arg p The probability at which to evaluate the quantile.)) ~
   (:key ~
   (:arg df 1  The degrees of freedom. ~
   This is the parameter of the K-dist distribution.  ~
   The K-dist is ~
   skewed to the right.  The smaller the parameter df, ~
   the greater is this skewness.  As degrees of freedom increase the density ~
   becomes more symmetric and converges to the point mass at 1.0) ~
   )"
  (declare (special *K-dist*))
  (let ((saved-df (df-of *K-dist*))
        result)
    (setf (df-of *K-dist*) df)
    (setf result (quantile-at *K-dist* p))
    (setf (df-of *K-dist*) saved-df)
    result)
  )

(defun random-K (&key (n 1) (df 1))
  "Returns n pseudo-random values from the K-dist on df degrees of freedom. ~
   ;;Location 0 and scale 1 define the standard K-dist distribution.  ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg df 1  The degrees of freedom. ~
   This is the parameter of the K-dist distribution.  ~
   The K-dist is ~
   skewed to the right.  The smaller the parameter df, ~
   the greater is this skewness.  As degrees of freedom increase the density ~
   becomes more symmetric and converges to the point mass at 1.0) ~
   ) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *K-dist*))
  (let ((saved-df (df-of *K-dist*))
        result)
    (setf (df-of *K-dist*) df)
    (setf result (random-value *K-dist* n))
    (setf (df-of *K-dist*) saved-df)
    result)
  )

(defun dist-K (x &key (df 1))
  "Calculates and returns the value of the K-dist distribution function ~
   on df degrees of freedom at x.  ~
   (i.e. prob(X <= x) .)  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg df 1  The degrees of freedom. ~
   This is the parameter of the K-dist distribution.  ~
   The K-dist is ~
   skewed to the right.  The smaller the parameter df, ~
   the greater is this skewness.  As degrees of freedom increase the density ~
   becomes more symmetric and converges to the point mass at 1.0) ~
   ) "
  (declare (special *K-dist*))
  (let ((saved-df (df-of *K-dist*))
        result)
    (setf (df-of *K-dist*) df)
    (setf result (cdf-at *K-dist* x))
    (setf (df-of *K-dist*) saved-df)
    result)
  )
