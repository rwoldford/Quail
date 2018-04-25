;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              chi-squared.lisp                              
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
;;; written by Colin and Dan 
;;; for R. W. Oldford

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) 
   (export '(chi-squared df-of density-chi  quantile-chi
          random-chi dist-chi)))

(defclass chi-squared (gamma-dist)
  ((df :reader df-of
       :initarg :df
       :initform 1)
   (scale :reader scale-of
          :initform 2.0
          :allocation :class))
  (:documentation "The Chi-squared distribution, with degrees of freedom = df")
  )

(defmethod shape-of ((dist chi-squared))
  (/ (df-of dist) 2))

(defmethod (setf df-of) (new-value (dist chi-squared))
  (cond
   ((> new-value 0)
    (setf (slot-value dist 'shape) (/ new-value 2))
    (setf (slot-value dist 'df) new-value)
    new-value)
   (T (quail-error "~&Degrees of Freedom must be greater than zero, not = ~s"
                   new-value))))

(defmethod (setf shape-of) (new-value (dist chi-squared))
  (call-next-method)
  (setf (df-of dist) (* new-value 2)))

(defmethod location-of ((dist chi-squared))
  0)

(defmethod (setf lower-bound-of) (new-value (dist chi-squared))
  (declare (ignore dist))
  (quail-error "~&(SETF LOWER-BOUND-OF):~&Chi-squared distributions have lower bound 0; ~
                this cannot be changed to ~s."
               new-value))

(defmethod (setf location-of) (new-value (dist chi-squared))
  (declare (ignore dist))
  (quail-error "~&(SETF LOCATION-OF):~&Chi-squared distributions have location 0; ~
                this cannot be changed to ~s."
               new-value))

(defmethod (setf scale-of) (new-value (dist chi-squared))
  (declare (ignore dist))
  (quail-error "~&(SETF SCALE-OF):~&Chi-squared distributions are gammas with scale = 2.0; ~
                this cannot be changed to ~s."
               new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;  CHI-SQUARE DISTRIBUTION
;    This set of methods defines the probability density function (pdf), cumulative
;    density function (cdf), the quantile function, and a random value for the 
;    chi-square distribution with parameter degrees of freedom (V).  The methods
;    use the following closed forms and techniques:

;    pdf at x:                     1                  ( v /2 - 1 )   - ( x / 2 )
;                  ------------------------------ * x              e
;                       ( v / 2 )
;                      2         * gamma ( v / 2)


;    cdf at x :    INSTANCE OF GAMMA WITH:  shape = V / 2     
;                                           SCALE = 2
;                     
;
;    random value :  For V integer, even,  and < 21 :  Sum of V   Exponentials (mean=1)     
;                    For V integer, odd,   and < 21 :  Sum of V/2 Exponentials (mean=1)
;                                                      + (gaussian (0,1)) **2
;                    For V non-integer : instance of gamma as above

;    quantile at p : INSTANCE OF GAMMA WITH:  shape = V / 2     
;                                             SCALE = 2



;***************************

(defmethod pdf-at ((distribution chi-squared) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline log-gamma))
  (with-CL-functions (+ * / - exp expt sqrt > < = /= )
    (/ (* (expt x (- (/ (df-of distribution) 2) 1))
          (exp (- (/ x 2))))
       (* (expt 2 (/ (df-of distribution) 2))
          (exp (log-gamma (/ (df-of distribution) 2)))))
    ))


;***************************
;;; cdf-at handled by gamma


;***************************

(defmethod random-value ((distribution chi-squared) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (let ((w 1))
    (cond ((> (df-of distribution) 20)
           (call-next-method))
          ((evenp (df-of distribution))
           ; (loop for i from 1 to (/ (df-of distribution) 2) do
           (do ((i 1 (incf i)))
               ((> i (/ (df-of distribution) 2)))
             (setf w (* w (random-uniform :n n))))
           (* -2 (log w)))
          ((oddp (df-of distribution))
           ; (loop for i from 1 to (/ (- (df-of distribution) 1) 2) do
           (do ((i 1 (incf i)))
               ((> i (/ (- (df-of distribution) 1) 2)))
             (setf w (* w (random-uniform :n n))))
           (- (expt (random-gaussian :n n) 2)
              (* 2 (log w)))))))
      

;***************************
;;
;;; quantile-at handled by gamma


    
    
(defvar *chi-squared*
  NIL
  "An instance of a chi-squared representing the chi-squared distribution ~
   with df degrees of freedom. ~
  ;; Location 0 and scale 1 define the standard chi-squared distribution.  ~
   This instance is used by the standard chi-squared functions random-chi, ~
   quantile-chi, etc.")

(defun density-chi (x &key (df 1))
  "Returns the value of a chi-squared density at x. ~
   (:required ~
   (:arg x The point at which to evaluate the density.)) ~
   (:key ~
   (:arg df 1  The degrees of freedom. ~
   This is the shape parameter of the chi-squared distribution.  ~
   The chi-squared is ~
   skewed to the right.  The smaller the degrees of fredom, ~
   the greater is this skewness.))"
  (declare (special *chi-squared*))
  (let ((saved-df (df-of *chi-squared*))
        result)
    (setf (df-of *chi-squared*) df)
    (setf result (pdf-at *chi-squared* x))
    (setf (df-of *chi-squared*) saved-df)
    result
    )
  )

(defun quantile-chi (p &key (df 1))
  
  "Returns the value of a chi-squared quantile at p with df degrees of freedom. ~
   (:required ~
   (:arg p The probability at which to evaluate the quantile.)) ~
   (:key ~
   (:arg df 1  The degrees of freedom. ~
   This is the shape parameter of the chi-squared distribution.  ~
   The chi-squared is ~
   skewed to the right.  The smaller the shape parameter df, ~
   the greater is this skewness.) ~
   ;;(:arg location 0.0 The location of the chi-squared density.  The chi-squared has ~
   ;;                   positive support from the location to +infinity.) ~
   ;;(:arg scale 1.0  A scale parameter to adjust the spread of the density.)
   ) ~
   (:references Algorithm AS 91 ... Applied Statistics Algorithms - 1985; by D.J. Best and D.E.~
   Roberts.)"
  (declare (special *chi-squared*))
  (let (;;(saved-l (location-of *chi-squared*))
        ;;(saved-s (scale-of *chi-squared*))
        (saved-df (df-of *chi-squared*))
        result)
    ;;(setf (location-of *chi-squared*) location)
    ;;(setf (scale-of *chi-squared*) scale)
    (setf (df-of *chi-squared*) df)
    (setf result (quantile-at *chi-squared* p))
    ;;(setf (location-of *chi-squared*) saved-l)
    ;;(setf (scale-of *chi-squared*) saved-s)
    (setf (df-of *chi-squared*) saved-df)
    result)
  )

(defun random-chi (&key (n 1) (df 1))
  "Returns n pseudo-random values from the chi-squared on df degrees of freedom. ~
   ;;Location 0 and scale 1 define the standard chi-squared distribution.  ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg df 1  The degrees of freedom. ~
   This is the shape parameter of the chi-squared distribution.  ~
   The chi-squared is ~
   skewed to the right.  The smaller the shape parameter df, ~
   the greater is this skewness.) ~
   ;;(:arg location 0.0 The location of the chi-squared density.  The chi-squared has ~
   ;;                   positive support from the location to +infinity.) ~
   ;;(:arg scale 1.0  A scale parameter to adjust the spread of the density.)
   ) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *chi-squared*))
  (let (;;(saved-l (location-of *chi-squared*))
        ;;(saved-s (scale-of *chi-squared*))
        (saved-df (df-of *chi-squared*))
        result)
    ;;(setf (location-of *chi-squared*) location)
    ;;(setf (scale-of *chi-squared*) scale)
    (setf (df-of *chi-squared*) df)
    (setf result (random-value *chi-squared* n))
    ;;(setf (location-of *chi-squared*) saved-l)
    ;;(setf (scale-of *chi-squared*) saved-s)
    (setf (df-of *chi-squared*) saved-df)
    result)
  )

(defun dist-chi (x &key (df 1))
  "Calculates and returns the value of the chi-squared distribution function ~
   on df degrees of freedom at x.  ~
   (i.e. prob(X <= x) .)  ~
   ;; Location 0 and scale 1 define the standard chi-squared distribution.  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg df 1  The degrees of freedom. ~
   This is the shape parameter of the chi-squared distribution.  ~
   The chi-squared is ~
   skewed to the right.  The smaller the shape parameter df, ~
   the greater is this skewness.) ~
   ;;(:arg location 0.0 The location of the chi-squared density.  The chi-squared has ~
   ;;                   positive support from the location to +infinity.) ~
   ;;(:arg scale 1.0  A scale parameter to adjust the spread of the density.)
   ) "
  (declare (special *chi-squared*))
  (let (;;(saved-l (location-of *chi-squared*))
        ;;(saved-s (scale-of *chi-squared*))
        (saved-df (df-of *chi-squared*))
        result)
    ;;(setf (location-of *chi-squared*) location)
    ;;(setf (scale-of *chi-squared*) scale)
    (setf (df-of *chi-squared*) df)
    (setf result (cdf-at *chi-squared* x))
    ;;(setf (location-of *chi-squared*) saved-l)
    ;;(setf (scale-of *chi-squared*) saved-s)
    (setf (df-of *chi-squared*) saved-df)
    result)
  )
