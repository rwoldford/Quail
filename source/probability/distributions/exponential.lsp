;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               exponential.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;     R.W. Oldford 1993.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 
;;; Fall 1992
;;; written by Colin and Dan 
;;; for R. W. Oldford

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (export '(exponential-dist density-exponential quantile-exponential
          random-exponential dist-exponential)))

(defclass exponential-dist (gamma-dist)
  ((shape    :reader shape-of
             :allocation :class
             :initform 1.0
             :documentation "The shape parameter of an exponential is 1.0")
   (upper-bound :reader upper-bound-of
                :allocation :class
                :initform +infinity)
   )
  (:documentation "The location-scale exponential distribution.")
  )

(defmethod (setf shape-of) (new-value (who exponential-dist))
  (declare (ignorable new-value who)) ;(declare (ignore new-value who)) ; 31JUL2023
  (quail-error "Sorry, can't change the shape parameter for the exponential.  ~
                Instead, consider changing its class to gamma first.")
  )



;  EXPONENTIAL DISTRIBUTION
;    This set of methods defines the probability density function (pdf), cumulative
;    density function (cdf), the quantile function, and a random value for the 
;    exponential distribution. 

;;;
;;;  The mathematical form of the location scale exponential distribution with 
;;;  location u, scale s and shape parameter a is as follows:
;;;
;;;
;;;  pdf at x:
;;;            for x > u
;;;
;;;                 1         - ((x - u) / s)
;;;                ---  *   e
;;;                 s   
;;;     
;;;            0 otherwise
;;;
;;; 
;;;  cdf at x:   
;;;            for x > u
;;;                        - ((x - u) / s)     
;;;                 1  -  e  
;;;     
;;;            0 otherwise
;;;   
;;;  random value :    u - s * ln ( uniform )
;;;
;;;  quantile at p :   u - s * ln ( 1 - p )

(defmethod pdf-at ((distribution exponential-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - exp expt sqrt > < = /= <=)
    (let ((sigma (scale-of distribution)))
      (/ (exp (/ (- (location-of distribution) x) sigma))
         sigma))))

               

               
 
;*************************

(defmethod cdf-at ((distribution exponential-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - exp)
    (- 1 (exp (/ (- (location-of distribution) x) (scale-of distribution))))))


;***************************

(defmethod random-value ((distribution exponential-dist) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let ((loc (eref (location-of distribution) 0))
        (s   (eref (scale-of distribution) 0)))
    (if (= n 1)
      (with-CL-functions (+ * / - exp log)
        (- loc (* s (log (random-uniform))))
        )
      (array
       (loop for i from 1 to n
             collect
             (with-CL-functions (+ * / - exp log)
               (- loc (* s (log (random-uniform))))
               )
             )))))


;***************************

(defmethod quantile-at ((distribution exponential-dist)
                        (p number)
                        &key (start NIL))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let ((loc (eref (location-of distribution) 0))
        (s   (eref (scale-of distribution) 0)))
    (with-CL-functions (+ * / - exp log)
      (- loc (* s (log (- 1 p))))
      )
    ))

(defvar *exponential-dist*
  NIL
  "An instance of a exponential-dist representing the exponential distribution. ~
   This instance is used by the standard exponential functions random-exponential, ~
   quantile-exponential, etc.")

(defun density-exponential (x &key (location 0.0) (scale 1.0))
  "Returns the value of a location scale exponential density at x. ~
   Location 0 and scale 1 define the standard exponential distribution.  ~
   (:required ~
     (:arg x The point at which to evaluate the density.)) ~
   (:key ~
   (:arg location 0.0 The location of the exponential density.  The exponential has ~
                      positive support from the location to +infinity.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.))"
  (declare (special *exponential-dist*))
  (let ((saved-l (location-of *exponential-dist*))
        (saved-s (scale-of *exponential-dist*))
        result)
    (setf (location-of *exponential-dist*) location)
    (setf (scale-of *exponential-dist*) scale)
    (setf result (pdf-at *exponential-dist* x))
    (setf (location-of *exponential-dist*) saved-l)
    (setf (scale-of *exponential-dist*) saved-s)
    result
    )
  )

(defun quantile-exponential (p &key (location 0.0) (scale 1.0))
  
  "Returns the value of a location scale exponential quantile at p. ~
   Location 0 and scale 1 define the standard exponential distribution.  ~
   (:required ~
     (:arg p The probability at which to evaluate the quantile.)) ~
   (:key ~
   (:arg location 0.0 The location of the exponential density.  The exponential has ~
                      positive support from the location to +infinity.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.))"
  (declare (special *exponential-dist*))
  (let ((saved-l (location-of *exponential-dist*))
        (saved-s (scale-of *exponential-dist*))
        result)
    (setf (location-of *exponential-dist*) location)
    (setf (scale-of *exponential-dist*) scale)
    (setf result (quantile-at *exponential-dist* p))
    (setf (location-of *exponential-dist*) saved-l)
    (setf (scale-of *exponential-dist*) saved-s)
    result)
  )

(defun random-exponential (&key (n 1) (location 0.0) (scale 1.0))
  "Returns n pseudo-random values from the location scale exponential. ~
   Location 0 and scale 1 define the standard exponential distribution.  ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg location 0.0 The location of the exponential density.  The exponential has ~
                      positive support from the location to +infinity.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *exponential-dist*))
  (let ((saved-l (location-of *exponential-dist*))
        (saved-s (scale-of *exponential-dist*))
        result)
    (setf (location-of *exponential-dist*) location)
    (setf (scale-of *exponential-dist*) scale)
    (setf result (random-value *exponential-dist* n))
    (setf (location-of *exponential-dist*) saved-l)
    (setf (scale-of *exponential-dist*) saved-s)
    result)
  )

(defun dist-exponential (x &key (location 0.0) (scale 1.0))
  "Calculates and returns the value of the specified exponential distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   Location 0 and scale 1 define the standard exponential distribution.  ~
   (:required ~
     (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg location 0.0 The location of the exponential density.  The exponential has ~
                      positive support from the location to +infinity.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.)) "
  (declare (special *exponential-dist*))
  (let ((saved-l (location-of *exponential-dist*))
        (saved-s (scale-of *exponential-dist*))
        result)
    (setf (location-of *exponential-dist*) location)
    (setf (scale-of *exponential-dist*) scale)
    (setf result (cdf-at *exponential-dist* x))
    (setf (location-of *exponential-dist*) saved-l)
    (setf (scale-of *exponential-dist*) saved-s)
    result)
  )
