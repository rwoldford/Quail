;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              geometric.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;     Ward Quinlan
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(geometric
          density-geometric  quantile-geometric
          random-geometric dist-geometric)))

(defclass  geometric  (negative-binomial)
  ((lower :accessor lower-of 
          :initarg :lower
          :initform 1
          :allocation :class))
  (:documentation "The geometric distribution")
  )
;;------------------------------------------------------------------
;;  Geometric Distribution (Geometric (p))         
;;                                                
;;  1. Parameter: p = probability of success.
;;  2. Range: x - integer
;;  3. pdf: f(x) = p * (1 - p)^(x - 1)
;;  4. cdf: F(x) = 1 - (1 - p)^x
;;  5. r.v.: rv = ceil( ln u / ln (1 - p) )    where u ~ Unif[0,1] 
;;  6. quantile: F(x) = q
;;               x = ceil( ln q / ln (1 - p))   
;;-----------------------------------------------------------------


(defmethod  pdf-at ((distribution geometric) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (if (integerp x)
    (if (> x 0)
      (* (prob-success distribution)
         (expt (- 1 (prob-success distribution)) (- x 1)))
      0)
    0))

(defmethod (setf number-of-successes) (new-value (dist geometric))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (if (= new-value 1)
    (setf (lower-bound-of dist) new-value)
    (quail-error "Total number of successes for a geometric must be 1, ~
                  not ~s ."
                 new-value)))

(defmethod  cdf-at ((distribution geometric) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / -  expt floor >)
    (let ((p (eref (prob-success distribution) 0))
          (integer-x (floor x)))
      (if (> integer-x 0)
        (- 1 (expt (- 1 p) integer-x))
        0))))

(defmethod  random-value ((distribution geometric) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let ((result
         (ceiling (/  (log (random-uniform :n n)) 
                      (log (- 1 (prob-success distribution)))))))
    result))

(defmethod quantile-at ((distribution geometric) 
                        (q number)
                        &key (start NIL))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / -  log ceiling <)
    (let ((p (eref (prob-success distribution) 0))
          result)
      (if (< q p)
        (setf result 1)
        (setf result
              (ceiling (/  (log (- 1 q))
                     (log (- 1 p))))))
      result)))



(defvar *geometric*
  NIL
  "An instance of geometric representing the geometric ~
   distribution. ~
   This instance is used by the standard functions random-geometric, ~
   quantile-geometric, etc.")

(defun density-geometric (x &key (p 0.5))
  "Returns the value of a  geometric probability at x. ~
   (:required ~
   (:arg x The point at which to evaluate the density, or probability.)) ~
   (:key ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *geometric*))
  (let ((saved-p (prob-success *geometric*))
        result)
    (setf (prob-success *geometric*) p)
    (setf result (pdf-at *geometric* x))
    (setf (prob-success *geometric*) saved-p)
    result
    )
  )

(defun quantile-geometric (prop &key (p 0.5))
  
  "Returns the value of the prop'th geometric quantilep. ~
   (:required ~
   (:arg prop The cumulative proportion at which to evaluate the quantile.)) ~
   (:key ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *geometric*))
  (let ((saved-p (prob-success *geometric*))
        result)
    (setf (prob-success *geometric*) p)
    (setf result (quantile-at *geometric* prop))
    (setf (prob-success *geometric*) saved-p)
    result))

(defun random-geometric (&key (n 1) (p 0.5))
  "Returns n pseudo-random values from the geometric. ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg p 0.5 The probability of success at each trial.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *geometric*))
  (let ((saved-p (prob-success *geometric*))
        result)
    (setf (prob-success *geometric*) p)
    (setf result (random-value *geometric* n))
    (setf (prob-success *geometric*) saved-p)
    result)
  )

(defun dist-geometric (x &key (p 0.5))
  "Calculates and returns the value of the specified geometric distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg p 0.5 The probability of success at each trial.))"
  (declare (special *geometric*))
  (let ((saved-p (prob-success *geometric*))
        result)
    (setf (prob-success *geometric*) p)
    (setf result (cdf-at *geometric* x))
    (setf (prob-success *geometric*) saved-p)
    result))


