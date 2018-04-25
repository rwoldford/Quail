;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               weibull.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;      R.W. Oldford 1993
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(weibull shape-of density-weibull dist-weibull random-weibull
          quantile-weibull)))

(defclass weibull (continuous-dist)
  ((shape :initform 1.0 :initarg :shape :reader shape-of)
   (lower-bound :initform 0.0 :reader lower-bound-of)
   (upper-bound :initform infinity :reader upper-bound-of)
   )
  (:documentation
   "The weibull distribution with a shape parameter."))

(defmethod (setf shape-of) (new-value (dist weibull))
  (if (> new-value 0)
    (setf (slot-value dist 'shape) new-value)
    (quail-error "~&The shape parameter must be positive. ~
                  Not ~s." new-value)))

(defmethod pdf-at ((dist weibull) (value number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions ( = - exp * expt)
    (if (= value 0.0)
      0.0
      (let ((shape (eref (shape-of dist) 0)))
        (* shape (expt value (- shape 1.0)) 
           (exp (* -1.0 (expt value shape))))))))

(defmethod cdf-at ((dist weibull) (value number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions ( = - exp * expt)
    (- 1.0 (exp (- (expt value (eref (shape-of dist) 0)))))))

(defmethod quantile-at ((dist weibull) (value number)
                        &key start)
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions ( = - exp * expt)
    (let ((shape (eref (shape-of dist) 0)))
      (expt (- (log (- 1.0 value)))
            (/ 1.0 shape)))))


(defvar *weibull*
  NIL
  "An instance of a weibull representing the weibull distribution. ~
   This instance is used by the standard weibull functions random-weibull, ~
   quantile-weibull, etc.")

(defun density-weibull (x &key (shape 1))
  "Returns the value of a  weibull probability at x. ~
   (:required ~
   (:arg x The point at which to evaluate the density, or probability.)) ~
   (:key ~
   (:arg shape 1 The shape parameter.))"
  (declare (special *weibull*))
  (let ((saved-shape (shape-of *weibull*))
        result)
    (setf (shape-of *weibull*) shape)
    (setf result (pdf-at *weibull* x))
    (setf (shape-of *weibull*) saved-shape)
    result
    )
  )

(defun quantile-weibull (prop &key (shape 1))
  
  "Returns the value of the prop'th weibull quantile. ~
   (:required ~
   (:arg prop The cumulative proportion at which to evaluate the quantile.)) ~
   (:key ~
   (:arg shape 1 The shape parameter.))"
  (declare (special *weibull*))
  (let ((saved-shape (shape-of *weibull*))
        result)
    (setf (shape-of *weibull*) shape)
    (setf result (quantile-at *weibull* prop))
    (setf (shape-of *weibull*) saved-shape)
    result))

(defun random-weibull (&key (n 1) (shape 1))
  "Returns n pseudo-random values from the weibull. ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg shape 1 The shape parameter.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *weibull*))
  (let ((saved-shape (shape-of *weibull*))
        result)
    (setf (shape-of *weibull*) shape)
    (setf result (random-value *weibull* n))
    (setf (shape-of *weibull*) saved-shape)
    result)
  )

(defun dist-weibull (x &key  (shape 1))
  "Calculates and returns the value of the specified weibull distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg shape 1 The shape parameter.))"
  (declare (special *weibull*))
  (let ((saved-shape (shape-of *weibull*))
        result)
    (setf (shape-of *weibull*) shape)
    (setf result (cdf-at *weibull* x))
    (setf (shape-of *weibull*) saved-shape)
    result))
