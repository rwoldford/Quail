;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               pareto.lisp                              
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(pareto shape-of density-pareto dist-pareto random-pareto
          quantile-pareto)))

(defclass pareto (continuous-dist)
  ((shape :initform 1.0 :initarg :shape :reader shape-of)
   (lower-bound :initform 0.0 :reader lower-bound-of)
   (upper-bound :initform infinity :reader upper-bound-of)
   )
  (:documentation
   "The pareto distribution parameterized by a single shape parameter."))

(defmethod (setf shape-of) (new-value (dist pareto))
  (if (> new-value 0)
    (setf (slot-value dist 'shape) new-value)
    (quail-error "~&The shape parameter must be positive. ~
                  Not ~s." new-value)))

(defmethod pdf-at ((dist pareto) (value number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - expt)
    (let ((shape (eref (shape-of dist) 0)))
      (* shape (expt (+ 1 value)
                     (- (+ 1 shape)))))))

(defmethod cdf-at ((dist pareto) (value number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - expt)
    (let* ((shape (eref (shape-of dist) 0))
           (power (expt (+ 1 value) shape)))
      (/ (- power 1.0 ) power))))

(defmethod quantile-at ((dist pareto) (value number) &key start)
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - expt)
    (let* ((shape (eref (shape-of dist) 0)))
      (- (expt  (- 1 value)  (/ -1.0 shape)) 1.0))))



(defvar *pareto*
  NIL
  "An instance of a pareto representing the pareto distribution. ~
   This instance is used by the standard pareto functions random-pareto, ~
   quantile-pareto, etc.")

(defun density-pareto (x &key (shape 1))
  "Returns the value of a  pareto probability at x. ~
   (:required ~
   (:arg x The point at which to evaluate the density, or probability.)) ~
   (:key ~
   (:arg shape 1 The shape parameter.))"
  (declare (special *pareto*))
  (let ((saved-shape (shape-of *pareto*))
        result)
    (setf (shape-of *pareto*) shape)
    (setf result (pdf-at *pareto* x))
    (setf (shape-of *pareto*) saved-shape)
    result
    )
  )

(defun quantile-pareto (prop &key (shape 1))
  
  "Returns the value of the prop'th pareto quantile. ~
   (:required ~
   (:arg prop The cumulative proportion at which to evaluate the quantile.)) ~
   (:key ~
   (:arg shape 1 The shape parameter.))"
  (declare (special *pareto*))
  (let ((saved-shape (shape-of *pareto*))
        result)
    (setf (shape-of *pareto*) shape)
    (setf result (quantile-at *pareto* prop))
    (setf (shape-of *pareto*) saved-shape)
    result))

(defun random-pareto (&key (n 1) (shape 1))
  "Returns n pseudo-random values from the pareto. ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg shape 1 The shape parameter.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *pareto*))
  (let ((saved-shape (shape-of *pareto*))
        result)
    (setf (shape-of *pareto*) shape)
    (setf result (random-value *pareto* n))
    (setf (shape-of *pareto*) saved-shape)
    result)
  )

(defun dist-pareto (x &key  (shape 1))
  "Calculates and returns the value of the specified pareto distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg shape 1 The shape parameter.))"
  (declare (special *pareto*))
  (let ((saved-shape (shape-of *pareto*))
        result)
    (setf (shape-of *pareto*) shape)
    (setf result (cdf-at *pareto* x))
    (setf (shape-of *pareto*) saved-shape)
    result))

