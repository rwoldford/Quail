;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               uniform.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; Stat 440/840:
;;;
(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(uniform density-uniform  quantile-uniform
          random-uniform dist-uniform)))

(defclass uniform (beta-dist)
  ((shape1 :reader shape1-of
             :initform 1
             :allocation :class)
   (shape2    :reader shape2-of
             :initform 1
             :allocation :class)
   ;;(scale    :reader scale-of
   ;;          :initform 1
   ;;          :allocation :class)
   (generator :accessor generator-of
              :initform *default-random-number-generator*
              :initarg :generator)
 )
   (:documentation "The Uniform distribution."))

(defmethod (setf lower-bound-of)
           (new-value (self uniform))
  "Resetting the lower bound is not allowed."
  (if (< new-value (upper-bound-of self))
    (setf (slot-value self 'lower-bound) new-value)
    (quail-error "Error: lower-bound-of a uniform distribution must be less ~
                  than its upper-bound -- New-value = ~s, ~
                  current upper-bound = ~s." new-value (upper-bound-of self))
    )
  )

(defmethod (setf upper-bound-of)
           (new-value (self uniform))
  "Resetting the lower bound is not allowed."
  (if (> new-value (lower-bound-of self))
    (setf (slot-value self 'upper-bound) new-value)
    (quail-error "Error: upper-bound-of a uniform distribution must be greater ~
                  than its lower-bound -- New-value = ~s, ~
                  current lower-bound = ~s." new-value (lower-bound-of self))
    )
  )


(defmethod cdf-at ((distribution uniform) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (> < / - )
    (let ((lower (eref (lower-bound-of distribution) 0))
          (upper (eref (upper-bound-of distribution) 0)))
      (cond (( < x (lower-bound-of distribution)) 0)
            (( > x upper) 1)
            ((/ (- x lower)
                (- upper lower)))
            )
      )))



(defmethod pdf-at ((distribution uniform) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (> < / - )
    (let ((lower (eref (lower-bound-of distribution) 0))
          (upper (eref (upper-bound-of distribution) 0)))
      (if (and ( > x lower)
               ( < x upper))
        (/ 1 (- upper lower))
        0))))


(defmethod random-value ((distribution uniform) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (let ((lower (eref (lower-bound-of distribution) 0))
        (upper (eref (upper-bound-of distribution) 0)))
    (float
     (+ lower
        (* (- upper lower)
           (/ (rand (generator-of distribution) n)
              (max-pseudo-rand-of (generator-of distribution))))))))


(defmethod quantile-at ((distribution uniform)
                        (p number)
                        &key (start NIL))
  (declare (ignore start)(optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (> < / - + *)
    (let ((lower (eref (lower-bound-of distribution) 0))
          (upper (eref (upper-bound-of distribution) 0)))
      (let ((a lower)
            (b upper))
        (+ a (* p (- b a)))))))




(defvar *uniform*
  NIL
  "An instance of a uniform representing the continuous uniform distribution. ~
   This instance is used by the standard uniform functions random-uniform, ~
   quantile-uniform, etc.")

(defun density-uniform (x &key (from 0.0) (to 1.0))
  "Returns the value of a location scale uniform density at x. ~
   Location 0 and scale 1 define the standard uniform distribution.  ~
   (:required ~
     (:arg x The point at which to evaluate the density.)) ~
   (:key ~
   (:arg from 0.0 The lower bound of the uniform density.) ~
   (:arg to 1.0  The upper bound of the uniform density.))"
  (declare (special *uniform*))
  (if (> to from)
    (let ((saved-l (lower-bound-of *uniform*))
          (saved-u (upper-bound-of *uniform*))
          result)
      (setf (lower-bound-of *uniform*) -infinity)
      (setf (upper-bound-of *uniform*) +infinity)
      (setf (lower-bound-of *uniform*) from)
      (setf (upper-bound-of *uniform*) to)
      (setf result (pdf-at *uniform* x))
      (setf (lower-bound-of *uniform*) -infinity)
      (setf (upper-bound-of *uniform*) +infinity)
      (setf (lower-bound-of *uniform*) saved-l)
      (setf (upper-bound-of *uniform*) saved-u)
      result
      )
    (quail-error "DENSITY-UNIFORM: from = ~s must be less than to = ~s"
                 from to)
    )
  )

(defun quantile-uniform (p &key (from 0.0) (to 1.0))
  "Returns the value of a uniform quantile at p. ~
   (:required ~
   (:arg p The probability at which to evaluate the quantile.)) ~
   (:key ~
   (:arg from 0.0 The lower bound of the uniform density.) ~
   (:arg to 1.0  The upper bound of the uniform density.))"
  (declare (special *uniform*))
  (if (> to from)
    (let ((saved-l (lower-bound-of *uniform*))
          (saved-u (upper-bound-of *uniform*))
          result)
      (setf (lower-bound-of *uniform*) -infinity)
      (setf (upper-bound-of *uniform*) +infinity)
      (setf (lower-bound-of *uniform*) from)
      (setf (upper-bound-of *uniform*) to)
      (setf result (quantile-at *uniform* p))
      (setf (lower-bound-of *uniform*) -infinity)
      (setf (upper-bound-of *uniform*) +infinity)
      (setf (lower-bound-of *uniform*) saved-l)
      (setf (upper-bound-of *uniform*) saved-u)
      result)
    (quail-error "QUANTILE-UNIFORM: from = ~s must be less than to = ~s"
                 from to)
    )
  )

(defun random-uniform (&key (n 1) (from 0.0) (to 1.0))
  "Returns n pseudo-random values from the uniform. ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg from 0.0 The lower bound of the uniform density.) ~
   (:arg to 1.0  The upper bound of the uniform density.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *uniform*))
  (if (> to from)
    (let ((saved-l (lower-bound-of *uniform*))
          (saved-u (upper-bound-of *uniform*))
          result)
      (setf (lower-bound-of *uniform*) -infinity)
      (setf (upper-bound-of *uniform*) +infinity)
      (setf (lower-bound-of *uniform*) from)
      (setf (upper-bound-of *uniform*) to)
      (setf result (random-value *uniform* n))
      (setf (lower-bound-of *uniform*) -infinity)
      (setf (upper-bound-of *uniform*) +infinity)
      (setf (lower-bound-of *uniform*) saved-l)
      (setf (upper-bound-of *uniform*) saved-u)
      result
      )
    (quail-error "DENSITY-UNIFORM: from = ~s must be less than to = ~s"
                 from to)
    )
  )

(defun dist-uniform (x &key (from 0.0) (to 1.0))
  "Calculates and returns the value of the specified uniform distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   (:required ~
     (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg from 0.0 The lower bound of the uniform density.) ~
   (:arg to 1.0  The upper bound of the uniform density.))"
  (declare (special *uniform*))
  (if (> to from)
    (let ((saved-l (lower-bound-of *uniform*))
          (saved-u (upper-bound-of *uniform*))
          result)
      (setf (lower-bound-of *uniform*) -infinity)
      (setf (upper-bound-of *uniform*) +infinity)
      (setf (lower-bound-of *uniform*) from)
      (setf (upper-bound-of *uniform*) to)
      (setf result (cdf-at *uniform* x))
      (setf (lower-bound-of *uniform*) -infinity)
      (setf (upper-bound-of *uniform*) +infinity)
      (setf (lower-bound-of *uniform*) saved-l)
      (setf (upper-bound-of *uniform*) saved-u)
      result
      )
    (quail-error "DIST-UNIFORM: from = ~s must be less than to = ~s"
                 from to)
    )
  )
