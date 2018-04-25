;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            discrete-uniform.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;     Joe Leung (ID# 89115226)
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) 
   (export '(discrete-uniform
          density-discrete-uniform  quantile-discrete-uniform
          random-discrete-uniform dist-discrete-uniform)))

(defclass discrete-uniform (discrete-dist)
  ((lower-bound :reader lower-bound-of
                :initarg :lower-bound
                :initform 0)
   (upper-bound :accessor upper-bound-of
                :initarg :upper-bound
                :initform 10))
  (:documentation "Finite-discrete-uniform.  A finite set of values can be taken by ~
                   the random variable.  Without loss of generality, we take these to ~
                   be the values from lower-bound-of this distribution increasing by ~
                   1 until the upper-bound-of this distribution is attained. ~
                   The set of values include the end-points.")
  )

(defmethod (setf lower-bound-of) (new-value (dist discrete-uniform))
  (unless (and (integerp new-value)
               (< new-value (upper-bound-of dist)))
    (quail-error "Lower bound must be an integer that is less than ~
                  the upper bound (= ~s),  not ~s ."
                 (upper-bound-of dist)
                 new-value))
  (setf (slot-value dist 'lower-bound) new-value))


(defmethod (setf upper-bound-of) (new-value (dist discrete-uniform))
  (unless (and (integerp new-value)
               (> new-value (lower-bound-of dist)))
    (quail-error "Upper bound must be an integer that is greater than ~
                  the lower bound (= ~s),  not ~s ."
                 (lower-bound-of dist)
                 new-value))
  (setf (slot-value dist 'upper-bound) new-value))

(defun set-discrete-uniform-bounds (dist &key (from NIL) (to NIL))
  (when (and from to)
    (let ((intl (truncate from))
          (intu (truncate to)))
      (if (or (> intl intu)
              (/= intl from)
              (/= intu to))
        (quail-error "Upper and lower bounds must be integers such that the ~
                      upper-bound  (= ~s) is greater the lower bound (= ~s)."
                     to
                     from))
      (setf (slot-value dist 'upper-bound) intu)
      (setf (slot-value dist 'lower-bound) intl))))

(defmethod initialize-instance :after ((dist discrete-uniform)
                                       &rest initargs
                                       &key
                                       (lower-bound NIL)
                                       (upper-bound NIL))
  (declare (ignore initargs))
  (cond
   ((and lower-bound upper-bound)
    (let ((intl (truncate lower-bound))
          (intu (truncate upper-bound)))
      (if (or (> intl intu)
              (/= intl lower-bound)
              (/= intu upper-bound))
        (quail-error "Upper and lower bounds must be integers such that the ~
                      upper-bound  (= ~s) is greater the lower bound (= ~s)."
                     upper-bound
                     lower-bound))
      (setf (slot-value dist 'upper-bound) intu)
      (setf (slot-value dist 'lower-bound) intl)))
   (upper-bound
    (let ((intu (truncate upper-bound)))
      (if (/= intu upper-bound)
        (quail-error "Upper and lower bounds must be integers; here ~
                      upper-bound = ~s ."
                     upper-bound))
      (setf (upper-bound-of dist) intu)))
   (lower-bound
    (let ((intl (truncate lower-bound)))
      (if (/= intl lower-bound)
        (quail-error "Upper and lower bounds must be integers; here ~
                      lower-bound = ~s ."
                     lower-bound))
      (setf (lower-bound-of dist) intl)))
   (T NIL)))
        
           
               
;;;-----------------------------------------------------------------------
;;; Finite Discrete Uniform Distribution ( UD(a,b) )
;;;    - This is a discrete version of the uniform distribution. It takes a 
;;;      number of values, each with the same probability. The key characteristics
;;;      of distribution are summarized as following:
;;;    
;;;     1. Parameters: a = lower bound 
;;;                    b = upper bound
;;;                       where a,b must be integers and b > a.
;;;     2. Range: x =  a,a+1,...,b-1,b
;;;     3. pdf: f(x) = 1/(b-a+1)
;;;     4. cdf:         /  0               ,if x < a
;;;              F(x) =<  (x-a+1)/(b-a+1)  ,if a <= x <= b
;;;                     \  1               ,if x > b
;;;     5. mean: (b+a)/2
;;;     6. variance: ((b-a+1)^2 -1 )/12
;;;
;;;
;;;-----------------------------------------------------------------------

;------------------------------------------
;;;  PDF-AT
;------------------------------------------

(defmethod pdf-at ((dist discrete-uniform) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (integerp + * / - exp expt sqrt > < = /= )
    (if (integerp x)
      (if (and (>= x (lower-bound-of dist)) (<= x (upper-bound-of dist)))
        (/ 1 (+ 1 (- (upper-bound-of dist) (lower-bound-of dist))))
        0)
      0)))

;------------------------------------------
;   CDF-AT
;------------------------------------------

(defmethod cdf-at ((dist discrete-uniform) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (+ * / - exp expt sqrt > < = /= )
    (let ((intx (if (< x 0)
                  (ceiling x)
                  (floor x))))
      
      (cond ((< intx (lower-bound-of dist)) 0) 
            ((> intx (upper-bound-of dist)) 1)
            (T    (/ (+ intx (- 1 (lower-bound-of dist))) 
                     (+ 1 (- (upper-bound-of dist) (lower-bound-of dist)))))
            ))
    ))

;------------------------------------------
;   QUANTILE-AT
;------------------------------------------

(defmethod quantile-at ((dist discrete-uniform) (p number) &key start)
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (+ *  - ceiling)
    (+ (ceiling (* (+ 1 (- (upper-bound-of dist)
                           (lower-bound-of dist)))
                   p))
       (- (lower-bound-of dist) 1))
    ))


;------------------------------------------
;   RANDOM-VALUE
;------------------------------------------

(defmethod random-value ((dist discrete-uniform) &optional (n 1))
  (quantile-at dist (random-uniform :n n))
  )



(defvar *discrete-uniform*
  NIL
  "An instance of a discrete-uniform representing the uniform ~
   distribution on a finite number of elements. ~
   The discrete-uniform distribution is characterized here by two ~
   parameters: the lower-bound-of it and the upper-bound-of it.  ~
   Inclusive of the bounds, the set of elements are those running from the ~
   lower-bound and increasing by one until the upper-bound is attained.  ~
   This instance is used by the standard functions random-discrete-uniform, ~
   quantile-discrete-uniform, etc.")

(defun density-discrete-uniform (x &key
                                 (from 0)
                                 (to 9))
  "Returns the value of a  discrete-uniform probability at x. ~
   (:elaboration ~
   The discrete-uniform distribution is characterized here by two ~
   parameters: the lower-bound-of it and the upper-bound-of it.  ~
   Inclusive of the bounds, the set of elements are those running from the ~
   lower-bound and increasing by one until the upper-bound is attained.) ~
   The random quantity is the number of successes observed in the sample.  ~
   (:required ~
   (:arg x The point at which to evaluate the density, or probability.)) ~
   (:key ~
   (:arg from 0 The lower bound of the discrete uniform.) ~
   (:arg to 9  The upper bound of the discrete uniform; this must be larger ~
               than the lower bound.) ~
   )"
  (declare (special *discrete-uniform*))
  (let ((saved-l (lower-bound-of *discrete-uniform*))
        (saved-u (upper-bound-of *discrete-uniform*))
        result)
    (set-discrete-uniform-bounds *discrete-uniform*
                                 :from from :to to)
    (setf result (pdf-at *discrete-uniform* x))
    (set-discrete-uniform-bounds *discrete-uniform*
                                 :from saved-l :to saved-u)
    result
    )
  )

(defun quantile-discrete-uniform (prop &key
                                 (from 0)
                                 (to 9))
  
  "Returns the value of the prop'th discrete-uniform quantile. ~
   (:elaboration ~
   The discrete-uniform distribution is characterized here by two ~
   parameters: the lower-bound-of it and the upper-bound-of it.  ~
   Inclusive of the bounds, the set of elements are those running from the ~
   lower-bound and increasing by one until the upper-bound is attained.) ~
   (:required ~
   (:arg prop The cumulative proportion at which to evaluate the quantile.)) ~
   (:key ~
   (:arg from 0 The lower bound of the discrete uniform.) ~
   (:arg to 9  The upper bound of the discrete uniform; this must be larger ~
               than the lower bound.) ~
   )"
  (declare (special *discrete-uniform*))
  (let ((saved-l (lower-bound-of *discrete-uniform*))
        (saved-u (upper-bound-of *discrete-uniform*))
        result)
    (set-discrete-uniform-bounds *discrete-uniform*
                                 :from from :to to)
    (setf result (quantile-at *discrete-uniform* prop))
    (set-discrete-uniform-bounds *discrete-uniform*
                                 :from saved-l :to saved-u)
    result
    )
  )

(defun random-discrete-uniform (&key (n 1) 
                                 (from 0)
                                 (to 9))
  "Returns n pseudo-random values from the discrete-uniform. ~
   (:elaboration ~
   The discrete-uniform distribution is characterized here by two ~
   parameters: the lower-bound-of it and the upper-bound-of it.  ~
   Inclusive of the bounds, the set of elements are those running from the ~
   lower-bound and increasing by one until the upper-bound is attained.) ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg from 0 The lower bound of the discrete uniform.) ~
   (:arg to 9  The upper bound of the discrete uniform; this must be larger ~
               than the lower bound.) ~
   ) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *discrete-uniform*))
  (let ((saved-l (lower-bound-of *discrete-uniform*))
        (saved-u (upper-bound-of *discrete-uniform*))
        result)
    (set-discrete-uniform-bounds *discrete-uniform*
                                 :from from :to to)
    (setf result (random-value *discrete-uniform* n))
    (set-discrete-uniform-bounds *discrete-uniform*
                                 :from saved-l :to saved-u)
    result
    )
  )

(defun dist-discrete-uniform (x &key 
                                 (from 0)
                                 (to 9))
  "Calculates and returns the value of the specified discrete-uniform distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   (:elaboration ~
   The discrete-uniform distribution is characterized here by two ~
   parameters: the lower-bound-of it and the upper-bound-of it.  ~
   Inclusive of the bounds, the set of elements are those running from the ~
   lower-bound and increasing by one until the upper-bound is attained.) ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg from 0 The lower bound of the discrete uniform.) ~
   (:arg to 9  The upper bound of the discrete uniform; this must be larger ~
   than the lower bound.) ~
   )"
  (declare (special *discrete-uniform*))
  (let ((saved-l (lower-bound-of *discrete-uniform*))
        (saved-u (upper-bound-of *discrete-uniform*))
        result)
    (set-discrete-uniform-bounds *discrete-uniform*
                                 :from from :to to)
    (setf result (cdf-at *discrete-uniform* x))
    (set-discrete-uniform-bounds *discrete-uniform*
                                 :from saved-l :to saved-u)
    result
    )
  )
