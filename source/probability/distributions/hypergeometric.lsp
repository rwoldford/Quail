;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          hypergeometric.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;     Joe Leung
;;;     R.W. Oldford 1995.
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(hypergeometric total-successes-of total-failures-of sample-size-of
          population-size-of
          density-hypergeometric  quantile-hypergeometric
          random-hypergeometric dist-hypergeometric)))

(defclass hypergeometric (discrete-dist)
  ((total-successes  :reader total-successes-of
        :initarg :total-successes)
   (total-failures  :accessor total-failures-of
        :initarg :total-failures)
   (sample-size  :reader sample-size-of
        :initarg :sample-size)
   (lower-bound :accessor lower-bound-of
                :allocation :class
                :initform 0))
  (:documentation "The Hypergeometric distribution for number of successes in ~
                   sample-size draws without replacement from a total of ~
                   total-successes + total-failures units."))

(defmethod initialize-instance :after ((self hypergeometric)
                                       &rest initargs &key sample-size)
  (declare (ignore initargs))
  (if sample-size (setf (sample-size-of self) sample-size))
  (setf (upper-bound-of self)
        (min (total-successes-of self) (sample-size-of self)))
  )


(defmethod (setf lower-bound-of) (new-value (dist hypergeometric))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (unless (and (integerp (eref new-value 0))
               (= new-value 0))
    (quail-error "Lower bound is zero and cannot be changed to ~s ." new-value))
  )


(defmethod (setf upper-bound-of) (new-value (dist hypergeometric))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (unless (and (integerp (eref new-value 0))
               (= new-value
                  (min (total-successes-of dist)
                       (sample-size-of dist))))
    (quail-error "Upper bound cannot be changed.  Change either the ~
                  sample-size-of or the total-successes-of ~s." dist))
  )

(defmethod (setf sample-size-of) (new-value (dist hypergeometric))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (unless (and (integerp (eref new-value 0))
               (> new-value 0)
               (<= new-value (+ (total-successes-of dist)
                                (total-failures-of dist))))
    (quail-error "Sample-size must be an integer in the range (0,~s], ~
                  not ~s ."
                 (+ (total-successes-of dist)
                    (total-failures-of dist))
                 new-value))
  (setf (slot-value dist 'sample-size) new-value)
  (setf (slot-value dist 'upper-bound)
        (min (total-successes-of dist) (sample-size-of dist)))
  new-value)



(defmethod (setf total-successes-of) (new-value (dist hypergeometric))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (unless (and (integerp (eref new-value 0))
               (>= new-value (sample-size-of dist)))
    (quail-error "Total number of successes must be an integer and >= ~s , ~
                  not ~s ."
                 (sample-size-of dist)
                 new-value))
  (setf (slot-value dist 'total-successes) new-value)
  (setf (slot-value dist 'upper-bound)
        (min (total-successes-of dist) (sample-size-of dist)))
  new-value)


(defmethod population-size-of ((dist hypergeometric))
  "Returns the total number of successes and failures of the hypergeometric ~
   distribution."
  (+ (total-successes-of dist)
     (total-failures-of dist)))

;;;-----------------------------------------------------------------------
;;;  Hypergeometric ( Hyper(n,total-successes,total-failures) )
;;;     
;;;  1. Parameters: n = sample size
;;;                 total-successes = population size of type 1 elements
;;;                 total-failures = population size of type 2 elements
;;;                    where total population size = N = total-successes + total-failures
;;;  2. Range: x =  0,1,...,min(n,total-successes)
;;;
;;;                 /total-successes\ / total-failures\
;;;                 \        x      / \      n-x      /
;;;  3. pdf: f(x) = ---------------------------------
;;;                               /N\
;;;                               \n/
;;;
;;;  4. cdf:         /  0                         ,if x < 0
;;;                  |
;;;           F(x) =<  SUM(from t = 0 to x){f(t)}  ,if 0 <= x <= min(n,total-successes)
;;;                  |
;;;                  \  1                         ,if x > min(n,total-successes)
;;;  5. mean: 
;;;  6. variance:
;;;
;;;   @ written by Joe Leung (ID# 89115226)
;;;
;;;-----------------------------------------------------------------------

;;;------------------------------------------
;;;   PDF-AT
;;;------------------------------------------

(defmethod pdf-at ((dist hypergeometric) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / -  >= <= )
    (let ((successes (eref (total-successes-of dist) 0))
          (failures (eref (total-failures-of dist) 0))
          (sample-size (eref (sample-size-of dist) 0)))
      (if (integerp x)
        (if (and (>= x 0) (<= x sample-size) 
                 (<= x successes)) 
          (* (choose successes x)
             (/ (choose failures 
                        (- sample-size x))
                (choose (+ successes 
                           failures)
                        sample-size)))
          0)
        0)
      )))

;------------------------------------------
;   CDF-AT
;------------------------------------------

(defmethod cdf-at ((dist hypergeometric) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (floor >= < )
    (let ((intx (floor x)))
      (cond ((< intx 0) 0)
            ((>= intx (eref (sample-size-of dist) 0)) 
             1)
            (T (loop
                 for i from 0 to intx 
                 sum (pdf-at dist i)))))))

;------------------------------------------
;   QUANTILE-AT
;------------------------------------------

(defmethod quantile-at ((dist hypergeometric) 
                        (p number)
                        &key (start NIL))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (bisection dist p)
  )

;------------------------------------------
;   RANDOM-VALUE
;------------------------------------------

(defmethod random-value ((dist hypergeometric) &optional  (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (if (= n 1)
    (quantile-at dist (random-uniform ))
    (array
     (loop for i from 1 to n
           collect (quantile-at dist (random-uniform ))
           )
     :dimensions (list n))))



(defvar *hypergeometric*
  NIL
  "An instance of a hypergeometric representing the hypergeometric ~
   distribution. ~
   A hypergeometric distribution is characterized by three ~
   parameters: total-successes, total-failures, and sample-size.  ~
   A total of sample-size units are drawn without replacement from ~
   a finite population of total-successes + total-failures. ~
   The random quantity is the number of successes observed in the sample.  ~
   This instance is used by the standard functions random-hypergeometric, ~
   quantile-hypergeometric, etc.")

(defun density-hypergeometric (x &key
                                 (total-successes 1)
                                 (total-failures 1)
                                 (sample-size 1))
  "Returns the value of a  hypergeometric probability at x. ~
   (:elaboration ~
   A hypergeometric distribution is characterized by three ~
   parameters: total-successes, total-failures, and sample-size.  ~
   A total of sample-size units are drawn without replacement from ~
   a finite population of total-successes + total-failures.) ~
   The random quantity is the number of successes observed in the sample.  ~
   (:required ~
   (:arg x The point at which to evaluate the density, or probability.)) ~
   (:key ~
   (:arg total-successes 1 The total number of successful ~
   units available to draw from.) ~
   (:arg total-failures 1 The total number of failed ~
   units available to draw from.) ~
   (:arg sample-size 1 The number of draws to take without replacement from ~
   the total of successful and failed units available.))"
  (declare (special *hypergeometric*))
  (let ((saved-s (total-successes-of *hypergeometric*))
        (saved-f (total-failures-of *hypergeometric*))
        (saved-n (sample-size-of *hypergeometric*))
        result)
    (setf (total-successes-of *hypergeometric*) total-successes)
    (setf (total-failures-of *hypergeometric*) total-failures)
    (setf (sample-size-of *hypergeometric*) sample-size)
    (setf result (pdf-at *hypergeometric* x))
    (setf (total-successes-of *hypergeometric*) saved-s)
    (setf (total-failures-of *hypergeometric*) saved-f)
    (setf (sample-size-of *hypergeometric*) saved-n)
    result
    )
  )

(defun quantile-hypergeometric (prop &key 
                                     (total-successes 1)
                                     (total-failures 1)
                                     (sample-size 1))
  
  "Returns the value of the prop'th hypergeometric quantile. ~
   (:elaboration ~
   A hypergeometric distribution is characterized by three ~
   parameters: total-successes, total-failures, and sample-size.  ~
   A total of sample-size units are drawn without replacement from ~
   a finite population of total-successes + total-failures.) ~
   (:required ~
   (:arg prop The cumulative proportion at which to evaluate the quantile.)) ~
   (:key ~
   (:arg total-successes 1 The total number of successful ~
   units available to draw from.) ~
   (:arg total-failures 1 The total number of failed ~
   units available to draw from.) ~
   (:arg sample-size 1 The number of draws to take without replacement from ~
   the total of successful and failed units available.))"
  (declare (special *hypergeometric*))
  (let ((saved-s (total-successes-of *hypergeometric*))
        (saved-f (total-failures-of *hypergeometric*))
        (saved-n (sample-size-of *hypergeometric*))
        result)
    (setf (total-successes-of *hypergeometric*) total-successes)
    (setf (total-failures-of *hypergeometric*) total-failures)
    (setf (sample-size-of *hypergeometric*) sample-size)
    (setf result (quantile-at *hypergeometric* prop))
    (setf (total-successes-of *hypergeometric*) saved-s)
    (setf (total-failures-of *hypergeometric*) saved-f)
    (setf (sample-size-of *hypergeometric*) saved-n)
    result
    )
  )

(defun random-hypergeometric (&key (n 1) 
                                   (total-successes 1)
                                   (total-failures 1)
                                   (sample-size 1))
  "Returns n pseudo-random values from the hypergeometric. ~
   (:elaboration ~
   A hypergeometric distribution is characterized by three ~
   parameters: total-successes, total-failures, and sample-size.  ~
   A total of sample-size units are drawn without replacement from ~
   a finite population of total-successes + total-failures.) ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg total-successes 1 The total number of successful ~
   units available to draw from.) ~
   (:arg total-failures 1 The total number of failed ~
   units available to draw from.) ~
   (:arg sample-size 1 The number of draws to take without replacement from ~
   the total of successful and failed units available.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *hypergeometric*))
  (let ((saved-s (total-successes-of *hypergeometric*))
        (saved-f (total-failures-of *hypergeometric*))
        (saved-n (sample-size-of *hypergeometric*))
        result)
    (setf (total-successes-of *hypergeometric*) total-successes)
    (setf (total-failures-of *hypergeometric*) total-failures)
    (setf (sample-size-of *hypergeometric*) sample-size)
    (setf result (random-value *hypergeometric* n))
    (setf (total-successes-of *hypergeometric*) saved-s)
    (setf (total-failures-of *hypergeometric*) saved-f)
    (setf (sample-size-of *hypergeometric*) saved-n)
    result
    )
  )

(defun dist-hypergeometric (x &key 
                              (total-successes 1)
                              (total-failures 1)
                              (sample-size 1))
  "Calculates and returns the value of the specified hypergeometric distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   (:elaboration ~
   A hypergeometric distribution is characterized by three ~
   parameters: total-successes, total-failures, and sample-size.  ~
   A total of sample-size units are drawn without replacement from ~
   a finite population of total-successes + total-failures.) ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg total-successes 1 The total number of successful ~
   units available to draw from.) ~
   (:arg total-failures 1 The total number of failed ~
   units available to draw from.) ~
   (:arg sample-size 1 The number of draws to take without replacement from ~
   the total of successful and failed units available.))"
  (declare (special *hypergeometric*))
  (let ((saved-s (total-successes-of *hypergeometric*))
        (saved-f (total-failures-of *hypergeometric*))
        (saved-n (sample-size-of *hypergeometric*))
        result)
    (setf (total-successes-of *hypergeometric*) total-successes)
    (setf (total-failures-of *hypergeometric*) total-failures)
    (setf (sample-size-of *hypergeometric*) sample-size)
    (setf result (cdf-at *hypergeometric* x))
    (setf (total-successes-of *hypergeometric*) saved-s)
    (setf (total-failures-of *hypergeometric*) saved-f)
    (setf (sample-size-of *hypergeometric*) saved-n)
    result
    )
  )


