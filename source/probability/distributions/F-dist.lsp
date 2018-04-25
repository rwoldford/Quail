;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               F-dist.lisp                              
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

(eval-when(:compile-toplevel :load-toplevel :execute) 
     (export '(F-dist location-of df-num-of df-den-of density-F  quantile-F
          random-F dist-F)))

(defclass F-dist (continuous-dist)
  ((df-num    :reader df-num-of
              :initarg :df-num
              :initform 1.0
              :documentation "The numerator degrees of freedom.")
   (df-den    :reader df-den-of
              :initarg :df-den
              :initform 1.0
              :documentation "The denominator degrees of freedom."))
   (:documentation "The F distribution parameterized by its numerator and ~
                    denominator degrees of freedom df-num and ~
                    df-den respectively.")
)
;;;  F DISTRIBUTION
;;;    This set of methods defines the probability density function (pdf), 
;;;    cumulative density function (cdf), the quantile function, and a random
;;;    value for the F distribution with parameters df-num (a)
;;;    and df-den (b).
;;;    
;;;    With the exception of the pdf, most methods appeal to the relationship
;;;    between F and Beta random-variables.
;;;
;;;    In particular if x is beta(m,n) then y = (n * x)/(m * (1 - x)) is 
;;;    distributed as an F(m,n) random variable.
;;;
;;;                                       (m/2)
;;;    pdf at y:      gamma((m+n)/2)  (m/n)      (m/2 - 1)       - ((m+n)/2)
;;;                  -----------------------  *  y         * (1 + my/n)
;;;                   gamma (m/2) gamma(n/2)               
;;;                    
;;;                              (m/2)
;;;                          (m/n)                (m/2 - 1)          -((m+n)/2)
;;;              =   -----------------------  *  y         * (1 + my/n)
;;;                    Beta((m/2),(n/2))
;;;
;;;    cdf at y :    From beta distribution.
;;;                  If y ~ F(m,n) , then
;;;                     P(y <= a) = P(nx <= m(1-x)a)
;;;                               = P(nx + mxa <= ma)
;;;                               = P(x <= ma/(n+ma))
;;;                  where x ~ Beta(m/2,n/2)
;;;                                     
;;;
;;;    random value :     y = (n * x)/(m * (1 - x))  where x ~ Beta(m/2,n/2)
;;;
;;;    quantile at p :   p = P(y <= a) = P(x <= ma/(n+ma))
;;;                      So if q is the p'th quantile from a beta(m/2,n/2)
;;;                      Then a = nq/(m(1-q)) is the p'th for F(m,n)

;;;***************************
;;;***************************



;;;***************************

(defmethod initialize-instance :after ((self F-dist) &key)
  (setf (lower-bound-of self) 0.0)
  (setf (df-num-of self) (float (df-num-of self)))
  (setf (df-den-of self) (float (df-den-of self))))

(defmethod df-of ((self F-dist))
  "Returns a list of the numerator and denominator ~
   degrees of freedom for an F-distribution."
  (list (df-num-of self) (df-den-of self)))

(defmethod (setf df-of) ((new-value number) (self F-dist))
  "Sets the numerator and denominator ~
   degrees of freedom for an F-distribution. ~
   (:required (:arg new-value A list of the numerator and denominator ~
   degrees of freedom.)) ~
   (:see-also df-num-of df-den-of)"
  (quail-error
   "~&(SETF DF-OF): The degrees of freedom for an F-dist ~
    must be two numbers in a list or other ~
    ref'able object, not ~s ." new-value))

(defmethod (setf df-of) ((new-value T) (self F-dist))
  "Sets the numerator and denominator ~
   degrees of freedom for an F-distribution. ~
   (:required (:arg new-value A list of the numerator and denominator ~
   degrees of freedom.)) ~
   (:see-also df-num-of df-den-of)"
  (with-CL-functions (float)
    (setf (df-num-of self) (float (eref new-value 0)))
    (setf (df-den-of self) (float (eref new-value 1)))))

(defmethod (setf df-num-of) (new-value (self F-dist))
  "Sets the numerator degrees of freedom for an F-distribution to be ~
   the float of the new value."
  (with-CL-functions (float)
    (setf (slot-value self 'df-num) (float new-value))))

(defmethod (setf df-den-of) (new-value (self F-dist))
  "Sets the denominator degrees of freedom for an F-distribution to be ~
   the float of the new value."
  (with-CL-functions (float)
    (setf (slot-value self 'df-den) (float new-value))))

           

(defmethod pdf-at ((distribution F-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline log-gamma))
  (with-CL-functions (+ * / - exp log =)
    (if (= x 0.0)
      0.0
      (let*
        ((m (df-num-of distribution))
         (n (df-den-of distribution))
         (m/2 (/ m 2.0))
         (n/2 (/ n 2.0))
         (m+n/2 (/ (+ m n) 2.0))
         (constant
          (- (+ (log-gamma m+n/2) (* m/2 (- (log m) (log n))))
             (log-gamma m/2)
             (log-gamma n/2))))
        (exp
         (+ constant
            (* (- m/2 1.0) (log x))
            (* (- m+n/2)
               (log (+ 1.0 (* x (/ m n)))))))))))

;;;***************************


(defmethod cdf-at ((distribution F-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (*)
    (let* ((m (df-num-of distribution))
           (n (df-den-of distribution))
           (mx (* m x)))
      (dist-beta (/ mx (+ mx n)) :a (/ m 2) :b (/ n 2)))))


;;;***************************


(defmethod random-value ((distribution F-dist) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let* ((m (df-num-of distribution))
         (d (df-den-of distribution))
         (x  (random-beta :n n :a (/ m 2) :b (/ d 2))))
    (* (/ d m) (/ x (- 1.0 x)))))
      
;***************************


(defmethod quantile-at ((distribution F-dist) (p number)
                        &key (start NIL))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (* / -)
    (let* ((m (df-num-of distribution))
           (n (df-den-of distribution))
           (q  (quantile-beta p :a (/ m 2) :b (/ n 2))))
      (* (/ n m) (/ q (- 1.0 q))))))
      


(defvar *F-dist*
  NIL
  "An instance of a F-dist representing the F distribution. ~
   This instance is used by the standard F functions random-F, ~
   quantile-F, etc.")

(defun density-F (x &key (df-num 1) (df-den 1))
  "Returns the value of an F density at x. ~
   Parameters df-num and df-den define the standard F distribution.  ~
   (:required ~
     (:arg x The point at which to evaluate the density.)) ~
   (:key ~
   (:arg df-num 1 The numerator degrees of freedom of the F distribution.) ~
   (:arg df-den 1 The denominator degrees of freedom of the F distribution.) ~
   )"
  (declare (special *F-dist*))
  (let ((saved-n (df-den-of *F-dist*))
        (saved-m (df-num-of *F-dist*))
        result)
    (setf (df-den-of *F-dist*) df-den)
    (setf (df-num-of *F-dist*) df-num)
    (setf result (pdf-at *F-dist* x))
    (setf (df-den-of *F-dist*) saved-n)
    (setf (df-num-of *F-dist*) saved-m)
    result
    )
  )

(defun quantile-F (p &key (df-num 1) (df-den 1))
  "Returns the pth quantiles from an F distribution. ~
   Parameters df-num and df-den define the standard F distribution.  ~
   (:required ~
     (:arg p The percentile at which to determine the quantile.)) ~
   (:key ~
   (:arg df-num 1 The numerator degrees of freedom of the F distribution.) ~
   (:arg df-den 1 The denominator degrees of freedom of the F distribution.) ~
   )"
  (declare (special *F-dist*))
  (let ((saved-n (df-den-of *F-dist*))
        (saved-m (df-num-of *F-dist*))
        result)
    (setf (df-den-of *F-dist*) df-den)
    (setf (df-num-of *F-dist*) df-num)
    (setf result (quantile-at *F-dist* p))
    (setf (df-den-of *F-dist*) saved-n)
    (setf (df-num-of *F-dist*) saved-m)
    result
    ))

(defun random-F (&key (n 1)  (df-num 1) (df-den 1))
  "Returns n pseudo-random values from an F distribution. ~
   Parameters df-num and df-den define the standard F distribution.  ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg df-num 1 The numerator degrees of freedom of the F distribution.) ~
   (:arg df-den 1 The denominator degrees of freedom of the F distribution.) ~
   ) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *F-dist*))
  (let ((saved-n (df-den-of *F-dist*))
        (saved-m (df-num-of *F-dist*))
        result)
    (setf (df-den-of *F-dist*) df-den)
    (setf (df-num-of *F-dist*) df-num)
    (setf result (random-value *F-dist*  n))
    (setf (df-den-of *F-dist*) saved-n)
    (setf (df-num-of *F-dist*) saved-m)
    result
    )
  )

(defun dist-F (x &key (df-num 1) (df-den 1))
  "Calculates and returns the value of the specified F distribution ~
   function at x. (i.e. prob(X <= x) .)  ~
   Parameters df-num and df-den define the standard F distribution.  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg df-num 1 The numerator degrees of freedom of the F distribution.) ~
   (:arg df-den 1 The denominator degrees of freedom of the F distribution.) ~
   )"
  (declare (special *F-dist*))
  (let ((saved-n (df-den-of *F-dist*))
        (saved-m (df-num-of *F-dist*))
        result)
    (setf (df-den-of *F-dist*) df-den)
    (setf (df-num-of *F-dist*) df-num)
    (setf result (cdf-at *F-dist* x))
    (setf (df-den-of *F-dist*) saved-n)
    (setf (df-num-of *F-dist*) saved-m)
    result
    )
  )
