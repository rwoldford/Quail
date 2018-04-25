;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               gamma.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Stat 440/840 students 1992.
;;;     R.W. Oldford 1993, 1995
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 
;;; Fall 1992
;;; written by Colin and Dan 
;;; for R. W. Oldford

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(gamma-dist location-of shape-of scale-of density-gamma  quantile-gamma
          random-gamma dist-gamma)))

(defclass gamma-dist (continuous-dist)
  ((location :initarg :location
             :initform 0)
   (lower-bound
    :reader    lower-bound-of
    :initarg  :lower-bound
    :initform  0)
   (shape    :accessor shape-of
             :initarg :shape
             :initform 2)
   (scale    :accessor scale-of
             :initarg :scale
             :initform 1))
   (:documentation "The Gamma distribution, at 'location', scaled by 'scale', and ~
                    with shape parameter 'shape'.")
)

(defmethod location-of ((self gamma-dist))
  (lower-bound-of self))

(defmethod (setf location-of) (new-value (dist gamma-dist))
  (setf (slot-value dist 'location) new-value)
  (setf (slot-value dist 'lower-bound) new-value)
  new-value)

(defmethod (setf lower-bound-of) (new-value (dist gamma-dist))
  (setf (slot-value dist 'lower-bound) new-value)
  (setf (slot-value dist 'location) new-value)
  new-value)

;  GAMMA DISTRIBUTION
;    This set of methods defines the probability density function (pdf), cumulative
;    density function (cdf), the quantile function, and a random value for the 
;    gamma distribution with parameters shape (a), location (u), and scale.  
;    The following closed forms and techniques are implemented:

;    pdf at x:              1                       (a - 1)       - ((x - u) / scale)
;                  --------------------   *  (x - u)         *  e
;                                     a 
;                  gamma (a) * (scale)   
;
;    cdf at x :    INCOMPLETE GAMMA FUNCTION                 
;
;    random value :     REJECTION TECHNIQUES
;
;    quantile at p :       Applied Statistics   AS91 ... percentiles of chi

;***************************
;***************************



;***************************

(defmethod initialize-instance :after ((self gamma-dist)
                                       &key
                                       (lower-bound NIL)
                                       (location NIL))
  (cond
   ((and lower-bound location)
    (unless (= lower-bound location)
      (quail-error "Gamma-dist: lower bound ~s /= location ~s"
                   lower-bound location)))
   (lower-bound
    (setf (location-of self) lower-bound))
   (location
    (setf (lower-bound-of self) location))
   )
  self)

(defmethod pdf-at :around ((distribution gamma-dist) (x number))
    (if (cl::<= x (eref (location-of distribution) 0))
      0
      (call-next-method)))
          
(defmethod pdf-at ((distribution gamma-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - exp expt sqrt > < = /=  >= <= abs log)
    (let ((location (eref (location-of distribution) 0))
          (scale (eref (scale-of distribution) 0))
          (shape (eref (shape-of distribution) 0)))
      (exp
       (- (* (- shape 1) (log (- x location)))
          (/ (- x location) scale)
          (log-gamma shape)
          (* shape (log scale)))))))
#|
      (* (/ 1 (* (exp (log-gamma shape))
                 (expt scale shape)))
         (expt (- x location) (- shape 1))
         (exp (- 0 (/ (- x location) scale))))
      )))
|#

;***************************
(defmethod quantile-at ((distribution gamma-dist)
                        (p number)
                        &key (start NIL))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ - * / log exp expt abs < > <= >= =)
    (let (chi-result
          (location (eref (location-of distribution) 0))
          (scale (eref (scale-of distribution) 0))
          (shape (eref (shape-of distribution) 0))
          )
      (setf chi-result
            (let*
              ((half 5.0D-1)
               (one 1.0D0)
               (two 0.2D1)
               (three 0.3D1)
               (six 0.6D1)
               (aa 0.6931471806)
               (e 0.5D-8)
               (v (*  2 shape))
               (xx (* half v))
               (c (- xx one))
               (g (log-gamma xx))
               (c1 0.01)
               (c2 0.222222)
               (c3 0.32)
               (c4 0.4)
               (c5 1.24)
               (c6 2.2)
               (c7 4.67)
               (c8 6.66)
               (c9 6.73)
               (c10 13.32)
               (c11 60.0)
               (c12 70.0)
               (c13 84.0)
               (c14 105.0)
               (c15 120.0)
               (c16 127.0)
               (c17 140.0)
               (c18 175.0)
               (c19 210.0)
               (c20 252.0)
               (c21 264.0)
               (c22 294.0)
               (c23 346.0)
               (c24 420.0)
               (c25 462.0)
               (c26 606.0)
               (c27 672.0)
               (c28 707.0)
               (c29 735.0)
               (c30 889.0)
               (c31 932.0)
               (c32 966.0)
               (c33 1141.0)
               (c34 1182.0)
               (c35 1278.0)
               (c36 1740.0)
               (c37 2520.0)
               (c38 5040.0)
               a z0
               current-value return-z0?
               )
              (flet
                ((update-approx (ch)
                   ;; calculation of seven term taylor series
                   ;; (format *terminal-io* "~&ch = ~s" ch)
                   (let*
                     ((p1 (* half ch))
                      (p2 (- p (incomplete-gamma xx p1 :max-iterations 1000)))
                      (tt (* p2
                             (exp
                              (- (+ (* xx aa) g p1)
                                 (* c (log ch))))))
                      (b (/ tt ch))
                      (a (- (* half tt)
                            (* b c)))
                      (s1 (/
                           (+ c19
                              (* a
                                 (+ c17
                                    (* a
                                       (+ c14
                                          (* a
                                             (+ c13
                                                (* a
                                                   (+ c12
                                                      (* a
                                                         c11))))))))))
                           c24))
                      (s2 (/
                           (+ c24
                              (* a
                                 (+ c29
                                    (* a
                                       (+ c32
                                          (* a
                                             (+ c33
                                                (* a
                                                   c35))))))))
                           c37))
                      (s3 (/
                           (+ c19
                              (* a
                                 (+ c25
                                    (* a
                                       (+ c28
                                          (* a
                                             c31))))))
                           c37))
                      (s4 (/
                           (+ c20
                              (* a
                                 (+ c27
                                    (* a c34)))
                              (* c
                                 (+ c22
                                    (* a
                                       (+ c30
                                          (* a
                                             c36))))))
                           c38))
                      (s5 (/
                           (+ c13
                              (* a c21)
                              (* c
                                 (+ c18
                                    (* a
                                       c26))))
                           c37))
                      (s6 (/
                           (+ c15
                              (* c
                                 (+ c23
                                    (* c
                                       c16))))
                           c38))
                      )
                     (setq ch
                           (+ ch 
                              (* tt
                                 (- (+ one (* half tt s1))
                                    (* b c
                                       (- s1
                                          (* b
                                             (- s2
                                                (* b
                                                   (- s3
                                                      (* b
                                                         (- s4
                                                            (* b
                                                               (- s5
                                                                  (* b s6)))))))))))))))
                     ;; (format *terminal-io* "~&ch(out) = ~s" ch)
                     ch)
                   )
                 (close-enough-p (old new)
                   (<= (abs (- (/ old new) one)) e))
                 (z0-close-enough-p (old new)
                   (<= (abs (- (/ old new) one)) c1))
                 )
                
                ;; get starting approximation             
                
                (cond
                 
                 ;; starting approximation for small chi-squared
                 ((< v (* (- c5) (log p)))
                  (setq z0 (expt (* p xx (exp (+ g (* xx aa))))
                                 (/ one xx)))
                  (if (<= z0 e) (setf return-z0? T))
                  )
                 
                 ;; starting approximation for v > 0.32
                 ((> v c3)
                  (let*
                    ((x (quantile-gaussian p))
                     (p1 (/ c2 v)))
                    
                    ;; starting approximation using wilson and hilferty estimate
                    ;;
                    (setq z0 (* v (expt (- (+ (* x (sqrt p1)) one)
                                           p1)
                                        3)))
                    ;; starting approximation for p tending to 1
                    ;;
                    (if (> z0 (+ (* c6 v) six))
                      (setq z0 (* -2.0
                                  (+ (- (log (- one p))
                                        (* c (log  (* half z0))))
                                     g))))
                    ))
                 (T
                  ;; starting approximation for v <= 0.32
                  ;; may require iteration
                  (flet
                    ((update-z0 (ch)
                       (let* ((p1 (+ one (* ch (+ c7 ch))))
                              (p2 (* ch (+ c9 (* ch (+ c8 ch)))))
                              (tt (-
                                   (/ (+  c7 (* two ch))
                                      p1)
                                   half
                                   (/ (+ c9 (* ch (+ c10 (* ch three))))
                                      p2))))
                         (- ch
                            (/ (- one
                                  (/ (* (exp (+ a g (* half ch) (* c aa)))
                                        p2)
                                     p1))
                               tt))))
                     )
                    (setq z0 c4)
                    (setf a (log (- one p)))
                    (setq current-value (update-z0 z0))
                    (if (z0-close-enough-p z0 current-value)
                      (setf z0 current-value)
                      (loop
                        until (z0-close-enough-p z0 current-value)
                        do
                        (setf z0 current-value)
                        (setf current-value (update-z0 z0))
                        finally
                        (setf z0 current-value))))))
                (cond
                 (return-z0? z0)
                 (T
                  (setq current-value (update-approx z0))
                  ;; (format *terminal-io* "~&current-value = ~s" current-value)
                  (if (close-enough-p z0 current-value)
                    (setf z0 current-value)
                    (loop
                      until (close-enough-p z0 current-value)
                      do
                      ;; (format *terminal-io* "~&current-value = ~s" current-value)
                      (setf z0 current-value)
                      (setf current-value (update-approx z0))))
                  current-value))
                )
              )
            )
      (+ (* scale chi-result 0.5) location)
      )
    )
  )


;***************************   

(defmethod cdf-at ((distribution gamma-dist) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline incomplete-gamma))
  (let ((location (eref (location-of distribution) 0))
        (scale (eref (scale-of distribution) 0))
        (shape (eref (shape-of distribution) 0))
        result)
    (with-CL-functions (/ -)
      (setf result
            (incomplete-gamma shape
                        (/ (- x location) 
                           scale)
                        :max-iterations 1000))
      result)))


;***************************


(defmethod random-value ((distribution gamma-dist) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
    (let ((location (eref (location-of distribution) 0))
          (scale (eref (scale-of distribution) 0))
          (shape (eref (shape-of distribution) 0))
          v1 v2 y am gamdev e s x dum uprv b) ;; keep)
           
      (cond ((and (integerp shape) (< shape 6))
             
             ;  For integer values of shape less than 6, a standard gamma deviate is 
             ;  created by adding shape number of exponential deviates
             
             (setf gamdev 1)
             ; (loop for index from 1 to shape do                  
             (do ((index 1 (incf index)))
                 ((> index shape))
               (setf gamdev (* gamdev (random-uniform :n n))))               
             (setf gamdev (- (log gamdev)))
             (setf gamdev (+ (* scale gamdev) location)))
            
            
            ((< shape 1)
             
             ;  For 0 < shape < 1, a rejection method is used with a combination of the 
             ;  exponential and a polynomial of degree shape as the overlying function.
             ;  This algorithm is described in Question #3 of Assignment #3
             (setf
              gamdev
              (loop
                for i from 1 to n
                collect
                (progn
                  (loop 
                    (setf dum (* (random-uniform)
                                 (* (/ 1 (exp (log-gamma shape)))
                                    (+ (/ 1 shape)
                                       (exp (- 0 1))))))
                    (cond ((<= dum (/ 1 (* shape
                                           (exp (log-gamma shape)))))
                           (setf uprv (expt (* shape
                                               (exp (log-gamma shape))
                                               dum)
                                            (/ 1 shape)))
                           ;;(setf keep 0)
                           )
                          (T 
                           (setf uprv (- 0 (log (+ (/ 1 shape)
                                                   ( - (exp (- 0 1))
                                                       (* dum 
                                                          (exp (log-gamma 
                                                                shape))
                                                          ))))))
                           ;;(setf keep 1)
                           ))
                    (setf b (* (random-uniform)
                               (cond ((< uprv 1)
                                      (/ (expt uprv (- shape 1))
                                         (exp (log-gamma shape))))
                                     (T
                                      (/ (exp (- 0 uprv)) 
                                         (exp (log-gamma shape)))))))
                    (cond ((<= b (pdf-at distribution uprv))
                           (setf gamdev uprv)
                           (return))
                          ))
                  
                  (+ (* scale gamdev) location)
                  )
                )
              )
             (if (= n 1)
               (setf gamdev (first gamdev))
               (setf gamdev (array gamdev)))
             )
            
            (T                
             
             ;  For other values of shape > 1, a rejection method is used with the 
             ;  Cauchy distribution as the overlying function.  This exact algorithm
             ;  was created by Ahrens and is described in 'Numerical Recipes' on page 206
             
             (setf
              gamdev
              (loop
                for i from 1 to n
                collect
                (progn
                  (loop            
                    (loop                    
                      (loop                                          
                        (setf v1 (- (* 2 (random-uniform)) 1))
                        (setf v2 (- (* 2 (random-uniform)) 1))
                        (if (<= (+ (* v1 v1) (* v2 v2)) 1)
                          (return)))
                      (setf y (/ v2 v1))
                      (setf am (- shape 1))
                      (setf s (sqrt (+ (* 2 am) 1)))
                      (setf x (+ (* s y) am))
                      (if (>= x 0) (return)))
                    (setf e (* (+ (* y y) 1) 
                               (exp (- (* am (log (/ x am))) (* s y)))))
                    (if (<= (random-uniform) e) (return)))
                  (+ (* scale x) location)
                  )))
             (if (= n 1)
               (setf gamdev (first gamdev))
               (setf gamdev (array gamdev)))
             ))
      gamdev))

           
      
;***************************


(defvar *gamma-dist*
  NIL
  "An instance of a gamma-dist representing the gamma distribution. ~
   This instance is used by the standard gamma functions random-gamma, ~
   quantile-gamma, etc.")

(defun density-gamma (x &key (shape 2.0)
                        (location 0.0) (scale 1.0))
  "Returns the value of a location scale gamma density at x. ~
   Location 0 and scale 1 define the standard gamma distribution.  ~
   (:required ~
     (:arg x The point at which to evaluate the density.)) ~
   (:key ~
   (:arg shape 2.0 The shape parameter of the gamma distribution.  The gamma is ~
                   skewed to the right.  The smaller the shape parameter shape, ~
                   the greater is this skewness.) ~
   (:arg location 0.0 The location of the gamma density.  The gamma has ~
                      positive support from the location to +infinity.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.))"
  (declare (special *gamma-dist*))
  (let ((saved-l (location-of *gamma-dist*))
        (saved-s (scale-of *gamma-dist*))
        (saved-a (shape-of *gamma-dist*))
        result)
    (setf (location-of *gamma-dist*) location)
    (setf (scale-of *gamma-dist*) scale)
    (setf (shape-of *gamma-dist*) shape)
    (setf result (pdf-at *gamma-dist* x))
    (setf (location-of *gamma-dist*) saved-l)
    (setf (scale-of *gamma-dist*) saved-s)
    (setf (shape-of *gamma-dist*) saved-a)
    result
    )
  )

(defun quantile-gamma (p &key (shape 2.0) (location 0.0) (scale 1.0))
  
  "Returns the value of a location scale gamma quantile at p. ~
   Location 0 and scale 1 define the standard gamma distribution.  ~
   (:required ~
     (:arg p The probability at which to evaluate the quantile.)) ~
   (:key ~
   (:arg shape 2.0 The shape parameter of the gamma distribution.  The gamma is ~
                   skewed to the right.  The smaller the shape parameter shape, ~
                   the greater is this skewness.) ~
   (:arg location 0.0 The location of the gamma density.  The gamma has ~
                      positive support from the location to +infinity.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.))~
   (:references Algorithm AS 91 ... Applied Statistics Algorithms - 1985; by D.J. Best and D.E.~
   Roberts.)"
  (declare (special *gamma-dist*))
  (let ((saved-l (location-of *gamma-dist*))
        (saved-s (scale-of *gamma-dist*))
        (saved-a (shape-of *gamma-dist*))
        result)
    (setf (location-of *gamma-dist*) location)
    (setf (scale-of *gamma-dist*) scale)
    (setf (shape-of *gamma-dist*) shape)
    (setf result (quantile-at *gamma-dist* p))
    (setf (location-of *gamma-dist*) saved-l)
    (setf (scale-of *gamma-dist*) saved-s)
    (setf (shape-of *gamma-dist*) saved-a)
    result)
  )

(defun random-gamma (&key (n 1) (shape 2.0) (location 0.0) (scale 1.0))
  "Returns n pseudo-random values from the location scale gamma. ~
   Location 0 and scale 1 define the standard gamma distribution.  ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg shape 2.0 The shape parameter of the gamma distribution.  The gamma is ~
                   skewed to the right.  The smaller the shape parameter shape, ~
                   the greater is this skewness.) ~
   (:arg location 0.0 The location of the gamma density.  The gamma has ~
                      positive support from the location to +infinity.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *gamma-dist*))
  (let ((saved-l (location-of *gamma-dist*))
        (saved-s (scale-of *gamma-dist*))
        (saved-a (shape-of *gamma-dist*))
        result)
    (setf (location-of *gamma-dist*) location)
    (setf (scale-of *gamma-dist*) scale)
    (setf (shape-of *gamma-dist*) shape)
    (setf result (random-value *gamma-dist* n))
    (setf (location-of *gamma-dist*) saved-l)
    (setf (scale-of *gamma-dist*) saved-s)
    (setf (shape-of *gamma-dist*) saved-a)
    result)
  )

(defun dist-gamma (x &key (shape 2.0) (location 0.0) (scale 1.0))
  "Calculates and returns the value of the specified gamma distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   Location 0 and scale 1 define the standard gamma distribution.  ~
   (:required ~
     (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg shape 2.0 The shape parameter of the gamma distribution.  The gamma is ~
                   skewed to the right.  The smaller the shape parameter shape, ~
                   the greater is this skewness.) ~
   (:arg location 0.0 The location of the gamma density.  The gamma has ~
                      positive support from the location to +infinity.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.)) "
  (declare (special *gamma-dist*))
  (let ((saved-l (location-of *gamma-dist*))
        (saved-s (scale-of *gamma-dist*))
        (saved-a (shape-of *gamma-dist*))
        result)
    (setf (location-of *gamma-dist*) location)
    (setf (scale-of *gamma-dist*) scale)
    (setf (shape-of *gamma-dist*) shape)
    (setf result (cdf-at *gamma-dist* x))
    (setf (location-of *gamma-dist*) saved-l)
    (setf (scale-of *gamma-dist*) saved-s)
    (setf (shape-of *gamma-dist*) saved-a)
    result)
  )
