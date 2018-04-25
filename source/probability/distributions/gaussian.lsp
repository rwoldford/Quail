;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               gaussian.lisp                              
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

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(gaussian-dist location-of scale-of
          density-gaussian quantile-gaussian dist-gaussian random-gaussian)))

(defclass gaussian-dist (student)
  ((df :initform +INFINITY
            :reader df-of
            :allocation
            :class)
   (prob-list :initform
              (list 0.15641707759018236 0.14730805612132933 0.13328984115671988 
                    0.11587662110459311 0.09678828980765734 0.07767442199328518 
                    0.05989098625429795 0.044368333871782226 0.03158006332035766 
                    0.021596386605275224 0.014189837138492575 0.008957812117937159 
                    0.0054331876934742476 0.003166180633191985 0.001772739364775203 
                    0.0021023412956437393 0.005016008194985072 0.007360440011075997 
                    0.008918817004809176 0.009611999122311088 0.009496744827013612 
                    0.008735036934679752 0.007546401021936107 0.006157884387109194 
                    0.004763984114857969 0.0035035321889076015 0.002454012531309788 
                    0.0016395078185443635 0.001045934646646905 6.377250010757031E-4 
                    0.0026997961257424485)
              :reader prob-list-of
              :allocation :class))
   (:documentation "The Gaussian or Normal distribution.")
)

;;;  Since the integral of the pdf for the normal cannot be found in closed form 
;;;  the incomplete-gamma function was used for cdf-at and the Illinois function was
;;;  used for quantile-at. For random-value the rectangle-wedge-tail method was used
;;;  which required using cdf-at and the Illinois function.

(defmethod cdf-at ((N gaussian-dist) (X number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline incomplete-gamma))
  (with-CL-functions (+ * / - exp expt sqrt > < =)
    (let ((z (/ (- x (location-of n)) (scale-of n))))
      (if (< z 0)
        (- .5 (* .5 (incomplete-gamma .5 (* z z .5))))
        (- 1 (- .5 (* .5 (incomplete-gamma .5 (* z z .5))))))
      )))
 
(defmethod pdf-at ((N gaussian-dist) (X number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (with-CL-functions (+ * / - exp expt sqrt > < =)
    (let ((sigma (eref (scale-of N) 0))
          (location (eref (location-of N) 0)))
      (/ (exp (* -.5 (expt (/ (- x location) sigma) 2)))
         (* (sqrt (* 2 pi)) sigma)))))


(defmethod quantile-at ((dist gaussian-dist)
                        (p number)
                        &key (start NIL))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           
           )
  "Calculates the inverse of the gaussian cumulative distribution ~
   function for p between 10^-20 and 1 - 10^-20. ~
   (:elaboration Algorithm used is that developed by Odeh and Evans, 1974 ~
   and is based on a rational fraction approximation.  The code is based ~
   on the algorithm GAUINV as found in Kennedy and Gentle, 1980.) ~
   (:references Kennedy, W.J. and J.E. Gentle 1980 Statistical Computing ~
   Marcel-Dekker, New York; Odeh, R.E., and J.O. Evans, 1974, Algorithm ~
   AS 70: Percentage Points of the Normal Distribution, Applied Statistics 23, ~
   pp. 96-97.)"
  
  (with-CL-functions (+ * / - log expt sqrt > < =)
    (let ((location (eref (location-of dist) 0))
          (scale (eref (scale-of dist) 0))
          (lim 10D-20)
          (p0 -0.322232431088)
          (p1 -1.0)
          (p2 -0.342242088547)
          (p3 -0.0204231210245)
          (p4 -0.453642210148D-4)
          (q0 0.0993484626060)
          (q1 0.588581570495)
          (q2 0.531103462366)
          (q3 0.103537752850)
          (q4 0.38560700634D-2)
          (error? NIL)
          xp
          y
          (flip-sign? T))
      (when (> p .5)
        (setf flip-sign? NIL)
        (setf p (- 1 p)))
      (cond
       ((= p 0.5) (setf xp 0.0))
       ((< p lim) (setf error? T))
       (T (setf y (sqrt (log (/ 1.0 (* p p)))))
          (setf
           xp
           (+ y
              (/ (+ p0 (* y (+ p1 (* y (+ p2 (* y (+ p3 (* y p4))))))))
                 (+ q0 (* y (+ q1 (* y (+ q2 (* y (+ q3 (* y q4)))))))))))))
      (if error?
        (if flip-sign?
          (values +infinity :out-of-bounds)
          (values -infinity :out-of-bounds))
        (if flip-sign?
          (+ location (* -1.0 xp scale))
          (+ location (* xp scale))))
      )))

        
  
  
 
(defmethod random-value ((dist gaussian-dist) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline random-uniform)
           )
  (with-CL-functions (+ * / - log expt sqrt > < =)
    (array
     (loop
       for i from 1 to n
       collect
       (let ((upick (random-uniform))(f 0)
             (j 0)
             urect z  utail vtail s xc
             h uwedge vwedge u1wedge u2wedge c chold d
             r)
         (loop for i from 1 while(< f upick)
               do
               (setf f (+ f (elt (prob-list-of dist) (- i 1))))
               (setf j (+ j 1)))
         
         ;; generate the rectangles:
         
         (cond ((< j 16) (setf urect (random-uniform))
                (setf z (+ (* .2 urect) (/ (- j 1) 5))))
               
               ;; generate the tail:
               
               ((= j 31) (setf utail (random-uniform))
                (setf vtail (random-uniform))
                (loop for i from 1 while (>= vtail (* 3 (expt (- 9 (* 2 (log utail))) -.5)))
                      do
                      (setf vtail (random-uniform))
                      (setf utail (random-uniform)))
                (setf z (expt (- 9 (* 2 (log utail))) .5)))
               
               ;; generate the wedges:
               
               (T (setf s (/ (- (- j 15) 1) 5))
                  (setf r (/ (- j 15) 5))
                  (flet ((solve-for-x (x)
                           (- (* x (exp (/ (- 0 (expt x 2)) 2))) 
                              (* 5 (- (exp (/ (- 0 (expt s 2)) 2)) 
                                      (exp (/ (- 0 (expt r 2)) 
                                              2))))))
                         (f-j-of-x (x) (/ (- (* 2 (density-gaussian x))
                                             (* 2 (density-gaussian r))) 
                                          (elt (prob-list-of dist) (- (- j 15) 1)))))
                    (setf xc (illinois #'solve-for-x s r))
                    (setf d (f-j-of-x s))
                    (setf c (+ (* -5 d (- s xc)) (f-j-of-x xc)))
                    (cond ((> j 20) (setf chold c) (setf c d) (setf d chold) (setf h .2))
                          (T (setf h (- c d .2))))
                    (setf u1wedge (random-uniform))
                    (setf u2wedge (random-uniform))
                    (cond ((> u1wedge u2wedge)
                           (setf uwedge u2wedge) (setf vwedge u1wedge))             
                          (T (setf uwedge u1wedge) (setf vwedge u2wedge)))
                    (loop for i from 1
                          while (> vwedge (+ uwedge (/ (f-j-of-x (+ s (* h uwedge))) c)))
                          do (setf u1wedge (random-uniform))
                          (setf u2wedge (random-uniform))
                          (setf uwedge (min u1wedge u2wedge))
                          (setf vwedge (max u1wedge u2wedge)))
                    (setf z (+ s (* h uwedge))))))
         
         
         ;; assign a positive or negative sign to the random value:
         
         
         (if (< (random-uniform) .5)
           (+ (location-of dist) (* (scale-of dist) -1.0 z))
           (+ (location-of dist) (* (scale-of dist) z)))
         ))
     :dimensions (list n)
    )))
  
(defvar *gaussian-dist*
  NIL
  "An instance of a gaussian-dist representing the gaussian or normal distribution. ~
   This instance is used by the standard normal functions random-gaussian, ~
   quantile-gaussian, etc.")

(defun density-gaussian (x &key (location 0.0) (scale 1.0))
  "Returns the value of a location scale gaussian density at x. ~
   Location 0 and scale 1 define the standard gaussian distribution.  ~
   (:required ~
     (:arg x The point at which to evaluate the density.)) ~
   (:key ~
   (:arg location 0.0 The location of the gaussian density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.))"
  (declare (special *gaussian-dist*))
  (let ((saved-l (location-of *gaussian-dist*))
        (saved-s (scale-of *gaussian-dist*))
        result)
    (setf (location-of *gaussian-dist*) location)
    (setf (scale-of *gaussian-dist*) scale)
    (setf result (pdf-at *gaussian-dist* x))
    (setf (location-of *gaussian-dist*) saved-l)
    (setf (scale-of *gaussian-dist*) saved-s)
    result
    )
  )

(defun quantile-gaussian (p &key (location 0.0) (scale 1.0))
  
  "Returns the value of a location scale gaussian quantile at p. ~
   Location 0 and scale 1 define the standard gaussian distribution.  ~
   (:required ~
     (:arg p The probability at which to evaluate the quantile.)) ~
   (:key ~
   (:arg location 0.0 The location of the gaussian density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.))"
  (declare (special *gaussian-dist*))
  (let ((saved-l (location-of *gaussian-dist*))
          (saved-s (scale-of *gaussian-dist*))
          result)
      (setf (location-of *gaussian-dist*) location)
      (setf (scale-of *gaussian-dist*) scale)
      (setf result (quantile-at *gaussian-dist* p))
      (setf (location-of *gaussian-dist*) saved-l)
      (setf (scale-of *gaussian-dist*) saved-s)
      result))

(defun random-gaussian (&key (n 1) (location 0.0) (scale 1.0))
  "Returns n pseudo-random values from the location scale gaussian. ~
   Location 0 and scale 1 define the standard gaussian distribution.  ~
   (:key ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg location 0.0 The location of the gaussian density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *gaussian-dist*))
  (let ((saved-l (location-of *gaussian-dist*))
          (saved-s (scale-of *gaussian-dist*))
          result)
      (setf (location-of *gaussian-dist*) location)
      (setf (scale-of *gaussian-dist*) scale)
      (setf result (random-value *gaussian-dist* n))
      (setf (location-of *gaussian-dist*) saved-l)
      (setf (scale-of *gaussian-dist*) saved-s)
      result)
  )

(defun dist-gaussian (x &key (location 0.0) (scale 1.0))
  "Calculates and returns the value of the specified gaussian distribution ~
   function at x.  ~
   (i.e. prob(X <= x) .)  ~
   Location 0 and scale 1 define the standard gaussian distribution.  ~
   (:required ~
     (:arg x Where to evaluate the cumulative distribution function.)) ~
   (:key ~
   (:arg location 0.0 The location of the gaussian density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.)) "
  (declare (special *gaussian-dist*))
  (let ((saved-l (location-of *gaussian-dist*))
          (saved-s (scale-of *gaussian-dist*))
          result)
      (setf (location-of *gaussian-dist*) location)
      (setf (scale-of *gaussian-dist*) scale)
      (setf result (cdf-at *gaussian-dist* x))
      (setf (location-of *gaussian-dist*) saved-l)
      (setf (scale-of *gaussian-dist*) saved-s)
      result))
