;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              student.lisp                              
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
;;; written by Rupa and Rita and Dianne
;;; for R. W. Oldford

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(student df-of location-of scale-of density-student  quantile-student
          random-student dist-student)))

(defclass student (continuous-dist)
  ((df  :accessor df-of
             :initarg :df)
   (location :accessor location-of
             :initarg :location
             :initform 0)
   (scale    :accessor scale-of
             :initarg :scale
             :initform 1))
  (:documentation "The location-scale Student's t-distribution, ~
                   with location location-of, and scale scale-of.") 
  )
;;; Since the pdf of the student-t distribution cannot be integrated in closed form
;;; cdf-at use the incomplete-beta function. Quantile-at uses the Illinois function.
;;; The random-value was generated from a standard-gaussian and chi-squared distribution.

(defmethod cdf-at ((dist student) a)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline incomplete-beta))
  (with-CL-functions (+ * / - exp expt sqrt > < =)
    (let* ((df (df-of dist))
           (d (/ df 2))
           (y (/ (- a (eref (location-of dist) 0))
                 (eref (scale-of dist) 0)))
           (x (/ df (+ df (expt y 2)))))
      (if (< a 0)
        (* .5 (incomplete-beta d .5 x))
        (- 1 (* .5 (incomplete-beta d .5 x)))))))

(defmethod pdf-at ((dist student) a)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline log-gamma))
  (with-CL-functions (+ * / - exp expt sqrt > < =)
    (let* ((df (eref (df-of dist) 0))
           (half-df+1 (/ (+ 1.0 df) 2.0))
           (sigma (eref (scale-of dist) 0))
           (y (/ (- a (eref (location-of dist) 0)) sigma)))
      (* (/ 1 sigma)
         (/ (* (exp (log-gamma half-df+1))
               (expt (+ 1 (/ (expt y 2) df))
                     (-  half-df+1)))
            (* (sqrt df) (sqrt pi) (exp (log-gamma (/ df 2)))))))))

(defmethod quantile-at ((dist student) 
                        (percentile number)
                        &key (start NIL))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ * / - exp expt sqrt > < =)
    (let*
      ((location (eref (location-of dist) 0))
       (scale (eref (scale-of dist) 0))
       (pi/2 1.5707963267948966)
       (left? (if (<= percentile 0.5)
                T
                NIL))
       (p (if left?
                 (* percentile 2.0)
                 (* (- 1.0 percentile) 2.0)))
       (n (eref (df-of dist) 0))
       result improved-result)
      (cond
       ((= n 2)
        (setf result (sqrt (- (/ 2.0 (* p (- 2.0 p))) 2.0))))
       ((= n 1)
        (setf result (* p pi/2))
        (setf result (/ (cos result) (sin result))))
       ((< n 1)
        (quail-error
         "~&Degrees of freedom must be >= 1, not = ~s !" n))
       (T
        (let*
          ((a (/ 1.0 (- n 0.5)))
           (b (/ 48.0 (expt a 2)))
           (c (+ 96.36
                 (* a
                    (- (* a (- (/ (* 20700.0 a) b)) 98.0) 16.0))))
           (d (* (+ 1.0 (/ (- (/ 94.5 (+ b c)) 3.0) b))
                 (sqrt (* a pi/2))
                 n))
           (x (* d p))
           (y (expt x (/ 2.0 n)))
           )
          (cond
           ((> y (+ 0.05 a))
            (setf x (quantile-gaussian (* 0.5 p)))
            (setf y (expt x 2))
            (if (< n 5)
              (setf c (+ c (* 0.3 (- n 4.5) (+ x 0.6)))))
            (setf c
                  (+ (* x
                        (- (* x
                              (- (* x
                                    (- (* 0.05 d x)
                                       5.0))
                                 7.0))
                           2.0))
                     b
                     c))
            (setf y
                  (* x
                     (+ 1.0
                        (/
                         (-
                          (/
                           (+ 94.5
                              (* y
                                 (+ 36.0
                                    (* y
                                       (+ 6.3
                                          (* y 0.4))))))
                           c)
                          y 3.0)
                         b))))
            (setf y (* a y y))
            (if (> y 0.002)
              (setf y (- (exp y) 1.0))
              (setf y (+ y (* 0.5 y y)))))
           (T
            (setf
             y
             (+ (/ 1.0 y)
                (*
                 (/ (+ n 1.0) (+ n 2.0))
                 (- 
                  (* y
                     (+ (/ 0.5 (+ n 4.0))
                        (/ 1.0
                           (* 3.0
                              (+ n 2.0)
                              (- (/ (+ n 6.0)
                                    (* n y))
                                 (* 0.089 d)
                                 0.822)))))
                  1.0)
                 )))))
          (setf result (sqrt (* n y)))
          )
        )
       )
      ;; result is a rough approximation which can be improved as described
      ;; in Kennedy and Gentle as follows
      (flet
        ((improve-result
             (t1)
           (let*
             ((z (* 0.5 (- (*  2.0
                               (- 1.0 (dist-student t1 :df n)))
                           p)))
              (w (/ z (density-student t1 :df n)))
              (n+1 (+ n 1))
              (n+t2 (+ n (* t1 t1)))
              (psi (/ (* t1  n+1) n+t2))
              (psi-prime (/ (* n+1 (- n (* t1 t1)))
                            (* n+t2 n+t2))))
             (+ t1
                (* w
                   (+ 1.0
                      (* w
                         0.5
                         (+
                          psi
                          (* w
                             (/
                              (+ (* 2.0 psi psi)
                                 psi-prime)
                              3.0))))))))
           ))
        (setf improved-result (improve-result result))
        (if left?
          (setf improved-result (* -1.0 improved-result)))
        
        (+ location (* scale improved-result))
        )
      )
    )
  )


(defmethod random-value ((dist student) &optional (n 1))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
    (let ((df (df-of dist)))
      (+ (* (scale-of dist) (/ (random-gaussian :n n)
                               (sqrt (/ (random-chi :n n :df df)
                                        df))))
         (location-of dist))))



(defvar *student*
  NIL
  "An instance of a student representing the student distribution. ~
   This instance is used by the standard student functions random-student, ~
   quantile-student, etc.")


(defun density-student (x &key (df 3) (location 0.0) (scale 1.0))
  "Returns the density at x of a location scale student on df ~
   degrees of freedom. ~
   Location 0 and scale 1 define the standard student distribution.  ~
   (:required ~
   (:arg x The point at which to evaluate the density.)) ~
   (:key ~
   (:arg df 3 The degrees of freedom of the student distribution.  ~
   A positive number, the smaller are the degrees of freedom, ~
   the more probability lies in the symmetric tails of the ~
   student density.) ~
   (:arg location 0.0 The location of the student density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.))"
  (declare (special *student*))
  (if (and df (> df 0))
    (let ((saved-l (location-of *student*))
          (saved-s (scale-of *student*))
          (saved-df (df-of *student*))
          result)
      (setf (location-of *student*) location)
      (setf (scale-of *student*) scale)
      (setf (df-of *student*) df)
      (setf result (pdf-at *student* x))
      (setf (location-of *student*) saved-l)
      (setf (scale-of *student*) saved-s)
      (setf (df-of *student*) saved-df)
      result
      )
    (quail-error "Degrees of freedom  cannot be ~s for a student distribution!"
                 df))
  )

(defun quantile-student (p &key (df 3) (location 0.0) (scale 1.0))
  
  "Returns the quantile at p of a location scale student on df degrees ~
   of freedom. ~
   Location 0 and scale 1 define the standard student distribution.  ~
   (:required ~
   (:arg p The probability at which to evaluate the quantile.) ~
   ) ~
   (:key ~
   (:arg df 3 The degrees of freedom of the student distribution.  ~
   A positive number, the smaller are the degrees of freedom, ~
   the more probability lies in the symmetric tails of the ~
   student density.) ~
   (:arg location 0.0 The location of the student density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.))~
   (:references Based on the CACM algorithm 396 by G.W. Hill, 1970 and ~
   a one step update as recommended by Kennedy and Gentle's 1980 book ~
   Statistical Computing.)"
  (declare (special *student*))
  (if (and df (> df 0))
    (let ((saved-l (location-of *student*))
          (saved-s (scale-of *student*))
          (saved-df (df-of *student*))
          result)
      (setf (location-of *student*) location)
      (setf (scale-of *student*) scale)
      (setf (df-of *student*) df)
      (setf result (quantile-at *student* p))
      (setf (location-of *student*) saved-l)
      (setf (scale-of *student*) saved-s)
      (setf (df-of *student*) saved-df)
      result)
    (quail-error "Degrees of freedom  cannot be ~s for a student distribution!"
                 df))
  )

(defun random-student (&key (df 3) (n 1) (location 0.0) (scale 1.0))
  "Returns n pseudo-random values from the location scale student on df degrees ~
   of freedom. ~
   Location 0 and scale 1 define the standard student distribution.  ~
   (:key ~
   (:arg df 3 The degrees of freedom of the student distribution.  ~
   A positive number, the smaller are the degrees of freedom, ~
   the more probability lies in the symmetric tails of the ~
   student density.) ~
   (:arg n 1 The number of pseudo-random values to return.) ~
   (:arg location 0.0 The location of the student density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.)) ~
   (:returns A vector of n pseudo-random values.)"
  (declare (special *student*))
  (if (and df (> df 0))
    (let ((saved-l (location-of *student*))
          (saved-s (scale-of *student*))
          (saved-df (df-of *student*))
          result)
      (setf (location-of *student*) location)
      (setf (scale-of *student*) scale)
      (setf (df-of *student*) df)
      (setf result (random-value *student* n))
      (setf (location-of *student*) saved-l)
      (setf (scale-of *student*) saved-s)
      (setf (df-of *student*) saved-df)
      result)
    (quail-error "Degrees of freedom  cannot be ~s for a student distribution!"
                 df))
  )

(defun dist-student (x &key (df 3) (location 0.0) (scale 1.0))
  "Calculates and returns the value of the distribution function at x ~
   for a student on df degrees of freedom distribution. ~
   (i.e. prob(X <= x) .)  ~
   Location 0 and scale 1 define the standard student distribution.  ~
   (:required ~
   (:arg x Where to evaluate the cumulative distribution function.) ~
   ) ~
   (:key ~
   (:arg df 3 The degrees of freedom of the student distribution.  ~
   A positive number, the smaller are the degrees of freedom, ~
   the more probability lies in the symmetric tails of the ~
   student density.) ~
   (:arg location 0.0 The location of the student density.) ~
   (:arg scale 1.0  A scale parameter to adjust the spread of the density.)) "
  (declare (special *student*))
  (if (> df 0)
    (let ((saved-l (location-of *student*))
          (saved-s (scale-of *student*))
          (saved-df (df-of *student*))
          result)
      (setf (location-of *student*) location)
      (setf (scale-of *student*) scale)
      (setf (df-of *student*) df)
      (setf result (cdf-at *student* x))
      (setf (location-of *student*) saved-l)
      (setf (scale-of *student*) saved-s)
      (setf (df-of *student*) saved-df)
      result)
    (quail-error "Degrees of freedom  cannot be ~s for a student distribution!"
                 df))
  )
