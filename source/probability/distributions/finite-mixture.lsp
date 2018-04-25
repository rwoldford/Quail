;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               finite-mixture.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Joe Leung 1992
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(finite-mixture mixing-probs-of distributions-of)))

(defclass finite-mixture (prob-measure)
  ((mixing-probs   :accessor mixing-probs-of
                   :initarg :mixing-probs
                   :initform NIL)
   (distributions  :accessor distributions-of
                   :initarg :distributions   
                   :initform NIL)
   (number         :accessor num-of
                   :initarg :num
                   :initform NIL))
  (:documentation "Finite discrete mixtures.")
  )
;;; *************************************************************************
;   Finite Discrete Mixtures
;
;   1. Parameters: p1, p2,...,pn denote the mixing probability 
;                                 where p1 + p2 +...+ pn = 1.
;                  f1, f2,...,fn denote the corresponding pdf.
;
;   2. pdf: f(x) = p1*f1(x) + p2*f2(x) + ... + pn*fn(x)
;
;   3. cdf: F(x) = p1*F1(x) + p2*F2(x) + ... + pn*Fn(x)
;
;   4. Randge: Suppose X1,X2,...,Xn denote the sets of random variable
;              space for the corresponding pdf's. Then, the space for 
;              the mixture is UNION of Xi, for i from  1 to n.
;
;   @written by Joe Leung (ID# 89115226)
;****************************************************************************


;------------------------------------------
;   PDF-AT
;------------------------------------------
(defmethod pdf-at ((dist finite-mixture) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (*)
    (loop for f in (distributions-of dist)
          as p in (mixing-probs-of dist)
          sum
          (* p (pdf-at f x))
          )  
    ))

;------------------------------------------
;   CDF-AT
;------------------------------------------
(defmethod cdf-at ((dist finite-mixture) (x number))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (*)
    (loop for f in (distributions-of dist)
          as p in (mixing-probs-of dist)
          sum
          (* p (cdf-at f x))
          ) 
    ))

;------------------------------------------
;   QUANTILE-AT
;   - For the quantile of the mixture, only
;     the pure mixture is allowed. That is,
;     the mixture with discrete variable 
;     only and with continuous variable only.
;   - Firstly, determine what type of the 
;     mixture is. This can be done by the
;     routines "all-discrete" and "all-con-
;     tinuous". Then , use the corresponding 
;     quantile-at routine to find the quantile
;     of the mixture.
;;;----------------------------------------

(defmethod quantile-at ((dist finite-mixture)
                        (p number)
                        &key (start NIL))
  (declare (ignore start)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (cond ((all-continuous dist) (Cquantile-at dist p))
        ((all-discrete dist) (Dquantile-at dist p))
        (T (quail-error "This program cannot handle the mixtures which include
                            discrete and continous random variates."))
        )
  )

(defmethod all-continuous ((dist finite-mixture))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let ((all-type-continuous T))
      (loop for f in (distributions-of dist)
            do
            (setf all-type-continuous
                  (and all-type-continuous 
                       (typep f 'continuous-dist)))
            )
      all-type-continuous
      ))

(defmethod all-discrete ((dist finite-mixture))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let ((all-type-discrete T))
      (loop for f in (distributions-of dist)
            do
            (setf all-type-discrete
                  (and all-type-discrete 
                       (typep f 'discrete-dist)))
            )
      all-type-discrete
      ))

(defmethod Dquantile-at ((dist finite-mixture) p)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (with-CL-functions (+ <= abs log)
    (let ((sum 0) (count 0))
      (loop for mp in (mixing-probs-of dist)
            do 
            (setf sum (+ sum mp))
            (when (<= p sum)
              (return (quantile-at (nth count (distributions-of dist)) p)))
            (setf count (+ count 1))
            )
      )
    ))


(defmethod Cquantile-at ((dist finite-mixture) p)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (multiple-value-bind (l u) (find-limits dist p)
    (bisection dist p :lower l :upper u))
  )
;------------------------------------------
;   RANDOM-VALUE
;   - to determine the random-value from
;     the mixture, we first determine the 
;     corresponding pdf in which the random
;     value should be from. This can be done
;     by invention method.
;------------------------------------------
(defmethod random-value ((dist finite-mixture) &optional (n 1))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let ((u-s (random-uniform :n n)))
    (array
     ; (loop for i from 0 to (- n 1) do
     (do ((i 0 (incf i)))
         ((= i n))
       (with-CL-functions ( - + <= )
         (let ((u (eref u-s i))
               (sum 0)
               (count 0))
           (loop
             for j from 0 to (- (length (distributions-of dist)) 1)
             do
             (setf sum (+ sum (nth j (mixing-probs-of dist))))
             (when (<= u sum)
               (return (random-value (nth count (distributions-of dist)) n)) 
               )
             (setf count (+ count 1)) 
             ))))
     )))

