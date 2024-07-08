;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               prob-methods.lisp                              
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
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)
;;;
;;;  Methods
;;;
(defmethod (setf scale-of) :around (new-value (dist T))
  (if (<= new-value 0.0)
    (quail-error "~&(SETF SCALE-OF): Scale must be a positive number! Not: ~s."
                 new-value)
    (call-next-method)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  pdf-at
;;;

(defmethod pdf-at ((p-m T) x)
  (missing-method 'pdf-at p-m x))

(defmethod pdf-at :around ((distribution T) (value number))
  (let ((l (lower-bound-of distribution))
        (u (upper-bound-of distribution)))
    (cond
     ((or (< value l) (> value u))    0)
     ((= l u value)                   1)
     (T                               (call-next-method))))
  )

(defmethod pdf-at :around ((distribution T) (value (eql NaN)))
  NaN)

(defmethod pdf-at :around ((distribution T) (value (eql +infinity)))
  (let ((l (lower-bound-of distribution))
        (u (upper-bound-of distribution)))
    (cond
     ((> value u)                     0)
     ((= l u value)                   1)
     (T                               (call-next-method)))))

(defmethod pdf-at :around ((distribution T) (value (eql -infinity)))
  (let ((l (lower-bound-of distribution))
        (u (upper-bound-of distribution)))
    (cond
     ((< value l)                     0)
     ((= l u value)                   1)
     (T                               (call-next-method)))))


(defmethod-multi pdf-at :around
  ((distribution prob-measure)
   (value (sequence array dimensioned-ref-object)))
  "This method captures the behaviour of pdf-at for any ~
   distribution when the values asked for are in a dimensioned-ref-object."
  (let ((result (make-dimensioned-result (dimensions-of value) value))
        (n (number-of-elements value)))
    ; (loop for i from 0 to (- n 1) do
    (do ((i 0 (incf i)))
        ((= i n))
      (setf (column-major-eref result i)
            (pdf-at distribution (column-major-eref value i))))
    result))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  cdf-at
;;;

(defmethod cdf-at ((p-m T) x)
  (missing-method 'cdf-at p-m x))

(defmethod cdf-at :around ((distribution T) (value number))
  (let ((l (lower-bound-of distribution))
        (u (upper-bound-of distribution)))
    (cond
     ((< value l)              0                 )
     ((>= value u)             1                 )
     (T                 (call-next-method)       ))))

(defmethod cdf-at :around ((distribution T) (value (eql NaN)))
  NaN)

(defmethod cdf-at :around ((distribution T) (value (eql +infinity)))
  (declare (ignorable distribution value)) ;(declare (ignore distribution value)) ; 31JUL2023
  1)

(defmethod cdf-at :around ((distribution T) (value (eql -infinity)))
  (let ((l (lower-bound-of distribution))
        (u (upper-bound-of distribution)))
    (cond
     ((< value l)               0                 )
     ((= value l u)             1                 )
     (T                 (call-next-method)        ))))


(defmethod-multi cdf-at  :around
  ((distribution prob-measure)
   (value (sequence array dimensioned-ref-object)))
  "This method captures the behaviour of cdf-at for any ~
   distribution when the values asked for are in a dimensioned-ref-object."
  (let ((result (make-dimensioned-result (dimensions-of value) value))
        (n (number-of-elements value)))
    ; (loop for i from 0 to (- n 1) do
    (do ((i 0 (incf i)))
        ((= i n))
      (setf (column-major-eref result i)
            (cdf-at distribution (column-major-eref value i))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  quantile-at
;;;

(defmethod quantile-at :around ((distribution T) (value number)
                                &key (start NIL))
  "Error checking on value being a probability."
  (declare (ignore start))
  (cond
   ((or (< value 0) (> value 1)) (quail-error "~s is not a probability!" value))
   ((= value 0) (lower-bound-of distribution))
   ((= value 1) (upper-bound-of distribution))
   (T (call-next-method))))

(defmethod quantile-at :around ((distribution T) (value (eql NaN))
                                &key (start NIL))
  "Error checking on value being a probability."
  (declare (ignore start))
  NaN)

(defmethod quantile-at :around ((distribution T) (value (eql +infinity))
                                &key (start NIL))
  "Error checking on value being a probability."
  (declare (ignore start))
  (quail-error "Not a probability! ~s " value))

(defmethod quantile-at :around ((distribution T) (value (eql -infinity))
                                &key (start NIL))
  "Error checking on value being a probability."
  (declare (ignore start))
  (quail-error "Not a probability! ~s " value))

(defmethod-multi quantile-at :around
  ((distribution prob-measure)
   (value (sequence array dimensioned-ref-object))
   &key start)
  "This method captures the behaviour of quantile-at for any ~
   distribution when the values asked for are in a dimensioned-ref-object."
  (let ((result (make-dimensioned-result (dimensions-of value) value))
        (n (number-of-elements value)))
    (if start
      (setf (column-major-eref result 0)
            (quantile-at distribution
                         (column-major-eref value 0)
                         :start start))
      (setf (column-major-eref result 0)
            (quantile-at distribution
                         (column-major-eref value 0))))
    (if (> n 1)
      ; (loop for i from 1 to (- n 1) do
      (do ((i 1 (incf i)))
          ((= i n))
        (setf (column-major-eref result i)
              (quantile-at distribution
                           (column-major-eref value i)
                           :start (column-major-eref result (- i 1))))))
    result))

(defmethod quantile-at ((p-m t) 
                        (p t) &key (start NIL))
  (declare (ignore start))
  (missing-method 'quantile-at p-m p)
  )

;; General random-value: return quantile-at

(defmethod random-value ((p-m t) &optional (n 1))
  "Uses a generalized probability integral transform from a uniform (0,1).~
   Depends on quantile-at returning a sensible value."
  (quantile-at p-m (random-uniform :n n))
  )

(defmethod probability ((distribution t) set)
  (missing-method 'probability distribution set))

(defmethod expectation ((p-m t) &optional g)
  (missing-method 'expectation p-m g))
