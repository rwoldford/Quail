;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               location-scale.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1993
;;;   *** NB There is no protection against negative scale.
;;;          Should divides-object and minus-object be handled?
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(location-scale-dist location-of scale-of)))

(defclass location-scale-dist (prob-measure)
  ((location :initarg :location
             :accessor location-of
             :initform 0)
   (scale    :initarg :scale
             :accessor scale-of
             :initform 1)
   (standard-dist
    :accessor standard-dist-of
    :initarg :standard-dist
    :initform NIL))
  (:documentation
   "A location-scale transformation of an arbitrary distribution.  ~
   The distribution is stored here as the standard distribution. ~
   The location must be a finite number and the scale a positive ~
   number."))


(defmethod plus-object ((a number) (b prob-measure))
  (make-instance
    'location-scale-dist
    :location a
    :standard-dist b))

(defmethod plus-object ((a prob-measure) (b number))
  (plus-object b a))

(defmethod plus-object ((a number) (b location-scale-dist))
  (make-instance
    'location-scale-dist
    :location (+ a (location-of b))
    :scale (scale-of b)
    :standard-dist (standard-dist-of b)))

(defmethod plus-object ((a location-scale-dist) (b number))
  (plus-object b a))

(defmethod times-object ((a number) (b prob-measure))
  (make-instance
    'location-scale-dist
    :scale a
    :standard-dist b))

(defmethod times-object ((a prob-measure) (b number))
  (times-object b a))

(defmethod times-object ((a number) (b location-scale-dist))
  (make-instance
    'location-scale-dist
    :location (* a (location-of b))
    :scale (* a (scale-of b))
    :standard-dist (standard-dist-of b)))

(defmethod times-object ((a location-scale-dist) (b number))
  (times-object b a))

(defmethod pdf-at ((dist location-scale-dist) (x number))
  (let ((location (location-of dist))
        (scale (scale-of dist)))
    (/ (pdf-at (standard-dist-of dist)
               (/ (- x location) scale))
       scale)))

(defmethod cdf-at ((dist location-scale-dist) (x number))
  (let ((location (location-of dist))
        (scale (scale-of dist)))
    (cdf-at (standard-dist-of dist)
            (/ (- x location) scale))))

(defmethod quantile-at ((dist location-scale-dist) (x number) &key start)
  (declare (ignore start))
  (let ((location (location-of dist))
        (scale (scale-of dist)))
    (+ location
       (* scale
          (quantile-at (standard-dist-of dist) x)))))

(defmethod random-value ((dist location-scale-dist) &optional (n 1))
  (let ((location   (eref (location-of dist) 0))
        (scale      (eref (scale-of dist) 0)))
    (if (> n 1)
      (array
       (loop
         for i from 1 to n
         collect
         (with-CL-functions (+ *)
           (+ location
              (* scale
                 (random-value (standard-dist-of dist))))))
       )
      (with-CL-functions (+ *)
        (+ location
           (* scale
              (random-value (standard-dist-of dist)))))
      )))



(defmethod lower-bound-of ((dist location-scale-dist))
  (+ (location-of dist)
     (lower-bound-of (standard-dist-of dist))))

(defmethod upper-bound-of ((dist location-scale-dist))
  (+ (location-of dist)
     (upper-bound-of (standard-dist-of dist))))

(defmethod (setf lower-bound-of) (new-value (dist location-scale-dist))
  (declare (ignore new-value dist))
  (quail-error "Cannot reset the lower bound of a location-scale-dist"))

(defmethod (setf upper-bound-of) (new-value (dist location-scale-dist))
  (declare (ignore new-value dist))
  (quail-error "Cannot reset the upper bound of a location-scale-dist"))

(defmethod initialize-instance :after ((dist location-scale-dist) &rest initargs)
  (declare (ignore initargs))
  (if (<= (scale-of dist) 0)
    (quail-error "Cannot initialize the scale to be non-positive! ~&~
                  Here scale = ~s ."
                 (scale-of dist)))
  (if (or (< (location-of dist) (lower-bound-of dist))
          (> (location-of dist) (upper-bound-of dist)))
    (quail-error "~&Cannot initialize the location (~s) to be ~
                  outside the bounds of the support of the probability distribution! ~&~
                  lower-bound = ~s ~&~
                  upper-bound = ~s ."
                 (location-of dist) (lower-bound-of dist) (upper-bound-of dist)))
  dist)

#|
(defmethod (setf location-of) :around (new-value (self location-scale-dist))
  (setf (lower-bound-of self) (+ (lower-bound-of self)
                                 (- new-value (location-of self))))
  (setf (upper-bound-of self) (+ (upper-bound-of self)
                                 (- new-value (location-of self))))
  (call-next-method))

(defmethod (setf scale-of)  :around (new-value (self location-scale-dist))
  (cond
   ((or (not (numberp new-value)) (<= new-value 0))
    (quail-error "Scale must be a positive real number! ~&~
                  ~s is not."
                 (scale-of self)))
   (T 
    (setf (lower-bound-of self)
          (* (lower-bound-of self)
             new-value))
    (setf (upper-bound-of self)
          (* (upper-bound-of self)
             new-value))
    (call-next-method))))
|#
