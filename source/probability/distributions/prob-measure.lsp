;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               prob-measure.lisp                              
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(prob-measure lower-bound-of upper-bound-of
          pdf-at cdf-at random-value quantile-at probability expectation)))

(defclass prob-measure
  (quail-object)
  ((lower-bound :accessor  lower-bound-of
                :initarg  :lower-bound
                :initform -infinity)
   (upper-bound  :accessor  upper-bound-of
                 :initarg  :upper-bound
                 :initform +infinity))
  (:documentation "A generic probability distribution.")
  )


(defmethod initialize-instance :after ((self prob-measure)
                                       &key lower-bound upper-bound)
  (setf lower-bound (or lower-bound (lower-bound-of self)))
  (setf upper-bound (or upper-bound (upper-bound-of self)))
  (unless (and lower-bound
               upper-bound
               (< (eref lower-bound 0)
                  (eref upper-bound 1)))
    (quail-error "Initialization error: Upper-bound = ~s <= lower-bound = ~s."
                 upper-bound lower-bound))
  self)

;;;
;;;   Generic functions
;;;

(defgeneric pdf-at (distribution value)
  (:documentation "Probability density function (or probability function) ~
                   of distribution evaluated at value"))

(defgeneric cdf-at (distribution value)
  (:documentation "Cumulative distribution function of distribution evaluated ~
                   at value"))

(defgeneric random-value (distribution &optional n)
  (:documentation "Produce n random values from distribution.~
                   Default is n=1."))

(defgeneric quantile-at (distribution value &key start)
  (:documentation "Quantile of distribution at value.  Because this is often ~
                   determined  by finding the root of an equation, the keyword ~
                   start gives a starting value for any iterative root finder ~
                   that is used."))

(defgeneric probability (distribution set)
  (:documentation "The probability of the set for this distribution."))

(defgeneric expectation (distribution &optional g)
  (:documentation "Expectation of a function g with respect to the given distribution. ~
                   If function is absent the mean of the distribution is returned."))
