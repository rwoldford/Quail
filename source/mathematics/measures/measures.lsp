;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               measures.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(defclass measure
  ((support :reader support-of
          :allocation :class
          :initform
          "Need a form to produce the set of values having positive probability mass."
          :documentation "Set of all values having positive probability mass."))
  ())

(defmethod measure ((set borel-set) (measure measure))
  (missing-method measure 'measure))

(defmethod measure ((measure measure) (set borel-set))
  (measure set measure))

(defclass lebesgue-measure
  (measure)
  ())

(defmethod measure ((set interval) (measure lebesgue-measure))
  (- (supremum set) (infimum set)))

(defclass counting-measure
  (measure)
  ())

(defmethod measure ((set countable-set) (measure counting-measure))
  (size set))

(defclass prob-measure (measure)
  ((support :reader support-of
          :allocation :class
          :initform
          "Need a form to produce the set of values having positive probability mass."
          :documentation "Set of all values having positive probability mass.")
   (distribution :initarg :cdf
                 :reader distribution-of)
   (density :initarg :pdf
            :reader density-of)
   (characteristic-function
    :initarg :c-theta
    :reader characteristic-function-of)))

(defmethod prob ((a borel-set) (dF prob-measure))
  (measure a dF))

(defmethod prob ((dF prob-measure) (a borel-set))
  (prob a dF))

(defmethod rand ((p prob-measure))
  (missing-method p 'rand))

(defmethod cdf ((self prob-measure) x)
  (declare (special -infinity))
  (prob (make-interval -infinity x) self))

(defmethod pdf ((self prob-measure) x)
  (missing-method self 'pdf))
