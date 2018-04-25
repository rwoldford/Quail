;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               number-of-elements.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1991 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     Michael Lewis 1991
;;;     Greg Anglin 1991
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(number-of-elements)))

(defgeneric number-of-elements (thing)
  (:documentation
   "The number of elements in thing."))

(defmethod number-of-elements ((thing symbol))
  1)

(defmethod number-of-elements ((thing number))
  1)

(defmethod number-of-elements ((thing sequence))
  (length thing))

(defmethod number-of-elements ((thing array))
  (array-total-size thing))

(defmethod number-of-elements ((thing dimensioned-ref-object))
  (array-total-size thing))
