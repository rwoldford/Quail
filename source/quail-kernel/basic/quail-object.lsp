;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-object.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.
;;;
;;;     mel, rwo touched in places
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          quail-object
;;;          proto-object
;;;          quail-class-of
;;;          quail-class-name
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(quail-object)))

(defclass quail-object ()
  ())

(push-extension-class 'quail-object)

;;;  Sometimes quail-objects are not what they appear to be.  These following
;;;  generic functions can be useful in clearing this up.  The major
;;;  application of this is for dimensioned-ref-objects.

(defgeneric quail-class-of (the-instance))

(defmethod quail-class-of ((self t))
  (class-of self))

(defgeneric quail-class-name (the-instance))

(defmethod quail-class-name ((self t))
  (class-name (quail-class-of self)))






