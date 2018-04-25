;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               system.lisp                               
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

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(system-generator
          *CL-system-random-number-generator*
          )))



;;;-------------------------------------------------------------------------------------------
;;;
;;; System defined random number generator.  See CLtL for definition
;;; N.B. CLtL allows random to be IMPLEMENTATION DEPENDENT !!!!!!
;;;
;;;-------------------------------------------------------------------------------------------


(defclass system-generator
  (random-number-generator)
  ((random-state :reader random-state-of
                 :initarg :random-state
                 :initform (make-random-state t))
   (maximum-value :reader maximum-value-of
                  :initarg :maximum-value
                  :initform most-positive-fixnum)))


(defmethod next ((generator system-generator))
  (setf (current-value-of generator)
        (random (maximum-value-of generator)
                (random-state-of generator))))

(defmethod max-pseudo-rand-of ((generator system-generator))
  (maximum-value-of generator))
    


(defparameter *CL-system-random-number-generator*
  (make-instance 'system-generator)
  "The default random number generator used by this Common Lisp implementation.")


