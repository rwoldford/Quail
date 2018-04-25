;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               default.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*default-random-number-generator*)))



;;;--------------------------------------------------------------------------------
;;;
;;;  Need a system-wide default generator.
;;;  Change this as necessary
;;;
;;;--------------------------------------------------------------------------------

(defparameter *default-random-number-generator*
  (make-instance 'lewis-goodman-miller-gen)
  "The default random number generator to be used when none is specified.")
