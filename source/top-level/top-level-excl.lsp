;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              top-level-excl.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1991.
;;;
;;;--------------------------------------------------------------------------------


(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*system-top-level-loop-function* install-top-level current-top-level
          get-next-form get-function-name setup-system-top-level-loop-variable)))

(defvar *system-top-level-loop-function* NIL
  "Variable containing the stack of top-level loop functions.")

(warn "Top level loop for Quail has not yet been ported.")

(defun setup-system-top-level-loop-variable ()
  (declare (special *system-top-level-loop-function*))
  NIL)
;;;----------

(eval-when (load eval)
  (setup-system-top-level-loop-variable))

;;;;;;;;;;;;
;;;
;;;

(defun install-top-level (&optional
                          (top-level *system-top-level-loop-function*))
  "Installs the top-level function, if provided.  Default ~
   is the one supplied with the system."
  (declare (special *system-top-level-loop-function*))
  NIL)

(defun current-top-level ()
  "Returns the current top-level loop function."
  NIL)

(defun get-next-form ()
  "Retrieves the next form to be evaluated."
  (declare (special *forms-awaiting-evaluation*))
  NIL)

(defun get-function-name (function)
  NIL)

;;;(defun set-listener-package (&optional (package :cl-user))
;;;  NIL)

(defun system-quit-lisp ()
  "System dependent function that quits Lisp."
  :exit)
