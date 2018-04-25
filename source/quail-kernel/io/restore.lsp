;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               restore.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1991.
;;;
;;;  Mods:
;;;     R.W. Oldford 1991.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(add-restore-lisp-functions)))

;--------------------------------------------------------------------------------

(defparameter *quail-restore-lisp-functions* nil)

(defun quail-restore ()
  (mapcar #'funcall *quail-restore-lisp-functions*)
  nil)

(defun add-restore-lisp-functions (&rest functions)
  "Adds the given quail lisp functions to the set that will be ~
   restored when the current image is restarted."
  (setf *quail-restore-lisp-functions*
        (append *quail-restore-lisp-functions* functions)))
