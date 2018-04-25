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
;;;     R.W. Oldford 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :window-basics)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (import '(quail-kernel:add-restore-lisp-functions)))

;--------------------------------------------------------------------------------

#| Following is unnecessary unless the quail-kernel package is not loaded.

(defparameter *window-basics-restore-lisp-functions* nil)

(defun window-basics-restore ()
  (mapcar #'funcall *window-basics-restore-lisp-functions*))


(defun add-restore-lisp-functions (&rest functions)
  "Adds the given quail lisp functions to the set that will be ~
   restored when the current image is restarted."
  (setf *window-basics-restore-lisp-functions*
        (append *window-basics-restore-lisp-functions* functions)))
|#
