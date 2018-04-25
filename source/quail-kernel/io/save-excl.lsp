;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;                               save-excl.lisp                               
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
;;;     Greg Anglin 1993.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(save save-quail-objects add-save-exit-functions)))

;--------------------------------------------------------------------------------

;; this is a quick and dirty version of save which doesn't do the
;; necessary stuff for setting up restart actions
(defun save (file &key (checkpoint NIL) &allow-other-keys)
  "Saves the current Quail image in the named file."
  (excl:dumplisp :name file :checkpoint checkpoint))

(defun save-quail-objects ()
  "Save all quail objects created in the current session."
  (quail-error "Sorry  don't know how to save your quail objects yet."))

(defparameter *quail-save-exit-functions* nil
  "A list of functions to be executed upon exit form lisp")

(defun quail-save-exit ()
  (mapcar #'funcall *quail-save-exit-functions*))

(defun add-save-exit-functions (&rest functions)
  "Adds the given quail lisp functions to the set that will be ~
   executed before the current image is saved."
  (setf *quail-save-exit-functions*
        (append *quail-save-exit-functions* functions)))


;;  To dodge problems while starting a port ... saving
;;  isn't usually a big issue just then ... Greg Anglin 93 12

#| Need a means like this in mcl to add to the save-exit-functions
(pushnew #'quail-save-exit 
         ccl:*save-exit-functions*)
|#

;; use warn here because quail-warn is undefined just yet ...
(warn 
 "Need to define *save-exit-functions* for this implementation.")

