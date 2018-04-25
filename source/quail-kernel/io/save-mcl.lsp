;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;                               save-mcl.lisp                               
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

(defun save (&optional file)
  "Saves the current Quail image in the named file. ~
   If no file named, user is prompted for name."
  (unless file
    (let ((release-info (get-quail-release-info)))
      (cond
       (release-info
        (let ((version (first release-info))
              (distribution (second release-info))
              )
          (if distribution
            (setf file (format NIL "Quail ~s (~a)" version distribution))
            (setf file (format NIL "Quail ~s" version))
            )
          )
        )
       (T (setf file "Quail"))
       )
      )
    )
  (ccl:save-application 
   (ccl:choose-new-file-dialog
    :prompt "Quail image location?"
    :directory file))
  )

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


(pushnew #'quail-save-exit 
         ccl:*save-exit-functions*)

