;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;                               save-sblx.lsp                               
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
;;;     Greg Bennett 1998, 2017
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(save save-quail-objects add-save-exit-functions)))

;--------------------------------------------------------------------------------
;;; A text form to get a pathname to a file
;;; This will be changed when we have a graphical interface.
;;; gwb 20APRIL2018

(defun ask-for-save-path ()
  (format t "~%The image will besaved under disrectory ~s " (user-homedir-pathname))
  (format t "~%Please enter a string for the path. \"ZZ\" to exit ~%")
  (let ((result (read)))
    (if (string= result "ZZ")
      (return-from ask-for-save-path "Exiting")
      (merge-pathnames result (user-homedir-pathname))))
      )

(defun save (&optional file)
   "Saves the current Quail image in the named file.  ~
   If no file named, user is prompted for name."
   (unless file
      (let ((release-info (get-quail-release-info)))
         (cond
          (release-info
           (let ((version (first release-info))
                 (distribution (second release-info))
                 )
              (if distribution
                 (setf file (format NIL "Quail~s(~a)" version distribution))
                 (setf file (format NIL "Quail~s" version))
                 )
              )
           )
          (T (setf file "Quail"))
          )
         )
      )
   (let ((image-loc (ask-for-save-path)
    ;(cg::ask-user-for-new-pathname 
    ;                 "Quail image location?"
    ;                 :initial-name (format NIL "~a.dxl" file))
   )
         )
      ;(apply #'excl::dumplisp (list ':name image-loc))
      (sb-ext::save-lisp-and-die image-loc)
      )
   )

(defun save-quail-objects ()
  "Save all quail objects created in the current session."
  (quail-error "Sorry  don't know how to save your quail objects yet."))

(defparameter *quail-save-exit-functions* nil
  "A list of functions to be executed upon exit form lisp")

(defun quail-save-exit (&rest args)
   (declare (ignore args))
  (mapcar #'funcall *quail-save-exit-functions*))

(defun add-save-exit-functions (&rest functions)
  "Adds the given quail lisp functions to the set that will be ~
   executed before the current image is saved."
  (setf *quail-save-exit-functions*
        (append *quail-save-exit-functions* functions)))

#|
;;; Don't know what these are yet
(pushnew #'quail-save-exit
          sys:*exit-cleanup-forms* ;(cg::session-exit-functions cg::*system*) spr29023
         )
|#
