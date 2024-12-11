;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              editor-sblx.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(edit-file)))

;;; Enables a second copy of emacs to be opened as an editor running
;;; on its own stream, thus not interfering with the main repl  

(setf sb-ext:*ed-functions*
               (list
                (lambda (f)
                  (when (probe-file f)
                    (sb-thread::make-thread
                    (lambda () (sb-ext:run-program "emacs" (list f)
                                        :search t)))
                    ))))

;;; Opens that editor

(defun edit-file (&optional pathname)
  "Opens an edit window.  If pathname is given, it opens ~
   the window to edit the file named by pathname."
  (ed pathname)
  )
