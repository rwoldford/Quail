;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          edit-file.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;      R.W. Oldford 1994
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(edit-file)))

(defun edit-file (&optional pathname)
  "Opens an edit window.  If pathname is given, it opens ~
   the window to edit the file named by pathname."
   (let ((fname (mk::force-to-pathname pathname)))
  (cond
   ((null fname) (ed))
   ((probe-file fname) (ed fname))
   (T (inform-user (format NIL "~%Sorry ~s was not found." fname))))))
