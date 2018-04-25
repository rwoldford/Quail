;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-save-old.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990.          [ Incomplete ]
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(save save-loop)))

;--------------------------------------------------------------------------------

;;  *save-dir* is defined in "q;Source:Kernel:IO:quail-file.lisp"

(defvar *save-stream*)

(defun set-save-dir (&optional (dir *save-dir-default*))
  (setf *save-dir* (make-pathname :directory dir)))

#|
(defun save (&key (dir *save-dir*))
  (declare (special *save-dir* *save-stream*))
  )
|#

(defun save-loop (&optional (stream *quail-terminal-io*))
  (let* ((sym (list-owned-symbols 'quail))
         (var-p (mapcar #'boundp sym))
         (sym (list-if-t sym var-p))
         (var (mapcar #'eval sym)))
    (loop for v in var
          as  s in sym
          do (output-make-instance stream v s)
             (format stream "~%~%")))
  (values))



