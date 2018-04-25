;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       load-quail-init.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1993.
;;; 
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(defun load-quail-init ()
  (let ((file (probe-file (cl-user::quail-init-file))))
    (when file
      (load file))))

;; pushed onto the restore functions in restored-lisp-functions.lsp