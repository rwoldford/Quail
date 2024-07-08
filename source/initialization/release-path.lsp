;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               release-path.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(defun quail-release-file ()
  (merge-pathnames
              (cl-user::path-quail) ;cl-user::*quail-init-directory*
   "release-info.lsp"))
