;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                Quail-init.lsp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1997 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1997.
;;;
;;;
;;;----------------------------------------------------------------------------

(in-package :quail-user)

;;; This file, if found in the same directory as the Quail image with name
;;; "Quail-init.lsp", is loaded immediately when Quail is run.  It may be
;;; used to modify Quail environment variables, add search directories to 
;;; Quail search lists.

(if (probe-file "q:comments;whats-new.lsp")
  (load "q:comments;whats-new.lsp"))

