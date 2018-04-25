;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               fixes.lsp                            
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1995 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1995.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 
;;;
;;;  This file sets up some MCL specific fixes.
;;;

(in-package :quail)

(export '(install-fixes))

(defun install-fixes ()
  "Installs the fixes to known bugs."
  #+:ccl (load "q:comments;fixes-mcl.lsp")
  #-:ccl (print "No fixes available."))
