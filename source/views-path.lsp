;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;                               views-path.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

 
(in-package :make)
 
(defun path-views ()
  (system-name-convert "Views;"))
 
(defun path-views-binary ()
  (system-name-convert "Views;"))

(defun path-views-utilities ()
  (system-name-convert "Utilities;"))

(defun path-views-macros ()
  (system-name-convert "Views-Macros;"))
 
(defun path-views-data ()
  (system-name-convert "Views-Data;"))
 
(defun path-views-mixins ()
  (system-name-convert "Views-Mixins;"))
 
(defun path-views-general ()
  (system-name-convert "Views-General;"))
 
(defun path-views-controls ()
  (system-name-convert "Controls;"))
 
(defun path-views-simple-views ()
  (system-name-convert "Simple-Views;"))
 
(defun path-views-dview-def ()
  (system-name-convert "Dview-Def;"))

(defun path-views-d-views ()
  (system-name-convert "D-Views;"))
 
(defun path-views-plots ()
  (system-name-convert "Plots;"))

(defun path-views-layout ()
  (system-name-convert "Layout;"))

(defun path-views-scroll ()
  (system-name-convert "Scroll;"))

(defun path-views-clone ()
  (system-name-convert "Clone;"))

(defun path-views-prompt-plot ()
  (system-name-convert "Prompt-Plot;"))
 

(defun path-views-display ()
  (system-name-convert "Display;"))

(defun path-views-other ()
  (system-name-convert "Other;"))
