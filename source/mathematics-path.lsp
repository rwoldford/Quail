;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mathematics-path.lisp                              
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

(in-package :make)

(defun path-mathematics ()
  (system-name-convert "Mathematics;"))

(defun path-special-functions ()
  (system-name-convert "Special-Functions;"))

(defun path-combinatorics ()
  (system-name-convert "Combinatorics;"))

(defun path-functions ()
  (system-name-convert "Functions;"))

(defun path-measures ()
  (system-name-convert "Measures;"))

(defun path-borel-sets ()
  (system-name-convert "Borel-Sets;"))


(defun path-calculus ()
  (system-name-convert "Calculus;"))

(defun path-root-finders ()
  (system-name-convert "Root-Finders;"))

(defun path-math-graphics ()
  (system-name-convert "Graphics;"))
