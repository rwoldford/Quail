;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             statistics-path.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun path-statistics ()
  (system-name-convert "Statistics;"))

(defun path-basic-statistics ()
  (system-name-convert "Basic-Statistics;"))

(defun path-stat-graphics ()
  (system-name-convert "Stat-Graphics;"))

(defun path-models ()
  (system-name-convert "Models;"))

(defun path-stat-sessions ()
  (system-name-convert "Stat-Sessions;"))
