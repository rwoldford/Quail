;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               probability-path.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1993.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :make)

(defun path-probability ()
  (system-name-convert "Probability;"))

(defun path-generators ()
  (system-name-convert "Generators;"))

(defun path-distributions ()
  (system-name-convert "Distributions;"))

(defun path-random ()
  (system-name-convert "Random;"))
