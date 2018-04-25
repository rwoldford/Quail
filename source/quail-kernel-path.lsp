;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-kernel-path.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1991.
;;;     R.W. Oldford 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :make)

(defun path-kernel ()
  (system-name-convert "quail-kernel;"))

(defun path-basic ()
  (system-name-convert "basic;"))

(defun path-io ()
  (system-name-convert "io;"))

(defun path-ref ()
  (system-name-convert "ref;"))

(defun path-math ()
  (system-name-convert "math;"))

(defun path-array ()
  (system-name-convert "array;"))

(defun path-test ()
  (system-name-convert "test;"))

(defun path-mop ()
  (system-name-convert "mop;"))
