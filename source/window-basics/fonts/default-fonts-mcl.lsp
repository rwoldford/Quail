;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               default-fonts-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1991
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*very-small-graphics-font* *small-graphics-font*
           *normal-graphics-font* *large-graphics-font*
           *help-normal-text-font* *help-small-text-font*
           *help-key-font* *help-little-key-font*
           *help-lisp-font* *help-normal-title-font* *help-small-title-font*
           *help-lisp-title-font*)))


(defparameter *very-small-graphics-font*
  (canvas-make-font :name "Monaco" :size 7)
  "Canvas font used for very small graphics characters.")

(defparameter *small-graphics-font*
  (canvas-make-font :name "Monaco" :size 9)
  "Canvas font used for small graphics characters.")

(defparameter *normal-graphics-font*
  (canvas-make-font :name "Monaco" :size 12)
  "Canvas font used for normal graphics characters.")

(defparameter *large-graphics-font*
  (canvas-make-font :name "Monaco" :size 12 :style :bold)
  "Canvas font used for small graphics characters.")

(defparameter *help-normal-text-font*
  (canvas-make-font :name "New York" :size 12)
  "Canvas font used for normal text in help.")

(defparameter *help-small-text-font*
  (canvas-make-font :name "New York" :size 9)
  "Canvas font used for small text in help.")

(defparameter *help-key-font*
  (canvas-make-font :name "New York" :size 9 :style :bold)
  "Canvas font used for keys in help.")

(defparameter *help-little-key-font*
  (canvas-make-font :name "New York" :size 7 :style :bold)
  "Small canvas font used for keys in help.")

(defparameter *help-lisp-font*
  (canvas-make-font :name "Courier" :size 12)
  "Canvas font used for displaying lisp code in help.")

(defparameter *help-normal-title-font*
  (canvas-make-font :name "New York" :size 12)
  "Canvas font used for normal text in help title.")

(defparameter *help-small-title-font*
  (canvas-make-font :name "New York" :size 9)
  "Canvas font used for small text in help title.")

(defparameter *help-lisp-title-font*
  (canvas-make-font :name "Courier" :size 12 :style :bold)
  "Canvas font used for displaying lisp code in help title.")
