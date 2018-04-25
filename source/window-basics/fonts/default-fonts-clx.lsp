;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               default-fonts-clx.lisp
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
;;;     N.G. Bennett 1993
;;;     R.W. Oldford 1992
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
  (canvas-make-font :name "times" :size 7 ) ;;11)
  "Canvas font used for very small graphics characters.")

(defparameter *small-graphics-font*
  (canvas-make-font :name "times" :size 9) ;;14)
  "Canvas font used for small graphics characters.")

(defparameter *normal-graphics-font*
  (canvas-make-font :name "times" :size 12) ;;17)
  "Canvas font used for normal graphics characters.")

(defparameter *large-graphics-font*
  (canvas-make-font :name "times" :size 12 ;;20
                    :style :bold)
  "Canvas font used for small graphics characters.")

(defparameter *help-normal-text-font*
  (canvas-make-font :name "helvetica" :size 12) ;;14)
  "Canvas font used for normal text in help.")

(defparameter *help-small-text-font*
  (canvas-make-font :name "helvetica" :size 9);;11)
  "Canvas font used for small text in help.")

(defparameter *help-key-font*
  (canvas-make-font :name "helvetica" :size 9 ;;14
                    :style :bold)
  "Canvas font used for keys in help.")

(defparameter *help-little-key-font*
  (canvas-make-font :name "helvetica" :size 7 ;;11
                    :style :bold)
  "Small canvas font used for keys in help.")

(defparameter *help-lisp-font*
  (canvas-make-font :name "courier" :size 12) ;;14)
  "Canvas font used for displaying lisp code in help.")

(defparameter *help-normal-title-font*
  (canvas-make-font :name "helvetica" :size 12) ;;14)
  "Canvas font used for normal text in help title.")

(defparameter *help-small-title-font*
  (canvas-make-font :name "helvetica" :size 9) ;;11)
  "Canvas font used for small text in help title.")

(defparameter *help-lisp-title-font*
  (canvas-make-font :name "courier" :size 12 ;;14
                    :style :bold)
  "Canvas font used for displaying lisp code in help title.")
