;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               default-fonts-sblx.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     N.G. Bennett 1993
;;;     R.W. Oldford 1992
;;;     G.W. Bennett 1996
;;;     
;;;----------------------------------------------------------------------------------
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*very-small-graphics-font* *small-graphics-font*
           *normal-graphics-font* *large-graphics-font*
           *help-normal-text-font* *help-small-text-font*
           *help-key-font* *help-little-key-font*
           *help-lisp-font* *help-normal-title-font* *help-small-title-font*
           *help-lisp-title-font*)))

;;; Most of the :name values should be faces, I think
;;; Thus times <-> times\ new\ roman
;;; courier <-> courier but helvetica <-?-> ms\ sans\ serif
;;; swiss is the name of a FAMILY
;;; See p7-8 of Volume 2 of ACLPC2.0

;;; for small things we could use small\ fonts

;;; and the sizes should be the ;;nn values
;;; For now only the sizes have been done

(defparameter *very-small-graphics-font*
   (canvas-make-font :name :sans-serif;(cons :modern :times\ new\ roman) 
    :size :very-small :style :roman) ;;11)
   "Canvas font used for very small graphics characters.")

(defparameter *small-graphics-font*
   (canvas-make-font :name :sans-serif;(cons :modern :times\ new\ roman) 
    :size :small :style :roman) ;;14)
   "Canvas font used for small graphics characters.")

(defparameter *normal-graphics-font*
   (canvas-make-font :name :sans-serif;(cons :modern :times\ new\ roman) 
    :size :normal :style :roman) ;;17)
   "Canvas font used for normal graphics characters.")

(defparameter *large-graphics-font*
   (canvas-make-font :name :sans-serif;(cons :modern :times\ new\ roman) 
    :size :large :style :roman)   
   "Canvas font used for large graphics characters.")

(defparameter *help-normal-text-font*
   (canvas-make-font :name nil;(cons :modern :times\ new\ roman) 
    :size :normal :style :roman);;14)
   "Canvas font used for normal text in help.")

(defparameter *help-small-text-font*
   (canvas-make-font :name nil;(cons :modern :times\ new\ roman) 
    :size :small :style :roman);;11)
   "Canvas font used for small text in help.")

(defparameter *help-key-font*
  (canvas-make-font :name nil;(cons :modern :times\ new\ roman) 
    :size :normal :style :bold)
  "Canvas font used for keys in help.")

(defparameter *help-little-key-font*
  (canvas-make-font :name nil;(cons :modern :times\ new\ roman) 
    :size :small :style :bold)
  "Small canvas font used for keys in help.")

(defparameter *help-lisp-font*
  (canvas-make-font :name :serif;(cons :modern :times\ new\ roman) 
    :size :normal :style :fix) ;;14)
  "Canvas font used for displaying lisp code in help.")

(defparameter *help-normal-title-font*
   (canvas-make-font :name nil;(cons :modern :times\ new\ roman) 
    :size :normal :style :roman) ;;14)
   "Canvas font used for normal text in help title.")

(defparameter *help-small-title-font*
  (canvas-make-font :name nil;(cons :modern :times\ new\ roman) 
    :size :small :style :roman) ;;11)
  "Canvas font used for small text in help title.")

(defparameter *help-lisp-title-font*
  (canvas-make-font :name :fix;(cons :modern :times\ new\ roman) 
    :size :normal :style :roman)
  "Canvas font used for displaying lisp code in help title.")

#|
(defparameter *prompt-normal-font*
  (canvas-make-font :name nil;(cons :modern :times\ new\ roman) 
    :size :normal :style :roman)
  "Canvas font used for prompts")
|#