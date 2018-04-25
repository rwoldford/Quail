;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               default-fonts-pc.lisp
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
   (canvas-make-font :name (cons :modern :times\ new\ roman) 
    :size 7 :style nil) ;;11)
   "Canvas font used for very small graphics characters.")

(defparameter *small-graphics-font*
   (canvas-make-font :name (cons :modern :times\ new\ roman) 
    :size 9 :style nil) ;;14)
   "Canvas font used for small graphics characters.")

(defparameter *normal-graphics-font*
   (canvas-make-font :name (cons :modern :times\ new\ roman)
    :size 12 :style nil) ;;17)
   "Canvas font used for normal graphics characters.")

(defparameter *large-graphics-font*
   (canvas-make-font :name (cons :modern :times\ new\ roman)
    :size 20 ;;20
    :style :bold)   
   "Canvas font used for large graphics characters.")

(defparameter *help-normal-text-font*
   (canvas-make-font :name (cons :swiss :courier)
    :size 10 :style nil) ;;14)
   "Canvas font used for normal text in help.")

(defparameter *help-small-text-font*
   (canvas-make-font :name (cons :swiss :courier)
    :size 9 :style nil);;11)
   "Canvas font used for small text in help.")

(defparameter *help-key-font*
  (canvas-make-font :name (cons :swiss :courier)
      :size 9 ;;14
      :style :bold)
  "Canvas font used for keys in help.")

(defparameter *help-little-key-font*
  (canvas-make-font :name (cons :swiss :courier)
      :size 7 ;;11
      :style :bold)
  "Small canvas font used for keys in help.")

(defparameter *help-lisp-font*
  (canvas-make-font :name (cons :swiss :courier)
      :size 12) ;;14)
  "Canvas font used for displaying lisp code in help.")

(defparameter *help-normal-title-font*
   (canvas-make-font :name (cons :swiss :courier)
      :size 12) ;;14)
   "Canvas font used for normal text in help title.")

(defparameter *help-small-title-font*
  (canvas-make-font :name (cons :swiss :courier)
      :size 9) ;;11)
  "Canvas font used for small text in help title.")

(defparameter *help-lisp-title-font*
  (canvas-make-font :name (cons :swiss :courier)
      :size 12 ;;14
      :style :bold)
  "Canvas font used for displaying lisp code in help title.")

(defparameter *prompt-normal-font*
  (canvas-make-font :name (cons :roman :times\ new\ roman)
    :size 12 :style NIL))
  "Canvas font used for prompts"
