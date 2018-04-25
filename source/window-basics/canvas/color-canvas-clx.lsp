;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                         color-canvas-clx.lisp
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
;;;     R.W. Oldford 1994
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-color-canvas)))



;;; =======================================================================
;;;
;;;                     Creating a colour canvas
;;;
;;; =======================================================================



(defun make-color-canvas (&rest
                          canvas-keywords
                          &key
                          left bottom width height
                          ;;           (type (device-type))
                          (canvas-class 'canvas)
                          (title "Color Canvas")
                          (background-color NIL)
                          (pen-color NIL)
                          (pen-width nil)
                          (pen-operation nil)
                          (font *normal-graphics-font*)
                          &allow-other-keys)
  "Creates and returns a black and white canvas."
  
  (declare (special *normal-graphics-font*
                    *default-canvas-background-color*
                    *default-canvas-pen-color*
		    *white-color*
                    *black-color*))
  (unless left
    (setq left (round (/ (- (screen-width) width) 2))))
  
  (unless bottom
    (setq bottom (round (/ (- (screen-height) height) 2))))
  
  
  (let* ((top (+ bottom height))
	 (c (apply
             #'make-instance canvas-class
             :top (- (wb::screen-height) top)
             :left left
             :height height
             :width width
             :title title
             :color? T
             :font font
             :allow-other-keywords t
             canvas-keywords)))
    
    ;;
    ;; Now set the colors 
    ;;
    (canvas-set-background-color c (or background-color
                                       *default-canvas-background-color*))
    (canvas-set-pen c
                    :color (or pen-color
                               *default-canvas-pen-color*
                               (canvas-default-draw-color c))
                    :width pen-width 
                    :operation pen-operation)
    ;;
    ;; Finally get the pen back to the correct origin
    ;;
    
    (canvas-move-to c 0 0)
    c))

