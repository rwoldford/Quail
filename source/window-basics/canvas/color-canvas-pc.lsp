;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         color-canvas-pc.lsp
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
;;;     R.W. Oldford 1994
;;;     G.W. Bennett 1996
;;;     
;;;----------------------------------------------------------------------------------


(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-color-canvas)))
;;; =======================================================================
;;;                     Creating a colour canvas
;;; =======================================================================

(defmethod initialize-instance :after ((self pen-mixin)
                                       &rest initargs
                                       &key pen-color pen-width pen-operation)
  (declare (ignore initargs))
  (unless (pen-of self)
    (setf (slot-value self 'pen)
          (make-instance 'pen)))
  (canvas-set-pen self :width pen-width :operation pen-operation :color pen-color)
  )
;;; 05MAR2021 - END defun to replace the :after method of pen-mixin



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
     (when font 
          (setf canvas-keywords (remove :font canvas-keywords))
          (setf canvas-keywords (remove font canvas-keywords)))
  (let* ((top (+ bottom height))
 (c (apply #'cg::make-window  (gensym "canvas") :device canvas-class 
        :parent (cg::screen  cg::*system*) :scrollbars T
        :exterior (cg::make-box left (- (screen-height) top 25)
                     (+ left width) (- (screen-height) bottom 25))
       #| Lines below replaced with the defn of :exterior above
       :window-exterior-top-left (cg::make-position left 
                (- (screen-height) top 25)                                  
                                  )
       :visible-box (cg::make-box left (- bottom  height )
                                             (+ left width) 
                        bottom  )
       |#
             :title title
             :color? T
             :allow-other-keywords t
             canvas-keywords))
    )
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
    ;; Now set the fonts
    ;;
     (setf (canvas-font c) font) 
    ;; 
    ;;
    ;; Finally get the pen back to the correct origin
    ;;
    
    (canvas-move-to c 0 0)
    c))
