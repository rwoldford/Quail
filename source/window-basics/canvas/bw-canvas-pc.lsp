;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         bw-canvas-pc.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-b&w-canvas)))
;;; =======================================================================
;;;                     Creating a black and white canvas
;;; =======================================================================
(defun make-b&w-canvas (&rest
                          canvas-keywords
                          &key
                          left bottom width height
                          ;;           (type (device-type))
                          (canvas-class 'canvas)
                          (title "Canvas")
                          (pen-color *black-shade*)
                          (pen-width nil)
                          (pen-operation nil)
                          (font *normal-graphics-font*)
                          &allow-other-keys)
   "Creates and returns a black and white canvas."
   (declare (special *normal-graphics-font*
               *black-shade* c))
   (unless left
      (setq left (round (/ (- (screen-width) width) 2))))
   (unless bottom
      (setq bottom (round (/ (- (screen-height) height) 2))))
     (when font
          (setf canvas-keywords (remove :font canvas-keywords))
          (setf canvas-keywords (remove font canvas-keywords)))
   (let* ((top (+ bottom height))
          (c (apply #'cg::make-window (gensym "canvas") :device 'canvas-class 
              :parent (cg::screen cg::*system*) :scrollbars T
                 :window-exterior-top-left (cg::make-position left
                                                            (- (screen-height) top 25))
              :title title 
                :visible-box (cg::make-box left (- bottom height)
                                             (+ left width) bottom)
                              :color? nil
              :allow-other-keywords t
              canvas-keywords))
             ))
      
      ;;
      ;; Now set the pen
      ;;
      ;;    (canvas-set-pen c :color pen-color :width pen-width :operation pen-operation)
      ;;
      ;; Finally get the pen back to the correct origin
      ;;
      
      ;;    (canvas-move-to c 0 0)
      c)
