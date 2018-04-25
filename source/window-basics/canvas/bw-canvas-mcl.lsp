;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               bw-canvas-mcl.lisp
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
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-b&w-canvas)))


;;;==============================================================================
;;;
;;;  The following function is called by make-canvas if color is not available
;;;  (i.e. specified)
;;;

(defun make-b&w-canvas (&rest canvas-keywords
                              &key left bottom 
                              (width (round (/ (screen-width) 2)))
                              (height (round (/ (screen-height) 2)))
                              (title "Canvas")
                              (canvas-class 'canvas)
                              (pen-color *black-shade*)
                              (pen-width nil)
                              (pen-operation nil)
                              (font *normal-graphics-font*))
  "Creates an returns a black and white canvas."
  (declare (special *normal-graphics-font*
                    *black-shade*))
  
  (unless left
    (setq left (round (/ (- (screen-width) width) 2))))
  
  (unless bottom
    (setq bottom (round (/ (- (screen-height) height) 2))))
  
  (let* ((w (apply
             #'make-instance canvas-class
             :view-position 
             (h-draw:make-point left (- (screen-height) bottom height))                                                
             :view-size (h-draw:make-point width height)
             :window-title title
             :window-show t
             :color? NIL
             :view-font (canvas-font-to-host-font font)
             :font font
             :window-type :document-with-zoom
             :close-box-p t
             :allow-other-keys t
             canvas-keywords)))
    
    ;;
    ;; Now set the pen
    ;;
    (canvas-set-pen w :color pen-color
                    :width pen-width :operation pen-operation)
    ;;
    ;; Finally get the pen back to the correct origin
    ;;
    (canvas-move-to w 0 0)
    w))
