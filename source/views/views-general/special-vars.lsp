;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               special-vars.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*color-menu-list* *shade-menu-list* nil
           *default-point-color*  *default-axis-color*
           *default-bar-color* *default-curve-color*
           *default-label-color* *default-highlight-color*
           *point-symbol-types* *default-point-size* *default-point-symbol*
           *default-label-font* *default-menu-font* 
           *variable-transforms*
           *default-brush-width* *default-brush-height*
           *view-fonts* new-font-size 
           color-to-string
           *default-draw-rate*
           *variable-summaries* 
           )))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(set-up-default-window-region)))

(defvar *color-menu-list*)

(setq *color-menu-list*
  `(("red" ,wb::*red-color*)
    ( "blue" ,wb::*blue-color*)
    ( "green" ,wb::*green-color*)
    ( "yellow" ,wb::*yellow-color*)
    ( "magenta" ,wb::*magenta-color*)
    ( "black" ,wb::*black-color*)
    ("light gray" ,wb::*light-gray-color*)
    ("gray" ,wb::*gray-color*)
    ("dark gray" ,wb::*dark-gray-color*)
    ( "white" ,wb::*white-color*)
    ("Other" :prompt)
    ))


(defvar *shade-menu-list* 
  `(("light gray" ,wb::*light-gray-color*)
    ("gray" ,wb::*gray-color*)
    ("dark gray" ,wb::*dark-gray-color*)
    ( "black" ,wb::*black-color*)
    ( "white" ,wb::*white-color*)
    ("Other" :prompt)))






(defvar *default-point-color*  nil)

(defvar *default-point-fill?* t)


(defvar *default-axis-color* nil)

  
(defvar *default-bar-color* nil)
(defvar *default-bar-fill?* nil)

(defvar *default-pie-color* nil)
(defvar *default-pie-fill?* t)

(defvar *default-curve-color* nil)
(defvar *default-label-color* nil)

(defvar *default-slider-color* nil)
(defvar *default-highlight-color* wb::*magenta-color*)

(defvar *control-button-color* wb::*default-canvas-background-color*)
(defvar *control-button-bg-color* nil)


(defvar *default-highlight-operation* :boole-xor)

(defvar *auto-draw-plot?* t)
(defvar *auto-draw-view?* nil)

(defvar *point-symbol-types* )
(setq *point-symbol-types* 
  '(:box :circle :cross :diamond 
    :star :triangle :poly-star))
(defvar *default-point-size*) 
(setq *default-point-size* 4)


(defvar *default-point-symbol*  :box )
(defvar *default-label-font* wb::*small-graphics-font*)
(defvar *default-menu-font* wb::*very-small-graphics-font*)

(defun sqr (x) (* x x))
(defun inverse(x) (/ 1.0 x))
(defvar *variable-transforms* )
(setq *variable-transforms* '( sqr identity sqrt log inverse :prompt))

(defvar *variable-summaries* )
(setq *variable-summaries* '( mean median sd iqr  :prompt))


(defvar *default-brush-width* 16)
(defvar *default-brush-height* 16)

(defvar *view-fonts* (list wb::*very-small-graphics-font* wb::*small-graphics-font*
                       wb::*normal-graphics-font* wb::*large-graphics-font*))



(defvar *default-window-region* NIL
  ;;(make-region 10 300 10 300)
  "Set this to nil for and make-view-window queries user for window location.  ~
   Otherwise set to a region where the window will appear.")

(defun set-up-default-window-region (&optional (region (make-region 10 300 10 300)))
  (setf *default-window-region*  region))


(defun new-font-size (old-font size)
  (let ((font (wb::copy-canvas-font old-font))
        (old-size (wb::canvas-font-size old-font)))
    
    (if (eq size :bigger)
      (setq size (+ 2 old-size))
      (if (eq size :smaller)
        (setq size (- old-size 2))))
    (if (numberp size)
      (wb::set-canvas-font-size font size))
    font))
      

(defvar *default-data-eq* #'eq-dataset)




(defvar *simple-view-menu-list*
  '((ARROW  "Arrow")
    (LINE-WMENU "line" )
    (ORIENTED-LINE-WMENU "oriented-line" )
    (LABEL "label") 
    (POINT-SYMBOL "point-symbol")
    (RECTANGLE "rectangle") 
    (PIE "pie")   
    (TEXT-VIEW "text-view") 
    (FUNCTION-VIEW "function-view")))


(defun color-to-string(color)
  (cond ((null color) "default")
        ((wb::colorp color)
         
         (cond ((wb::eq-colors color wb::*red-color*) "red")
               ((wb::eq-colors color wb::*blue-color*) "blue")
               ((wb::eq-colors color wb::*green-color*) "green")
               ((wb::eq-colors color wb::*yellow-color*) "yellow")
               ((wb::eq-colors color wb::*magenta-color*) "magenta")
               ((wb::eq-colors color wb::*black-color*) "black")
               ((wb::eq-colors color wb::*gray-color*) "gray")
               ((wb::eq-colors color wb::*light-gray-color*) "light gray")
               ((wb::eq-colors color wb::*dark-gray-color*) "dark-gray")
               ((wb::eq-colors color wb::*white-color*) "white")
               (t "unknown")))
        (t
         
         (cond ((wb::eq-patterns color wb::*black-shade*) "black")
               ((wb::eq-patterns color wb::*gray-shade*) "gray")
               ((wb::eq-patterns color wb::*light-gray-shade*) "light gray")
               ((wb::eq-patterns color wb::*dark-gray-shade*) "dark-gray")
               ((wb::eq-patterns color wb::*white-shade*) "white")
               (t "unknown")))))






(defvar  *default-draw-rate* 150
  "The approximate number of frames per second in moving views")


