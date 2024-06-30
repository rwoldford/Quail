;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           color-pc.lsp
;;;  Based on color-clx.lsp
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     H.A. Chipman  1991
;;;     C.B. Hurley   1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford  1989-1991
;;;     J.O. Pedersen 1988-89
;;;     G.W. Bennett  1996
;;;     
;;;----------------------------------------------------------------------------------

(in-package :wb)
(defun make-color (red-arg green-arg blue-arg)
  "Returns a colour record.  0 0 0 is black 1 1 1 is white ~
   (:required (:arg red-arg A number between 0 and 1.) ~
   (:arg green-arg A number between 0 and 1.) (:arg blue-arg A number between 0 and 1.))"
   (cg::make-rgb :red red-arg :green green-arg :blue blue-arg)
   )
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-color)))
;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*max-color-saturation*
           wb::make-color
           red-of green-of blue-of 
           colorp eq-colors order-colors
           prompt-user-for-color
           optimize-color-record
           deoptimize-color-record)))
;;;========================================================================
;;;                             Colors/Colours
;;;========================================================================
;;;  These follow a red green blue (RGB) model for representing colour.
(defvar *max-color-saturation* 255.0
  "The number which when given as the red green or blue of a colour ~
   saturates that dimension of the color in the host system.")

;;; An rgb triple is a STRUCTURE with name rgb and fields red, blue, green.
;;; So we have rgb-red rgb-green rgb-blue as well as rgb-p and make-rgb
;;; already available


(defun red-of (color)
  "Returns the red coordinate of the color."
  (declare (special *max-color-saturation*))
  (min 1.0
       (/
        (cg::rgb-red
         (if (cg::rgb-p color)
           color
           (deoptimize-color-record color)))
        *max-color-saturation*)
       ))

(defun green-of (color)
  "Returns the green coordinate of the color."
  (declare (special *max-color-saturation*))
  (min 1.0
       (/
        (cg::rgb-green
         (if (cg::rgb-p color)
           color
           (deoptimize-color-record color)))
        *max-color-saturation*)
       ))

 
(defun blue-of (color)
  "Returns the blue coordinate of the color."
  (declare (special *max-color-saturation*))
  (min 1.0
       (/
        (cg::rgb-blue
         (if (cg::rgb-p color)
           color
           (deoptimize-color-record color)))
        *max-color-saturation*)
       )) 
   
(defun colorp (color)
  "Test whether the argument is a color."
   (cg::rgb-p color))

(defun eq-colors (color-1 color-2)
  "Test whether two colors are equal.~
   Colors may be nil here"
  (cond
   ((and (null color-1) (null color-2))
    t)
   ((null color-1) nil)
   ((null color-2) nil)
   ((and (cg::rgb-p color-1) (cg::rgb-p color-2))
    (real-color-equal color-1 color-2))
   (T NIL)))

;;; There is a built-in rgb-equal
(defun real-color-equal (color-1 color-2)
   (cg::rgb-equal color-1 color-2))

(defun order-colors (colors)
  "Orders the colors in the list given"
  (flet
    ((lt-colors (color-1 color-2)
       (cond
        ((null color-2) t)
        ((null color-1) nil)
        ((<= (red-of color-1) (red-of color-2)) t)
        ((<= (green-of color-1) (green-of color-2)) t)
        ((<= (blue-of color-1) (blue-of color-2)) t))))   
    (sort colors #'lt-colors)))

;; The function pick-one is in pick-on1.lsp inside wb\prompts
(defun prompt-user-for-color ()
  "Prompt the user for a colour."
   (cg::ask-user-for-color)
   )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Optimized colours
;;; The following functions appear to do nothing to their arguments

(defun optimize-color-record (color)
  "Returns a data structure optimized for the current machine that ~
   represents the color."
  color)

(defun deoptimize-color-record (optimized-color)
  "Returns the color record from an optimized-color ~
   that was optimized for the current machine."
  optimized-color)
   
