;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           color-sblx.lsp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-color)))
;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*max-color-saturation*
           wb::make-color
           red-of green-of blue-of 
           colorp eq-colors order-colors
           prompt-user-for-color
           optimize-color-record
           deoptimize-color-record)))

(defun make-color (red-arg green-arg blue-arg)
  "Returns a colour record.  0 0 0 is black 1 1 1 is white ~
   (:required (:arg red-arg A number between 0 and 1.) ~
   (:arg green-arg A number between 0 and 1.) (:arg blue-arg A number between 0 and 1.))"
   (clim:make-rgb-color  red-arg  green-arg  blue-arg)
   )

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

#|
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
 |#  

(defun rgbs (color)
  "Returns a list of red, blue, green coordinates of color"
  (multiple-value-list (clim:color-rgb color)))

(defun red-of (color)
  "Returns the red coordinate of color"
  (first (rgbs color)))

(defun blue-of (color)
  "Returns the blue coordinate of color"
  (second (rgbs color)))

(defun green-of (color)
  "Returns the green coordinate of color"
  (third (rgbs color)))


(defun colorp (color)
  "Test whether the argument is a color."
  (if (eq (type-of color) 'CLIM-INTERNALS::NAMED-COLOR) T NIL)
   )


(defun eq-colors (color-1 color-2)
  "Test whether two colors are equal.~
   Colors may be nil here"
  (cond
   ((and (null color-1) (null color-2))
    t)
   ((null color-1) nil)
   ((null color-2) nil)
   ((and (eq (red-of color-1) (red-of color-2))
    (eq (blue-of color-1) (blue-of color-2))
      (eq (green-of color-1) (green-of color-2))
      T))
   (T NIL)))

#|
;;; There is a built-in rgb-equal
(defun real-color-equal (color-1 color-2)
   (cg::rgb-equal color-1 color-2))
|#

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

;;; fuction to select r,g,b values
(defun accepting-color (&key (stream *query-io*) (ow t))
  "Collects red, blue, green floating point values."
  (let (red-arg blue-arg green-arg)
    (accepting-values
  (stream :initially-select-query-identifier 'the-tag :own-window ow :label "Enter floats for r,g,b")
      (terpri stream)
      (setq red-arg (accept 'float :prompt "Single float for red-arg" :stream stream))
      (terpri stream)
      (setq blue-arg (accept 'float :prompt "Single-float for blue-arg"
           :query-identifier 'the-tag :stream stream))
      (terpri stream)
      (setq green-arg (accept 'float :prompt "Single-float for green-arg" :stream stream)))
    (multiple-value-list (values red-arg blue-arg green-arg))))

;;; make a custom color
(defun prompt-user-for-color ()
  "Prompts for r,g,b floating point values, normalises, makes a custom color."
  (let ((result-list (accepting-color)))
    (setf result-list
      (mapcar #'(lambda (x) (/ x (reduce #'+ result-list))) result-list))
    (make-color (first result-list) (second result-list) (third result-list))))

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
   
