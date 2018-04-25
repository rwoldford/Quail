;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               color-clx.lisp
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
;;;     H.A. Chipman  1991
;;;     C.B. Hurley   1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford  1989-1991
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)
(defun make-color (red green blue)
  "Returns a colour record.  0 0 0 is black 1 1 1 is white ~
   (:required (:arg red A number between 0 and 1.) ~
   (:arg green A number between 0 and 1.) (:arg blue A number between 0 and 1.))"
   (xlib:make-color :red red :green green :blue blue)
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
;;;
;;;========================================================================
;;;
;;;  These follow a red green blue (RGB) model for representing colour.
;;;
;;;


(defvar *max-color-saturation* 1.0
  "The number which when given as the red green or blue of a colour ~
   saturates that dimension of the color in the host system.")

(defun red-of (color)
  "Returns the red coordinate of the color."
   (xlib::color-red
    (if (colorp color)
      color
      (deoptimize-color-record color)))
  )

(defun green-of (color)
  "Returns the green coordinate of the color."
   (xlib::color-green 
    (if (colorp color)
      color
      (deoptimize-color-record color)))
  )
(defun blue-of (color)
   "Returns the blue coordinate of the color."
   (xlib::color-blue 
    (if (colorp color)
      color
      (deoptimize-color-record color)))
  )
(defun colorp (color)
  "Test whether the argument is a color."
   (xlib::color-p color))

(defun eq-colors (color-1 color-2)
  "Test whether two colors are equal.~
   Colors may be nil here"
  (cond
   ((and (null color-1) (null color-2))
    t)
   ((null color-1) nil)
   ((null color-2) nil)
   ((and (colorp color-1) (colorp color-2))
    (real-color-equal color-1 color-2))
   (T NIL)))

(defun real-color-equal (color-1 color-2)
   (multiple-value-bind (red1 green1 blue1) (xlib::color-rgb color-1)
      (multiple-value-bind (red2 green2 blue2) (xlib::color-rgb color-2)
		(and (= red1 red2) (= green1 green2) (= blue1 blue2)))))

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


(defun prompt-user-for-color ()
  "Prompt the user for a colour."
  (let
    ((colors 
      (list
       (cons "white" *white-color*)
       (cons "light gray" *light-gray-color*)
       (cons "gray" *gray-color*)
       (cons "dark gray" *dark-gray-color*)
       (cons "black" *black-color*)
       (cons "pink" *pink-color*)
       (cons "red" *red-color*)
       (cons "orange" *orange-color*)
       (cons "yellow" *yellow-color*)
       (cons "green" *green-color*)
       (cons "dark green" *dark-green-color*)
       (cons "light blue" *light-blue-color*)
       (cons "blue" *blue-color*)
       (cons "purple" *purple-color*)
       (cons "tan" *tan-color*)
       (cons "brown" *brown-color*)))
     )
    (cdr (pick-one colors :prompt-text "Pick a color."
                    :item-print-function
                    #'(lambda (&rest args)
                        (car (first args)))
                    )
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optimized colours
;;;

(defun optimize-color-record (color)
  "Returns a data structure optimized for the current machine that ~
   represents the color."
  color)

(defun deoptimize-color-record (optimized-color)
  "Returns the color record from an optimized-color ~
   that was optimized for the current machine."
  color)

