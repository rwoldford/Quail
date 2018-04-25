;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               color-mcl.lisp
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

;;;
;;;  The following exist in ccl hence they are
;;;  shadowed in window-basics Shadowed in defpackage
#|
(shadow '(make-color *black-color* *white-color* *pink-color* *red-color*
          *orange-color* *yellow-color* *green-color* *dark-green-color*
          *light-blue-color* *blue-color* *purple-color* *brown-color*
          *tan-color* *light-gray-color* *gray-color* *dark-gray-color*)
        )
|#
(defun make-color (red green blue)
  "Returns a colour record.  0 0 0 is black 1 1 1 is white ~
   (:required (:arg red A number between 0 and 1.) ~
   (:arg green A number between 0 and 1.) (:arg blue A number between 0 and 1.))"
  (declare (special *max-color-saturation*))
  (ccl::make-color 
   (truncate (* *max-color-saturation* red))
   (truncate (* *max-color-saturation* green))
   (truncate (* *max-color-saturation* blue))
   ))

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


(defvar *max-color-saturation* 65280
  "The number which when given as the red green or blue of a colour ~
   saturates that dimension of the color in the host system.")



(defun red-of (color)
  "Returns the red coordinate of the color."
  (declare (special *max-color-saturation*))
  (min 1.0
       (/
        (ccl::color-red
         (if (colorp color)
           color
           (deoptimize-color-record color)))
        *max-color-saturation*)
       ))

(defun green-of (color)
  "Returns the green coordinate of the color."
  (declare (special *max-color-saturation*))
  (min 1.0
       (/
        (ccl::color-green 
         (if (colorp color)
           color
           (deoptimize-color-record color)))
        *max-color-saturation*)))

(defun blue-of (color)
  "Returns the blue coordinate of the color."
  (declare (special *max-color-saturation*))
  (min
   (/
    (ccl::color-blue 
     (if (colorp color)
       color
       (deoptimize-color-record color)))
    *max-color-saturation*)
   1.0))

(defun colorp (color)
  "Test whether the argument is a color."
  ;; colors are encoded as fixnums in MCL
  (and (integerp color)
       (<= color most-positive-fixnum)))


(defun eq-colors (color-1 color-2)
  "Test whether two colors are equal.~
   Colors may be nil here"
  (cond
   ((and (null color-1) (null color-2))
    t)
   ((null color-1) nil)
   ((null color-2) nil)
   ((and (colorp color-1) (colorp color-2))
    (ccl::real-color-equal color-1 color-2))
   (T NIL)))


(defun order-colors (colors)
  "Orders the colors in the list given"
  (flet
    ((lt-colors (color-1 color-2)
       (cond
        ((null color-2) t)
        ((null color-1) nil)
        (t (<= color-1 color-2)))))
    
    (sort colors #'lt-colors)))


(defun prompt-user-for-color ()
  "Prompt the user for a colour."
  (user-pick-color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optimized colours
;;;

(defun optimize-color-record (color)
  "Returns a data structure optimized for the current machine that ~
   represents the color."
  (declare (inline ccl::color-to-rgb)
           (type fixnum color))
  (ccl::color-to-rgb color))

(defun deoptimize-color-record (optimized-color)
  "Returns the color record from an optimized-color ~
   that was optimized for the current machine."
  (declare (inline ccl::rgb-to-color))
  (ccl::rgb-to-color optimized-color))

