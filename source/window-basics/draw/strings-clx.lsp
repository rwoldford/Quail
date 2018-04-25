;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               strings-clx.lisp
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
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)


(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-string-width)))

(defun canvas-string-width (canvas string 
                                   &key
                                   (font (if canvas
                                           (canvas-font canvas)
                                           *normal-graphics-font*)))
  "Returns the width of the string displayed in the canvas according to ~
   the characteristics of font.  If canvas is NIL, it determines the ~
   width according to the font alone."
  (declare (special *normal-graphics-font*))
  (xlib::text-extents (canvas-font-to-host-font font) string))

#|   
(defmethod ccl::stream-tyo ((view canvas) char)
  (stream-tyo (the-scroller view) char))
|#
