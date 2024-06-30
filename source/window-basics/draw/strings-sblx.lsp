;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            strings-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     
;;;-------------------------------------------------------------------
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-string-width)))

#| acl version
(defun canvas-string-width (canvas string 
                            &key
                            (font (if canvas
                                     (canvas-font canvas)
                                     *normal-graphics-font*)))
   "Returns the width of the string displayed in the canvas according to ~
the characteristics of font.  If canvas is NIL, it determines the ~
width according to the font alone."
   (declare (special *normal-graphics-font*))
(cg::font-string-width (canvas-font-to-host-font font) string)
)
|#
;;; NOTE for compatibility with Q, the keyword used is font
;;; although its value is a text-style
;;; gwb 24JAN2021
(defun canvas-string-width (canvas string &key (font *default-text-style*))
	"returns the width of string on the pane of canvas ~
	with respect to  *default-text-style*. ~
	There could be an &k argument for text-style."
 (declare (ignorable font)) ;10MAY20204
	(let* ((mp (get-frame-pane canvas 'host-pane)))
		(stream-string-width mp string))) ;:font font))) mcclim does not support this arg