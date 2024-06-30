;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      cursor-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     R.W. Oldford 1991
;;;     G.W. Bennett 1996
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;------------------------------------------------------------------------
;;; Much experimenting to be done here!

(in-package :wb)
#|
(eval-when (:compile-toplevel :load-toplevel :execute)
 (export '(make-cursor with-cursor)))

(defun make-cursor (bitmap &key mask (hotspot (h-draw:make-point 0 0)))
   "Returns a cursor structure for use with with-cursor."
;;; There is such a function in CG =>
;;; (cg::make-cursor texture hot-spot mask)
;;; <hot-spot> is a position relative to the upper left corner of the
;;; cursor and <mask> is a bitmap indicating which points of the cursor
;;; should be opaque.
;;; <texture> is a texture which is a pixel-map or bitmap packages to
;;; work on a particular <device>
;;; See Ver2 Ch4 pp17 ff.         
   )
   |#               
