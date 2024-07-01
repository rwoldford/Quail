;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                device-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     R.W. Oldford 1992
;;;     G.W. Bennett 1996
;;;     
;;;----------------------------------------------------------------------------------
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*color-available* color-available-p)))
;;; 
;;; *color-available* is a Mac Common Lisp variable that is T if
;;; the Mac has color and NIL otherwise.
;;; In window-basics, this information is stored as function.

;;; For the PC we are assuming that color is available

(defvar *color-available* T)

(defun color-available-p ()
  "Returns T if colour (or gray-scale) ~
   is available on the present monitor, NIL otherwise."
  T)
