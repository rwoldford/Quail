;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          pen-mcl.lisp
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
;;;      R.W. Oldford 1989-1992
;;;     
;;;
;;;
;;;
(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*pen-operations*)))

(defconstant *pen-operations*
  (list :boole-1
	:boole-2
	:boole-andc1
	:boole-andc2
	:boole-and
	:boole-c1
	:boole-c2
	:boole-clr
	:boole-eqv
	:boole-ior
	:boole-nand
	:boole-nor
	:boole-orc1
	:boole-orc2
	:boole-set
	:boole-xor)
  "List of legal pen operations.")
