;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          start-windows-mcl.lisp
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
;;;     R.W. Oldford 1994
;;;     
;;;

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(start-windows stop-windows)))

(defun start-windows ()
  "Starts up the windowing environment for this implementation. ~
   Does nothing for the MCL implementation."
  )
(defun stop-windows ()
  "Shuts down the windowing environment for this implementation ~
   Does nothing for the MCL implementation."
  )
