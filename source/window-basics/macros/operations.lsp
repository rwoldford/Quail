;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               operations.lisp
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
;;;     R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*canvas-operations*
          )
        ))


(defvar *canvas-operations*
  (loop
    for boole
    in (list :boole-1 :boole-2
             :boole-c1 :boole-c2
             :boole-and :boole-nand
             :boole-ior :boole-xor
             :boole-nor :boole-eqv
             :boole-andc1 :boole-andc2
             :boole-orc1 :boole-orc2)
    when (boole-to-op boole)
    collect (boole-to-op boole))
  "List of all transfer operations available for a canvas.")
