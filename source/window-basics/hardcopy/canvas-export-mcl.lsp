;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               canvas-export-mcl.lisp
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
;;;     R.W. Oldford 1999
;;;     
;;;
;;;
;;;-------------------------------------------------------------------

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute)
    (export '(canvas-export)))


(defmethod canvas-export ((self canvas) &key left top width height filename filetype)
  "Exports the canvas contents to a file of type filetype."
  (unless filetype
    (setf filetype (pick-one  (list :postscript :bitmap))))
  (case
    filetype
    (:postscript
     (canvas-to-ps self))
    (:something-else 
     "OK")))
