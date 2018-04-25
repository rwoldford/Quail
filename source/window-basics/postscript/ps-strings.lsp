;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               ps-strings.lisp
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
;;;    N.G. Bennett 1992
;;;    R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '()))


(defun ps-canvas-princ-character (canvas character)
  "This function types a character in the current font and color~
   onto the postscript canvas"
  (write-ps canvas show (concatenate 'string "(" (string character) ")")))


(defun ps-canvas-princ-string (canvas str)
  "This function types a string in the current font and color~
   onto the postscript canvas"
  (write-ps canvas show 
            (concatenate 'string 
                         "(" (string str) ")")))

(defun ps-canvas-draw-vertical-str (canvas xpos ypos string
                                           &key bottom clip?)
  "This function types a vertical string in the current font and color~
   onto the postscript canvas"
  (declare (ignore xpos))
  (let ((str)
        (char-ht (canvas-font-height (canvas-font canvas))))
    (if clip?
      (loop for i from 0 below (length string)
            for char = (elt string i)
            while (> ypos bottom)
            do
            (setf str (concatenate 'string str (string char))))
      (setf str string))
    (write-ps canvas vshow char-ht 
              (concatenate 'string "(" str ")"))))


