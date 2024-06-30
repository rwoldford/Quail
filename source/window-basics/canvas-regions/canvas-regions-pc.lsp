;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               canvas-regions-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988-1992
;;;     G.W. Bennett 1996
;;;----------------------------------------------------------------------------------
(in-package :window-basics)
#|
(defun display-rect (window)
  "Returns the rectangle in which the display is drawn inside the window in ~
   local coordinates."
  (when (wptr window)
    (let ((rgn (%get-ptr (rref (wptr window) :window.contRgn))))
      (make-record :rect
                   :top 0
                   :left 0
                   :bottom (- (%get-signed-word rgn 6) (%get-signed-word rgn 2) 15)
                   :right (- (%get-signed-word rgn 8) (%get-signed-word rgn 4) 15)))))

(defun display-rect-right (display-rect)
  (rref display-rect rect.right))

(defun display-rect-bottom (display-rect)
  (rref display-rect rect.bottom))

|#