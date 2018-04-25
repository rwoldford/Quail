;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               button-default.lisp
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
;;;     R.W. Oldford 1989-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(default-left-button-fn
           default-middle-button-fn
           default-right-button-fn
           default-ctrl-left-button-fn
           default-ctrl-middle-button-fn
           default-ctrl-right-button-fn
           default-shift-left-button-fn
           default-shift-middle-button-fn
           default-shift-right-button-fn)))

;;;=====================================================================
;;; Default button event functions.
;;;=====================================================================
;;;
;;; These can/should be replaced on any particular canvas by
;;; application specific functions.
;;; There is some consistency intended here but you are free to follow
;;; your own rules.
;;; In particular, we imagine that a canvas has a 'display' variable
;;; holding the graphical display structure.
;;;
;;; We propose that:
;;;
;;;        - left and middle buttons be used to interact with elements
;;;          of the underlying display
;;;        - right button behaviours be associated with the canvas
;;;          itself
;;;        - ctrl modifier be reserved to return the pointer to the
;;;          selected object (in keeping with above rules)
;;;        - shift modifier be used for multiply accessing objects
;;;          (not implemented)
;;;
;;;          

(defun draw-on-canvas-fn (canvas mouse-pos)
  (let ((old-x (position-x mouse-pos))
        (old-y (position-y mouse-pos))
        new-mouse-pos new-x new-y)
    (do ((i 1 (+ i 1)))
        ((not (mouse-down-p)))
      (setf new-mouse-pos (mouse-position canvas))
      (setf new-x (position-x new-mouse-pos))
      (setf new-y (position-y new-mouse-pos))
      (canvas-draw-line canvas old-x old-y new-x new-y)
      (setf old-x new-x)
      (setf old-y new-y))))
       

(defun default-left-button-fn (canvas mouse-pos)
  (draw-on-canvas-fn canvas mouse-pos))

(defun default-middle-button-fn (canvas mouse-pos)
  (let* ((x (position-x mouse-pos))
         (y (position-y mouse-pos))
         (left (max 0 (- x 10)))
         (bottom (max 0 (- y 10))))
    (canvas-flash-region canvas
                        :left left :bottom bottom
                        :width 20 :height 20
                        :times 5)))

(defun default-right-button-fn (canvas mouse-pos)
  (declare (ignore mouse-pos))
  (do ((i 1 (+ i 1)))
      ((not (mouse-down-p)))
    (let* ((new-mouse-pos (mouse-position canvas))
           (x (position-x new-mouse-pos))
           (y (position-y new-mouse-pos))
           (left (max 0 (- x 10)))
           (right (+ x 10))
           (r-diff (- right (canvas-width canvas)))
           (bottom (max 0 (- y 10)))
           (width (if (> r-diff 0) (- 20 r-diff) 20))
           (height 20))
       (canvas-invert canvas
                      :canvas-left left
                      :canvas-bottom bottom
                      :width width :height height))))

(defun default-ctrl-left-button-fn (canvas mouse-pos)
  (declare (ignore mouse-pos))
  (quail-print (canvas-display canvas)))

(defun default-ctrl-middle-button-fn (canvas mouse-pos)
  (declare (ignore canvas))
  (quail-print mouse-pos))

(defun default-ctrl-right-button-fn (canvas mouse-pos)
  (declare (ignore mouse-pos))
  (quail-print canvas))

;;; No suggestion at all what shift button selection should do.
;;;

(defun default-shift-left-button-fn (canvas mouse-pos)
  (declare (ignore canvas mouse-pos)))

(defun default-shift-middle-button-fn (canvas mouse-pos)
  (declare (ignore canvas mouse-pos)))

(defun default-shift-right-button-fn (canvas mouse-pos)
  (declare (ignore canvas mouse-pos)))
