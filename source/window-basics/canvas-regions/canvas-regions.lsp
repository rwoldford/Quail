;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               canvas-regions.lisp
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
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988-1992
;;;
;;;
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :window-basics)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-region canvas-bounds set-canvas-region
           canvas-x-offset canvas-y-offset set-canvas-x-offset set-canvas-y-offset
           extent-region-of
           set-extent-canvas-region
           )))

(defun canvas-region (canvas)
  "Returns the region of the canvas that is displayed (in canvas-coordinates)."
  (make-region 0 0 (canvas-width canvas) (canvas-height canvas)))


(defun canvas-bounds (canvas)
  (region-bounds (canvas-region canvas))
  )

(defun set-canvas-region (canvas new-region)
  (shape-canvas canvas new-region))

(defun canvas-x-offset (canvas)
  (region-left (canvas-region canvas)))

(defun canvas-y-offset (canvas)
  (region-bottom (canvas-region canvas)))

(defun set-canvas-x-offset (canvas value)
  (let ((er (canvas-region canvas)))
    (setf (region-left er) value)
    (set-canvas-region canvas er)))

(defun set-canvas-y-offset (canvas value)
  (let ((er (canvas-region canvas)))
    (setf (region-bottom er) value)
    (set-canvas-region canvas er)))


(defun extent-region-of (canvas)
  (canvas-region canvas))


(defun set-extent-canvas-region (canvas extent)
  (shape-canvas canvas extent))
;;(setf (extent-region-of canvas) extent))
