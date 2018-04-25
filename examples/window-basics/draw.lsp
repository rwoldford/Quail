(in-package :window-basics)

(setf c (make-canvas))
(canvas-to-top c)
(canvas-clear c)
(multiple-value-bind
  (left top right bottom)
  (h-draw::radii-to-rect 100 100 100 100)
  (canvas-draw-arc c left right bottom top 0 45))
;;;
;;; radius defs
(canvas-draw-arc c 0 45 100 100 100 100)
(canvas-erase-arc c 0 45 100 100 100 100)
(canvas-draw-filled-arc c 75 45 100 100 100 100)
(canvas-erase-filled-arc c 75 45 100 100 100 100)
