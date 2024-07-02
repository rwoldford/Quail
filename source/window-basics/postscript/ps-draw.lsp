;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               ps-draw.lisp
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


(in-package :window-basics)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '()))

(defun ps-canvas-set-background-color (canvas color)
  "This function sets the postscript canvas background.~
   NOTE:  This function should only be called at the beginning of a canvas~
   definition, for it clears the entire canvas while setting the background."
  (write-ps canvas b
            (canvas-to-ps-red canvas (red-of color))
            (canvas-to-ps-green canvas (green-of color))
            (canvas-to-ps-blue canvas (blue-of color))))

(defun ps-set-canvas-x (canvas x)
  "This function sets the x coordinate of the pen on the postscript~
   canvas while maintaining the current y coordinate of the pen."
  (write-ps canvas x
            (canvas-to-ps-x canvas x)))

(defun ps-set-canvas-y (canvas y)
  "This function sets the y coordinate of the pen on the postscript~
   canvas while maintaining the current x coordinate of the pen."
  (write-ps canvas y
            (canvas-to-ps-y canvas y)))

(defun ps-canvas-draw-to (canvas x y)
  "This function draws a line from the current point to (x,y) on the~
   postscript canvas.  NOTE:  The current point MUST be set for this~
   function to work correctly."
  (write-ps canvas t
            (canvas-to-ps-x canvas x)
            (canvas-to-ps-y canvas y)))

(defun ps-canvas-unstroked-draw-to (canvas x y)
  "This function draws a line from the current point to (x,y) on the~
   postscript canvas.  NOTE:  The current point MUST be set for this~
   function to work correctly."
  (write-ps canvas u
            (canvas-to-ps-x canvas x)
            (canvas-to-ps-y canvas y)))

(defun ps-canvas-clear (canvas &key
                               (canvas-left 0)
                               (canvas-bottom 0)
                               (width (canvas-width canvas))
                               (height (canvas-height canvas)))
  "This function clears a rectangular area of the~
    postscript canvas bounded by (x1,y1) and (x2,y2)"
  (write-ps canvas c
            (canvas-to-ps-x canvas canvas-left)
            (canvas-to-ps-y canvas canvas-bottom)
            (canvas-to-ps-x canvas (+ canvas-left width))
            (canvas-to-ps-y canvas (+ canvas-bottom height))
            (canvas-to-ps-red canvas 
                              (red-of (canvas-background-color canvas)))
            (canvas-to-ps-green canvas 
                                (green-of (canvas-background-color canvas)))
            (canvas-to-ps-blue canvas 
                               (blue-of (canvas-background-color canvas)))))

(defun ps-canvas-invert (canvas left right bottom top)
  "This function 'inverts' a rectangular area of the~
    postscript canvas bounded by (x1,y1) and (x2,y2)."
  (write-ps canvas i
            (canvas-to-ps-x canvas left)
            (canvas-to-ps-y canvas bottom)
            (canvas-to-ps-x canvas right)
            (canvas-to-ps-y canvas top)))

(defun ps-canvas-draw-circle (canvas x y radius)
  "This function mimics the canvas-draw-circle, calling~
   ps_canvas_draw_ellipse with both x and y radius arguments~
   being the same."
  (let ((scale (min (ps-x-scale-of canvas) (ps-y-scale-of canvas))))
    (write-ps canvas e 
              (canvas-to-ps-x canvas x)
              (canvas-to-ps-y canvas y)
              (round (* scale radius))
              (round (* scale radius)))))

(defun ps-canvas-draw-filled-circle (canvas x y radius)
  "This function is almost the same as ps-canvas-draw-circle~
    save that the circle drawn is then filled."
  ;(declare (ignore operation)
  ;         (ignore width)
  ;         (ignore color))
  (let ((scale (min (ps-x-scale-of canvas) (ps-y-scale-of canvas))))
    (write-ps canvas fe 
              (canvas-to-ps-x canvas x)
              (canvas-to-ps-y canvas y)
              (round (* scale radius))
              (round (* scale radius)))))


;(defun ps-canvas-draw-polygon (canvas list-of-points)
;  "This function draws a polygon on the postscript canvas"
;  (declare (ignore operation)
;           (ignore width)
;           (ignore color))
;  (ps-canvas-move-to canvas 
;                     (canvas-to-ps-x canvas (car (first list-of-points)))
;                     (canvas-to-ps-y canvas (cdr (first list-of-points))))
;  (dolist (point (cdr list-of-points))
;    (ps-canvas-unstroked-draw-to canvas 
;                                 (canvas-to-ps-x canvas (car point)) 
;                                 (canvas-to-ps-y canvas (cdr point))))
;  (write-ps canvas closepath)
;  (write-ps canvas stroke))

;;;
;;; updated by Catherine Hurley Nov 1/1996
;;;
(defun ps-canvas-draw-polygon (canvas list-of-points)
  "This function draws a polygon on the postscript canvas"
  ;(declare (ignore operation)
  ;         (ignore width)
  ;         (ignore color))
  (ps-canvas-move-to canvas
                     (car (first list-of-points))
                     (cdr (first list-of-points)))
  (dolist (point (cdr list-of-points))
    (ps-canvas-unstroked-draw-to canvas
                                 (car point)
                                 (cdr point)))
  (ps-canvas-unstroked-draw-to canvas
                               (car (first list-of-points))
                               (cdr (first list-of-points)))
  (write-ps canvas closepath)
  (write-ps canvas stroke))

;;
;; a utility function. this should probably be somewhere else, like a wb utility file. cw.
;;
(defun flatten-pointlist (alist)
  (cond ((null alist) nil)
        ((null (cdr alist)) (list (car (first alist)) (cdr (first alist))))
        (T (append (list (car (first alist)) (cdr (first alist))) (flatten-pointlist (cdr alist))))))

;(defun ps-canvas-draw-filled-polygon (canvas list-of-points)
;  "This function draws a filled polygon on the postscript canvas"
;  (declare (ignore operation)
;           (ignore width)
;           (ignore color))
;  (write-ps canvas
;            fp
;            (append (flatten-pointlist list-of-points)
;                    (list (length list-of-points)))))

;;;
;;; updated by Catherine Hurley Nov 1/1996
;;;
(defun ps-canvas-draw-filled-polygon (canvas list-of-points)
  "This function draws a filled polygon on the postscript canvas"
  ;(declare (ignore operation)
  ;         (ignore width)
  ;         (ignore color))
  (setq list-of-points
        (mapcar #'(lambda(p)
              (cons (canvas-to-ps-x canvas (car p))
                    (canvas-to-ps-y canvas (cdr p))))
          list-of-points))
  (write-ps canvas
            fp
            (append (flatten-pointlist list-of-points)
                    (list (length list-of-points)))))

(defun ps-canvas-draw-inside-square (canvas x y width)
  "This function draws a square which is drawn one pixel inside the~
   region bounded by (x,y) and (x+width,y+width)"
  (let* ((scale (min (ps-x-scale-of canvas) (ps-y-scale-of canvas)))
         (new-width (round (* scale width))))
    (write-ps canvas 
              r
              (canvas-to-ps-x canvas (1+ x))
              (canvas-to-ps-y canvas (1+ y))
              (+ (canvas-to-ps-x canvas (1- x)) new-width)
              (+ (canvas-to-ps-y canvas (1- y)) new-width))))

(defun ps-canvas-draw-square (canvas x y width)
  "This function draws an unfilled square on the postscript canvas."
  (let* ((scale (min (ps-x-scale-of canvas) (ps-y-scale-of canvas)))
        (new-width (round (* scale width))))
    (write-ps canvas 
              r
              (canvas-to-ps-x canvas x)
              (canvas-to-ps-y canvas y)
              (+ (canvas-to-ps-x canvas x) new-width)
              (+ (canvas-to-ps-y canvas y) new-width))))

(defun ps-canvas-draw-filled-square (canvas x y width)
  "This function draws a filled square on the postscript canvas."
  (let* ((scale (min (ps-x-scale-of canvas) (ps-y-scale-of canvas)))
        (new-width (round (* scale width))))
    (write-ps canvas 
              fr
              (canvas-to-ps-x canvas x)
              (canvas-to-ps-y canvas y)
              (+ (canvas-to-ps-x canvas x) new-width)
              (+ (canvas-to-ps-y canvas y) new-width))))

(defun ps-canvas-draw-inside-rectangle (canvas x1 x2 y1 y2)
  "This function draws a rectangle which is drawn one pixel inside the~
   region bounded by (x1,y1) and (x2,y2)"
    (ps-canvas-draw-rectangle canvas 
                              (1+ x1)
                              (1- x2)
                              (1+ y1)
                              (1- y2)))

(defun ps-canvas-draw-rectangle (canvas x1 x2 y1 y2)
  "This function draws an unfilled rectangle on the postscript canvas."
  (write-ps canvas 
            r
            (canvas-to-ps-x canvas x1)
            (canvas-to-ps-y canvas y1)
            (canvas-to-ps-x canvas x2)
            (canvas-to-ps-y canvas y2)))

(defun ps-canvas-draw-filled-rectangle (canvas x1 x2 y1 y2)
  "This function draws a filled rectangle on the postscript canvas."
  (write-ps canvas 
            fr
            (canvas-to-ps-x canvas x1)
            (canvas-to-ps-y canvas y1)
            (canvas-to-ps-x canvas x2)
            (canvas-to-ps-y canvas y2)))

(defun ps-canvas-move-to (canvas x y)
  "This function moves the pen to (x,y) on the postscript canvas"
  (write-ps canvas moveto
            (canvas-to-ps-x canvas x)
            (canvas-to-ps-y canvas y)))

(defun ps-canvas-draw-line (canvas x1 y1 x2 y2)
  "This function draws a line from (x1,y1) to (x2,y2) on the postscript canvas"
  (write-ps canvas
            l
            (canvas-to-ps-x canvas x1)
            (canvas-to-ps-y canvas y1)
            (canvas-to-ps-x canvas x2)
            (canvas-to-ps-y canvas y2)
))

(defun ps-canvas-draw-filled-arc (canvas start-angle arc-angle
                                         x y xradius yradius)
  "This function draws a filled arc centred at coordinates x y ~
   of radius xradius in the x direction and yradius in the y direction ~
   from start-angle degrees to an additional arc-angle degrees."
  ;(declare (ignore operation)
  ;         (ignore width)
  ;         (ignore color))
  (write-ps canvas fa 
            (canvas-to-ps-x canvas x)
            (canvas-to-ps-y canvas y)
            (canvas-to-ps-x canvas xradius)
            (canvas-to-ps-y canvas yradius)
            start-angle arc-angle))

(defun ps-canvas-draw-arc (canvas start-angle arc-angle
                                  x y xradius yradius)
  "This function draws an arc centred at coordinates x y ~
   of radius xradius in the x direction and yradius in the y direction ~
   from start-angle degrees to an additional arc-angle degrees."
  ;(declare (ignore operation)
  ;         (ignore width)
  ;         (ignore color))
  (write-ps canvas a 
            (canvas-to-ps-x canvas x)
            (canvas-to-ps-y canvas y)
            (canvas-to-ps-x canvas xradius)
            (canvas-to-ps-y canvas yradius)
            start-angle arc-angle))


