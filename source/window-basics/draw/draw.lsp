;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               draw.lisp
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
;;;     C.B. Hurley 1989-1992
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1991
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(canvas-x set-canvas-x canvas-y set-canvas-y canvas-draw-to
          canvas-draw-inside-square canvas-draw-square canvas-draw-filled-square
          canvas-draw-filled-rectangle canvas-draw-inside-rectangle
          canvas-draw-circle canvas-draw-filled-circle canvas-draw-polygon
          canvas-draw-filled-polygon canvas-invert canvas-clear
          canvas-draw-line canvas-draw-rectangle canvas-move-to
          canvas-draw-region canvas-flash-region draw-rect&corners
          canvas-draw-arc canvas-draw-filled-arc)))


(defun canvas-x (canvas)
  (h-draw:point-x (h-draw:pen-position canvas)))

(defun set-canvas-x (canvas new-x)
  (let ((current-y (h-draw:point-y (h-draw:pen-position canvas))))
    (with-focused-canvas canvas
      (h-draw:move-to canvas new-x current-y))))

(defun canvas-y (canvas)
  (host-to-canvas-y canvas (h-draw:point-y (h-draw:pen-position canvas))))

(defun set-canvas-y (canvas new-y)
  (let ((current-x (h-draw:point-x (h-draw:pen-position canvas))))
    (with-focused-canvas canvas
      (h-draw:move-to canvas current-x
                      (canvas-to-host-y canvas new-y)))))

(defun canvas-draw-to (canvas x y
                              &key
                              (width NIL)
                              (operation NIL)
                              (color NIL)
                              dashing)
  (declare (ignore dashing))
  (with-focused-canvas canvas
    (with-pen-values canvas
                     color width operation
      (with-display-mode canvas
                         (display-mode-of canvas)
                         (canvas-draw-to canvas x y)
        (h-draw:line-to canvas x (canvas-to-host-y canvas y))))))

(defun canvas-draw-inside-square (canvas x y sq-width    
                                         &key (width NIL) (operation NIL) (color NIL) dashing)
  (declare (ignore dashing))
  (with-focused-canvas canvas
    (with-pen-values canvas color width operation
      (with-display-mode canvas (display-mode-of canvas)
                         (canvas-draw-inside-square canvas x y sq-width)
        (h-draw:draw-inside-rectangle canvas x (canvas-to-host-y canvas (+ y sq-width)) 
                                      (+ x sq-width) (canvas-to-host-y canvas y))))))
;;; Original code
#|
(defun canvas-draw-square (canvas x y sq-width
                                  &key (width NIL) (operation NIL) (color NIL) dashing)
  (declare (ignore dashing))
  (with-focused-canvas canvas
    (with-pen-values canvas color width operation 
      (with-display-mode
        canvas
        (display-mode-of canvas) 
        (canvas-draw-square canvas x y sq-width)
        (h-draw:draw-rectangle canvas x (+ x sq-width) 
                               (canvas-to-host-y canvas y) 
                               (canvas-to-host-y canvas (+ y sq-width)))))))
|#
;;; Original code

(defun canvas-draw-filled-square (canvas x y sq-width 
                                         &key (operation NIL) (color NIL))
  (with-focused-canvas canvas
    (with-pen-values canvas color NIL operation
      (with-display-mode canvas
                         (display-mode-of canvas)
                         (canvas-draw-filled-square canvas x y sq-width)
        (h-draw:draw-filled-rectangle canvas x 
                                      (canvas-to-host-y canvas (+ y sq-width)) 
                                      (+ x sq-width) (canvas-to-host-y canvas y))))))

;;; Revised code to get h-draw:: call correct
 (defun canvas-draw-square (canvas x y sq-width
                                  &key (width NIL) (operation NIL) (color NIL) dashing)
  (declare (ignore dashing))
  (with-focused-canvas canvas
    (with-pen-values canvas color width operation 
      (with-display-mode
        canvas
        (display-mode-of canvas) 
        (canvas-draw-square canvas x y sq-width)
        (h-draw:draw-rectangle canvas x 
                               (canvas-to-host-y canvas y)  (+ x sq-width)
                              (canvas-to-host-y canvas  (+ y  sq-width)))))))

(defun canvas-draw-inside-rectangle (canvas left right bottom top    
                                            &key 
                                            (width NIL)
                                            (operation NIL)
                                            (color NIL)
                                            dashing)
  (declare (ignore  dashing))
  (with-focused-canvas canvas
    (with-pen-values canvas color width operation
      (with-display-mode canvas (display-mode-of canvas)
                         (canvas-draw-inside-rectangle canvas
                                                       left right bottom top)
        (h-draw:draw-inside-rectangle canvas left (canvas-to-host-y canvas top) 
                                      right (canvas-to-host-y canvas bottom))))))
;;; Original code
#|
(defun canvas-draw-rectangle (canvas x1 x2 y1 y2
                                     &key 
                                     (width NIL)
                                     (operation NIL)
                                     (color NIL)
                                     dashing)
  (declare (ignore  dashing))
  (with-focused-canvas canvas
    (with-pen-values canvas color width operation 
      (with-display-mode
        canvas
        (display-mode-of canvas) 
        (canvas-draw-rectangle canvas x1 x2 y1 y2)
        (h-draw:draw-rectangle canvas x1 x2 
                               (canvas-to-host-y canvas y1) 
                               (canvas-to-host-y canvas y2))))))
|#

;;; Revised code
(defun canvas-draw-rectangle (canvas x1 x2 y1 y2
                                     &key 
                                     (width NIL)
                                     (operation NIL)
                                     (color NIL)
                                     dashing)
  (declare (ignore  dashing))
  (with-focused-canvas canvas
    (with-pen-values canvas color width operation 
      (with-display-mode
        canvas
        (display-mode-of canvas) 
        (canvas-draw-rectangle canvas x1 x2 y1 y2)
        (h-draw:draw-rectangle canvas x1  
                               (canvas-to-host-y canvas y1)
                               x2
                               (canvas-to-host-y canvas y2))))))


(defun canvas-draw-filled-rectangle (canvas left right bottom top 
                                            &key 
                                            (operation NIL)
                                            (color NIL))
  (with-focused-canvas canvas
    (with-pen-values canvas color NIL operation
      (with-display-mode canvas
                         (display-mode-of canvas)
                         (canvas-draw-filled-rectangle canvas
                                                       left right bottom top)
        (h-draw:draw-filled-rectangle canvas left 
                                      (canvas-to-host-y canvas top) 
                                      right (canvas-to-host-y canvas bottom))))))



(defun canvas-draw-circle (canvas x y radius 
                                  &key 
                                  ;;(pattern *black-shade*)
                                  (width NIL)
                                  (operation NIL)
                                  (color NIL))
  (with-focused-canvas canvas
    (with-pen-values canvas color width operation
      (with-display-mode canvas (display-mode-of canvas)
                         (canvas-draw-circle canvas x y radius)
        (h-draw:draw-ellipse canvas
                             (- x radius)
                             (canvas-to-host-y canvas (+ y radius))
                             (+ x radius)
                             (canvas-to-host-y canvas (- y radius))
                             )
        ))))

(defun canvas-draw-filled-circle (canvas x y radius 
                                         &key 
                                         (operation NIL)
                                         (color NIL))
  (with-focused-canvas canvas
    (with-pen-values canvas color NIL operation 
      (with-display-mode canvas (display-mode-of canvas)
                         (canvas-draw-filled-circle canvas x y radius)
        (h-draw:draw-filled-ellipse canvas 
                                    (- x radius)
                                    (canvas-to-host-y canvas (+ y radius))
                                    (+ x radius) 
                                    (canvas-to-host-y canvas (- y radius))
                                    )
        ))))

(defun canvas-draw-polygon (canvas 
                            list-of-points
                            &key
                            (width NIL)
                            (operation NIL)
                            (color NIL)
                            )
  (with-focused-canvas canvas
    (with-pen-values canvas color width operation
      (with-display-mode canvas (display-mode-of canvas)
                         (canvas-draw-polygon canvas list-of-points)
        (h-draw:draw-polygon
         canvas
         (mapcar #'(lambda (point)
                     (cons (car point)
                           (canvas-to-host-y canvas (cdr point))))
                 list-of-points))))))

(defun canvas-draw-filled-polygon (canvas
                                   list-of-points  
                                   &key
                                   (operation NIL)
                                   (color NIL))
  (with-focused-canvas canvas
    (with-pen-values canvas color NIL operation
      (with-display-mode canvas (display-mode-of canvas)
                         (canvas-draw-filled-polygon canvas list-of-points)
        (h-draw:draw-filled-polygon
         canvas 
         (mapcar #'(lambda (point)
                     (cons (car point)
                           (canvas-to-host-y canvas (cdr point))))
                 list-of-points))))))

(defun canvas-invert (canvas &key                            ; added by cbh
                             (canvas-left 0)
                             (canvas-bottom 0)
                             (width (canvas-width canvas))
                             (height (canvas-height canvas))
                             )
  (with-focused-canvas canvas
    (with-display-mode canvas (display-mode-of canvas)
                       (canvas-invert canvas
                                      canvas-left
                                      canvas-bottom
                                      width
                                      height)
      
      (let ((canvas-top (+ -1 canvas-bottom height))
            (canvas-right (+ -1 canvas-left width)))
        (h-draw:invert-rectangle canvas canvas-left
                                 (canvas-to-host-y canvas canvas-top)
                                 canvas-right
                                 (canvas-to-host-y canvas canvas-bottom))))))

(defun canvas-clear (canvas &key                            ; modified by cbh to clear a region
                            (canvas-left 0)
                            (canvas-bottom 0)
                            (width (canvas-width canvas))
                            (height (canvas-height canvas))
                            )
  "Clears specified rectangular areas of the canvas."
  (with-focused-canvas canvas
    (with-display-mode canvas (display-mode-of canvas)
                       (canvas-clear canvas
                                     :canvas-left canvas-left
                                     :canvas-bottom canvas-bottom
                                     :width width
                                     :height height)
      
      (let ((canvas-top (+  canvas-bottom height))
            (canvas-right (+  canvas-left width)))
        (h-draw:erase-rect canvas                        
                           canvas-left
                           (canvas-to-host-y canvas canvas-top) 
                           canvas-right
                           (canvas-to-host-y canvas canvas-bottom))))))

(defsetf canvas-x (canvas ) (new-x)
  `(let ((the-canvas ,canvas)
         (my-new-x ,new-x))
     (with-display-mode 
       the-canvas
       (display-mode-of the-canvas)
       (set-canvas-x the-canvas my-new-x)
       (set-canvas-x the-canvas my-new-x))
     my-new-x))



(defsetf canvas-y (canvas) (new-y)
  `(let ((the-canvas ,canvas)
         (my-new-y ,new-y))
     (with-display-mode
       the-canvas
       (display-mode-of the-canvas)
       (set-canvas-y the-canvas my-new-y)
       (set-canvas-y the-canvas my-new-y))
     my-new-y))

(defun canvas-draw-line (canvas x1 y1 x2 y2
                                &key
                                (width NIL)
                                (operation NIL)
                                (color NIL)
                                dashing)
  "Draw a line from (x1,y1) to (x2,y2) on the canvas.  ~
   Keywords width, operation, color, and dashing indicate the ~
   style of line drawn."
  (declare (ignore dashing))
  (with-focused-canvas canvas
    (with-pen-values canvas color width operation 
      (with-display-mode
        canvas
        (display-mode-of canvas)
        (canvas-draw-line canvas x1 y1 x2 y2)
        (h-draw:draw-line canvas x1 
                          (canvas-to-host-y canvas y1)
                          x2
                          (canvas-to-host-y canvas y2))))))

(defun canvas-move-to (canvas x y)
  (with-focused-canvas canvas
    (with-display-mode canvas (display-mode-of canvas)
                       (canvas-move-to canvas x y)
      (setf (canvas-x canvas) x)
      (setf (canvas-y canvas) (canvas-to-host-y canvas y)))))

(defun canvas-draw-region (canvas region
                                  &key (width nil) (operation nil) 
                                  (color nil) dashing)
  "Draws a rectangle in canvas around the boundary of the supplied region."
  (canvas-draw-rectangle canvas
                         (region-left region)
                         (region-right region)
                         (region-bottom region)
                         (region-top region) 
                         :width width
                         :operation operation
                         :color color
                         :dashing dashing
                         ))

(defun canvas-flash-region (canvas &key                      ; added by rwo
                                   (left 0)
                                   (bottom 0)
                                   (width (canvas-width canvas))
                                   (height (canvas-height canvas))
                                   (times 2))
  (if (< times 1)
    (quail-error "times must be greater than 0.  times = ~S " times))
  
  (loop for i from 1 to times do
        (canvas-invert canvas
                       :canvas-left left
                       :canvas-bottom bottom
                       :width width
                       :height height)
        (canvas-invert canvas
                       :canvas-left left
                       :canvas-bottom bottom
                       :width width
                       :height height)))

(defun draw-rect&corners (canvas rect &key (corner-width 10)
                                 (width NIL)
                                 (operation NIL)
                                 (color NIL))
  "Draw squares of size corner-width in the corners of rect."
  
  (let ((d corner-width)
        vl vb vr vt)
    (multiple-value-setq (vl vr vb vt) (region-bounds rect))
    (when (and (> (- vr vl) d)
               (> (- vt vb) d))
      (canvas-draw-rectangle canvas vl (+ vl d) vb (+ vb d)
                             :width width :operation operation :color color)
      (canvas-draw-rectangle canvas vr (- vr d) vb (+ vb d)
                             :width width :operation operation :color color)
      (canvas-draw-rectangle canvas vl (+ vl d) vt (- vt d)
                             :width width :operation operation :color color)
      (canvas-draw-rectangle canvas vr (- vr d) vt (- vt d)
                             :width width :operation operation :color color))
    (canvas-draw-rectangle canvas vl vr vb vt
                           :width width :operation operation :color color)))


(defun canvas-draw-arc (canvas start-angle arc-angle
                               x-centre y-centre
                               x-radius y-radius
                               &key
                               (width NIL)
                               (operation NIL)
                               (color NIL))
  (with-focused-canvas canvas
    (with-pen-values canvas color width operation
      (with-display-mode canvas (display-mode-of canvas)
                         (canvas-draw-arc
                          canvas
                          start-angle arc-angle
                          x-centre y-centre
                          x-radius y-radius)
        (h-draw:draw-arc canvas
                         start-angle arc-angle
                         x-centre (canvas-to-host-y canvas y-centre)
                         x-radius y-radius)))))

(defun canvas-draw-filled-arc (canvas start-angle arc-angle
                                      x-centre y-centre
                                      x-radius y-radius
                                      &key
                                      (operation NIL)
                                      (color NIL))
  (with-focused-canvas canvas
    (with-pen-values canvas color NIL operation
      (with-display-mode canvas (display-mode-of canvas)
                         (canvas-draw-filled-arc
                          canvas
                          start-angle arc-angle
                          x-centre y-centre
                          x-radius y-radius)
        (h-draw:fill-arc canvas
                         start-angle arc-angle
                         x-centre (canvas-to-host-y canvas y-centre)
                         x-radius y-radius)))))
