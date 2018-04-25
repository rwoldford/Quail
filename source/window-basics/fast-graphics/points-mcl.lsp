;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              points-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1991 George Washington University
;;;     
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :wb)

(defun xor-draw-points (canvas points &key erase? &allow-other-keys)
  
  "Draws or erases single pixel points ~
   Results with color background are undefined."
  
  (declare (special *black-color* *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (ignore erase? ))
  (with-focused-canvas canvas
    (#_PenMode *host-xor-mode*)
    (loop with h fixnum = (canvas-height canvas)
          for (x y1) fixnum in points
          for y fixnum = (- h y1)
          do 
          (fast-draw-line canvas x y x y))))


(defun xor-move-points (canvas old new &key none &allow-other-keys )
  "Moves  single pixel points.~
   Results with color background are undefined."
  
  (declare (ignore  none)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (special  *host-xor-mode*))
  (with-focused-canvas canvas
    (#_PenMode *host-xor-mode*)
    (loop with h fixnum = (canvas-height canvas)
          for (xo yo1 ) fixnum in old
          for (x y1 ) fixnum  in new
          for yo fixnum = (- h yo1)
          for y fixnum = (- h y1)
          do 
          (fast-draw-line canvas xo yo xo yo)
          (fast-draw-line canvas x y x y)
          )))


(defun xor-draw-boxes (canvas points &key size erase? 
                              &allow-other-keys)
  
  "Draws or erases  filled squares with varying size.~
   Results with color background are undefined."
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (ignore erase? ))
  (let* ((h (canvas-height canvas)))
    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (#_PenMode *host-xor-mode*)
        (loop 
          for (x y ) fixnum in points for s fixnum in size
          do  
          (set-point-rect r x (- h y) s)
          (#_PaintRect r) )))))


(defun xor-move-boxes (canvas old new &key size (old-size size)  
                              &allow-other-keys)
  
  "Moves filled squares with varying size.~
   Results with color background are undefined."
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (let* ((h (canvas-height canvas)))
    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (#_PenMode *host-xor-mode*)
        (loop for (xo yo ) fixnum in old
              for (x y ) fixnum in new 
              for so fixnum in old-size 
              for s fixnum in size
              do  
              (set-point-rect r xo (- h yo) so )
              (#_PaintRect r)
              (set-point-rect r x (- h y) s )
              (#_PaintRect r))))))

(defun xor-draw-circles (canvas points &key size erase?
                                &allow-other-keys)
  
  "Draws or erases  filled circles with varying size.~
   Results with color background are undefined."
  
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (ignore erase? ))
  (let* ((h (canvas-height canvas)))
    
    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (#_PenMode *host-xor-mode*)
        (loop 
          for (x y ) fixnum in points for s fixnum in size
          do  
          (set-point-rect r x (- h y) s )
          (#_PaintOval r) )))))


(defun xor-move-circles (canvas old new  &key size (old-size size)
                                &allow-other-keys)
  
  "Moves  filled circles with varying size.~
   Results with color background are undefined."
  
  
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (let* ((h (canvas-height canvas)))
    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (#_PenMode *host-xor-mode*)
        (loop for (xo yo ) fixnum in old
              for (x y ) fixnum in new 
              for so fixnum in old-size 
              for s fixnum in size
              do  
              (set-point-rect r xo (- h yo) so )
              (#_PaintOval r)
              (set-point-rect r x (- h y) s )
              (#_PaintOval r))))))