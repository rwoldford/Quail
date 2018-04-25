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
    (xlib::set-gcontext-function (gcontext canvas) *host-xor-mode*)
    (xlib:draw-lines (host-window canvas) (gcontext canvas)
      (map 'list #'(lambda (x) (list (car x) 
        (- (canvas-height canvas) (cadr x)))) points))))

(defun xor-move-points (canvas old new &key none &allow-other-keys )
  "Moves  single pixel points.~
   Results with color background are undefined."
  
  (declare (ignore  none)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (special  *host-xor-mode*))
  (with-focused-canvas canvas
    (xlib::set-gcontext-function (gcontext canvas) *host-xor-mode*)
    (xlib:draw-lines (host-window canvas) (gcontext canvas)
      (map 'list #'(lambda (x) (list (car x) 
        (- (canvas-height canvas) (cadr x)))) old))
    (xlib:draw-lines (host-window canvas) (gcontext canvas)
      (map 'list #'(lambda (x) (list (car x)
	(- (canvas-height canvas) (cadr x)))) new))))

(defun xor-draw-boxes (canvas points &key size erase? 
                              &allow-other-keys)
  
  "Draws or erases  filled squares with varying size.~
   Results with color background are undefined."
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (ignore erase? ))
      (with-focused-canvas canvas
        (xlib::set-gcontext-function (gcontext canvas) *host-xor-mode*)
        (loop with h = (canvas-height canvas)
          for (x y ) fixnum in points for s fixnum in size
          do  
          (xlib:draw-rectangle (host-window canvas) (gcontext canvas) 
	    x (- h y) s s t))))


(defun xor-move-boxes (canvas old new &key size (old-size size)  
                              &allow-other-keys)
  
  "Moves filled squares with varying size.~
   Results with color background are undefined."
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
      (with-focused-canvas canvas
        (xlib::set-gcontext-function (gcontext canvas) *host-xor-mode*)
        (loop with h = (canvas-height canvas)
	      for (xo yo ) fixnum in old
              for (x y ) fixnum in new 
              for so fixnum in old-size 
              for s fixnum in size
              do  
              (xlib:draw-rectangle (host-window canvas) (gcontext canvas) 
		xo (- h yo) so so t)
              (xlib:draw-rectangle (host-window canvas) (gcontext canvas)
		x (- h y) s s t))))

(defun xor-draw-circles (canvas points &key size erase?
                                &allow-other-keys)
  
  "Draws or erases  filled circles with varying size.~
   Results with color background are undefined."
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (ignore erase? ))
      (with-focused-canvas canvas
        (xlib::set-gcontext-function (gcontext canvas) *host-xor-mode*)
        (loop with h = (canvas-height canvas) 
          for (x y ) fixnum in points for s fixnum in size
          do  
	  (xlib:draw-arc (host-window canvas) (gcontext canvas) x (- h y) s s 0 7 t))))

(defun xor-move-circles (canvas old new  &key size (old-size size)
                                &allow-other-keys)
  
  "Moves  filled circles with varying size.~
   Results with color background are undefined."
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
      (with-focused-canvas canvas
        (xlib::set-gcontext-function (gcontext canvas) *host-xor-mode*)
        (loop with h = (canvas-height canvas)
	      for (xo yo ) fixnum in old
              for (x y ) fixnum in new 
              for so fixnum in old-size 
              for s fixnum in size
              do  
              (xlib:draw-arc (host-window canvas) (gcontext canvas) xo (- h yo) so so 0 7 t)
              (xlib:draw-arc (host-window canvas) (gcontext canvas) x (- h y) s s 0 7 t))))
