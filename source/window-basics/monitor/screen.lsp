;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               screen.lisp
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
;;;     C.B. Hurley 1989-1991
;;;     R.W. Oldford 1989-1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-screen-region
           screen-height screen-width screen-x screen-y)))

;;;-----------------------------------------------------------------------
;;; grabbing the screen and screen coordinates 


(defun screen-region ()
  (make-region 0 0 (screen-width) (screen-height)))

(defun canvas-screen-region (canvas)
  "Returns the (active) screen location occupied by canvas as a region."
  (let* ((left (screen-x canvas))
         (bottom (screen-y canvas))
         (region (canvas-region canvas)))
    (make-region left bottom (region-width region) (region-height region))))

(defun screen-to-canvas (canvas location)
  "Converts location from screen to canvas coords ~
   location may be a region or position ~
   returns a region or position as appropriate."
  
  (let* ((dx (screen-x canvas))
         (dy (screen-y canvas)))
    (if (region-p location)
      (make-region (- (region-left location) dx)
                   (- (region-bottom location) dy)
                   (region-width location) (region-height location))
      ;;else
      (make-position (- (position-x location) dx)
                     (- (position-y location) dy)  ))))

(defun canvas-to-screen (canvas location)
  "Converts location from canvas to screen coords ~
   location may be a region or position.  ~
   Returns a region or position as appropriate."
  (let* ((dx (screen-x canvas))
         (dy (screen-y canvas)))
    (if (region-p location)
      (make-region (+ (region-left location) dx)
                   (+ (region-bottom location) dy)
                   (region-width location) (region-height location))
      ;;else
      (make-position (+ (position-x location) dx)
                     (+ (position-y location) dy)  )))

  )
  
