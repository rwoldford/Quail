;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               make-canvas.lisp
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
;;;     N.G. Bennett  1993
;;;     J.R. MacPhail 1995
;;;     R.W. Oldford  1994
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-canvas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  User-controllable margins to leave free at window creation (1995, jrm)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let user control margins left free when e.g. the Browser asks for a huge window.
;; (We can't fault the browser for optimism.)
;; (User may want easy access to other X-Windows or Mac programs.  Also there exist
;; Mac utilities that react to a click at one of the screen edges.)
;; Four pixels is a good default.  We might use defparameter, but a defvar wrapped
;; in a boundp is nicest to the user, maximizing the ease of setting the thing
;; before this code or after it.  (User can change it anytime at all.)

(unless (boundp '*screen-top-margin*)
  (defvar *screen-top-margin* 4 "Top margin to leave when making big window"))

(unless (boundp '*screen-left-margin*)
  (defvar *screen-left-margin* 4 "Left margin to leave when making big window"))

(unless (boundp '*screen-bottom-margin*)
  (defvar *screen-bottom-margin* 4 "Bottom margin to leave when making big window"))

(unless (boundp '*screen-right-margin*)
  (defvar *screen-right-margin* 4 "Right margin to leave when making big window"))


;; Assume a smart screen-height function (e.g. subtracting menubar on MacOS)
(defun window-max-height ()
  "Maximum height for a window, including title-bar, scrollbar and so on"
  (declare (special *screen-top-margin*
                    *screen-bottom-margin*))
  (- (screen-height) *screen-top-margin* *screen-bottom-margin*))


(defun window-max-width ()
  "Maximum width for a window, including scrollbar and so on"
  (declare (special *screen-left-margin*
                    *screen-right-margin*))
  (- (screen-width) *screen-left-margin* *screen-right-margin*))


(defun window-min-left ()
  "Minimum left for a window being created, but not to hinder window movement"
  (declare (special *screen-left-margin*))
  *screen-left-margin*)


(defun window-min-bottom ()
  "Minimum bottom for a window being created, but not to hinder window movement"
  (declare (special *screen-bottom-margin*))
  *screen-bottom-margin*)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Default screen positions for a canvas (modified 1995, jrm)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-canvas-region*
  (make-region *screen-left-margin* *screen-bottom-margin* 300 300)
  "Set to a default screen region where the canvas will appear.")

(defun set-up-default-canvas-region
       (&optional (left *screen-left-margin*) (bottom *screen-bottom-margin*)
                  (width 300) (height 300))
       "Sets the default region where a new canvas will appear on the ~
        screen."
       
  (setf *default-canvas-region*  (make-region left bottom width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Canvas functions
;;;

(defun make-canvas (&rest canvas-keywords
                          &key
                          left bottom width height
                          region
                          (color? (color-device-p))
                          (type (device-type))
                          (canvas-class 'canvas)
                          (font *normal-graphics-font*)
                          (title "Canvas")
                          background-color
                          pen-color
                          &allow-other-keys)
  "Creates and returns a canvas.  Canvas size is calculated by: ~
   first, using left/bottom/width/height if they are given; ~
   second, using the bounding box of region if it is a region; ~
   third, using *default-canvas-region* if region is NIL; ~
   fourth, prompting user to use pointing device to give size."

  (declare (special *default-canvas-region* *normal-graphics-font*))
  (declare (ignore pen-color background-color title font canvas-class))

  ;; If left/bottom/width/height not given, try to extract from region.
  ;; Where all these things are NIL, we quietly use the default. -- jrm
  
  (unless (and left bottom width height)
    (when (null region) (setf region *default-canvas-region*))
    (when (region-p region)
      (setf left (region-left region)
            bottom (region-bottom region)
            width  (region-width region)
            height (region-height region))))
  
  (if (and left bottom width height)
    
    (apply
     (if (and color? (color-device-p))
       #'make-color-canvas
       #'make-b&w-canvas)
     :left (max left (window-min-left))
     :bottom (max bottom (window-min-bottom))
     :width (min width (window-max-width)) 
     :height (min height (window-max-height))
     :type type
     :allow-other-keys t canvas-keywords)
    
    (apply #'prompt-for-canvas canvas-keywords))
  
  )

