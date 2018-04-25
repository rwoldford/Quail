;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               host-window-clx.lisp
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
;;;     R.W. Oldford 1989-1992
;;;     
;;;
;;;
;;; ----------------------------------------------------------------------------------
;;;  A new class of windows which contain scroll-bars and a scrollable
;;;  area.
;;;
;;; ----------------------------------------------------------------------------------

(in-package :wb)

(export '(host-window))

(defvar *default-host* (short-site-name)
	"The default host for the X window system.")

(defclass host-window (scrolling-window)
  ((host-window :accessor host-window)
   (gcontext :accessor gcontext :documentation "The graphics context.")
   (current-position :accessor current-position 
		     :documentation "The pen position.")))

(defmethod initialize-instance ((self host-window)
				&key
	                        (host *default-host*)
                        	(font   *normal-graphics-font*)
                        	(left   0)
                        	(top    0)
                        	(width  0)
                        	(height 0)
				(title "Canvas"))
 (unwind-protect
    (let* (
            (display        (xlib:open-display host))
            (screen         (xlib:display-default-screen display))
            (text-font      (xlib:open-font display 
			      (canvas-font-to-host-font font)))
            (black          (xlib:screen-black-pixel screen))
            (white          (xlib:screen-white-pixel screen))
            (window         (xlib:create-window
						:parent
						(xlib:screen-root screen)
                                            	:x left :y top
                                            	:width width :height height
                                            	:background black
                                            	:border white
                                            	:border-width 1
                                            	:colormap
                                               	(xlib:screen-default-colormap
						screen)
                                            	:bit-gravity :center
                                            	:event-mask
						(xlib:make-event-mask
						  :exposure :button-press
						  :enter-window
						)))
    	   (gcontext       (xlib:create-gcontext :drawable window
                                             :background black
                                             :foreground white
                                             :font text-font))
	)

  (setf (host-window self) window)
  (setf (gcontext self) gcontext)
  (setf (current-position self) (list 0 0))

#|  (unless (xlib:display-after-function (xlib:window-display window))

  (setf (xlib:display-after-function (xlib:window-display window))
                              #'xlib:display-finish-output))
|#
  (xlib:set-wm-properties window
                    :name title
                    :icon-name title
                    :resource-name "Canvas"
                    :resource-class 'make-canvas
                    :x left :y top :width width :height height
                    :min-width width :min-height height
                    :input :off :initial-state :normal)

 (xlib:map-window window)
 (call-next-method)
 ;;(mp:process-run-function "Quail Canvas Event Handler" #'event-handler self)

)))

