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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(host-window)))

(defclass host-window ()
  ((host-window :accessor host-window)
   (window-manager :accessor window-manager
                   :initform (or *window-manager*
                                 (prompt-for-window-manager
                                  *known-window-managers*))
                   :initarg :window-manager)
   (gcontext :accessor gcontext :documentation "The graphics context.")
   (current-position :accessor current-position 
		     :documentation "The pen position.")))


(defmethod xwindow-of ((self host-window))
  (unless (window-manager self)
    (setf (window-manager self)
          (prompt-for-window-manager *known-window-managers*)))
  (case (window-manager self)
   (:4Dwm
    (parent-of (parent-of (host-window self))))
   (:twm
    (parent-of (host-window self)))
   (:macx
    (host-window self))
   (T (parent-of (host-window self)))))

(defmethod wb-refresh ((self host-window))
  (declare (ignore self)))


(defmethod initialize-instance ((self host-window)
				&key
	                        (host NIL)
                        	(font   *normal-graphics-font*)
                        	(left   0)
                        	(top    0)
                        	(width  0)
                        	(height 0)
				(title "Canvas"))
	(call-next-method))

(defmethod initialize-instance ((self host-window)
				&key
	                        (host NIL)
                        	(font   *normal-graphics-font*)
                        	(left   0)
                        	(top    0)
                        	(width  0)
                        	(height 0)
				(title "Canvas"))
  (unwind-protect
	(let (display screen text-font black white window gcontext)
           (setf display        (host-display host))
		;;(format *terminal-io* "~&display = ~s~%~%" display)
           (setf screen         (xlib:display-default-screen display))
		;;(format *terminal-io* "~&screen = ~s~%~%" screen)
           (setf text-font      (canvas-font-to-host-font font))
		;;(format *terminal-io* "~&text-font = ~s~%~%" text-font)
           (setf black          (xlib:screen-black-pixel screen))
		;;(format *terminal-io* "~&black = ~s~%~%" black)
           (setf white          (xlib:screen-white-pixel screen))
		;;(format *terminal-io* "~&white = ~s~%~%" white)
           (setf window         (xlib:create-window
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
                             :button-release
                             :enter-window
                             )))
		;;(format *terminal-io* "~&window = ~s~%~%" window)
    	   (setf gcontext       (xlib:create-gcontext :drawable window
                                                 :background black
                                                 :foreground white
                                                 :font text-font))
		;;(format *terminal-io* "~&gcontext = ~s~%~%" gcontext)
      
      (setf (host-window self) window)
      (setf (gcontext self) gcontext)
      (setf (current-position self) (list 0 0))
      (xlib:set-wm-properties window
                              :name title
                              :icon-name title
                              :resource-name "Canvas"
                              :resource-class 'canvas
                              :x left :y top :width width :height height
                              :min-width width :min-height height
                              :input :off :initial-state :normal)
      (setf (xlib:window-plist (host-window self))
            (acons :wb-class-instance self NIL))
      (setf *current-canvas* self)
	;;(print "usin it")
      (call-next-method))))

(defmethod initialize-instance :after ((self host-window)
				&key
	                        (host NIL)
                        	(font   *normal-graphics-font*)
                        	(left   0)
                        	(top    0)
                        	(width  0)
                        	(height 0)
				(title "Canvas"))
      ;;(xlib:map-window (host-window self))
)

