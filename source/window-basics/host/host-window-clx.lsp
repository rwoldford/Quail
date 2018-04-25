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
;;;    N.G. Bennett 1993.
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
                   :initform NIL
                   :initarg :window-manager)
   (gcontext :accessor gcontext :documentation "The graphics context.")
   (current-position :accessor current-position 
		     :documentation "The pen position.")))

(defmethod window-manager :around ((Self host-window))
  (unless (call-next-method)
    (or (setf (window-manager self) *window-manager*)
        (setf (window-manager self)
              (prompt-for-window-manager
               *known-window-managers*)))))
#|
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
    (T (host-window self))))
|#

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
  (declare (special *default-display*))
  (unwind-protect
    (let* ((screen         (xlib:display-default-screen
                            *default-display*))
           (text-font      (canvas-font-to-host-font font))
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
                             :exposure
                             :button-press
                             :button-release
                             :enter-window
                             :structure-notify
                             )))
           (gcontext       (xlib:create-gcontext :drawable window
                                                 :background black
                                                 :foreground white
                                                 :font text-font))
           )
      
      (setf (host-window self) window)
      (setf (gcontext self) gcontext)
      (setf (current-position self) (list 0 0))
      ;; The following is an undocumented feature of CLX
      (unless (xlib:display-after-function (xlib:window-display window))
        (setf (xlib:display-after-function (xlib:window-display window))
              #'xlib:display-finish-output))
             ;; #'xlib:display-force-output))
      (xlib:set-wm-properties window
                              :name title
                              :icon-name title
                              :resource-name "Canvas"
                              :resource-class 'canvas
                              :x left :y top :width width :height height
                              :min-width 10 :min-height 10
                              :input :off :initial-state :normal)
      (setf (xlib:window-plist (host-window self))
            (acons :wb-class-instance self NIL))
      (setf *current-canvas* self)
      (call-next-method))))

#|
(defmethod initialize-instance :after
           ((self host-window)
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
|#

