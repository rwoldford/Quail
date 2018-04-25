;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              inform-user.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1993
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(inform-user inform-user-busy
          *info-background-color* *info-foreground-color*
          set-info-pen-color set-info-background-color)))


(defvar *info-window* NIL "Information window for user.")
(defvar *info-text* NIL "Information for user.")

(defvar *info-background-color*
  wb::*gray-color*
  "The default background colour for info windows.")

(defvar *info-foreground-color*
  wb::*black-color*
  "The default foreground or pen colour for info windows.")

(defclass info-window (view-window)
  ()
  (:documentation "An empty class to distinguish the information window from other ~
                   view-windows.  This is especially convenient when closing all ~
                   view-windows of a particular type."))

(defun info-window ()
  "Returns the information window. Creates it if it didn't already exist."
  (declare (special *info-window*))
  (unless *info-window*
    (setf *info-window*
          (make-view-window :left 10 :right 500
                            :bottom (- (wb::screen-height) 300)
                            :top (- (wb::screen-height) 50)
                            :view-window-class 'info-window
                            ;;:window-show NIL
                            :background-color *info-background-color*
                            :pen-color *info-foreground-color*
                            :title "Information")))
  *info-window*)


(defun set-info-pen-color (&optional (color (wb:prompt-user-for-color)))
  "Sets the foreground or pen colour of the information window to color. ~
   If color is not supplied the user is prompted to choose a color."
  (when (wb::canvas-p *info-window*)
    (wb:set-pen-color *info-window* color))
  (when *info-text*
    (set-drawing-style *info-text* :color color))
  (setf *info-foreground-color* color))


(defun set-info-background-color (&optional (color (wb:prompt-user-for-color)))
  "Sets the background colour of the information window to color. ~
   If color is not supplied the user is prompted to choose a color."
  (when (wb::canvas-p *info-window*)
    (wb::canvas-set-background-color *info-window* color))
  (setf *info-background-color* color))


(defun info-text-view ()
  (declare (special *info-text* wb::*help-normal-text-font*))
  (unless (view-p *info-text*)
    (setf *info-text*
          (text-view :draw? NIL
                     :justification '(:left :top)
                     :font wb::*help-normal-text-font*
                     :color *info-foreground-color*))
    
    )
    *info-text*)

(defun info-window-alive-p ()
  (let ((i-w (info-window)))
    (and (wb::canvas-p i-w)
         (wb::canvas-visible-p i-w))
    ))


(defun inform-user (message)
  "Informs the user about the message. ~
   (:required (:arg message String to be displayed to the user.)) ~
   (:returns No value.)"
  (let ((i-t (info-text-view))
        (i-w (info-window)))
    (setf (text-of i-t) (format NIL "~a" (format NIL "~%~a" message)))
    (cond
     ((info-window-alive-p)
      (wb::canvas-clear i-w)
      (wb::canvas-to-top i-w)
      (if (viewports-of i-t)
        (draw-view i-t)
        (draw-view i-t :viewport (make-viewport i-w))))
     (T (setf *info-window* NIL)
        (setf (viewports-of i-t) NIL)
        ;;(loop for vp in (viewports-of i-t)
        ;;      do (remove-view i-t :viewport vp))
        (setf i-w (info-window))
        (draw-view i-t :viewport (make-viewport i-w))))))

(let ((i 0))
  (defun inform-user-busy ()
    "A signal to the user that something is happening.  ~
     Repeated calls will show something changing on the screen."
    (setf i (mod (+ i 1) 10))
    (cond
     ((zerop i) (vw::inform-user "***"))
     ((= i 1) (vw::inform-user " ***"))
     ((= i 2) (vw::inform-user "  ***"))
     ((= i 3) (vw::inform-user "   ***"))
     ((= i 4) (vw::inform-user "    ***"))
     ((= i 5) (vw::inform-user "     ***"))
     ((= i 6) (vw::inform-user "      ***"))
     ((= i 7) (vw::inform-user "       ***"))
     ((= i 8) (vw::inform-user "        ***"))
     ((= i 9) (vw::inform-user "         ***")))))
