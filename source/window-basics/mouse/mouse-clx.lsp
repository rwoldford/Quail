;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mouse-clx.lisp
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
;;;     N.G. Bennett 1993
;;;     R.W. Oldford 1994
;;;
;;;==========================================================================
;;; History:
;;;
;;;    - created June 1989 by rwoldford@watstat.uwaterloo.ca to provide
;;;      uniform three-button mouse interaction.
;;;
;;;      
;;;      Each key can also be modified by either the shift-key or the
;;;      control-key.
;;;      
;;;===========================================================================

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(mouse-position mouse-x mouse-y
          screen-mouse-position screen-mouse-x screen-mouse-y)))


(defmacro host-to-canvas-mouse-state (state)
  `(cond ((eq ,state 256)  :left)
	 ((eq ,state 257)  :left)
	 ((eq ,state 260)  :left)
	 ((eq ,state 512)  :middle)
	 ((eq ,state 513)  :middle)
	 ((eq ,state 516)  :middle)
	 ((eq ,state 1024) :right)
	 ((eq ,state 1025) :right)
	 ((eq ,state 1028) :right)))


(defun mouse-down-p (&optional canvas)
  "Tests whether a mouse button is being held down."
  (if canvas
    (multiple-value-bind 
      (a b c d state)
      (xlib::query-pointer (host-window canvas))
      (host-to-canvas-mouse-state state))
    (multiple-value-bind
      (foo bar root-window)
      (xlib::global-pointer-position *default-display*)
      (multiple-value-bind 
        (a b c d state)
        (xlib::query-pointer root-window)
        (host-to-canvas-mouse-state state)))))

(defun mouse-position (canvas)
  (multiple-value-bind
    (x y)
    (xlib::pointer-position (host-window canvas))
    (make-position x (host-to-canvas-y canvas y))))

(defun mouse-x (canvas)
  (xlib::pointer-position (host-window canvas)))
                    
(defun mouse-y (canvas) 
  (multiple-value-bind
    (x y)
    (xlib::pointer-position (host-window canvas))
    (host-to-canvas-y canvas y)))

;;;========================================================================================
;;; mouse position in screen coordinates, cbh
;;;========================================================================================

(defun screen-mouse-position ()
  "Returns position of mouse in screen coords."
  (multiple-value-bind
    (x y)
    (xlib::global-pointer-position *default-display*)
    (make-position x y)))

(defun screen-mouse-x ()
  (multiple-value-bind
    (x y)
  (xlib::global-pointer-position *default-display*)
  x))

(defun screen-mouse-y ()
  (multiple-value-bind
    (x y)
    (xlib::global-pointer-position *default-display*)
    y))

(defun same-button-state-p (state test)
	(equal state test))

(defmacro get-wb-class-instance (window)
  `(cdr (assoc :wb-class-instance (xlib:window-plist ,window))))

(defmethod wb-refresh ((self null))
  (declare (ignore self)))
#|
(defmethod xwindow-of ((self xlib::window))
  (parent-of (parent-of self)))
|#

(defun event-handler (display &aux (last NIL))
  (xlib:event-case
   (display :force-output-p t :discard-p t)
   (:map-notify
    (window)
    (let ((c (get-wb-class-instance window)))
      (when (canvas-p c)
        (menu-present (menu-bar-of c))))
    nil)
   (:exposure
    (window count)
    (when (zerop count)
      (xlib::with-event-queue (display)
        (wb-refresh (get-wb-class-instance window))))
    nil)
   (:configure-notify
    (window x y width)
    (let ((c (get-wb-class-instance window)))
      (when (canvas-p c)
        (menu-present (menu-bar-of c))
        ))
    nil)
   (:visibility-notify
    ()
    #|(window state)
       (let ((c (get-wb-class-instance window)))
         (when (and (member state
                            '(:partially-obscured :fully-obscured)) 
                    (menu-p c))
           (menu-present
            c 
           )))
       |#
    nil)
   (:button-press
    (window code state x y root-x root-y)
    (let ((c (get-wb-class-instance window)))
      (cond
       ((canvas-p c)
        (let ((mouse-pos (make-position
                          x 
                          (host-to-canvas-y c y))))
          (cond
           ((equal code *left-button-code*)
            (cond
             ((equal state *button-no-modifier*)
              (funcall (left-button-fn-of c) c mouse-pos))
             ((equal state *shift-button*)
              (funcall (shift-left-button-fn-of c) c mouse-pos))
             ((equal state *ctrl-button*)
              (funcall (ctrl-left-button-fn-of c) c mouse-pos))))
           ((equal code *middle-button-code*)
            (cond
             ((equal state *button-no-modifier*)
              (funcall (middle-button-fn-of c) c mouse-pos))
             ((equal state *shift-button*)
              (funcall (shift-middle-button-fn-of c) c mouse-pos))
             ((equal state *ctrl-button*)
              (funcall (ctrl-middle-button-fn-of c) c mouse-pos))))
           ((equal code *right-button-code*)
            (cond
             ((equal state *button-no-modifier*)
              (funcall (right-button-fn-of c) c mouse-pos))
             ((equal state *shift-button*)
              (funcall (shift-right-button-fn-of c) c mouse-pos))
             ((equal state *ctrl-button*)
              (funcall (ctrl-right-button-fn-of c) c mouse-pos)))))))
       
       ((and (menu-p c) (enabled-p c))
        (menu-highlight c)
        (when ;;(and (eq window (item-window-of c))
          (sub-menus-of c)
          ;;)
          (xlib::with-event-queue (display)
            (place-item-sub-menu c))))
       ((dialog-item-p c)
        (process-button-press-event c window))
       )
      )
    nil)
   (:button-release
    (event-window state)
    (let ((c (get-wb-class-instance event-window)))
      (when (canvas-p *current-canvas*)
        (unmap-submenus (menu-bar-of *current-canvas*))
        ;;(dolist (thing (sub-menus-of (menu-bar-of *current-canvas*)))
        ;;  (menu-downlight thing)
        ;;  (when (eq (xlib::window-map-state (window-of thing)) :viewable)
        ;;    (xlib::unmap-window (window-of thing))))
        )
      (cond
       ((and (menu-p c)
             (enabled-p c)
             (mouse-in-item-p c))
        (funcall (when-selected-fn-of c)
                 (items-of c)
                 (super-menu-of c)
                 (host-to-canvas-mouse-state state))
        )
       ((dialog-item-p c)
        (catch :cancel
          (process-button-release-event c event-window)))
       )
      )
    nil)
   (:key-press
    (window code state)
    (let ((c (get-wb-class-instance window)))
      (when (dialog-item-p c)
          (process-key-press-event c window code state)
          ))
    NIL)
   (:enter-notify
    (window state)
    (let ((c (get-wb-class-instance window)))
      (if (canvas-p c)
        (setf *current-canvas* c)
        (cond
         ((and (menu-p c) (host-to-canvas-mouse-state state))
          (when (and last (sub-menus-of last)
                     (not (member c (sub-menus-of last))))
            (dolist (thing (sub-menus-of (super-menu-of c)))
              (when (eq (xlib::window-map-state (window-of thing)) :viewable)
                (xlib::unmap-window (window-of thing)))))
          (when (enabled-p c)
            (menu-highlight c)
            (when (sub-menus-of c)
              (xlib::with-event-queue (display)
                (place-item-sub-menu c)))
            ))
         ((dialog-item-p c)
          (dialog-enter-notify c window state)))))
    nil)
   (:leave-notify
    (window state)
    (let ((c (get-wb-class-instance window)))
      (cond
       ((and (menu-p c) (host-to-canvas-mouse-state state))
        (setf last c)
        (menu-downlight c))
       ((dialog-item-p c)
        (dialog-leave-notify c window))
       )
      )
    nil)
   (:unmap-notify
    (window)
    (let ((c (get-wb-class-instance window)))
      (if ;;(and 
           (canvas-p c)
           ;;(eq window (xwindow-of c)))
        (shutdown-menu (menu-bar-of c))
        (when (and (menu-p c) (eq window (window-of c)))
          (unmap-submenus c)
          ;;(dolist (thing (sub-menus-of c))
          ;;  (when (eq (xlib::window-map-state (window-of thing)) :viewable)
          ;;    (xlib::unmap-window (window-of thing))))
          )))
    nil)
   (otherwise  nil)	
   ))

;;--------------------------------------------------------------
;;	EVENT MASKS FOR BUTTON-PRESS EVENTS
;;--------------------------------------------------------------

(defparameter *left-button-code*    1)
(defparameter *middle-button-code*  2)
(defparameter *right-button-code*   3)
(defparameter *button-no-modifier*  0)
(defparameter *shift-button*        1)
(defparameter *ctrl-button*         4)

;;--------------------------------------------------------------
;;	EVENT MASKS FOR BUTTON-RELEASE EVENTS
;;--------------------------------------------------------------

(defparameter *left-button-no-modifier* 256)
(defparameter *shift-left-button* 257)
(defparameter *ctrl-left-button* 260)

(defparameter *middle-button-no-modifier* 512)
(defparameter *shift-middle-button* 513)
(defparameter *ctrl-middle-button* 516)

(defparameter *right-button-no-modifier* 1024)
(defparameter *shift-right-button* 1025)
(defparameter *ctrl-right-button* 1028)

