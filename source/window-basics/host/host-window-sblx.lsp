;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     host-window-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     G.W. Bennett 1996 2017
;;;--------------------------------------------------------------------

(in-package :wb)
;;; host-pane is an addition to the export list
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(host-window))) ;host-pane)))

;;; A definition to capture the McCLIM aspects of what will become a host-window

(define-application-frame host-window ()                          
  ((left :initarg :left :accessor left)
   (bottom :initarg :bottom :accessor bottom)
    (height :initarg :height :accessor height)
    (width :initarg :width :accessor width)
    (title :initarg :title :accessor title)
    (color :initarg :color :accessor color)
    (canvas-keywords :initarg :canvas-keywords :accessor canvas-keywords)
    (pen-color :initarg :pen-color :accessor pen-color)
    (pen-width :initarg :pen-width :accessor pen-width)
    (pen-operation :initarg :pen-operation :accessor pen-operation)
    (font :initarg :font :accessor font)
    (region :initarg :region :accessor region-of ) 
    (background-color :initarg :background-color :accessor background-of)
    ;(title :initarg "HOST WINDOW" :accessor title-of) 
    )
  (:menu-bar nil) ;quail-canvas-command-table) ;<- here
  (:panes
    (host-pane :application 
               ;:scroll-bars T 
               :width (width *application-frame*) :height (height *application-frame*)
      :background (background-of *application-frame*) :foreground (pen-color *application-frame*)
      :text-style *default-text-style* :display-time nil ;(canvas-font-to-host-font (font *application-frame*)) :display-time nil
      ) 
    )
  (:layouts
   (default
      host-pane
   )))

;;; The revised definition of host-window

;(defclass host-window (mcclim-window);(clim::standard-application-frame) ;;14OCT2024
;  ()
;  (:documentation
;   "Mixin to canvas that captures properties of the host window system."))


#|
(defmethod cg::resize-window ((self host-window) pos)
   "What happens when the window is resized"
   (let ((result (call-next-method)))
      (redisplay self)  ;;;;;; <---- important bit
      result)
   )
 
(defmethod initialize-instance ((self host-window) &key)
   (call-next-method)
;;   (setf (current-position self) (list 0 0))
   (setf (current-position self) (cg::make-position 0 0))
   (setf *current-canvas* self))
|#

;;; Added Oct 28, 1997
;;; to set wb::*current-canvas*
;(defclass host-pane (clim::application-pane) ()) ;;14OCT2024
;(defclass host-pane (clim-stream-pane) ())
#|
(defmethod cg::default-pane-class ((c host-window))
   'host-pane)

(defmethod cg::select-window  :after ((self host-window) &optional recursive-p)
   (declare (special *current-canvas*)
           (ignore recursive-p))
   (setf *current-canvas* self))
|#
#|   ;;14)CT2024
(defmethod clim::raise-frame :after ((self host-window))
  (declare (special *current-canvas*))
    (setf *current-canvas* (clim::raise-frame self)))

(defmethod clim:destroy-frame :after ((self host-window))
  (declare (special *current-canvas*))
  (setf *current-canvas* nil))

(defmethod clim:bury-frame :after ((self host-window))
  (declare (special *current-canvas*))
  (setf *current-canvas* nil))

;(defmethod cg::bring-window-to-front  :after ((self host-pane))
;     (declare (special *current-canvas*))
;   (setf *current-canvas* (clim::raise-frame (clim::pane-frame self)))

|#  ;; 14OCT2024
#| 
;;;28JAN02 this functionality has been assumed by calls to STATE
(defmethod cg::expand-window :around ((self host-pane))
   (declare (special *current-canvas*))
   (let ((result (call-next-method)))
      (if result
         (setf *current-canvas* (cg::parent self)))
      result))
|#

#|
;;; Moved from Redisplay/canvas-redisplay-pc.lsp 052598  gwb.
(defmethod clim:redisplay-pane-frame :after ((c host-pane) &optional box)
   (declare (special *current-canvas*)
           (ignore box))
   (let ((pw (clim::pane-frame c)))
      (when (eq pw (first (wb::canvases)))
         (setf *current-canvas* pw))
      )
   )
|#