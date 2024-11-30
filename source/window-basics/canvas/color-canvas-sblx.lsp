;;; This is ~/RESERVE/lc2-Quail/source/window-basics/canvas/new-color-canvas-sblx.lsp
;;; modeled on (same-dir)/new-bw-canvas-sblx.lsp
;;; to fix location, names, threads etc..

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         color-canvas-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     N.G. Bennett 1993
;;;     R.W. Oldford 1994
;;;     G.W. Bennett 1996
;;;     
;;;----------------------------------------------------------------------------------


(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-color-canvas)))
;;; =======================================================================
;;;                     Creating a colour canvas
;;; =======================================================================

#|
;;; a new copy
;(define-application-frame quail-color-canvas (canvas) ;24SEP2024
(define-application-frame color-canvas (canvas) ;24SEP2024                             
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
    )
  (:menu-bar nil) ;quail-canvas-command-table) ;<- here
  (:panes
    (host-pane :application :scroll-bars T :width (width *application-frame*) :height (height *application-frame*)
      :background (background-of *application-frame*) :foreground (pen-color *application-frame*)
      :text-style  clim-user::*default-text-style* :display-time nil );(canvas-font-to-host-font (font *application-frame*)) :display-time nil) 
    )
  (:layouts
   (default
      host-pane
   )))
|#
#|
;;; See comments in make-color-canvas
;;; the color and font activity is taken care of in the pane piece of the definition
;;; and in 4 lines added to make-color-canvas on 09OCT2024

;;; 05MAR2021 - START defun to replace the :after method of pen-mixin
(defun set-color-initial-penproperties (self &rest initargs
                                       &key pen-color pen-width pen-operation)
(declare (ignore initargs))
(format t "~%Entering set-color-inital-penproperties")
(format t "~%s-c-i-pp input pen-color is ~s " pen-color)
(format t "~%s-c-i-pp input pen-width is ~s " pen-width)
(format t "~%s-c-i-pp input pen-operation is ~s " pen-operation)
(format t "~%s-c-i-pp input self is ~s " self)
(format t "~%s-c-i-pp is self a frame ? ~s " (clim:application-frame-p self))
(format t "~%s-c-i-pp does is have a host-pane ? ~s " (clim:get-frame-pane self 'wb::host-pane))
(format t "~%s-c-i-pp is self a pen-mixin ? ~s" (member (find-class 'pen-mixin)
      (sb-mop:class-direct-superclasses
       (first (sb-mop:class-direct-superclasses
         (class-of self))))))
(unless (pen-of self)
  (setf (slot-value self 'pen)
    (make-instance 'pen)))
(canvas-set-pen self :width pen-width :operation pen-operation :color pen-color)
)
#|
(defmethod initialize-instance :after ((self pen-mixin)
                                       &rest initargs
                                       &key pen-color pen-width pen-operation)
  (declare (ignore initargs))
 (sleep 10) ;; 05MAR2021
  (format t "~%i-i input pen-color is ~s " pen-color)
  (format t "~%i-i input pen-width is ~s " pen-width)
  (format t "~%i-i input pen-operation is ~s " pen-operation)
  (format t "~%i-i input self is ~s " self)
  (format t "~%i-i is self a frame ~s , a colored-canvas ~s " (clim:application-frame-p self) (colored-canvas-p self))
  (format t "~%i-i does it have a host-pane ~s " (clim:get-frame-pane self 'host-pane))
  (unless (pen-of self)
    (setf (slot-value self 'pen)
          (make-instance 'pen)))
  (canvas-set-pen self :width pen-width :operation pen-operation :color pen-color)
  )
|#
;;; 05MAR2021 - END defun to replace the :after method of pen-mixin

;;; 10MAR2021 - START defun to replace the initialize-instance :after method of font-mixin
(defun set-color-initial-font (self &rest initargs
                                       &key font)
(declare (ignore initargs))
(format t "~%Entering set-inital-font")
(format t "~%s-i-f input font is ~s " font)
(format t "~%s-i-f is it a canvas-font ~s" (canvas-font-p font))
(format t "~%s-i-f is it a host-font ~s " (eql (type-of font) 'standard-text-style))
(format t "~%s-i-f input self is ~s " self)
(format t "~%s-i-f is self a frame ? ~s " (clim:application-frame-p self))
(format t "~%s-i-f does is have a host-pane ? ~s " (clim:get-frame-pane self 'wb::host-pane))
(format t "~%s-i-f is self a font-mixin ? ~s" (member (find-class 'font-mixin)
      (sb-mop:class-direct-superclasses
       (first (sb-mop:class-direct-superclasses
         (class-of self))))))
(if (canvas-font-p font);(canvas-font-p self)
  (setf (canvas-font self) font)
  (setf (canvas-font self)
    (host-font-to-canvas-font font)))
  )
;;; 10MAR2021 - END defun to replace the initialize-instance :after method of font-mixin
|#
  (defun make-color-canvas (&rest
                          canvas-keywords
                          &key
                          left bottom width height
                          ;;           (type (device-type))
                          (canvas-class 'canvas)
                          (title "Color Canvas")
                          (background-color NIL)
                          (pen-color NIL)
                          (pen-width nil)
                          (pen-operation nil)
                          (font clim-user::*default-text-style*) ;*normal-graphics-font*)
                          &allow-other-keys)
  "Creates and returns a color canvas."
  (declare ;(special *normal-graphics-font*
           ;         *default-canvas-background-color*
           ;         *default-canvas-pen-color*
           ;         *white-color*
           ;         *black-color*)
           (ignorable canvas-class))
  (format t "~%mcc-input pen-color is ~s " pen-color)
  (format t "~%mcc-input pen-width is ~s " pen-width)
     (let ((frame (make-application-frame 'canvas :pretty-name title;'color-canvas :pretty-name title                                                                
      :left left
      :bottom (- (screen-height) bottom) 
      :width width 
      :height height
      :pen-color pen-color
      :pen-width pen-width
      :pen-operation pen-operation
      :font font
      :canvas-keywords (list NIL)
      :color? T 
      :region (make-region left bottom width height)
      :background-color background-color 
      :title title
      :&allow-other-keys T
      )))
  (sb-thread::make-thread (lambda () (run-frame-top-level frame)))
  (sleep 1) ;<<== IMPORTANT .. allows thread to start (?)
  (format t "~% c-c-sblx frame is ~s " frame)
  (format t "~% c-c-sblx (pen-of frame) is ~s " (pen-of frame))
    ;; Set the pen attributes
  (unless (pen-of frame) ;;; 4 lines in 09OCT2024 to get pen-color-of non-nil for subsequent drawing
  (setf (slot-value frame 'pen)
    (make-instance 'pen)))
  (format t "~% c-c-sblx (slot-value frame 'pen) is ~s " (slot-value frame 'pen))
(canvas-set-pen frame :width pen-width :operation pen-operation :color pen-color)
(format t "~% c-c-sblx just after canvas-set-pen") ;13OCT2024

;(set-color-initial-penproperties frame :pen-color pen-color :pen-width pen-width :pen-operation pen-operation)  ;;09OCT2024
; done in the frame definition and immediately above
  ;; Set the font/text-style
;(set-color-initial-font frame :font font) ;;09OCT2024
    ;; Now set the colors 
    ;;
    ;(canvas-set-background-color frame (or background-color
    ;                                  *default-canvas-background-color*))
    
;adjust menus and menu-bar if necessary
(unless (member 'com-canvas (command-table-items 'qmbar)) ;quailmenubar))
       (execute-frame-command *system-default-menubar* '(com-change-menu-bar qpc-command-table)));new-command-table))) ;quailqpc-command-table))) ;26NOV2024   
    
    ;; Finally get the pen back to the correct origin
    
    ;(canvas-move-to frame 0 0)
  frame))
#|
  ;;; gesture so that l-click on pane makes frame *current-canvas*
  (define-presentation-action do-change-canvas
    ;(blank-area nil quail-color-canvas :gesture :select)
    (blank-area nil color-canvas :gesture :select)
    (object)
  (with-application-frame (frame)
    (update-ccqc frame)
    ))
    |#