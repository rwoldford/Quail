;;; This is ~/RESERVE/lc2-Quail/source/window-basics/canvas/new-bw-canvas-sblx.lsp
;;; to try to fix :left and :bottom args
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         bw-canvas-sblx.lsp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-b&w-canvas)))
;;; Wholesale import of color-canvas-sbxl then modify

;;; =======================================================================
;;;                     Creating a black&white canvas
;;; =======================================================================

;;; Replacement for initialiaze-instance :afrer until that gets fixed gwb 18MAR2021
(defun set-bw-initial-penproperties (self &rest initargs
                                       &key pen-color pen-width pen-operation)
(declare (ignore initargs))
(format t "~%Entering set-color-inital-pen&properties")
(format t "~%s-bw-i-pp input pen-color is ~s " pen-color)
(format t "~%s-bw-i-pp input pen-width is ~s " pen-width)
(format t "~%s-bw-i-pp input pen-operation is ~s " pen-operation)
(format t "~%s-bw-i-pp input self is ~s " self)
(format t "~%s-bw-i-pp is self a frame ? ~s " (clim:application-frame-p self))
(format t "~%s-bw-i-pp does is have a host-pane ? ~s " (clim:get-frame-pane self 'host-pane))
(format t "~%s-bw-i-pp is self a pen-mixin ? ~s" (member (find-class 'pen-mixin)
      (sb-mop:class-direct-superclasses
       (first (sb-mop:class-direct-superclasses
         (class-of self))))))
(unless (pen-of self)
  (setf (slot-value self 'pen)
    (make-instance 'pen)))
(canvas-set-pen self :width pen-width :operation pen-operation :color pen-color)
)


;;; 10MAR2021 - START defun to replace the initialize-instance :after method of font-mixin
(defun set-bw-initial-font (self &rest initargs
                                       &key font)
(declare (ignore initargs))
(format t "~%Entering set-inital-font")
(format t "~%s-i-f input font is ~s " font)
(format t "~%s-i-f is it a canvas-font ~s" (canvas-font-p font))
(format t "~%s-i-f is it a host-font ~s " (eql (type-of font) 'standard-text-style))
(format t "~%s-i-f input self is ~s " self)
(format t "~%s-i-f is self a frame ? ~s " (clim:application-frame-p self))
(format t "~%s-i-f does is have a host-pane ? ~s " (clim:get-frame-pane self 'host-pane))
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


;;; a new copy
(define-application-frame quail-bw-canvas (canvas)
  ((left :initarg :left :accessor left)
    (bottom :initarg :bottom :accessor bottom)
    (height :initarg :height :accessor height)
    (width :initarg :width :accessor width)
    (title :initarg :title :accessor title)
    ;(color :initarg :color :accessor color)
    (canvas-keywords :initarg :canvas-keywords :accessor canvas-keywords)
    (pen-color :initarg :pen-color :accessor pen-color)
    (pen-width :initarg :pen-width :accessor pen-width)
    (pen-operation :initarg :pen-operation :accessor pen-operation)
    (font :initarg :font :accessor font)
    )
  (:menu-bar nil) ;quail-canvas-command-table) ;<- here
  (:panes
    (host-pane :application :scroll-bars T :min-width (width *application-frame*) :min-height (height *application-frame*)
      :background *default-canvas-background-color* :foreground (pen-color *application-frame*)
      :text-style (canvas-font-to-host-font (font *application-frame*)))
    )
  (:layouts
   (default
      host-pane
   )))

  (defun make-b&w-canvas (&rest
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
                          (font *normal-graphics-font*)
                          &allow-other-keys)
  "Creates and returns a color canvas."
  (declare (special *normal-graphics-font*
                    *default-canvas-background-color*
                    *default-canvas-pen-color*
                    *white-color*
                    *black-color*)
           (ignorable canvas-class background-color canvas-keywords) ;10MAY2024
           )
  (format t "~%mcc-input pen-color is ~s " pen-color)
  (format t "~%mcc-input pen-width is ~s " pen-width)
     (let ((frame (make-application-frame 'quail-bw-canvas :pretty-name title 
      :left left
      :bottom (- (screen-height) bottom) 
      :width width
      :height height
      :pen-color pen-color
      :pen-width pen-width
      :pen-operation pen-operation
      :canvas-keywords (list NIL)
      :font font
      :&allow-other-keys T
      )))
  (sb-thread::make-thread (lambda () (run-frame-top-level frame)))
  (sleep 1) ;;<<== IMPORTANT .. allows thread to start and panes to be instantiated
   ;;
    ;; set inital pen properties
    (set-bw-initial-penproperties frame :pen-color pen-color :pen-width pen-width :pen-operation pen-operation)
  ;; Set the font/text-style
(set-bw-initial-font frame :font font) 
    
    ;; Finally get the pen back to the correct origin
    ;;
    (canvas-move-to frame 0 0)
  frame))

  ;;; gesture to that l-click on pane makes frame *current-canvas*
  (define-presentation-action do-change-canvas
    (blank-area nil quail-bw-canvas :gesture :select)
    (object)
  (with-application-frame (frame)
    (update-ccqc frame)
    ))