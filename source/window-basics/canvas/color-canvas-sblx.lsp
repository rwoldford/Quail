;;; This is ~/RESERVE/lc2-Quail/source/window-basics/canvas/color-canvas-sblx.lsp
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

;;; 10MAR2021 - START defun to replace the initialize-instance :after method of font-mixin
(defun set-color-initial-font (self &rest initargs
                                       &key font)
(declare (ignore initargs))
(if (canvas-font-p font);(canvas-font-p self)
  (setf (canvas-font self) font)
  (setf (canvas-font self)
    (host-font-to-canvas-font font)))
  )
;;; 10MAR2021 - END defun to replace the initialize-instance :after method of font-mixin

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
  (declare (special *normal-graphics-font*
                    *default-canvas-background-color*
                    *default-canvas-pen-color*
                    *white-color*
                    *black-color*)
           (ignorable canvas-class))
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
    ;; Set the pen attributes
  (unless (pen-of frame) ;;; 4 lines in 09OCT2024 to get pen-color-of non-nil for subsequent drawing
  (setf (slot-value frame 'pen)
    (make-instance 'pen)))
(canvas-set-pen frame :width pen-width :operation pen-operation :color pen-color)
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