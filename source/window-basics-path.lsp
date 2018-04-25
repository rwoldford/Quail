;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               window-basics-path.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :make)

(defun path-window-basics ()
  (system-name-convert "Window-Basics;"))

(defun fullpath-window-basics ()
  ;;(append-directories
  (concatenate 'string (path-source) (path-window-basics)))

(defun fullpath-postscript ()
  (concatenate 'string (path-quail)
                      (system-name-convert "Foreign;Postscript;")))

(defun path-window-basics-startup ()
  (system-name-convert "Startup;"))

(defun path-window-basics-macros ()
  (system-name-convert "Macros;"))

(defun path-window-basics-host ()
  (system-name-convert "Host;"))

(defun path-window-basics-region ()
  (system-name-convert "Region;"))

(defun path-window-basics-transforms ()
  (system-name-convert "Transforms;"))

(defun path-window-basics-color ()
  (system-name-convert "Color;"))

(defun path-window-basics-fonts ()
  (system-name-convert "Fonts;"))

(defun path-window-basics-pen ()
  (system-name-convert "Pen;"))

(defun path-window-basics-bitmap ()
  (system-name-convert "Bitmap;"))

(defun path-window-basics-monitor ()
  (system-name-convert "Monitor;"))

(defun path-window-basics-mouse ()
  (system-name-convert "Mouse;"))

(defun path-window-basics-menus ()
  (system-name-convert "Menus;"))

(defun path-window-basics-redisplay ()
  (system-name-convert "Redisplay;"))

(defun path-window-basics-postscript ()
  (system-name-convert "Postscript;"))

(defun path-window-basics-canvas ()
  (system-name-convert "Canvas;"))

(defun path-window-basics-draw ()
  (system-name-convert "Draw;"))

(defun path-window-basics-canvas-regions ()
  (system-name-convert "Canvas-Regions;"))

(defun path-window-basics-prompt ()
  (system-name-convert "Prompt;"))

(defun path-window-basics-hardcopy ()
  (system-name-convert "Hardcopy;"))

(defun path-window-basics-fast-graphics ()
  (system-name-convert "Fast-Graphics;"))

(defun path-window-basics-surface ()
  (system-name-convert "Surface;"))

(defun path-window-basics-start-windows ()
  (system-name-convert "Start-Windows;"))

