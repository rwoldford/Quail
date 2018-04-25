;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               canvas-button.lisp
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
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1991
;;;     J.O. Pedersen 1988-89
;;;     
;;;      
;;;===========================================================================

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-button get-left-button-fn
           get-shift-left-button-fn get-ctrl-left-button-fn
           get-middle-button-fn get-shift-middle-button-fn
           get-ctrl-middle-button-fn get-right-button-fn
           get-shift-right-button-fn get-ctrl-right-button-fn
           set-left-button-fn set-shift-left-button-fn
           set-ctrl-left-button-fn set-middle-button-fn
           set-shift-middle-button-fn set-ctrl-middle-button-fn
           set-right-button-fn set-shift-right-button-fn
           set-ctrl-right-button-fn)))

;;;=====================================================================
;;; Want various button behaviours for interior of windows.
;;; Accomplished by defining an appropriate mixin for canvas:
;;;
;;;                  canvas-button
;;;
;;; ...rwo
;;;=====================================================================


(defclass canvas-button ()
  ((left-button-fn           :initarg :left-button-fn
                             :initform #'default-left-button-fn
                             :accessor left-button-fn-of)

   (middle-button-fn         :initarg :middle-button-fn
                             :initform #'default-middle-button-fn
                             :accessor middle-button-fn-of)

   (right-button-fn          :initarg :right-button-fn
                             :initform #'default-right-button-fn
                             :accessor right-button-fn-of)

   (shift-left-button-fn     :initarg :shift-left-button-fn
                             :initform #'default-shift-left-button-fn
                             :accessor shift-left-button-fn-of)

   (shift-middle-button-fn   :initarg :shift-middle-button-fn
                             :initform #'default-shift-middle-button-fn
                             :accessor shift-middle-button-fn-of)

   (shift-right-button-fn    :initarg :shift-right-button-fn
                             :initform #'default-shift-right-button-fn
                             :accessor shift-right-button-fn-of)

   (ctrl-left-button-fn      :initarg :ctrl-left-button-fn
                             :initform #'default-ctrl-left-button-fn
                             :accessor ctrl-left-button-fn-of)

   (ctrl-middle-button-fn    :initarg :ctrl-middle-button-fn
                             :initform #'default-ctrl-middle-button-fn
                             :accessor ctrl-middle-button-fn-of)

   (ctrl-right-button-fn     :initarg :ctrl-right-button-fn
                             :initform #'default-ctrl-right-button-fn
                             :accessor ctrl-right-button-fn-of)))

;;;========================================================================================
;;; functions for accessing and setting button functions, cbh
;;;========================================================================================

(defun get-left-button-fn (canvas)
  (left-button-fn-of canvas))
(defun get-shift-left-button-fn (canvas)
  (shift-left-button-fn-of canvas))
(defun get-ctrl-left-button-fn (canvas)
  (ctrl-left-button-fn-of canvas))

(defun get-middle-button-fn (canvas)
  (middle-button-fn-of canvas))
(defun get-shift-middle-button-fn (canvas)
  (shift-middle-button-fn-of canvas))
(defun get-ctrl-middle-button-fn (canvas)
  (ctrl-middle-button-fn-of canvas))

(defun get-right-button-fn (canvas)
  (right-button-fn-of canvas))
(defun get-shift-right-button-fn (canvas)
  (shift-right-button-fn-of canvas))
(defun get-ctrl-right-button-fn (canvas)
  (ctrl-right-button-fn-of canvas))


(defun set-left-button-fn (canvas new-fn)
  (setf (left-button-fn-of canvas) new-fn))
(defun set-shift-left-button-fn (canvas new-fn)
  (setf (shift-left-button-fn-of canvas) new-fn))
(defun set-ctrl-left-button-fn (canvas new-fn)
  (setf (ctrl-left-button-fn-of canvas) new-fn))

(defun set-middle-button-fn (canvas new-fn)
  (setf (middle-button-fn-of canvas) new-fn))
(defun set-shift-middle-button-fn (canvas new-fn)
  (setf (shift-middle-button-fn-of canvas) new-fn))
(defun set-ctrl-middle-button-fn (canvas new-fn)
  (setf (ctrl-middle-button-fn-of canvas) new-fn))

(defun set-right-button-fn (canvas new-fn)
  (setf (right-button-fn-of canvas) new-fn))
(defun set-shift-right-button-fn (canvas new-fn)
  (setf (shift-right-button-fn-of canvas) new-fn))
(defun set-ctrl-right-button-fn (canvas new-fn)
  (setf (ctrl-right-button-fn-of canvas) new-fn))

