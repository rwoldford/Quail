;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mouse.lisp
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
;;;     R.W. Oldford 1989-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '()))


;;; The mouse event dispatcher

(defun mouse-button-event-fn (canvas mouse-pos)
  "Function dispatches to other button-event-fn's on basis of the ~
   state of the mouse buttons."
  (case (mouse-state)
    (:left   (mouse-left-button-event-fn canvas mouse-pos))
    (:middle (mouse-middle-button-event-fn canvas mouse-pos))
    (:right  (mouse-right-button-event-fn canvas mouse-pos))
    (:none   (mouse-no-button-event-fn canvas mouse-pos))))


;;; Decided to mimic the Xerox Interlisp-D windows - attaching left,
;;; middle, and right button event functions directly on each window.
;;; Each of these can be modified with either the shift-key or the ctrl-key.
;;; Note that these functions are instance variables and hence can be
;;; different for each instance of canvas-button.
;;;


(defun mouse-left-button-event-fn (canvas mouse-pos)
  (if (shift-key-p)
    (funcall (shift-left-button-fn-of canvas) canvas mouse-pos)
    (if (control-key-p)
      (funcall (ctrl-left-button-fn-of canvas) canvas mouse-pos)
      (funcall (left-button-fn-of canvas) canvas mouse-pos))))

(defun mouse-middle-button-event-fn (canvas mouse-pos)
  (if (shift-key-p)
    (funcall (shift-middle-button-fn-of canvas) canvas mouse-pos)
    (if (control-key-p)
      (funcall (ctrl-middle-button-fn-of canvas) canvas mouse-pos)
      (funcall (middle-button-fn-of canvas) canvas mouse-pos))))


(defun mouse-right-button-event-fn (canvas mouse-pos)
  (if (shift-key-p)
    (funcall (shift-right-button-fn-of canvas) canvas mouse-pos)
    (if (control-key-p)
      (funcall (ctrl-right-button-fn-of canvas) canvas mouse-pos)
      (funcall (right-button-fn-of canvas) canvas mouse-pos))))


(defun mouse-no-button-event-fn (canvas mouse-pos)
  (declare (ignore canvas mouse-pos))
  )


