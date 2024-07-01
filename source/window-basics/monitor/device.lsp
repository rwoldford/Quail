;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               device.lisp
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
;;;     R.W. Oldford 1992
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*device-types* *current-device-type* set-device-type
           device-type color-device-p gray-scale-device-p black&white-device-p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Device types
;;;
;;; Useful for setting up default kinds of canvas.
;;; User can always make a canvas that is not applicable to the
;;; current (device-type).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *device-types* '(:black&white :color :gray-scale)
  "Allowable device types.  Minimally, one of :black&white :color or :gray-scale.  ~
   Device type is used by window-basics to determine the ~
   default kind of canvas.  ~
   Default type is :black&white.")

(defvar *current-device-type* :black&white
  "The current device type.  ~
   Device type is used by window-basics to determine the ~
   default kind of canvas.  ~
   Default type is :black&white. ~
   Set with set-device-type, access with device-type.")

(defun set-device-type (&optional (type :black&white))
  "Sets the current device type to be the value of type.  ~
   Device type is used by window-basics to determine the ~
   default kind of canvas.  ~
   Default type is :black&white."
  (declare
   (special *current-device-type* *device-types*))
  (if (member type *device-types*)
    (setf *current-device-type*
          (or
           (case type
             ;; Allow different spelling
             (:colour :color)
             ;; Allow different spelling
             (:grey-scale :gray-scale)
             )
           type))
    (quail-error "Illegal device type: ~s." type)))

(defun device-type ()
  "Returns the current device type."
  (declare (special *current-device-type*))
  *current-device-type*)

(defun color-device-p ()
  "Determine if :color is the current device type."
  (eq :color (device-type)))

(defun gray-scale-device-p ()
  "Determine if :gray-scale is the current device type."
  (eq :gray-scale (device-type)))

(defun black&white-device-p ()
  "Determine if :black&white is the current device type."
  (eq :black&white (device-type)))

(eval-when (:load-toplevel :execute)
  (if (color-available-p)
    (set-device-type :color)
    (set-device-type :black&white)))
