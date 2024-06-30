;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               key-event.lisp
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
;;;     R.W. Oldford 1996
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(handle-key-event)))


(defgeneric handle-key-event (receiver event)
  (:documentation "Generic function called on the receiver when the keyboard event, ~
                   event, has occurred.  Methods should be defined for particular receivers ~
                   and events.  If receiver does handle the event, for whatever ~
                   reason, then this function must return NIL.  Otherwise it returns ~
                   non-NIL.")
  )

(defmethod handle-key-event ((receiver T) (event T))
  "Ignores both receiver and event and returns NIL.  The default behaviour ~
   in that most things just do not handle key-events."
  (declare (ignorable receiver event)) ;(declare (ignore receiver event)) ; 28JUL2023
  NIL)

(defmethod handle-key-event ((receiver canvas) (event T))
  "For a canvas, the event is handed off to its display."
  (let ((handled? NIL))
    (loop for vp-v in (display-of receiver)
            do (setf handled?
                     (or handled? (handle-key-event (cdr vp-v) event))))
    handled?)
  )

(defmethod handle-key-event ((receiver list) (event t)) ;(defmethod handle-key-event ((receivers list) (event T)) ; 28JUL2023
  "Recursively calls handle-key-event on the elements of the list giving each ~
   the opportunity to handle the event."
  (let ((handled? NIL))
    (when receiver ;(when receivers ; 28JUL2023
      (loop for r in receiver ;(loop for r in receivers ; 28JUL2023
            do (setf handled?
                     (or handled? (handle-key-event r event))))
      )
    handled?))
