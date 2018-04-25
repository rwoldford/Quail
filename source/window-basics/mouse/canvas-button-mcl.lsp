;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               canvas-button-mcl.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '()))


;;;===================================================================
;;;
;;; Now make button events function uniformly
;;;
;;;===================================================================


(defmethod ccl:view-click-event-handler ((canvas canvas-button) click-pos)
  ;; Call the button event handler.  Note that click-pos
  ;; is translated into a window-basics position.
  (declare (ignore click-pos))
  (mouse-button-event-fn canvas (mouse-position canvas))
  (call-next-method))

(defmethod ccl:window-mouse-up-event-handler ((canvas canvas-button))
  (mouse-button-event-fn canvas (mouse-position canvas))
  (call-next-method))

