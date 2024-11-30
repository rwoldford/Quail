;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               canvas-button-sblx.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1991
;;;     J.O. Pedersen 1988-89
;;;     G.W. Bennett 1996
;;;      
;;;===========================================================================
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '()))
;;;===================================================================
;;; Now make button events function uniformly
;;;===================================================================

(defmethod handle-event ((pane application-pane) (p-event pointer-button-press-event)) ;host-pane) (p-event pointer-button-press-event))
  (declare (ignore p-event)) ;; seems like we don't need it here?
  (let ((canvas (pane-frame pane)))
     (mouse-button-event-fn canvas (mouse-position canvas))
     )
  (call-next-method) ;; probably need this still?
  )