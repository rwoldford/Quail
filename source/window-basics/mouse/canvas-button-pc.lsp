;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               canvas-button-pc.lisp
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

#| ;; defined on host-window-pc
;; which loads earlier
(defclass host-pane (cg::bitmap-pane) NIL)

(defmethod cg::default-pane-class ((window host-window)) 'host-pane)
|#

;;; Methods on mouse-left-down & co go here = see Wayne's email.
(defmethod cg::mouse-left-down ((pane host-pane) (buttons T) (cursor-position T))
  (mouse-left-button-event-fn (cg::owner pane) (make-position (h-draw:point-x cursor-position)
                                                              (host-to-canvas-y (cg::owner pane) (h-draw:point-y cursor-position))))
         )

(defmethod cg::mouse-right-down ((pane host-window) (buttons T) (cursor-position T))
  (mouse-right-button-event-fn pane (make-position (h-draw:point-x cursor-position)
                                                   (host-to-canvas-y pane (h-draw:point-y cursor-position)))))

(defmethod cg::mouse-middle-down ((pane host-window) (buttons T) (cursor-position T))
  (mouse-middle-button-event-fn pane (make-position (h-draw:point-x cursor-position)
                                                  (host-to-canvas-y pane (h-draw:point-y cursor-position)))))


#|  02Dec 2014
(defmethod cg::event ((pane host-pane) event (button-state t) data )
  (call-next-method)
  (let ((canvas (cg::parent pane)))
         (cond
                  ;; ((eql event cg::null-event) NIL)
                   ((cg::mouse-event-p event)
                    (mouse-button-event-fn canvas (mouse-position canvas)))
                   (T (call-next-method))
              )
         )
  )
|#

;;; 04Dec 2014
(defmethod cg::mouse-left-down ((pane host-pane) (buttons T) (cursor-position T))
  (let* ((canvas (cg::parent pane))
         (mouse-position (make-position 
                          (h-draw:point-x cursor-position)
                          (host-to-canvas-y canvas (h-draw:point-y cursor-position))
                          ))
         )
    (mouse-left-button-event-fn canvas mouse-position)
    )
  )


(defmethod cg::mouse-right-down ((pane host-pane) (buttons T) (cursor-position T))
  (let* ((canvas (cg::parent pane))
         (mouse-position (make-position 
                          (h-draw:point-x cursor-position)
                          (host-to-canvas-y canvas (h-draw:point-y cursor-position))
                          ))
         )
    (mouse-right-button-event-fn canvas mouse-position)
    )
  )


(defmethod cg::mouse-middle-down ((pane host-pane) (buttons T) (cursor-position T))
  (let* ((canvas (cg::parent pane))
         (mouse-position (make-position 
                          (h-draw:point-x cursor-position)
                          (host-to-canvas-y canvas (h-draw:point-y cursor-position))
                          ))
         )
    (mouse-middle-button-event-fn canvas mouse-position)
    )
  )

