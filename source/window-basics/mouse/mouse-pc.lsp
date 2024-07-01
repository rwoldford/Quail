;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               mouse-pc.lisp
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
;;;     R.W. Oldford 1989-1996
;;;     J.O. Pedersen 1988-89
;;;    G.W. Bennett 1996
;;;     
;;;==========================================================================
;;;    - created to provide
;;;      uniform three-button mouse interaction
;;;      left   
;;;      middle
;;;      right
;;;      Each key can also be modified by either the shift-key or the
;;;      control-key.
;;;  NOTES:
;;;  For the purposes of PCs with only L and R buttons (eg laptops)
;;;  meta-L or meta-R have been defined to function as a middle
;;;  button. meta-  corresponds to alt-  on PCs.
;;;      
;;;===========================================================================
(in-package :wb)
;;;;;;;;;;;;;;;;;;;;;;
;;;  Redefining shadowed symbols  SEE DEFPACKAGE
;;;(shadow '(mouse-down-p shift-key-p control-key-p))

(defun mouse-down-p ()
  (cg::process-pending-events-if-event-handler)
  (let ((result)) ; 27JUL2023
  (setf result
     (or (cg::key-is-down-p cg::vk-lbutton)
          (cg::key-is-down-p cg::vk-mbutton)
         (cg::key-is-down-p cg::vk-rbutton)))
  result)
  ) ;27JUL2023

(defun shift-key-p ()
     "Tests whether a shift key is being held down."
            (or (cg::key-is-down-p cg::vk-shift)
                (cg::key-is-down-p cg::vk-left-shift)
                (cg::key-is-down-p cg::vk-right-shift)
                ))

  
(defun control-key-p ()
  "Tests whether a control key is being held down."
  (let ((result)) ;27JUL2023
  (setf result
    (or (cg::key-is-down-p cg::vk-control)
        (cg::key-is-down-p cg::vk-left-control)
        (cg::key-is-down-p cg::vk-right-control)
        ))
  ;(when result (cg.gtk::update-key-state cg::vk-right-control :up) ;; 23JUL2023 package cg.gtk nolonger exists
  ;  (cg.gtk::update-key-state cg::vk-left-control :up)) ;; DITTO
  result) ; 27JUL2023
  )
  

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(mouse-down-p shift-key-p control-key-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(mouse-state mouse-position mouse-x mouse-y
          screen-mouse-position screen-mouse-x screen-mouse-y)))

#|
(defun mouse-state ()
     (cg::process-pending-events)
     (let ((state (cg::mouse-button-state)))
         (cond
                   ((cg::button-match state
                     (or cg::middle-mouse-button
                          ;;(and cg::left-mouse-button cg::right-mouse-button)
                          (and cg::meta-key (or cg::left-mouse-button
                                                              cg::right-mouse-button))))
                    :middle)
                   ((cg::button-match state cg::left-mouse-button) :left)
                   ((cg::button-match state cg::right-mouse-button) :right)
          ( T :none))))
|#


(defun mouse-state ()
  ;(format t "Calling mouse-state~%")
  (let ((result)) ; 27JUL2023
  (setf result
  (cond
   ((cg::key-is-down-p cg::vk-lbutton) :left)
   ((cg::key-is-down-p cg::vk-mbutton) :middle)
   ((cg::key-is-down-p cg::vk-rbutton) :right)
   ( T :none)))
  (cg::process-pending-events)
  result)
  ) ; 27JUL2023

(defun mouse-position (canvas)
     (let ((position (cg::cursor-position canvas)))
         ;; Now convert to window-basics position
         (make-position (h-draw:point-x position)
          (host-to-canvas-y canvas (h-draw:point-y position)))))

(defun mouse-x (canvas)
     (h-draw:point-x (cg::cursor-position canvas)))

(defun mouse-y (canvas) 
     (host-to-canvas-y  canvas
      (h-draw:point-y (cg::cursor-position canvas))))

;;;========================================================================================
;;; mouse position in screen coordinates, cbh
;;;========================================================================================
(defun screen-mouse-position ()
  "Returns position of mouse in screen coords."
  (cg::with-device-context (hdc (cg::screen cg::*system*))
                           (let* ((scrn (cg::screen cg::*system*)) ; 27JUL2023
                             (position (cg::cursor-position scrn)))
                           (make-position (h-draw::point-x position)
                                          (- (screen-height) (h-draw::point-y position))))))

        
(defun screen-mouse-x ()
     (position-x (screen-mouse-position)))
(defun screen-mouse-y ()
     (position-y (screen-mouse-position)))