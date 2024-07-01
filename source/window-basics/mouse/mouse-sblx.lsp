;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               mouse-sblx.lisp
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
;;;(shadow '(mouse-down-p shift-key-p control-key-p)) ;now in window-basics-package.lsp 01SEP2021

(defun mouse-down-p ()
  "Determines whether the mouse pointer in down"
  (if (>  (pointer-button-state (port-pointer (find-port))) 0)
    T
    NIL))


(defun shift-key-p ()
     "Tests whether a shift key is being held down."
     (if (eql (port-modifier-state (find-port)) +shift-key+)
      T nil))

  
(defun control-key-p ()
  "Tests whether a control key is being held down."
  (if (eql (port-modifier-state (find-port)) +control-key+)
    T nil))



(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(mouse-down-p shift-key-p control-key-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(mouse-state mouse-position mouse-x mouse-y
          screen-mouse-position screen-mouse-x screen-mouse-y)))


(defun mouse-state ()
  (let ((result (pointer-button-state (port-pointer (find-port)))))
    (cond ((eql result +pointer-left-button+)
      (setf result :left))
    ((eql result +pointer-middle-button+)
      (setf result :middle))
    ((eql result +pointer-right-button+)
      (setf result :right))
    (t (setf result :none)))
  result
  ))

;;; Old form .. in which position would NOT be one!
#|
(defun mouse-position (canvas)
  (let ((mp (get-frame-pane canvas 'host-pane)))
     (let ((position (stream-cursor-position mp)))
         ;; Now convert to window-basics position
         (make-position (h-draw:point-x position)
          (host-to-canvas-y canvas (h-draw:point-y position))))))
|#
;;; New version
 (defun mouse-position (canvas)
      (let* ((mp (get-frame-pane canvas 'host-pane))
       (coord-list (multiple-value-list (stream-cursor-position mp))))
  (h-draw:make-point (first coord-list) (host-to-canvas-y canvas (second coord-list)))))          

(defun mouse-x (canvas)
     (h-draw:point-x (mouse-position canvas)))

(defun mouse-y (canvas)
      (h-draw:point-y (mouse-position canvas)))

;;;========================================================================================
;;; mouse position in screen coordinates, cbh
;;;========================================================================================
(defun screen-mouse-position ()
     "Returns position of mouse in screen coords."
     (let ((pointer-pos
     (multiple-value-list (pointer-position (port-pointer (find-port))))))
     (make-position (first pointer-pos) (second pointer-pos))
    ))
    
        
(defun screen-mouse-x ()
     (position-x (screen-mouse-position)))
(defun screen-mouse-y ()
     (position-y (screen-mouse-position)))
