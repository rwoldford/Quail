;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               font-mixin-pc.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     R.W. Oldford 1992
;;;     G.W. Bennett 1996
;;;----------------------------------------------------------------------------------

(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(font-mixin canvas-font))) ;15MAR2021

(defclass font-mixin ()
  ((canvas-font :initarg :font :reader canvas-font
         :documentation "The window-basics font used for drawing characters."))
  (:documentation
   "A mixin class that allows a window-basics font to be stored."))

#| not needed because fonts are not associated with canvases
(defmethod (setf canvas-font) (new-font (self font-mixin))
  ;(cg::set-font self (canvas-font-to-host-font new-font)) 18oct05
  (setf (cg::font self) (canvas-font-to-host-font new-font)) ;18oct05
  (setf (slot-value self 'canvas-font) new-font)
  new-font)
|#
(defmethod (setf canvas-font) (new-font (self font-mixin))
  (let ((mp (clim:get-frame-pane self 'host-pane)))
  (setf (clim:medium-text-style mp)
    (canvas-font-to-host-font new-font))
   (setq new-font (host-font-to-canvas-font (clim:medium-text-style mp)))
  (setf (slot-value self 'canvas-font)  new-font)
  new-font))


;;; I don't yet know what has to happen here  16MAR2021
;(defmethod (setf canvas-font) (new-font (self scrolling-window))
;  (ccl::set-view-font (the-scroller self)
;                      (canvas-font-to-host-font new-font))
;  (call-next-method))

#|
(defmethod initialize-instance :after ((self font-mixin)
                                       &rest initargs
                                       &key font)
   (declare (ignore initargs))
   (if (canvas-font-p self)
      (setf (canvas-font self) font)
      (setf (canvas-font self)
            (host-font-to-canvas-font (cg::font self)))))
|#
