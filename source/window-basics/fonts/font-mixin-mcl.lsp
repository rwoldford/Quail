;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               font-mixin-mcl.lisp
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
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(font-mixin canvas-font)))

(defclass font-mixin ()
  ((canvas-font :initarg :font :reader canvas-font
         :documentation "The window-basics font used for drawing characters."))
  (:documentation
   "A mixin class that allows a window-basics font to be stored."))


#|
(defmethod (setf canvas-font) (new-font (self font-mixin))
  (ccl::set-view-font self (canvas-font-to-host-font new-font))
    
  (setf (slot-value self 'canvas-font) new-font)
 new-font)
|#

;; changed by cbh
(defmethod (setf canvas-font) (new-font (self font-mixin))
  (ccl::set-view-font self (canvas-font-to-host-font new-font))
   (setq new-font (host-font-to-canvas-font (ccl::view-font self)))
  (setf (slot-value self 'canvas-font) new-font)
  new-font)
(defmethod (setf canvas-font) (new-font (self scrolling-window))
  (ccl::set-view-font (the-scroller self)
                      (canvas-font-to-host-font new-font))
  (call-next-method))

(defmethod initialize-instance :after ((self font-mixin)
                                       &rest initargs
                                       &key font)
  (declare (ignore initargs))
  (if (canvas-font-p font)
    (setf (canvas-font self) font)
    (setf (canvas-font self)
          (host-font-to-canvas-font (ccl::view-font self)))
    ))
