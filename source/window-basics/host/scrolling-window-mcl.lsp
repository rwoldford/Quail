;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               scrolling-windows-mcl.lisp
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
;;;     R.W. Oldford 1989-1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;  A new class of windows which contain scroll-bars and a scrollable
;;;  area.
;;;
;;; ----------------------------------------------------------------------------------
;;;
;;;  Adapted from the scrolling-windows.lisp 1989 examples file distributed by apple
;;;
;;;(require :scrollers)     ;; got it in wb-system-mcl.lisp
;;;
;;;  Default changed to t for track-thumb-p
;;;  my-scroller changed to the-scroller
;;;  set-view-size changed to redisplay the entire window               ... rwo 92

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '()))

(defclass scrolling-window (window)
  ((the-scroller :accessor the-scroller)))

(defmethod initialize-instance ((self scrolling-window) &key
                                (scroller-class 'ccl::scroller)
                                (scroll-bar-class 'scroll-bar-dialog-item)
                                h-scroll-class v-scroll-class
                                (track-thumb-p t)
                                field-size)
  (call-next-method)
  (setf (the-scroller self)
        (make-instance
         scroller-class
         :view-container self
         :view-size (subtract-points (view-size self) #@(15 15))
         :view-position #@(0 0)
         :draw-scroller-outline nil
         :scroll-bar-class scroll-bar-class
         :h-scroll-class h-scroll-class
         :v-scroll-class v-scroll-class
         :track-thumb-p track-thumb-p
         :field-size field-size)))


(defmethod set-view-size ((self scrolling-window) h &optional v)
  (declare (ignore h v))
  (without-interrupts
   (call-next-method)
   (let* ((new-size (subtract-points (view-size self) #@(15 15))))    
       (set-view-size (the-scroller self) new-size))
   (redisplay self)
   ))

(defmethod window-zoom-event-handler ((self scrolling-window) message)
  (declare (ignore message))
  (without-interrupts
   (call-next-method)
   (let* ((new-size (subtract-points (view-size self) #@(15 15))))
       (set-view-size (the-scroller self) new-size))
   ))

#|

;;(require 'quickdraw)

(defclass scrolling-window-scroller (scroller) ())

(defmethod view-draw-contents ((self scrolling-window-scroller))
  (call-next-method)
  (h-draw::paint-oval self 75 75 200 200)
  (h-draw::frame-rect self 20 20 100 100)
  (h-draw::erase-oval self 50 50 135 135))

(setq foo (make-instance 'scrolling-window
                         :scroller-class 'scrolling-window-scroller
                         :window-type :document-with-zoom
                         :track-thumb-p t))

|#
