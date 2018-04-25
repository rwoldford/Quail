;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               close-button.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1995 Maynooth
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(  fixed-size-rectangle close-button)))

(in-package :views)

(defclass close-button(fixed-size-rectangle control-button)
  ()
  (:default-initargs :text "Close"
    :font wb:*normal-graphics-font*
    :bg-color wb:*white-color*
    :color wb:*black-color*
    :width 60 :height 20
   ))




(defmethod left-button-fn  ((self close-button) 
                            &key viewport ) 
  (control-start self)
  (wb::close-canvas (window-of viewport ))
  
  )




(defmethod reshape-viewport :after ((self fixed-size-rectangle) viewport &rest ignore)

  (declare (ignore ignore viewport))
  (fix-viewports self))

(defmethod add-viewport :after ((self fixed-size-rectangle) viewport pvp &key)
  (declare (ignore pvp))
  (fix-viewports self :viewport viewport))


(defmethod fix-viewports ((self fixed-size-rectangle) &key viewport ) 
  (let ((w (rectangle-width-of self))
        (h (rectangle-height-of self)))
    (loop for vp in (if viewport 
                      (list viewport) (viewports-of self) )
          do (set-viewport-width-height vp w h))))
