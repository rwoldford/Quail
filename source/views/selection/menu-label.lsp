;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               menu-label.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(  menu-label marked-menu-label select-menu-label deselect-menu-label)))

(in-package :views)

(defclass menu-label(bg-color-mixin  label)
  ((style-keys :initform '(:font :color :bg-color :highlight-color) :allocation :class)
   )
  (:default-initargs :bg-color wb:*white-color*
    :color wb:*black-color* 
    :highlight-color *default-highlight-color*)
  ) 
   


  
(defclass marked-menu-label( menu-label)
  ((mark 
    :initarg :mark
    :accessor mark-of 
    :initform ""
    :documentation "marks the label")))

(defgeneric select-menu-label (menu-label &key &allow-other-keys))
(defgeneric deselect-menu-label (menu-label))

(defmethod select-menu-label ((self menu-label) &key)
  
  (draw-view self :color (draw-style self :highlight-color) :erase? t))

(defmethod deselect-menu-label ((self menu-label) )
  
  (draw-view self  :erase? t))



(defmethod get-text :around ((self marked-menu-label))
  (if (mark-of self)
    (format nil "~A ~A" (mark-of self) (call-next-method))
    (call-next-method)))

(defmethod default-mark ((self marked-menu-label))
  #\dot)


(defmethod select-menu-label ((self marked-menu-label) 
                              &key (mark (default-mark self))) 
  (unless (equal mark (mark-of self))
    (erase-view self)
    (setf (mark-of self) mark)
    (draw-view self :color (draw-style self :highlight-color)))
  )

(defmethod deselect-menu-label ((self marked-menu-label) )
  (erase-view self)
  (setf (mark-of self) "")
  (draw-view self)
  )

(defmethod set-label-mark ((self marked-menu-label) 
                           &key (mark (default-mark self)) (draw? t))
  (if draw?  (erase-view self))
  (setf (mark-of self) mark)
  (if draw?  (draw-view self)))
