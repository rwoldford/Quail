;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               prompt-widget.lsp
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
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------



(in-package :vw)


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(color-chip logical-widget logical-value-of check-box ballot-box radio-button )))

(defclass color-chip (simple-view)
  ((style-keys :initform '(:color) :allocation :class)
   )
  (:default-initargs :color wb:*black-colour* :left-fn nil )
  (:documentation "A simple view that produces a rectangular color ~
                   chip the same size as its viewport."))

(defmethod draw-view ((self color-chip) &key viewport)
  (with-exposed-viewports self viewport vp
    (let ((bw (window-of vp))
          (color (draw-style self :color)))
      (multiple-value-bind (left right bottom top) 
                           (bounds-of vp)
        (wb::canvas-draw-filled-rectangle bw left right bottom top :color color)
        ))))





(defmethod get-menu-items ((self color-chip) 
                           (slot-name (eql 'left-menu)))
  (color-menu-items))

(defclass logical-widget (simple-view )
  ((style-keys :initform '(:color) :allocation :class))
  (:default-initargs :viewed-object  NIL)
  (:documentation "A simple view that records and displays a logical value."))


(defgeneric logical-value-of (logical-widget)
  (:documentation "Returns the logical value of the widget."))

(defmethod logical-value-of ((thing logical-widget))
  (viewed-object-of thing))

(defmethod (setf logical-value-of) (new-value (thing logical-widget))
  (setf (viewed-object-of thing) new-value))

(defmethod draw-view ((self logical-widget) &key viewport)
 (if (viewed-object-of self)
   (with-exposed-viewports self viewport vp
     (let ((bw (window-of vp))
           (color (draw-style self :color)))
       (multiple-value-bind (left right bottom top) 
                            (bounds-of vp)
         (wb::canvas-draw-filled-rectangle bw left right bottom top :color color)
         )))
   (with-exposed-viewports self viewport vp
     (let ((bw (window-of vp))
           (color (draw-style self :color)))
       (multiple-value-bind (left right bottom top) 
                            (bounds-of vp)
         (wb::canvas-draw-rectangle bw left right bottom top :color color :width 2)
         ))))
 )


;;; Change button behaviours.

(defmethod default-left-fn ((self logical-widget) 
                           &key viewport ) 
  (setf (viewed-object-of self) (not (viewed-object-of self)))
  (draw-view self :viewport viewport :erase? T))


(defmethod right-button-fn ((self logical-widget) 
                            &key viewport)
  (declare (ignorable self viewport)) ;(declare (ignore self viewport)) ; 29JUL2023
  ())


(defmethod shift-left-button-fn ((self logical-widget) 
                                 &key viewport )
  (declare (ignorable self viewport)) ;(declare (ignore self viewport)) ; 29JUL2023
  ())
 

(defmethod shift-middle-button-fn ((self logical-widget) 
                                   &key viewport)
  (declare (ignorable self viewport)) ;(declare (ignore self viewport)) ; 29JUL2023
  ())
 

(defmethod shift-right-button-fn ((self logical-widget) 
                                   &key viewport)
  (declare (ignorable self viewport)) ;(declare (ignore self viewport)) ; 29JUL2023
  ())

;;; Some special classes.

(defclass check-box (logical-widget)
  ()
  (:documentation "A simple view that records and displays a logical value via a check mark."))


(defmethod draw-view ((self check-box) &key viewport)
  (if (viewed-object-of self)
    (with-exposed-viewports self viewport vp
      (let ((bw (window-of vp))
            (color (draw-style self :color)))
        (multiple-value-bind (left right bottom top) 
                             (bounds-of vp)
            (wb::canvas-draw-rectangle bw  left  right bottom top :color color :width 2)
            (wb::canvas-draw-line bw
                                  (+ left 4) (truncate (+ bottom top) 2)
                                  (+ left (truncate (- right left) 3)) (+ bottom 4) :width 2  :color color)
            (wb::canvas-draw-line  bw
                                   (+ left (truncate (- right left) 3)) (+ bottom 4)
                                   (- right 4) (- top 4) :width 2 :color color)
            )))
    (with-exposed-viewports self viewport vp
      (let ((bw (window-of vp))
            (color (draw-style self :color)))
        (multiple-value-bind (left right bottom top) 
                             (bounds-of vp)
          (wb::canvas-draw-rectangle bw left right bottom top :color color :width 2)
          ))))
  )


(defclass ballot-box (logical-widget)
  ()
  (:documentation "A simple view that records and displays a logical value via an X."))


(defmethod draw-view ((self ballot-box) &key viewport)
  (if (viewed-object-of self)
    (with-exposed-viewports self viewport vp
      (let ((bw (window-of vp))
            (color (draw-style self :color)))
        (multiple-value-bind (left right bottom top) 
                             (bounds-of vp)
            (wb::canvas-draw-rectangle bw  left  right bottom top :color color :width 2)
            (wb::canvas-draw-line bw left top right bottom   :color color :width 2)
            (wb::canvas-draw-line bw left bottom right top  :color color :width 2)
            )))
    (with-exposed-viewports self viewport vp
      (let ((bw (window-of vp))
            (color (draw-style self :color)))
        (multiple-value-bind (left right bottom top) 
                             (bounds-of vp)
          (wb::canvas-draw-rectangle bw left right bottom top :color color :width 2)
          ))))
  )


(defclass radio-button (logical-widget)
  ()
  (:documentation "A simple view that records and displays a logical value via a circle."))


(defmethod draw-view ((self radio-button) &key viewport)
  (with-exposed-viewports self viewport vp
    (let* ((bw (window-of vp))
           (color (draw-style self :color))
           (centre (centre-of vp))
           (x (2d-position-x centre))
           (y (2d-position-y centre))
           (radius (truncate (min (height-of vp) (width-of vp)) 2)))
      
      (wb::canvas-draw-circle  bw x y radius :color color :width 2)
      
      (if (viewed-object-of self)
        (wb::canvas-draw-filled-circle  bw x y (max 2 (truncate (* .4 radius))) :color color)
        ))
    )
  )


#|

(view-layout :subviews (list (make-instance 'ballot-box)  (make-instance 'check-box)
                             (make-instance 'radio-button) (make-instance 'color-chip))
             :positions '((100 140 100 140)
                          (150 190 100 140)
                          (100 140 150 190)
                          (150 190 150 190))
             :bounding-region (make-region 0 300 0 300)
             :draw? T)

|#
