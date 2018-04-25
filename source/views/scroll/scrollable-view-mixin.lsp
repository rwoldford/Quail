;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               scrollable-view-mixin.lisp
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
;;;     C.B. Hurley 1992 George Washington University
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(scrollable-view-mixin max-view-start view-start
          check-active-sub-viewports
          scroll-view move-view-start)))


(defclass scrollable-view-mixin ()
  ())


 
(defgeneric max-view-start (scrollable-view-mixin &key viewport axis)
  (:documentation 
   "Returns the maximum view start position in viewport along axis."))


(defgeneric view-start (scrollable-view-mixin &key viewport axis)
  (:documentation 
   "Returns the current view start position in viewport along axis."))


(defgeneric scroll-view (scrollable-view-mixin  positions &key axis)
  (:documentation "Scrolls the view  position  along axis."))

(defgeneric move-view-start (scrollable-view-mixin  &key x y  )
  (:documentation "Moves the view start to x and y."))
  


  


(defmethod compute-sub-viewports ((self scrollable-view-mixin)
                                  &optional viewport start)
  (declare (ignore viewport start))
   (quail-error "Please supply a method"))


(defmethod compute-sub-viewports :after ((self scrollable-view-mixin)
                                  &optional viewport start)
  (declare (ignore  start))
  (check-active-sub-viewports self :viewport viewport))



(defmethod reshape-sub-viewports ((self scrollable-view-mixin) viewport  
                                  &key new-location transform )
  (declare (ignore new-location transform))
  (map-subviews-to-viewport self viewport))



(defmethod check-active-sub-viewports ((self scrollable-view-mixin) &key viewport)
  
  (loop for vp in (if viewport (list viewport) (viewports-of self))  do
        (loop for s in (subviews-of self)  
              for vp-sub = (select-viewport s vp) do
              (if (contains-p vp vp-sub) (activate-viewport vp-sub)
                  (deactivate-viewport vp-sub)))))



(defmethod move-view-start ((self scrollable-view-mixin) &key x y )
  (erase-view self )
  (compute-sub-viewports self nil (list x y))
  (draw-view self :erase? nil))




(defmethod scroll-view ((self scrollable-view-mixin)  positions &key axis)
  
  (let ((inc (if (minusp positions) -1 1 )))
    ;;(erase-view self )
    (loop for vp in (viewports-of self)
          for start = (view-start self :viewport vp :axis axis)
          for new-start = (min (max 0 (+ start positions)) 
                               (max-view-start self :viewport vp
                                                  :axis axis))
          do
          (loop repeat (abs (- new-start start) ) do
                (erase-view self :viewport vp)
                (compute-sub-viewports self vp
                                       (ecase axis
                                         (:x (list (incf start inc ) nil)) 
                                         (:y (list nil (incf start inc )) )))
                (draw-view self  :erase? nil :viewport vp)
                ))))
  
