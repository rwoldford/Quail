;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               rotating-plot.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export '( rotating-plot rotate-controls)))



(defclass rotating-plot (3d-plot ) 
  ((position-keys :allocation :class :initform 
                  '(:panel-size :left-margin-size :right-margin-size
                    :bottom-margin-size :top-margin-size 
                    :title-height :title-width) )
   (controls :initform nil :accessor controls-of :initarg :controls))
  (:default-initargs :panel-size 0.1 :default-interior 'rotating-cloud))





;;;----------------------------------------------------------------------------------




(defmethod construct-sub-views ((self rotating-plot) &rest keyword-pairs)
  (apply #'construct-interior-view self keyword-pairs)
    (setf (controls-of self)
        (rotate-controls :target (interior-view-of self)))
  )
           
           
           


(defmethod init-position-subviews ((self rotating-plot)
                                   &key panel-size)
  
  (let ((space-left (make-region (subview-position-region self))))
    (place-subview self (controls-of self)
                   (sub-region space-left :w
                               :width (* panel-size (width-of space-left))
                               :remains space-left))
    (place-subview self (interior-view-of self) space-left)))




;;;----------------------------------------------------------------------------------
(defun rotate-controls ( &rest args &key (x-text "X") target (y-text "Y") (z-text "Z") (draw? nil)
                              &allow-other-keys)
  ;(declare (ignore args))
  (let ((r
         (make-instance 'rotate-controls :x-text x-text 
                        :y-text y-text 
                        :z-text z-text 
                        :target target)))
    (if draw? (draw-view r))
    r))

(defclass rotate-controls (compound-view orientation-mixin)
  ((x-button :initform nil :accessor x-button-of )
   (y-button :initform nil :accessor y-button-of )
   (z-button :initform nil :accessor z-button-of )
   (speed-control :initform nil :accessor speed-control-of )
   )
  (:default-initargs  :orientation :vertical))
  


(defmethod construct-sub-views ((self rotate-controls) &rest keyword-pairs
                                &key target x-text y-text z-text orientation  )
  (declare (ignore keyword-pairs))
  (setf (x-button-of self)
        (control-button :text x-text
                        :left-fn #'move-points
                        :target (list target :direction :x)))
  (setf (y-button-of self)
        (control-button :text y-text
                        :left-fn #'move-points
                        :target (list target :direction :y)))
  (setf (z-button-of self)
        (control-button :text z-text
                        :left-fn #'move-points
                        :target (list target :direction :z)))
  (let* ((sc (needle-slider :min (- (/ pi 10)) :max (/ pi 10) :orientation orientation 
                            :level (if target (increment-of target) 0)
                            :left-fn #'set-increment 
                            :needle-size 2
                            ))
         (sc-plot 
          (plot :title "" :draw? NIL
                :right-view-size 0.5
                :interior-view sc
                :right-view `(:type axis :orientation :vertical :color nil
                                    :tic-list ((,(- (/ pi 10)) "Back") (0 "Stop") (,(/ pi 10) "Ahead"))
                                    )
                :left-label "Speed"
                )))
    
    (setf (target-of sc) `(,target (slider-level-of ,sc)))
    (setf (speed-control-of self) sc-plot);;sc)
    )
  )
  

 

(defmethod init-position-subviews ((self rotate-controls)
                                   &key )
  
  
  (let* ((region (subview-position-region self))
        (g 0.05)  
        (l  (+ g (left-of region)))
        (b (+ g (bottom-of region)))
        w h)

    (if (eql (orientation-of self) :horizontal)
      (progn
        (setq w (/  (- (width-of region) (* 5 g)) 5) 
              h (- (height-of region) (* 2 g)))
        (loop for button in 
              (list (x-button-of self) (y-button-of self) (z-button-of self))
              do (place-subview self button
                       (make-region l (+ l w) b (+ b h)))
              (incf l (+ w g)))
        
        (place-subview self (speed-control-of self)
                       (make-region l (+ l (* 2 g)) b (+ b h))))

      (progn
        (setq h (/ (- (height-of region) (* 5 g)) 5) w (- (width-of region) (* 2 g)))
        (loop for button in 
              (list (z-button-of self) (y-button-of self) (x-button-of self))
              do (place-subview self button 
                                (make-region l (+ l w) b (+ b h)))
              (incf b (+ h g)))

        (place-subview self (speed-control-of self)
                       (make-region l (+ l w) b (+ b (* 2 h)) ))
        ))))


