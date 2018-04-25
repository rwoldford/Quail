;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               layer.lisp
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
;;;     C.B. Hurley 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(layer-view layer-subview layer-selected-view
           make-layer-view
           layer-link-view compute-default-clip-region
           compute-ignore-x compute-ignore-y
            add-line  add-lines add-simple-lines add-simple-smooth
            add-smooth add-fitted-line 
           add-point-cloud add-function)))

(defgeneric layer-link-view (view on-view
                           &key parent    &allow-other-keys)
  (:documentation 
   "Places  layer on view and links "))


(defgeneric layer-subview (parent view on
                               &key &allow-other-keys)
  (:documentation 
   "Places  view on top of  view on .~
    Adds view as subview of parent"))


(defgeneric layer-view (view layer 
                           &key viewport    &allow-other-keys)
  (:documentation 
   "Places  layer on view in viewport "))

(defgeneric make-layer-view (view layer &rest args)
  (:documentation "Make a view pf type layer to be layered on view"))


(defgeneric layer-selected-view (view  
                           &key viewport    &allow-other-keys)
  (:documentation 
   "Places the  selected view on view in viewport "))

(defgeneric compute-default-clip-region (view on-view)
  (:documentation "Computes the clip region used~
                   when on-view is layered on top of view"))

(defgeneric compute-ignore-x (view)
  (:documentation "Computes whether the x-bounds are to be ~
                  left unchanged when view is layered"))

(defgeneric compute-ignore-y (view)
  (:documentation "Computes whether the y-bounds are to be ~
                  left unchanged when view is layered"))

 
(defgeneric add-line (view &key   &allow-other-keys )
  (:documentation "Makes line and layers on a subview")) 
 
(defgeneric add-function (view  &key  &allow-other-keys)
  (:documentation "Makes function-view and layers on a subview"))

(defgeneric add-point-cloud (view
                        &key  &allow-other-keys)
  (:documentation "Makes point-cloud and layers on a subview"))


(defgeneric add-fitted-line (view &key &allow-other-keys )
  (:documentation "Makes fitted line and layers on a subview"))


(defgeneric add-lines (view   &key &allow-other-keys)
  (:documentation "Makes lines view and layers on a subview"))

(defgeneric add-simple-lines (view &key  &allow-other-keys)
  (:documentation "Makes simple-lines view and layers on a subview"))

(defgeneric add-smooth (view &key  &allow-other-keys)
  (:documentation "Makes smoothed-lines view and layers on a subview"))

(defgeneric add-simple-smooth (view &key  &allow-other-keys)
  (:documentation "Makes smoothed-simple-lines view and layers on a subview"))


;;----------------------------------------------------------------------

(defmethod layer-view ((self view ) 
                       (layer view) &rest arg &key viewport   )
  (loop for vp in (if viewport
                    (list viewport)
                    (viewports-of self))
        for parent = (find-parent self :viewport vp )
        do    
        (if (and parent (legal-subview-p parent layer))
          (apply #'layer-subview parent layer  self arg)
          (apply #'layer-link-view  layer  self :parent vp arg)
          )
        (when (member self *selected-views*)
          (remove-view self :viewport vp)))
  
  (when (member self *selected-views*)
    (deselect-view self))
  layer)


(defmethod layer-view ((self view) (layer symbol) &rest args &key viewport )
  (let ((new (apply #'make-layer-view self layer args)))
   (apply #'layer-view self new :viewport viewport args)))


(defmethod make-layer-view ((self view) (layer symbol) &rest args)
  (apply #'view :type layer
         :draw? nil args))

(defmethod layer-subview ((self view) (view view ) 
                          (on view )  
                          &rest arg  )
  
  (let ((locn (select-sub-view-locn self on)))
    (if (null locn)
      (quail-error "~S is not a subview of ~S" on self))
    (when (legal-subview-p self view)
      (place-subview self view (make-region locn))
      (apply #'layer-link-view  view  on :parent self arg)
      )
    view))

(defmethod layer-subview ((self view) (layer symbol) (on view )  
                          &rest args )
  (let ((new (apply #'make-layer-view on layer args)))
   (apply #'layer-subview self new on args)
   new))

(defmethod layer-subview :around ((self view) (view view) 
                          (on view )  
                          &rest arg  )
  (declare (ignore arg))
  (call-next-method)
  (when (member on *selected-views*)
          (deselect-view on)
          (remove-subview self on )))

(defmethod compute-default-clip-region ((view view) (on-view view))
  (bounding-region-of on-view))





(defmethod compute-ignore-x ((view view) )
  (not (linkable-bounds-x-p view)))

        

(defmethod compute-ignore-y ((view view) )
  (not (linkable-bounds-y-p view)))

(defmethod layer-link-view ((view linkable-bounds-mixin ) 
                             (on-view linkable-bounds-mixin )
                       &key  parent (delta-region nil) (draw? t)
                                 (clip-region :default) 
                                 (ignore-x? :default)
                                 (ignore-y? :default))
  "Places  view on top of  view on .~
   By default the coordinate system of added view ~
    is the same as that of on-view .~
   If delta-region is non-nil it specifies the shift  ~
   from the view coords to those of added view.~
   If delta-region is t it is computed using the current ~
   bounding regions. All views are clipped to clip-region, ~
    which should be in the coords of  view on.~
   If clip-region is :max the bounding regions of views ~
   with related coordinate systems are maximized .~
   If clip-region is :default, a reasonable region ~
   is computed from on-view and view. ~ Typically, it ~
    will be set to the bounding region of the on view.~
   However if view is the only one of view and on-view ~
   which is a d-view, then its bounding region ~
   is used as the d-view."

  
  (if (eq clip-region :default)
    (setq clip-region 
          (compute-default-clip-region view on-view)))
  
  (if (eq ignore-x? :default)
    (setq ignore-x? (compute-ignore-x view)))

  (if (eq ignore-y? :default)
    (setq ignore-y? (compute-ignore-y view)))
  
  (let* (
         (br-v (bounding-region-of view))
         (br-ov (bounding-region-of on-view))
         (draw-new-only? 
          (or (eq clip-region (bounding-region-of on-view))
              (and (eq clip-region :max) (eq t delta-region)) )))
   
   
    (change-bounding-region view  
                              br-ov
                              :pretty? nil :ignore-x? ignore-x? 
                              :ignore-y? ignore-y? :draw? nil)
    (unless ignore-x?
      (link-view-bounds
       (union (link-bounds-x-of on-view) 
              (link-bounds-x-of view)) :x))

    (unless ignore-y?
      (link-view-bounds
       (union (link-bounds-y-of on-view) 
              (link-bounds-y-of view)) :y))
   
    
    (if draw-new-only?
      (if (typep parent 'view)
        (remap-sub-to-viewports parent view :erase? nil :draw? draw?)
        (add-view-to-window (window-of parent) view :draw? draw?
                            :viewport (copy-region parent)))

      (let ()
        (setq clip-region (if (region-p clip-region) 
                            clip-region     
                            (maximize-regions br-ov 
                                              br-v)))
        (if (typep parent 'view)
        (remap-sub-to-viewports parent view :erase? nil :draw? nil)
        (add-view-to-window (window-of parent) view :draw? nil
                            :viewport (copy-region parent)))
        (change-bounding-region on-view 
                                (or clip-region (bounding-region-of on-view))
                   :draw? draw?
                   :pretty? nil :ignore-x? ignore-x? :ignore-y? ignore-y?)   ))
    ))




(defmethod add-line ((self view) &rest keyword-args )
  (apply #'layer-view self 'line keyword-args))

(defmethod add-line-segment ((self view) &rest keyword-args )
  (apply #'layer-view self 'line-segment keyword-args))
         

(defmethod add-function ((self view) &rest keyword-args )
  (apply #'layer-view self 'function-view keyword-args))


(defmethod add-fitted-line ((self view) &rest keyword-args )
    (apply #'layer-view self 'fitted-line keyword-args))
        

(defmethod add-lines ((self view) &rest keyword-args )
  (apply #'layer-view self 'lines keyword-args))

(defmethod add-simple-lines ((self view) &rest keyword-args)
  (apply #'layer-view self 'simple-lines keyword-args))

(defmethod add-smooth ((self view) &rest keyword-args)
  (apply #'layer-view self 'smooth keyword-args))

(defmethod add-simple-smooth ((self view) &rest keyword-args)
  (apply #'layer-view self 'simple-smooth keyword-args))

(defmethod add-point-cloud ((self view) &rest keyword-args) 
  (apply #'layer-view self '2d-point-cloud keyword-args))
