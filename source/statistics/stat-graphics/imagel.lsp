;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               imagel.lsp
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
;;;     N. Wiebe 1999
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(imagel)))
;;;----------------------------------------------------------------------------------

(defmacro with-imagel-bounds
          (imagel-sym vp xmin xmax ymin ymax &body do-forms)
  
  (let ((c (gensym "c"))
        (xc (gensym "xc")) 
        (yc (gensym "yc"))
        (rad1 (gensym "rad1"))
        (rad2 (gensym "rad2"))
        (width (gensym "width"))
        (height (gensym "height")))   
    `(let* ((,width (draw-style ,imagel-sym :width))
            (,height (draw-style ,imagel-sym :height))
            (,c (centre-of ,vp))
            (,xc (2d-position-x ,c))
            (,yc (2d-position-y ,c))
            (,rad1 (truncate ,width 2))
            (,rad2 (truncate ,height 2)))
       (setq ,xmin (- ,xc ,rad1))
       (setq ,xmax (+ ,xmin ,width))
       (setq ,ymin (- ,yc ,rad2))
       (setq ,ymax (+ ,ymin ,height))
       ,@do-forms)))

(defclass imagel (fixed-size-rectangle linkable-mixin simple-view)
  
  ((middle-menu :allocation :class :initform nil)
   (style-keys :initform '(:symbol :width :height :color :highlight-color) 
               :allocation :class)
   (label
    :initarg :label
    :initform #'dataset-name  ;identifier-name 
    :documentation "Function which when applied to subject gives a label, or a  label")
   )
  (:default-initargs :symbol :box :width 4 :height 4 :color *default-point-color* 
    :highlight-color *default-highlight-color* 
    :left-fn (list #'default-left-fn :viewport )))

(defmethod viewed-object-description-string :around ((self imagel) )
  (if (find-parent self)
    (format nil "~A: ~s" (z-variate-of (find-parent self))
            (elt (z-coords-of (find-parent self) 
                              :cases (list (viewed-object-of self))) 0))
    (call-next-method)))



(defmethod default-left-fn ((self imagel) &key viewport &allow-other-keys)
  (identify-view self :viewport viewport)
  )

(defmethod highlight-operation ((self imagel))
  :default
  )

(defmethod highlight-view ((self imagel)
                           &key viewport operation) 
  (setq operation (or operation (highlight-operation self)))
  (let* (l r b tp)
    (with-exposed-viewports self viewport vp
      (with-imagel-bounds self vp l r b tp
        (wb:canvas-highlight-rectangle (window-of vp) l r b tp  
                                       :color (draw-style self :highlight-color)
                                       :operation operation)))))

(defmethod draw-view ((self imagel) &key viewport)  
  (with-exposed-viewports self viewport vp
    (let ((color (draw-style self :color))
          (bw (window-of vp))
          xmin xmax ymin ymax  )
      (with-imagel-bounds self vp xmin xmax ymin ymax
        (wb:canvas-draw-filled-rectangle bw xmin xmax ymin ymax :color color)))))


(defmethod label-of ((self imagel))
  (let ((l (slot-value self 'label)))
    (if (functionp l)
      (funcall l (viewed-object-of self))
      l)))

#|
(defmethod reshape-viewport :after ((self view-with-width-height) viewport &rest ignore)

;; point symbol viewports are not resized when window is reshaped
  (declare (ignore ignore viewport))
  (fix-viewports self))

(defmethod fix-viewports ((self imagel) &key viewport )
  (let* ((width (draw-style self :width))
         (height (draw-style self :height)))
    (loop for vp in (if viewport 
                      (list viewport ) (viewports-of self) )
          do
          (set-viewport-width-height vp (+ 2 width) (+ 2 height))
          )))

(defun imagel-key (sym)
  "Returns the symbol key for sym, if one exists."
  (cond
   ((member sym *point-symbol-types*) sym)
   ((stringp sym)  :text)
   ((wb:bitmap-p sym) :bitmap)
   (t nil)))

(defmethod set-draw-style :after ((self imagel) (style (eql :width)) new &key)
  (declare (ignore new))
  (fix-viewports self))
(defmethod set-draw-style :after ((self imagel) (style (eql :height)) new &key)
  (declare (ignore new))
  (fix-viewports self))

|#

(defmethod clear-view-viewport ((self imagel) 
                                &key viewport)
  
  ;; erases the view in VIEWPORT, if supplied, else erases  in all exposed
  ;; viewports
  (with-exposed-viewports self viewport vp
    (let* ((w (window-of vp))
           rl rb rw rh)
      (when vp
        (setq rl   (+ 1 (left-of vp)))
        (setq rb  (+ 1 (bottom-of vp)))
        (setq rw (- (width-of vp) 2))
        (setq rh (- (height-of vp) 2))
        (wb:canvas-clear  w
                          :canvas-left rl :canvas-bottom rb
                          :width rw :height rh)))))

(defmethod rectangle-width-of ((self imagel))
  (draw-style self :width))
(defmethod rectangle-height-of ((self imagel))
  (draw-style self :height))
(defmethod (setf rectangle-width-of)  (new (self imagel) )
  (set-draw-style self :width new))
(defmethod (setf rectangle-height-of)  (new (self imagel) )
  (set-draw-style self :height new))
(defmethod rectangle-width-increment ((self imagel))
  (if (>= (draw-style self :width) 3) 2 1))
(defmethod rectangle-height-increment ((self imagel))
  (if (>= (draw-style self :height) 3) 2 1))
(defmethod set-view-width ((self imagel) new &rest args)
  (apply #'set-drawing-style self :width new args)
  (setf (slot-value self 'rectangle-width) (draw-style self :width)))
(defmethod set-view-height ((self imagel) new &rest args)
  (apply #'set-drawing-style self :height new args)
  (setf (slot-value self 'rectangle-height) (draw-style self :height)))


(defmethod description-string ((self imagel))
  (let ((name (or (label-of self) (viewed-object-description-string self))))
    (if name
      (format nil "~A viewing ~A" self name)
      (format nil "~A" self))))



(defmethod distance-to-location ((self imagel) viewport location)
  (let ((locn-c (if (region-p location)
                  (centre-of location) location)))
    (distance-from viewport locn-c)))


(defmethod add-viewport ((self imagel) vp pvp
                         &key compute-transform?)
  (declare (ignore compute-transform?))
  (call-next-method self vp pvp :compute-transform? nil))

;;should be in clone.lisp
(defmethod toplevel-clone-list  ((self imagel))
  (append
   (list :label (label-of self) )
   (call-next-method)))

#|
(defmethod set-highlight-range-gray ((self imagel))
  (let* ((value (image-value-of self))
         (lower (wb::prompt-user :prompt-string (format NIL "Enter lower end of range:")
                                 :type 'number :read-type :eval
                                 :initial-string (format NIL "~s" (- value 5))))  
                                                  ;make 5 a constant?
         (upper (wb::prompt-user :prompt-string (format NIL "Enter higher end of range:")
                                 :type 'number :read-type :eval
                                 :initial-string (format NIL "~s" (+ value 5))))
         (parent (find-parent self)))
    (if parent
      (progn (loop for sb in (subviews-of parent)
            for sb-inten = (image-value-of sb) do
            (when (and (<= lower sb-inten) (>= upper sb-inten))
              (select-view sb )))))))


(defmethod set-highlight-same-gray ((self imagel))
  (let ((value (image-value-of self))
        (parent (find-parent self)))
    (if parent
      (progn (loop for sb in (subviews-of parent) do
                   (when (eq value (image-value-of sb))
                     (select-view sb)))
             ))))
|#


(defun map-with-lhs-matrix (value mat)
  ; the lhs matrix
  ; 
  ; | singular lightness value | min lightness         | max lightness           |
  ; | singular hue value       | min hue               | max hue                 |
  ; | saturation value         | min . max image-value | lightness, hue, or both |
  ;
  (let* ((min-value (car (q::eref mat 2 1)))
         (max-value (cdr (q::eref mat 2 1)))
         (val (/ (- value min-value) (- max-value min-value)))
         )            
    (case (q::eref mat 2 2)
      (:lightness
       (let* ((hue (if (q::eref mat 1 0) (mod (q::eref mat 1 0) 360)))
              (low-light (q::eref mat 0 1))
              (hi-light (q::eref mat 0 2))
              (light (+ low-light (* val (- hi-light low-light))))
              )
         (apply #'wb::make-color (wb::lhs_to_rgb light hue (q::eref mat 2 0)))
         ))
      (:hue
       (let* ((light (q::eref mat 0 0))
              (low-hue (q::eref mat 1 1))
              (hi-hue (q::eref mat 1 2))
              (hue (mod (+ low-hue (* val (- hi-hue low-hue))) 360))
              )
         (apply #'wb::make-color (wb::lhs_to_rgb light hue (q::eref mat 2 0)))
         ))
      (:both
       (let* ((low-light (q::eref mat 0 1))
              (hi-light (q::eref mat 0 2))
              (light (+ low-light (* val (- hi-light low-light))))
              (low-hue (q::eref mat 1 1))
              (hi-hue (q::eref mat 1 2))
              (hue (mod (+ low-hue (* val (- hi-hue low-hue))) 360))
              )
         (apply #'wb::make-color (wb::lhs_to_rgb light hue (q::eref mat 2 0)))
         ))
      )
    ))

(defmethod set-draw-style :around ((self imagel) (slot-name (eql :color)) val &key)
  ;val shouldn't be a colour (an integer), for example use :map if you want to 
  ;use the lhs map matrix of the parent image
  (if (find-parent self)
    (let* ((parent (find-parent self))
           (mat (image-colour-map-of parent))
           (image-value (elt (z-coords-of parent 
                                          :cases (list (viewed-object-of self))) 0)))
      (if (wb::colorp val) ;change colour to something appropriate for an image
        (let ((default-mat (qk::sel mat))
              )
          (case (q::eref mat 2 2)
            (:lightness
             (setf (q::eref default-mat 1 0) (wb::hue-of val))
             )
            (:hue
             (setf (q::eref default-mat 1 1) (- (wb::hue-of val) 20))
             (setf (q::eref default-mat 1 2) (+ (wb::hue-of val) 20))
             )
            (:both
             (setf (q::eref default-mat 1 1) (- (wb::hue-of val) 20))
             (setf (q::eref default-mat 1 2) (+ (wb::hue-of val) 20))
             )
            )
          (set-draw-style (drawing-style-of self) :color 
                          (map-with-lhs-matrix 
                           image-value default-mat))    
          )
        (set-draw-style (drawing-style-of self) :color 
                        (map-with-lhs-matrix 
                         image-value mat))))
    
    (call-next-method)))

(defmethod set-draw-style :around ((self imagel) (slot-name (eql :highlight-color)) val 
                                   &key)
  ;val shouldn't be a colour (an integer), for example use :map if you want to 
  ;use the lhs map matrix of the parent image
  (if (find-parent self)
    (let* ((parent (find-parent self))
           (mat (image-colour-map-of parent))
           (image-value (elt (z-coords-of parent 
                                          :cases (list (viewed-object-of self))) 0)))
      (if (wb::colorp val)
        (let ((default-mat (make-array '(3 3)))
              )
          (setf (q::eref default-mat 2 0) (q::eref mat 2 0))
          (setf (q::eref default-mat 2 1) (q::eref mat 2 1))
          (case (q::eref mat 2 2)
            (:lightness
             (setf (q::eref default-mat 2 2) :lightness)
             (setf (q::eref default-mat 0 1) (q::eref mat 0 1))
             (setf (q::eref default-mat 0 2) (q::eref mat 0 2))
             (setf (q::eref default-mat 1 0) (wb::hue-of val))
             )
            (:hue
             (setf (q::eref default-mat 2 2) :hue)
             (setf (q::eref default-mat 0 0) (q::eref mat 0 0))
             (setf (q::eref default-mat 1 1) (- (wb::hue-of val) 15))
             (setf (q::eref default-mat 1 2) (+ (wb::hue-of val) 15))
             )
            (:both
             (setf (q::eref default-mat 0 1) (q::eref mat 0 1))
             (setf (q::eref default-mat 0 2) (q::eref mat 0 2))
             (setf (q::eref default-mat 1 1) (- (wb::hue-of val) 10))
             (setf (q::eref default-mat 1 2) (+ (wb::hue-of val) 10))
             )
            )
          (set-draw-style (drawing-style-of self) :highlight-color 
                          (map-with-lhs-matrix 
                           image-value default-mat))    
          )
        (set-draw-style (drawing-style-of self) :highlight-color 
                        (map-with-lhs-matrix 
                         image-value mat)))
      
      (call-next-method))))
