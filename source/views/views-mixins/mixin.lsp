;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mixins.lisp
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


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( orientation-mixin  justification-mixin viewport-coords-cache-mixin
            boxed-view-mixin boxed-subview-mixin bg-color-mixin
            square-view-mixin *bg-color* pass-draws-to-subviews
            set-orientation set-justification
            get-viewed-obj-styles styles-cache flip-mixin
            view-with-size set-box-p set-box-views-p
            box-color-mixin set-flip-x-p set-flip-y-p 
            set-viewport-coords viewport-coords-of remove-viewport-coords list-legal-justifications
            viewed-elements-cache viewed-elements-of
            )))

(defclass viewport-coords-cache-mixin ()
  ((viewport-coords-cache :initarg :viewport-coords-cache
                          :initform nil :accessor viewport-coords-cache-of )
   (viewport-compute-method :initarg :viewport-compute-method
                          :initform nil :accessor viewport-compute-method-of)
  ))

(defclass view-with-size ()
  ((view-size :initform 10 :initarg :size 
              :accessor view-size-of)
   (size-menu? :initform t :initarg :size-menu?
              :accessor size-menu-p))
  )

(defclass fixed-size-rectangle ()
  ((rectangle-width :initform nil :initarg :rectangle-width 
              :accessor rectangle-width-of)
   (rectangle-height :initform nil :initarg :rectangle-height
              :accessor rectangle-height-of)
   (size-menu? :initform t :initarg :size-menu?
              :accessor size-menu-p))
  )

(defclass pass-draws-to-subviews () ())

(defclass flip-mixin ()
  ((flip-x :initarg :flip-x?
                 :accessor flip-x-p :initform nil)
   (flip-y :initarg :flip-y?
                 :accessor flip-y-p :initform nil)))


(defclass orientation-mixin ()
  ((orientation :initarg :orientation
                :initform :horizontal
                :accessor orientation-of
                :documentation ":horizontal or :vertical")
   (orientation-menu? :initarg :orientation-menu?
                      :initform t
                      :accessor orientation-menu-p)
   )
  (:default-initargs  :orientation :horizontal))



(defclass justification-mixin ()
  ((justification :initarg :justification
                  :initform :center
             :accessor justification-of
             :documentation "Determines justification of view in viewport. ~
                             Legal justifications are at least~
                             :left :right :top  :bottom or :center")
   (justification-menu? :initarg :justification-menu?
                      :initform t
                      :accessor justification-menu-p))
  (:default-initargs  :justification :center :orientation :horizontal
    :justification-menu? t ))


(defclass box-color-mixin () 
  ((style-keys :initform '(:box-color) :allocation :class)
   (box-color-menu? :initarg :box-color-menu?
                      :initform t
                      :accessor box-color-menu-p))
  (:default-initargs  :box-color nil )
   (:documentation "Used by boxed-view-mixin and boxed-subview-mixin "))

(defclass boxed-view-mixin (box-color-mixin) 
  ((box
    :initarg :box? 
    :initform t 
    :accessor box-p)
   (box-margin
    :initarg :box-margin
    :initform 0
    :accessor box-margin-of)
   (box-menu? :initarg :box-menu?
                      :initform t
                      :accessor box-menu-p))
  (:documentation "Mix in this with box-p non-nil to get a view with a box around it"))

(defclass boxed-subview-mixin (box-color-mixin) 
  ((box-views? :initform t :initarg :box-views? :accessor box-views-p)
   (subview-box-margin
    :initarg :subview-box-margin
    :initform 0
    :accessor subview-box-margin-of)
   (box-views-menu? :initarg :box-views-menu?
                      :initform t
                      :accessor box-views-menu-p))
   (:documentation "Mix in this with box-p non-nil to get a view which~
                    draws boxes around its subviews."))



(defclass bg-color-mixin () 
  ((style-keys :initform '(:bg-color) :allocation :class)
   (bg-color-menu? :initarg :bg-color-menu?
                      :initform t
                      :accessor bg-color-menu-p))
  (:default-initargs  :bg-color wb:*default-canvas-background-color* )
  (:documentation "Mix this in last for a background color.~
                   Should also add bg-color to style-keys"))


(defclass square-view-mixin () ()
  (:documentation "Views with this mixin will appear in the ~
                   largest square centered in the viewport"))

(defclass styles-cache () 
  ((viewed-obj-styles 
    :initform nil
    :accessor viewed-obj-styles-of
    :documentation "A cache for draw styles ordered by viewed-object, may be mixed into d-views")))

(defgeneric get-viewed-obj-styles (styles-cache &key &allow-other-keys)
  (:documentation "returns draw-styles in order of viewed-objects"))


(defgeneric set-flip-x-p (flip-mixin val &key draw? )
  (:documentation "Flips the x axis of view."))

(defgeneric set-flip-y-p (flip-mixin val &key draw? )
  (:documentation "Flips the y axis of view."))


(defgeneric viewport-coords-of (viewport-coords-cache-mixin 
                                &key viewport)
  (:documentation "Returns the viewport coords"))

(defgeneric set-viewport-coords (viewport-coords-cache-mixin 
                                &key viewport coords)
  (:documentation "SEts the coords for viewport"))
  
;;;----------------------------------------------------------------------------------

(defmethod axis-of-orientation ((self orientation-mixin))
  (ecase (orientation-of self) 
           (:horizontal :x)
           (:vertical :y)))

(defmethod (setf axis-of-orientation) (axis (self orientation-mixin))
  (setf (orientation-of self)
        (ecase axis 
           (:x :horizontal )
           (:y :vertical ))))

(defmethod extent-of ((self orientation-mixin))
  (multiple-value-bind (l r b tp) (bounds-of (bounding-region-of self))
    (ecase (orientation-of self) 
           (:horizontal (values l r))
           (:vertical (values b tp)))))

(defmethod region-of-extent ((self orientation-mixin) min max)
  (ecase (orientation-of self) 
           (:horizontal (make-region min max 0 1))
           (:vertical (make-region 0 1 min max ))))



(defmethod (setf extent-of) (new-vals (self orientation-mixin))
  
  (let ((br (bounding-region-of self)) )
    (if (interval-p new-vals)
      (setq new-vals (list (min-of new-vals) (max-of new-vals))))
    (ecase (orientation-of self) 
           (:horizontal 
            (setf (x-extent-of br) new-vals))
           (:vertical 
            (setf (y-extent-of br) new-vals) 
            ) )))


(defmethod expand-extent (  (self orientation-mixin) new-vals)
  
  (let ((br (bounding-region-of self)) min max)
    (if (interval-p new-vals)
      (setf min (min-of new-vals) max (max-of new-vals))
      ( setf min (first new-vals) max (second new-vals)))
    (ecase (orientation-of self) 
           (:horizontal 
            (setf (left-of br) (min (left-of br) min) )
            (setf (right-of br) (max (right-of br) max)))
           (:vertical 
            (setf (bottom-of br) (min (bottom-of br) min)) 
            (setf (top-of br) (max (top-of br) max)) ))))


(defmethod orientation-of :before ((self orientation-mixin))
  (if (eq :prompt (slot-value self 'orientation))
    (setf (slot-value self 'orientation)
          (car (wb:prompt-for-items 
                (list :horizontal :vertical) 
                :prompt-text "Choose orientation")))))


(defmethod set-orientation ((self orientation-mixin) orientation &key (draw? nil) )
  (if (eq orientation :toggle)
    (setq orientation
          (if (eq (orientation-of self) :horizontal) :vertical :horizontal)))
  (setf (orientation-of self) orientation)
  (set-bounding-region self )
  (init-position-subviews self)
  (if draw?
    (remap-to-viewports self)))


(defmethod get-menu-items :around ((self orientation-mixin) (slot-name (eql 'middle-menu)))
  
    (let (( menu-list (call-next-method)))
      (if (orientation-menu-p self)
      (append menu-list
              `(( "Orientation" (set-orientation  :toggle :draw? T) "Toggle the orientation."
                  :sub-items (("Toggle" (set-orientation  :toggle :draw? T))
                              ("Horizontal" (set-orientation :horizontal :draw? T))
                              ("Vertical" (set-orientation :vertical :draw? T))))
                ))
      menu-list)))


;;;----------------------------------------------------------------------------------

(defmethod list-legal-justifications ((self justification-mixin))
  (list :left :right :x-center :bottom :top :y-center))

(defmethod set-justification ((self justification-mixin) justification &key (draw? nil) )
  (setf (justification-of self) justification)
  (set-bounding-region self )
  (init-position-subviews self)
  (if draw?
    (remap-to-viewports self)))


(defmethod justification-of :before ((self justification-mixin))
  (if (eq :prompt (slot-value self 'justification))
    (setf (slot-value self 'justification)
          (car (wb:prompt-for-items (list-legal-justifications self)
                :prompt-text "Choose justification")))))



(defmethod get-menu-items :around ((self justification-mixin) (slot-name (eql 'middle-menu)))
  
  
    (let (( menu-list (call-next-method)))
      (if (justification-menu-p self)
        (append menu-list
                `(( "Justification" (set-justification  :prompt :draw? T)
                    "Prompt for a change in the justification of the text.")))
        menu-list)))
;;;----------------------------------------------------------------------------------
(defgeneric set-box-p (boxed-view-mixin val &key draw?)
  (:documentation "Changes the parameter which controls whether or not~
                   a box is drawn around the views."))

(defmethod set-box-p ((self boxed-view-mixin) val &key (draw? t))
  (if (eql val :toggle)
    (setq val (not (box-p self))))
  (if draw? (erase-view self))
  (setf (box-p self) val)
  (if draw? (draw-view self :erase? nil)))

;; 
(defmethod draw-view :after ((self boxed-view-mixin)
                             &key viewport)
  (when (box-p self)
    (with-exposed-viewports  self viewport vp
      (draw-viewport vp :color (draw-style self :box-color) :margin (box-margin-of self)))))

(defmethod get-menu-items :around ((self boxed-view-mixin) 
                                   (slot-name (eql 'middle-menu)))
   (if (box-menu-p self)
  (add-menu-items self  
                  '(("Boxes?"  (set-box-p  :toggle )) )
                  (call-next-method))
  (call-next-method)))

;; added 11/95
(defmethod erase-view :after ((self boxed-view-mixin)
                             &key viewport)
  (when (box-p self)
    (with-exposed-viewports  self viewport vp
      (draw-viewport vp :color (draw-style self :box-color)
                            :operation :boole-andc1
                            :margin (box-margin-of self)))))

(defmethod update-menu-items :after ((self boxed-view-mixin) 
                                     (slot-name (eql 'middle-menu)))
  (let* ((m (slot-value self slot-name)))
     (if (box-menu-p self) 
       (wb:check-menu-item m "Boxes?" (box-p self)))
     (if (box-color-menu-p self)
     (if (box-p self)
      (wb:enable-menu-item  m "BoxColor")
      (wb:disable-menu-item  m "BoxColor")))))

;;;----------------------------------------------------------------------------------
(defun box-color-menu-items ()
  (declare (special  *color-menu-list* *shade-menu-list*))
  (loop for c in 
        (if (wb:color-device-p)
          *color-menu-list* *shade-menu-list*)
        collect 
        (list (car c) `(set-drawing-style :box-color ,(cadr c)))))

(defmethod style-menu-items :around ((self box-color-mixin))
  (let ((result (call-next-method)))
    ( if (box-color-menu-p self)
    (add-menu-items self `( ( "BoxColor" nil "" :sub-items ,(box-color-menu-items)) )
                    result)
    result)))

;;;----------------------------------------------------------------------------------

(defgeneric set-box-views-p (boxed-subview-mixin val &key draw?)
  (:documentation "Changes the parameter which controls whether or not~
                   boxes are drawn around the subviews."))

(defmethod get-menu-items :around ((self boxed-subview-mixin) 
                                   (slot-name (eql 'middle-menu)))
  (if (box-views-menu-p self)
  (add-menu-items self  
                  '(("Boxes?"  (set-box-views-p  :toggle )) )
                  (call-next-method))
  (call-next-method)))

(defmethod update-menu-items :after ((self boxed-subview-mixin) 
                                     (slot-name (eql 'middle-menu)))
  (let* ((m (slot-value self slot-name)))
    (if (box-views-menu-p self)
      (wb:check-menu-item m "Boxes?" (box-views-p self)))
    (if (box-color-menu-p self)
     (if (box-views-p self)
      (wb:enable-menu-item  m "BoxColor")
      (wb:disable-menu-item  m "BoxColor")))))


(defmethod set-box-views-p ((self boxed-subview-mixin) val &key (draw? t))
  (if (eql val :toggle)
    (setq val (not (box-views-p self))))
  (setf (box-views-p self) val)
  (if draw?
    (if val (draw-subview-boxes self) 
        (erase-subview-boxes self))))
  

(defmethod draw-subview-boxes ((self boxed-subview-mixin)
                               &key viewport) 
  (let ((color (draw-style self :box-color))
        (margin  (subview-box-margin-of self)))
    (with-exposed-viewports self viewport vp
      (loop for sv in (subviews-of self) 
            for sv-vp = (if sv (select-viewport sv vp))
            when sv-vp do
            (draw-viewport sv-vp :color color :margin margin)))))

(defmethod erase-subview-boxes ((self boxed-subview-mixin)
                             &key viewport)
  (let ((margin  (subview-box-margin-of self)))
    (with-exposed-viewports self viewport vp
          (loop for sv in (subviews-of self) 
                for sv-vp = (if sv (select-viewport sv vp))
                when sv-vp do
                (erase-viewport sv-vp :margin margin )))))

(defmethod draw-view :after ((self boxed-subview-mixin)
                             &key viewport)
  (when (box-views-p self)
    (draw-subview-boxes self :viewport viewport)))

(defmethod erase-view :after ((self boxed-subview-mixin)
                              &key viewport)
  (when (box-views-p self)
    (erase-subview-boxes self :viewport viewport)))




(defmethod reposition-view :after ((self boxed-subview-mixin)
                            &key default-positions draw? )
  (declare (ignore default-positions))
  (when (and draw? (box-views-p self))
    (draw-subview-boxes self)))

;;;----------------------------------------------------------------------------------


(defun bg-color-menu-items ()
  (declare (special  *color-menu-list* *shade-menu-list*))
  (loop for c in 
        (if (wb:color-device-p)
          *color-menu-list* *shade-menu-list*)
        collect 
        (list (car c) `(set-drawing-style :bg-color ,(cadr c)))))

(defmethod style-menu-items :around ((self bg-color-mixin))
  (let ((result (call-next-method)))
    (if (bg-color-menu-p self)
    (add-menu-items self `(( "BGColor" nil "" :sub-items ,(bg-color-menu-items)) )
                    result)
    result)))
 
     


(defmethod draw-view :before ((self bg-color-mixin) &key viewport)
  (if (has-draw-style-p self :bg-color)
    (let ((bg-color (draw-style self :bg-color)))   
      (with-exposed-viewports self viewport vp 
        (unless (wb:eq-colors bg-color (wb:canvas-background-color (window-of vp)))
          (fill-viewport vp :color bg-color))))))
 
;;;----------------------------------------------------------------------------------

(defmethod get-draw-portion ((self square-view-mixin) viewport)
  (let ((square (copy-region viewport)))
    (setf (radius-of square) (radius-of viewport))
    square))

(defmethod transform-mapping-draw-portions ((self square-view-mixin) 
                                            region transform)
  (let ((new-region (apply-transform transform region)))
    (make-transform-for-regions 
                     (get-draw-portion self region) 
                     (get-draw-portion self new-region))
  ))




 

;; the following methods xx-view 
;; (with the exception of remove-view, which performs
;; surgery on the view hierarchy at a different level)
;;  differ from the default xx-view in that

;; (i) if highlit? is non-nil the operation is applied only
;; to the highlighted subviews and



(defclass pass-draws-to-subviews () ())


(defmethod styles-to-subs ((self pass-draws-to-subviews) ) 
  (list :highlight?))

#|
;; comment these out because draw-view calls highlight if any subs are highlit.
(defmethod all-highlight? ((self pass-draws-to-subviews)) 
  (loop for v in (subviews-of self)
        for vis in (visible-subs-p self)
           always (or (not vis) (all-highlight?  v))))


(defmethod any-highlight? ((self pass-draws-to-subviews)) 
  (loop for v in (subviews-of self)
        for in? in (visible-subs-p self)
        thereis (and in? (any-highlight?  v))))

|#

(defmethod apply-to-subviews ((self pass-draws-to-subviews) fn viewport &optional highlit?)
  (loop for vp in (enlist-viewport self viewport) do
  (loop for sv in (some-subviews self :highlit? highlit?)
        for sv-vp = (select-viewport sv vp)
         do
        (funcall fn sv :viewport sv-vp :check-viewport? nil))))


(defmethod draw-view ((self pass-draws-to-subviews) &key viewport highlit?)
  (apply-to-subviews self #'draw-view viewport highlit?))

(defmethod invert-view ((self pass-draws-to-subviews) &key viewport highlit?)
  (apply-to-subviews self #'invert-view viewport highlit?))


(defmethod highlight-view ((self pass-draws-to-subviews)  &key viewport )
  (apply-to-subviews self #'highlight-view viewport ))


(defmethod downlight-view ((self pass-draws-to-subviews)  &key viewport )
  (apply-to-subviews self #'downlight-view viewport ))

(defmethod erase-view ((self pass-draws-to-subviews)
                       &key viewport highlit?)
  (apply-to-subviews self #'erase-view viewport highlit? ))




(defmethod get-viewed-obj-styles ((self styles-cache) &rest style-pairs  )
;; returns draw-styles in order of viewed-objects
  (setf (viewed-obj-styles-of self)
        (or (viewed-obj-styles-of self)
            (loop for vo in (cases-of self)
                collect 
                (loop repeat (length (list-viewed-elements vo))
                      collect
                (apply #'default-drawing-style self style-pairs))))))

(defmethod compute-map-to-viewport((self flip-mixin) vp)
  (let* ((br (bounding-region-of self))
         (tr (make-transform-for-regions br 
                                         (get-draw-portion self vp))))
    
    (if (or (flip-x-p self) (flip-y-p self))
      
      (multiple-value-bind (l r b tp) (bounds-of br)
        (let ((x-scale (x-scale tr))
              (y-scale (y-scale tr))
              (x-shift (x-shift tr))
              (y-shift (y-shift tr))
              (sx 1) (sy 1) (ox 0) (oy 0))
          (if (flip-x-p self)
            (setq sx -1 ox (* x-scale (+ l r))))
          (if (flip-y-p self)
            (setq sy -1 oy (* y-scale (+ b tp))))
          
          (translate-transform! tr (- x-shift) (- y-shift))
          (scale-transform! tr sx sy)
          (translate-transform! tr (+ x-shift ox)  (+ y-shift oy)))))
    tr))


(defmethod set-flip-x-p ((self flip-mixin) val &key (draw? nil ) )
  (if (eq val :toggle)
    (setf val (not (flip-x-p self))))
  (loop for v in (link-bounds-x-of self) do
        (if draw? (erase-view v))
        (setf (flip-x-p v) val)
        (remap-to-viewports v :erase? nil :draw? draw?)))

(defmethod set-flip-y-p ((self flip-mixin) val &key (draw? nil ) )
  (if (eq val :toggle)
    (setf val (not (flip-y-p self))))
  (loop for v in (link-bounds-y-of self) do
        (if draw? (erase-view v))
        (setf (flip-y-p v) val)
        (remap-to-viewports v :erase? nil :draw? draw?)))

;;----------------------------------------------------------------------
(defmethod fix-viewports ((self view-with-size) &key viewport ) 
  (let ((size (view-size-of self)))
    (loop for vp in (if viewport 
                      (list viewport) (viewports-of self) )
          do (set-viewport-size vp size))))





(defmethod set-view-size ((self view-with-size) new &key (draw? t))
  (if draw? (erase-view self))
  (setf (view-size-of self) new)
  (fix-viewports self)
  (if draw? (draw-view self)))
  

(defmethod set-view-size :around ((self view-with-size) new &key (draw? t))
  (unless (numberp new)
    (setq new (get-view-size self new)))
  (call-next-method self new :draw? draw?))

(defmethod  get-view-size  ((self view-with-size) new)
  (let ((old (view-size-of self))
          (inc (view-size-increment self)))
    (unless old  (setq new :prompt))
      (case new
              (:larger (+  old inc)) 
              (:smaller (-  old inc))
              (:prompt (wb:prompt-user :type 'number 
                                       :read-type :eval
                                       :prompt-string 
                                       (format nil "Change size from ~A" old)))
              (t old))))
      
(defmethod view-size-increment ((self view-with-size))
  (if (view-size-of self)
  (round (* (view-size-of self) 0.5))))

(defmethod reshape-viewport :after ((self view-with-size) viewport &rest ignore)

;; point symbol viewports are not resized when window is reshaped
  (declare (ignore ignore viewport))
  (fix-viewports self))

(defmethod add-viewport :after ((self view-with-size) viewport pvp &key)
  (declare (ignore pvp))
  (fix-viewports self :viewport viewport))


(defmethod get-menu-items :around ((self view-with-size) (slot-name (eql 'middle-menu)))
  (if (size-menu-p self)
  (let (( menu-list (call-next-method))
        (size-menu-items '(("larger" (set-view-size  :larger))
                           ("smaller" (set-view-size :smaller))
                           ("prompt" (set-view-size :prompt)))))
    (append menu-list
            `(( "Size" nil "" :sub-items ,size-menu-items)
              )))
  (call-next-method)))




;;--------------------------------------------------------------------------------

(defmethod fix-viewports ((self fixed-size-rectangle) &key viewport ) 
  (let ((h (rectangle-height-of  self))
        (w (rectangle-width-of  self)))
    (loop for vp in (if viewport 
                      (list viewport) (viewports-of self) )
          do (set-viewport-width-height vp (or w (width-of vp)) (or h (height-of vp))))))





(defmethod set-view-width ((self fixed-size-rectangle) w  &key (draw? t))
  (if draw? (erase-view self))
  (setf (rectangle-width-of  self) w)
  (fix-viewports self)
  (if draw? (draw-view self)))

(defmethod set-view-height ((self fixed-size-rectangle) h &key (draw? t))
  (if draw? (erase-view self))
  (setf (rectangle-height-of  self) h)
  (fix-viewports self)
  (if draw? (draw-view self)))
  

(defmethod set-view-height :around ((self fixed-size-rectangle)  h &key (draw? t))
  
  (unless (numberp h)
    (setq h (get-view-height self h)))
  (call-next-method self  h :draw? draw?))

(defmethod set-view-width :around ((self fixed-size-rectangle) w  &key (draw? t))
  (unless (numberp w)
    (setq w (get-view-width self w)))
 
  (call-next-method self w :draw? draw?))

(defmethod  get-view-width  ((self fixed-size-rectangle) new)
  (let ((old (rectangle-width-of self))
          (inc (rectangle-width-increment self)))
    (unless old (setq new :prompt))
      (case new
              (:larger (+  old inc)) 
              (:smaller (-  old inc))
              (:prompt (wb:prompt-user :type 'number 
                                       :read-type :eval
                                       :prompt-string 
                                       (format nil "Change width from ~A" old)))
              (t old))))

(defmethod  get-view-height  ((self fixed-size-rectangle) new)
  (let ((old (rectangle-height-of self))
          (inc (rectangle-height-increment self)))
      (unless old (setq new :prompt))
        (case new
              (:larger (+  old inc)) 
              (:smaller (-  old inc))
              (:prompt (wb:prompt-user :type 'number 
                                       :read-type :eval
                                       :prompt-string 
                                       (format nil "Change height from ~A" old)))
              (t old))))

      
(defmethod rectangle-height-increment ((self fixed-size-rectangle))
  (let ((h (rectangle-height-of self))
        vp)
    
    (cond ((not (null h))
            (round (* h 0.5)))
           ((setq vp (car (viewports-of self)))
            ( round (* .5 (height-of vp))))
           (t 1))))

(defmethod rectangle-width-increment ((self fixed-size-rectangle))
  (let ((w (rectangle-width-of self))
        vp)
    (cond ((not (null w))
            (round (* w 0.5)))
           ((setq vp (car (viewports-of self)))
            ( round (* .5 (width-of vp))))
           (t 1))))
    


(defmethod reshape-viewport :after ((self fixed-size-rectangle) viewport &rest ignore)

;; point symbol viewports are not resized when window is reshaped
  (declare (ignore ignore viewport))
  (fix-viewports self))

(defmethod add-viewport :after ((self fixed-size-rectangle) viewport pvp &key)
  (declare (ignore pvp))
  (fix-viewports self :viewport viewport))


(defmethod get-menu-items :around ((self fixed-size-rectangle) (slot-name (eql 'middle-menu)))
  (if (size-menu-p self)
  (let (( menu-list (call-next-method))
        (width-menu-items '(("wider" (set-rectangle-width  :larger))
                           ("narrower" (set-rectangle-width :smaller))
                           ("prompt" (set-rectangle-width :prompt))))
        (height-menu-items '(("taller" (set-rectangle-width  :larger))
                           ("shorter" (set-rectangle-height :smaller))
                           ("prompt" (set-rectangle-height :prompt)))))
    (append menu-list
            `(( "Width" nil "" :sub-items ,width-menu-items)
              ( "Height" nil "" :sub-items ,height-menu-items)
              )))
  (call-next-method)))


(defmethod set-rectangle-width ((self fixed-size-rectangle) new &key (draw? t))
  (if draw? (erase-view self))
  (setf (rectangle-width-of self) new)
  (fix-viewports self)
  (if draw? (draw-view self)))

(defmethod set-rectangle-height ((self fixed-size-rectangle) new &key (draw? t))
  (if draw? (erase-view self))
  (setf (rectangle-height-of self) new)
  (fix-viewports self)
  (if draw? (draw-view self)))
  

(defmethod set-rectangle-width :around ((self fixed-size-rectangle) new &key (draw? t))
  (unless (numberp new)
    (setq new (get-view-width self new)))
  (call-next-method self new :draw? draw?))

(defmethod set-rectangle-height :around ((self fixed-size-rectangle) new &key (draw? t))
  (unless (numberp new)
    (setq new (get-view-height self new)))
  (call-next-method self new :draw? draw?))



;;--------------------------------------------------------------------------------


(defmethod delete-viewport :before ((self viewport-coords-cache-mixin) viewport)
  
  ;; remove the viewport  VIEWPORT and corresponding  MAP and PARENT-VIEWPORT 
  
  (let ((temps (viewport-coords-cache-of self))
        (p (position viewport (viewports-of self))))
    (setf (viewport-coords-cache-of self)
          (delete (elt temps p) temps))))


(defmethod add-viewport :before ((self viewport-coords-cache-mixin) viewport pvp &key)
  (declare (ignore pvp))
  (if (member viewport (viewports-of self))
    (remove-viewport-coords self :viewport viewport)
    (push nil (viewport-coords-cache-of self))))
 



(defmethod viewport-coords-of ((self viewport-coords-cache-mixin) 
                           &key viewport)
  (setq viewport (or viewport (car (viewports-of self))))
  (let ((p (position viewport (viewports-of self)))
        (temps (viewport-coords-cache-of self))
        (meth (viewport-compute-method-of self)))
    (or
     (elt temps p)
     (setf (elt temps p) (if meth (funcall meth self viewport))))))

(defmethod set-bounding-region :after ((self viewport-coords-cache-mixin) &key  &allow-other-keys)
  (remove-viewport-coords self))

(defmethod remove-viewport-coords ((self viewport-coords-cache-mixin) 
                           &key viewport)
  (if viewport
  (let ((p (position viewport (viewports-of self))))
     (setf (elt (viewport-coords-cache-of self) p) nil))
  (setf (viewport-coords-cache-of self) (make-list (length (viewports-of self))))))


(defmethod reshape-viewport :after ((self viewport-coords-cache-mixin) viewport  
                                    &key  new-location transform draw? )
  
  (declare (ignore  new-location transform draw? ))
  (remove-viewport-coords self :viewport viewport)
  )


(defmethod set-viewport-coords  ((self viewport-coords-cache-mixin)
                                 &key  viewport coords )
  
  (let ((p (position viewport (viewports-of self)))
        (temps (viewport-coords-cache-of self))
        )
    (setf (elt temps p) coords)))




(defclass viewed-elements-mixin () 
  ((viewed-elements 
    :initform :viewed-object 
    :accessor viewed-elements-of 
    :initarg :viewed-elements
    :documentation "Cache for list of elements in object being viewed")
   ))

#|
Use of viewed-elements-mixin:
    multi-style-views have one drawing style per viewed element.

    linkable-views: 
       views with common (as determined by the link test) viewed-elements are linked


viewed-elements = :single 
       regard viewed object as a single viewed element
       (eg data-label)

viewed-elements = :expand-viewed-object
       list-viewed-elements returns (list-viewed-elements viewed-object)
       (eg group-label)

initial viewed-elements = function
       list-viewed-elements returns the fuinction applied to the viewed object


initial viewed-elements = viewed object
       list-viewed-elements returns the viewed object as a list.
       the default


|#
(defmethod list-viewed-elements ((self viewed-elements-mixin))
  (let ((vo-elt (viewed-elements-of self)))
    (if (listp vo-elt)
      vo-elt
      (let ((vo (viewed-object-of self)))
        (setf (viewed-elements-of self) 
              (cond 
               ((eq vo-elt :single)
                (if (null vo) vo (list vo)))
               ((eq vo-elt :viewed-object)
                (if (or (null vo) (list-of-datasets-p vo))
                  vo 
                  (list vo)))
               ((eq vo-elt :expand-viewed-object)
                (cond ((null vo) vo)
                      ((dataset-p vo) (list-cases vo))
                      (t (list vo))))
                 ((functionp vo-elt)
                (funcall vo-elt self))
               (t nil)))))))

(defmethod list-viewed-elements ((vo t))
  (cond  ((dataset-p vo)
         (list-cases vo))
          
        (t (list vo))))





                























