;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               grid-view.lsp
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
;;;     C.B. Hurley 1998 National University of Ireland Maynooth
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)


(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(grid-lines grid-view set-grid)))


(defclass grid-lines (basic-axis)
  ((style-keys :initform '(:tics?   :color :font ) :allocation :class)
   (line-interval :initform  nil  :accessor line-interval-of :initarg :line-interval)
   (middle-menu :allocation :class :initform nil))
  (:default-initargs :tic-length :viewport :box? nil ))



(defmethod draw-view ((self grid-lines) &key viewport)
  (let ((orient (orientation-of self))
        (c (draw-style self :color)))
    (with-exposed-viewports self viewport vp
      (draw-grid-lines self orient vp :color c))))


(defmethod erase-view ((self grid-lines) 
                       &key viewport  )
  (let ((orient (orientation-of self)))
  
  (with-exposed-viewports self viewport vp
    (draw-grid-lines self orient vp :operation :boole-andc1))))

(defmethod highlight-operation ((self grid-lines))
  :default)


(defmethod highlight-view ((self grid-lines)
                           &key viewport )
  (let ((orient (orientation-of self)))
  
  (with-exposed-viewports self viewport vp
    (draw-grid-lines self orient vp :operation  (highlight-operation self)
                    :color *default-highlight-color* ))))


(defmethod invert-view ((self grid-lines) 
                        &key viewport )
  (let ((orient (orientation-of self)))
  
  (with-exposed-viewports self viewport vp
    (draw-grid-lines self  orient vp :operation :boole-xor))))


(defmethod draw-grid-lines ((self grid-lines) orient vp &key color operation)
  (let ((bw (window-of vp))
     (tic-list (tic-list-for-viewport self vp))
     (line-interval (line-interval-of self)))
        
      (wb:with-pen-values bw color 1 operation
      (multiple-value-bind (xmin xmax ymin ymax) (bounds-of vp)
         (if (draw-style self :tics?)
          (if (eq orient :horizontal)
            (progn
              (if line-interval
              (let* ((map (select-map-to-viewport self vp) )
                    ( oy (y-shift map)) ( sy (y-scale map)))
                (unless (eq (car line-interval) :viewport)
                (setq ymin (truncate (+ oy (* sy (car line-interval))))))
                (unless (eq (cadr line-interval) :viewport)
                  (setq    ymax (truncate (+ oy (* sy (cadr line-interval))))))))
                
         
                  (loop  for t-l in tic-list
                         for tic-x = (car t-l)
                         do
                         (wb:canvas-draw-line bw tic-x  ymin
                                              tic-x ymax)))
            (progn 
              (if line-interval
              (let* ((map (select-map-to-viewport self vp) )
                    ( ox (x-shift map)) ( sx (x-scale map)))
                (unless (eq (car line-interval) :viewport)
                (setq xmin (truncate (+ ox (* sx (car line-interval))))))
                (unless (eq (cadr line-interval) :viewport)
                  (setq    xmax (truncate (+ ox (* sx (cadr line-interval))))))))
                  (loop  for t-l in tic-list
                         for tic-y = (car t-l)
                         do
                         (wb:canvas-draw-line bw xmin tic-y  
                                              xmax tic-y)))))))))
          





(defmethod style-menu-items ((self grid-lines))
  )

(defmethod get-menu-items ((self grid-lines) (slot-name (eql 'middle-menu)))
  )

(defmethod get-menu-items :around ((self grid-lines) 
                                   (slot-name (eql 'middle-menu)))
 (append  (style-menu-items self)
  `(( "#Lines"  (set-ntics nil :viewport :msg "Enter number of lines"))
    
    ( "Line positions"  (set-tics  nil :viewport :msg "Enter line locations"))
    ( "Line length"  (set-line-length  nil  nil :viewport ))
    ( "Grid extent"  (set-extent nil nil :viewport))
   
    )))


(defmethod set-line-length ((self grid-lines) min max &key  viewport (draw? t) )
  (let ((pos (prompt-position self viewport))
        
        (current (line-interval-of self))
         )
    (cond ((and (null min) (null max))
           (let ((new (wb:prompt-user 
                       :type 'list 
                       :read-type :read
                       :prompt-string (if current (format nil "Change min and max from ~S to:"
                                                                      current)
                                                      (format nil "Change min and max from viewport extent to:"
                                                              ))
                       :left (2d-position-x pos)
                       :top (2d-position-y pos))))
             (setq min (first new))
             (setq max (second new))))
          ((null min)
           (setq min (wb:prompt-user 
                      :type 'number 
                      :read-type :eval
                      :prompt-string (if current (format nil "Change min from ~S to:"
                                                                      (first current))
                                                      (format nil "Change min from viewport minimum to:"
                                                              ))
                      :left (2d-position-x pos)
                      :top (2d-position-y pos))))
          ((null max)
           (setq max (wb:prompt-user 
                      :type 'number 
                      :read-type :eval
                      :prompt-string (if current (format nil "Change max from ~S to:"
                                                                      (second current))
                                                      (format nil "Change max from viewport maximum to:"
                                                              ))
                      :left (2d-position-x pos)
                      :top (2d-position-y pos))))
          (t nil))
    (if draw? (erase-view self :viewport viewport ))
    (setf (line-interval-of self) (list min max))
    (if draw? (draw-view self :viewport viewport :erase? nil))))
   

(defmethod region-of-extent ((self grid-lines) min max)
  (multiple-value-bind (l r b tp) (bounds-of self)
  (ecase (orientation-of self) 
           (:horizontal (make-region (min min l) (max max r) 0 1))
           (:vertical (make-region 0 1 (min min b) (max max tp) )))))




(defmethod style-menu-items :around ((self grid-lines))
  `(("Color" nil "" :sub-items ,(color-menu-items))
    ("Invisible?" (set-drawing-style :invisible? :toggle))))

(defmethod distance-to-location ((self grid-lines) viewport location) 
  (if (draw-style self :tics?)
    (if (selected-p self viewport location)
      (let* ((c (if (region-p location)
                  (centre-of location) location))
             (x (2d-position-x c))
             (y (2d-position-y c))
             (tics-vp (tic-list-for-viewport self viewport)))
        (if (eq (orientation-of self) :horizontal)
          (expt (loop for tic in tics-vp
                for ti = (car tic)
                minimize (abs (- x ti))) 2)
          (expt (loop for tic in tics-vp
                for ti = (car tic)
                minimize (abs (- y ti)) ) 2)
          
          ))
      (call-next-method))))

(defclass grid-view(pass-draws-to-subviews 2d-view compound-view)
  ((style-keys :initform '( :grid  :color) :allocation :class)
   ;;(action-target :initform nil :initarg :action-target :accessor action-target-of)
   (horizontal-lines :initform nil :initarg :x-lines :accessor horizontal-lines-of)
   (vertical-lines :initform nil :initarg :y-lines :accessor vertical-lines-of))
  (:default-initargs 
    :initform-fn nil
  ))

(defmethod init-position-subviews ((self grid-view)
                                   &key )
  (let ((br (bounding-region-of self)))
    (setf (sub-view-locns-of self)
          (loop repeat (length (subviews-of self))
                collect
                (make-region br))))
  (constrain-bounds self))


(defmethod styles-to-subs ((self grid-view ) ) 
  (list :highlight?  :color ))

(defmethod construct-sub-views ((self grid-view)  &rest keyword-pairs &key  )
  (setf (subviews-of self)
        (list (setf (vertical-lines-of self) 
                    (apply #'view :type 'grid-lines  
                    :orientation :horizontal keyword-pairs))
              (setf (horizontal-lines-of self)  
                    (apply #'view :type 'grid-lines
                    :orientation :vertical keyword-pairs))))
  )

(defmethod constrain-bounds ((self grid-view)  &key draw?
                             region (link-bounds-x? t) (link-bounds-y? t))
  "Constrains the bounds of the  views."
  (let ((views (list self (horizontal-lines-of self) (vertical-lines-of self))))
    (setq region (or region (bounding-region-of self) ))
    (when link-bounds-x?
      (link-view-bounds  views :x)
      (set-view-extents views  :x :region region :recompute? nil))
    (when link-bounds-y?
      (link-view-bounds  views :y)
      (set-view-extents views  :y :region region :recompute? nil))
    
    (if (or link-bounds-y? link-bounds-x?)
      (loop for v in views do
            (remap-to-viewports v :erase? t :draw? draw?)))))


(defmethod set-grid  ((self grid-view) value &key (draw? t))
  (setq value
        (case value
          (:on t)
          (:off nil)
          (t (not (draw-style self :grid)))))
  (if value
    (progn
      (set-draw-style  self :grid t)
      (loop for v in (subviews-of self) do
            (set-drawing-style v :tics? t :draw? draw? :erase? nil)))
    (progn (set-draw-style  self :grid nil)
         (loop for v in (subviews-of self) do
               (set-drawing-style v :tics? nil  :erase? draw?)))))

(defmethod style-menu-items ((self grid-view))
  
  `(("Grid?" (set-grid  :toggle)
      )))

(defmethod set-color-style((self grid-view) &rest keys &key dir color &allow-other-keys)
  (declare (ignore keys))
  (if (eq color :prompt)
    (setq color (wb::prompt-user-for-color)))
 (if (eq dir :horizontal)
   (set-drawing-style (vertical-lines-of self) :color color)
   (if (eq dir :vertical)
   (set-drawing-style (horizontal-lines-of self) :color color)
   (set-drawing-style self :color color))))
         


(defmethod distance-to-location ((self grid-view) viewport location)
  (if (selected-p self viewport location)
  (let* ((c (if (region-p location)
                  (centre-of location) location))
         (x (2d-position-x c))
         (y (2d-position-y c)))
    (multiple-value-bind (la ra ba ta) (bounds-of viewport)
 
    (min  (abs (-  x la)) (abs (-  x ra)) (abs (-  y ba)) (abs (-  y ta)))))))



(defmethod row-views((self grid-view))
  (list (list (vertical-lines-of self) (horizontal-lines-of self))))


(defmethod col-views((self grid-view))
  (list (list (vertical-lines-of self) (horizontal-lines-of self))))



(defmethod compute-sub-viewports ((self grid-view)
                                  &optional viewport subviews)
  (let ((viewports (if viewport (list viewport) (viewports-of self)))
        
        )
    (setq subviews (or subviews (subviews-of self)))
    (loop for sv in subviews
           do
          (loop for vp in viewports 
                for sv-vp = (or (select-viewport sv vp)
                                (make-viewport (window-of vp)))
                do
                (setf (bounds-of sv-vp) vp)
                (add-viewport sv sv-vp vp))))
  )



#| testing

(setq s (scatterplot :data squids :x "weight" :y "width"))

|#








