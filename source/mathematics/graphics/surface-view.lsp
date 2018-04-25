;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               surface-view.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) 1992
;;;                Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     P. Poirier 1992
;;;     R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(surface-view surface-of color-table-of fast-color-table-of
          vp-saved-rotations-of moving-p spin-orientation-of
          hidden-lines-p toggle-hidden-lines
          surface-fill-p toggle-surface-fill
          depth-cue-p toggle-depth-cue
          standardize-surface-data number-of-colors-of
          set-spin-orientation)
        ))


(defclass surface-view (square-view-mixin view)
  ((middle-menu :allocation :class :initform nil)
   (surface :initform NIL :accessor surface-of :initarg :surface)
   (color-table :initform NIL :accessor color-table-of :initarg :color-table)
   (fast-color-table :initform NIL :accessor fast-color-table-of 
                     :initarg :fast-color-table)
   (vp-rotation 
    :initarg :vp-rotation 
    :initform nil
    :accessor vp-rotation-of
    :documentation "An association list of viewport rotation pairs.")
   (vp-depth-cue? 
    :initarg :vp-depth-cue? 
    :initform nil
    :accessor vp-depth-cue?-of
    :documentation "An association list of viewport depth-cue? pairs.")
   (vp-hidden-lines? 
    :initarg :vp-hidden-lines? 
    :initform nil
    :accessor vp-hidden-lines?-of
    :documentation "An association list of viewport hidden-lines? pairs.")
   (vp-surface-fill? 
    :initarg :vp-surface-fill? 
    :initform nil
    :accessor vp-surface-fill?-of
    :documentation "An association list of viewport surface-fill? pairs.")
   (increment 
    :initform  (/ pi 180)
    :initarg  :increment 
    :accessor vw::increment-of
    :documentation
    "An angle increment.")
   (vp-x :initform NIL
         :initarg :vp-x 
         :accessor vp-x-of
         :documentation
         "An association list of viewport x-coordinate pairs.")
   (vp-y :initform NIL :initarg :vp-y :accessor vp-y-of
         :documentation
         "An association list of viewport y-coordinate pairs.")
   (vp-old-x :initform NIL
             :initarg :vp-old-x 
             :accessor vp-old-x-of
             :documentation
             "An association list of viewport old-x-coordinate pairs.")
   (vp-old-y :initform NIL :initarg :vp-old-y :accessor vp-old-y-of
             :documentation
             "An association list of viewport old-y-coordinate pairs.")
   (update-scratch-x :initform NIL :initarg :update-scratch-x
                     :accessor update-scratch-x-of
                     :documentation
                     "A temporary storage for x-coordinates.")
   (update-scratch-y :initform NIL :initarg :update-scratch-y
                     :accessor update-scratch-y-of
                     :documentation
                     "A temporary storage for y-coordinates.")
   (vp-lower-y :initform NIL :initarg :vp-lower-y :accessor vp-lower-y-of
               :documentation
               "An association list of viewport . vector pairs, each vector ~
                gives the current lower y bound of the surface for each x ~
                coordinate in the viewport.")
   (vp-upper-y :initform NIL :initarg :vp-upper-y :accessor vp-upper-y-of
               :documentation
               "An association list of viewport . vector pairs, each vector ~
                gives the current upper y bound of the surface for each x ~
                coordinate in the viewport.")
   (x-cache :initform NIL :initarg :x-cache :accessor x-cache-of)
   (y-cache :initform NIL :initarg :y-cache :accessor y-cache-of)
   (z-cache :initform NIL :initarg :z-cache :accessor z-cache-of)
   (vp-saved-rotations
    :initform NIL
    :initarg :vp-saved-rotations
    :accessor vp-saved-rotations-of
    :documentation
    "An association list of viewport . list pairs.  The elements of each list ~
     are the saved rotations from the starting (identity) position.")
   (vp-theta-cache
    :initform NIL
    :accessor vp-theta-cache-of
    :documentation
    "An association list of viewport . (con cos-theta sin-theta) pairs.  ~
     A cache to avoid multiple trigonometric calculations.")
   (vp-col-pars :initform NIL
                :accessor vp-col-pars-of
                :documentation
                "An association list of viewport . vector pairs, each vector ~
                 contains 3  colour parameters used to select colours from the ~
                 colour table.")
   (vp-z-pos-pars :initform NIL
                  :accessor vp-z-pos-pars-of
                  :documentation
                  "An association list of viewport . vector pairs, each vector ~
                   contains 5  parameters used to select the entry point ~
                   in the surface vector and to determine the direction and step size ~
                   for traversing the surface vector.")
   (vp-draw-line :initform NIL
                 :accessor vp-draw-line-of
                 :documentation
                 "An association list of viewport . vector pairs, each vector ~
                  contains 8  parameters used to determine the lines needed to draw ~
                  between two positions.")
   (vp-num-lines
    :initform NIL
    :accessor vp-num-lines-of
    :documentation
    "An association list of viewport . vector pairs, each vector ~
     contains the number of lines needed to draw ~
     between two positions.")
   (screen-range
    :initform (expt 2 16)
    :initarg :screen-range
    :accessor screen-range-of)
   (moving?
    :initform nil 
    :accessor moving-p)
   (spin-orientation
    :initform :surface
    :accessor spin-orientation-of
    :documentation "One of :screen (the default) ~
                    or :surface indicating whether the rotation ~
                    of the view is to be with respect to the screen coordinate ~
                    system or with respect to the surface coordinate system.")
   
   )
  )


(defmethod right-button-fn ((self surface-view) 
                            &key viewport  )
  (declare (ignore self viewport))
  ;;(menu-choose-item self 'right-menu  viewport )
  )

(defmethod initialize-instance :after 
           ((self surface-view)
            &rest initargs
            &key 
            (red? NIL)
            (green? NIL)
            (blue? NIL)
            (from-color NIL)
            (to-color NIL)
            (colors NIL)
            (n-colors 256))
  (declare (ignore initargs))
  (unless (fast-color-table-of self)
    (setf (fast-color-table-of self)
          (wb:make-color-table :colors (or colors (color-table-of self))
                               :number (cond
                                        (colors (length colors))
                                        ((color-table-of self)
                                         (length (color-table-of self)))
                                        (T n-colors))
                               :red? red?
                               :green? green?
                               :blue? blue?
                               :from-color from-color
                               :to-color to-color
                               :fast-colors? T)))
  (unless (and (color-table-of self)
               (= (length (color-table-of self))
                  (length (fast-color-table-of self))))
    (setf (color-table-of self)
          (wb:make-color-table :colors (fast-color-table-of self)
                               :number (number-of-colors-of self)
                               :red? red?
                               :green? green?
                               :blue? blue?
                               :from-color from-color
                               :to-color to-color
                               :fast-colors? NIL)))
  (if (surface-of self)
    (init-surface-view self) )
  )

(defmethod viewed-object-of ((self surface-view))
  (surface-of self))

(defmethod (setf viewed-object-of)  (new-value (self surface-view))
  (setf (surface-of self) new-value))

(defmethod number-of-colors-of ((self surface-view))
  (length (fast-color-table-of self)))

(defmethod hidden-lines-p ((self surface-view) &optional viewport)
  (unless viewport
    (setq viewport
          (if (> (length (viewports-of self)) 1)
            (which-viewport self)
            (car (viewports-of self)))))
  (cdr (assoc viewport (vp-hidden-lines?-of self))))

(defun toggle-hidden-lines (surface-view viewport)
  (let ((result (assoc viewport (vp-hidden-lines?-of surface-view))))
    (setf (cdr result) (not (cdr result)))))

(defmethod surface-fill-p ((self surface-view) &optional viewport)
  (unless viewport
    (setq viewport
          (if (> (length (viewports-of self)) 1)
            (which-viewport self)
            (car (viewports-of self)))))
  (cdr (assoc viewport (vp-surface-fill?-of self))))

(defun toggle-surface-fill (surface-view viewport)
  (let ((result (assoc viewport (vp-surface-fill?-of surface-view))))
    (setf (cdr result) (not (cdr result)))))

(defmethod depth-cue-p ((self surface-view) &optional viewport)
  (unless viewport
    (setq viewport
          (if (> (length (viewports-of self)) 1)
            (which-viewport self)
            (car (viewports-of self)))))
  (cdr (assoc viewport (vp-depth-cue?-of self))))

(defun toggle-depth-cue (surface-view viewport)
  (let ((result (assoc viewport (vp-depth-cue?-of surface-view))))
    (setf (cdr result) (not (cdr result)))))

(defun set-spin-orientation (surface-view &optional (orientation :screen))
  (if (member orientation (list :screen :surface))
    (setf (spin-orientation-of surface-view) orientation)
    (quail-error "~&Illegal spin orientation: ~a for ~a"
                 orientation surface-view)))

(defun standardize-surface-data (surface-view)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let*
    ((s (surface-of surface-view))
     (x (x-grid-of s))
     (y (y-grid-of s))
     (z (surface-heights-of s))
     (z-loc (mean z))
     (xmin (min x))
     (xmax (max x))
     (xloc (/ (+ xmax xmin) 2.0))
     (ymin (min y))
     (ymax (max y))
     (yloc (/ (+ ymax ymin) 2.0)))
    (flet
      ((s-data (cx x range location)
         (let ((maxx 0)
               (dx (array-total-size cx)))
           #|
         (loop for i from 0 to (- dx 1)
               maximize
               (abs (setf (eref cx i)
                          (round
                           (- (eref x i) location))))
               into max
               finally (setf maxx max))
         (loop for i from 0 to (- dx 1) do
               (setf (aref cx i)
                     (round (+ (/ (* (eref cx i)
                                     range) 
                                  maxx)))))
            |#
           (loop for i from 0 to (- dx 1)
                 maximize
                 (abs (- (eref x i) location))
                 into max
                 finally (setf maxx max))
           (loop for i from 0 to (- dx 1) do
                 (setf (eref cx i)
                       (round (/ (* (- (eref x i) location)
                                    range)
                                 maxx))))
           )))
      (s-data (x-cache-of surface-view)
              x
              (screen-range-of surface-view)
              xloc) 
      (s-data (y-cache-of surface-view)
              y
              (screen-range-of surface-view)
              yloc) 
      (s-data (z-cache-of surface-view)
              z
              (screen-range-of surface-view)
              z-loc)
      )))

(defun init-surface-view (s-v)
  (let*
    ((s (surface-of s-v))
     (x (x-grid-of s))
     (y (y-grid-of s))
     (dim-x (array-total-size x))
     (dim-y (array-total-size y))
     (dim-z (* dim-x dim-y)))
    
    (setf (x-cache-of s-v) (make-array (list dim-x) :element-type
                                       'integer :initial-element 0))
    (setf (y-cache-of s-v) (make-array (list dim-y) :element-type 
                                       'integer :initial-element 0))
    (setf (z-cache-of s-v) (make-array (list dim-z) :element-type 
                                       'integer :initial-element 0))
    (standardize-surface-data s-v)
    ))

;;; Add do-nothing primary FEB 04, 1998
(defmethod add-viewport ((self surface-view) viewport pvp
                                 &key (theta .05) (depth-cue? NIL)
                                 (hidden-lines? NIL) (surface-fill? NIL))
   (declare (ignore self viewport pvp theta depth-cue? hidden-lines? surface-fil
l?))
   (call-next-method))

(defmethod add-viewport :before ((self surface-view) viewport pvp
                                 &key (theta .05) (depth-cue? NIL)
                                 (hidden-lines? NIL) (surface-fill? NIL))
  (declare (ignore pvp))
  ;;(format *terminal-io* "~&viewport = ~a" viewport)
  (unless (member viewport (viewports-of self))
    (let*
      ((s (surface-of self))
       (x (x-grid-of s))
       (y (y-grid-of s))
       (dim-x (array-total-size x))
       (dim-y (array-total-size y))
       (dim-z (* dim-x dim-y))
       (max-x (width-of viewport))
       (max-y (height-of viewport))
       (a (make-array (list 9)
                      :element-type 'single-float
                      :initial-contents
                      (list 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0)))
       (sx (make-array (list dim-z) :element-type 'integer :initial-element 0))
       (sy (make-array (list dim-z) :element-type 'integer :initial-element 0))
       (old-x (make-array (list dim-z) :element-type 'integer :initial-element 0))
       (old-y (make-array (list dim-z) :element-type 'integer :initial-element 0))
       (szv (make-array (list 5) :element-type 'integer :initial-element 0))
       (col (make-array (list 3) :element-type ;;'fixnum
                        'single-float :initial-element 0.0
                        ))
       (ax (or (update-scratch-x-of self)
               (make-array
                   (list (* 2 dim-x))
                   :element-type 'single-float 
                   :initial-element 0.0)))
       (ay (or (update-scratch-y-of self)
               (make-array
                   (list (* 2 dim-y))
                   :element-type 'single-float 
                   :initial-element 0.0)))
       (vp-theta-cache (cons (cos theta) (sin theta)))
       (ncol (array-total-size (fast-color-table-of self)))
       )
      
      (setf (vw::increment-of self) theta)
      
      (push
       (cons viewport (make-array (list max-x) :element-type 'integer :initial-element 0))
       (vp-lower-y-of self))
      
      (push
       (cons viewport 
             (make-array (list max-x) :element-type 'integer :initial-element 0))
       (vp-upper-y-of self))
      
      (push
       (cons viewport  depth-cue?)
       (vp-depth-cue?-of self))
      
      (push
       (cons viewport  hidden-lines?)
       (vp-hidden-lines?-of self))
      
      (push
       (cons viewport surface-fill?)
       (vp-surface-fill?-of self))
      
      (push
       (cons viewport  sx)
       (vp-x-of self))
      
      (push
       (cons viewport  sy)
       (vp-y-of self))
      
      (push
       (cons viewport  old-x)
       (vp-old-x-of self))
      
      (push
       (cons viewport  old-y)
       (vp-old-y-of self))
      
      (push
       (cons viewport  a)
       (vp-rotation-of self))
      
      (wb::spin-surface-x a (cos 4.5) (sin 4.5))
      (wb::spin-surface-z a (cos -1.3) (sin -1.3))
      
      (push
       (cons viewport
             (list (make-array (list 9)
                               :element-type 'single-float
                               :initial-contents
                               (loop for i from 0 to 8
                                     collect (aref a i)))))
       (vp-saved-rotations-of self))
      
      (setf (update-scratch-x-of self) ax)
      (setf (update-scratch-y-of self) ay)
      
      (push
       (cons viewport  vp-theta-cache)
       (vp-theta-cache-of self))
      
      (push
       (cons viewport col)
       (vp-col-pars-of self))
      
      (push
       (cons viewport szv)
       (vp-z-pos-pars-of self))
      
      (push
       (cons viewport
             (make-array (list 8) :element-type 'integer :initial-element 0))
       (vp-draw-line-of self))
      
      (push
       (cons viewport (make-array (list 1) :element-type 'integer :initial-element 0))
       (vp-num-lines-of self))
      
      (wb::update-surface-data (x-cache-of self)
                               (y-cache-of self)
                               (z-cache-of self)
                               a sx sy old-x old-y szv ax ay col
                               dim-x dim-y dim-z max-x max-y
                               (truncate (* (screen-range-of self)
                                            (sqrt 3)))
                               ncol)
      )
    )
  )



;;; Add do-nothing primary for delete-viewport FEB 04, 1998
(defmethod delete-viewport ((self surface-view) viewport)
   (declare (ignore self viewport))
   (call-next-method))

(defmethod delete-viewport :before ((self surface-view) viewport)
  
  ;; remove the viewport  VIEWPORT and corresponding  MAP and PARENT-VIEWPORT 
  
  (unless (not (member viewport (viewports-of self)))
    (let ((p (position viewport (viewports-of self))))
      
      (setf (vp-lower-y-of self)
            (delete (elt (vp-lower-y-of self) p)
                    (vp-lower-y-of self)))
      
      (setf (vp-upper-y-of self)
            (delete (elt (vp-upper-y-of self) p)
                    (vp-upper-y-of self)))
      
      
      (setf (vp-depth-cue?-of self)
            (delete (elt (vp-depth-cue?-of self) p)
                    (vp-depth-cue?-of self)))
      
      (setf (vp-hidden-lines?-of self)
            (delete (elt (vp-hidden-lines?-of self) p)
                    (vp-hidden-lines?-of self)))
      
      (setf (vp-surface-fill?-of self)
            (delete (elt (vp-surface-fill?-of self) p)
                    (vp-surface-fill?-of self)))
      
      (setf (vp-x-of self)
            (delete (elt (vp-x-of self) p)
                    (vp-x-of self)))
      
      (setf (vp-y-of self)
            (delete (elt (vp-y-of self) p)
                    (vp-y-of self)))
      
      (setf (vp-old-x-of self)
            (delete (elt (vp-old-x-of self) p)
                    (vp-old-x-of self)))
      
      (setf (vp-old-y-of self)
            (delete (elt (vp-old-y-of self) p)
                    (vp-old-y-of self)))
      
      (setf (vp-rotation-of self)
            (delete (elt (vp-rotation-of self) p)
                    (vp-rotation-of self)))
      
      (setf (vp-col-pars-of self)
            (delete (elt (vp-col-pars-of self) p)
                    (vp-col-pars-of self)))
      
      (setf (vp-z-pos-pars-of self)
            (delete (elt (vp-z-pos-pars-of self) p)
                    (vp-z-pos-pars-of self)))
      
      (setf (vp-draw-line-of self)
            (delete (elt (vp-draw-line-of self) p)
                    (vp-draw-line-of self)))
      
      (setf (vp-num-lines-of self)
            (delete (elt (vp-num-lines-of self) p)
                    (vp-num-lines-of self)))
      )
    )
  )

(defmethod draw-view ((self surface-view) &key viewport)
  (draw-surface self viewport))

(defmethod draw-surface ((self surface-view) viewport)
  (let*
    ((color-table (color-table-of self))
     (hide? (hidden-lines-p self viewport))
     (depth-cue? (depth-cue-p self viewport))
     (fill? (surface-fill-p self viewport))
     (canvas (window-of viewport))
     (sx (cdr (assoc viewport (vp-x-of self))))
     (sy (cdr (assoc viewport (vp-y-of self))))
     (old-x (cdr (assoc viewport (vp-old-x-of self))))
     (old-y (cdr (assoc viewport (vp-old-y-of self))))
     (cx (x-cache-of self))
     (cy (y-cache-of self))
     (cz (z-cache-of self))
     (szv (cdr (assoc viewport (vp-z-pos-pars-of self))))
     (col (cdr (assoc viewport (vp-col-pars-of self))))
     (ncol (number-of-colors-of self))
     (range (truncate (* (screen-range-of self)
                         (sqrt 3))))
     (colrange (/ ncol 2 range))
     (a (cdr (assoc viewport (vp-rotation-of self))))
     (ax (update-scratch-x-of self))
     (ay (update-scratch-y-of self))
     (dim-x (array-total-size cx))
     (dim-y (array-total-size cy))
     (dim-z (array-total-size cz))
     (lower-y (cdr (assoc viewport (vp-lower-y-of self))))
     (upper-y (cdr (assoc viewport (vp-upper-y-of self))))
     (max-x (width-of viewport))
     (max-y (height-of viewport))
     (dl (cdr (assoc viewport (vp-draw-line-of self))))
     (nl (cdr (assoc viewport (vp-num-lines-of self))))
     (y-origin (bottom-of viewport))
     (x-origin (left-of viewport))
     )
    (unless (= max-x
               (array-total-size lower-y)
               (array-total-size upper-y))
      (let ((vp-ly (assoc viewport (vp-lower-y-of self)))
            (vp-uy (assoc viewport (vp-upper-y-of self))))
        (setf (cdr vp-ly)
              (make-array (list max-x) :element-type 'integer :initial-element 0))
        (setf (cdr vp-uy)
              (make-array (list max-x) :element-type 'integer :initial-element 0))
        (setq lower-y (cdr vp-ly))
        (setq upper-y (cdr vp-uy))))
    
    (wb::update-surface-data cx
                             cy
                             cz
                             a sx sy old-x old-y szv ax ay col
                             dim-x dim-y dim-z max-x max-y
                             range
                             ncol)
    (cond
     (fill?
      (wb:surface-fill canvas
                       x-origin y-origin
                       sx sy cz
                       szv col
                       ncol
                       (* (aref a 8) colrange)
                       color-table)
      (when hide?
        (wb:hide-lines canvas
                       sx sy cz lower-y upper-y
                       dl nl
                       szv x-origin y-origin
                       max-x max-y
                       a col
                       ncol
                       (* (aref a 8) colrange)
                       NIL
                       color-table)
        ))
     (hide?
      (wb:hide-lines canvas
                     sx sy cz lower-y upper-y
                     dl nl
                     szv x-origin y-origin
                     max-x max-y
                     a col
                     ncol
                     (* (aref a 8) colrange)
                     depth-cue?
                     color-table)
      )
     (T
      (wb:show-lines canvas
                     x-origin y-origin
                     sx sy cz
                     szv col
                     ncol
                     (* (aref a 8) colrange)
                     depth-cue?
                     color-table))
     )
    ))

(defun depth-cue-menu-fn (surface-view)
  (let ((viewport (which-viewport surface-view)))
    (toggle-depth-cue surface-view viewport)
    (wb:canvas-clear 
     (window-of viewport) 
     :canvas-left (1+ (left-of viewport)) 
     :canvas-bottom (1+ (bottom-of viewport))
     :width (width-of viewport) 
     :height (height-of viewport))
    (draw-surface surface-view viewport)
    ))

(defun hidden-lines-menu-fn (surface-view)
  (let ((viewport (which-viewport surface-view)))
    (toggle-hidden-lines surface-view viewport)
    (wb:canvas-clear 
     (window-of viewport) 
     :canvas-left (1+ (left-of viewport)) 
     :canvas-bottom (1+ (bottom-of viewport))
     :width (width-of viewport) 
     :height (height-of viewport))
    (draw-surface surface-view viewport)
    ))

(defun surface-fill-menu-fn (surface-view)
  (let ((viewport (which-viewport surface-view)))
    (toggle-surface-fill surface-view viewport)
    (wb:canvas-clear 
     (window-of viewport) 
     :canvas-left (1+ (left-of viewport)) 
     :canvas-bottom (1+ (bottom-of viewport))
     :width (width-of viewport) 
     :height (height-of viewport))
    (draw-surface surface-view viewport)
    ))

;;;**** This is not the way to do things!  ... rwo
;;; Anyway add do-nothing primaries FEB 04, 1998

(defmethod get-menu-items ((self surface-view)
                           (slot-name (eql 'vw::middle-menu)))
   (declare (ignore self slot-name))
   (call-next-method))

(defmethod get-menu-items :around ((self surface-view) 
                                   (slot-name (eql 'vw::middle-menu)))
  `(("Highlight?" (toggle-select-view))
    ("Invisible?" (set-drawing-style :invisible? :toggle))
    ("Toggle depth cueing" (depth-cue-menu-fn))
    ("Toggle hidden lines" (hidden-lines-menu-fn))
    ("Toggle fill surface" (surface-fill-menu-fn))
    ("Choose new colors" (prompt-for-surface-info))
    ("Coordinate system"  nil ""
     :sub-items ((":screen" (set-spin-orientation :screen))
                 (":data" (set-spin-orientation :surface))))
    ))

(defmethod get-menu-items :around ((self surface-view) 
                                   (slot-name (eql 'vw::right-menu)))
  `(("Redraw" (draw-view :viewport :erase? t))
    ("Inspect" (inspect-view) "Inspect this view"
     :sub-items (("Inspect this view" (inspect-view))
                 ("Inspect the viewed object" (inspect-viewed-object))))
    ))


(defmethod prompt-for-surface-info ((self surface-view))
  (let*
    ((blue?-prompt  "Whole blue spectrum? Enter T or NIL")
     (blue-default "NIL")
     (red?-prompt  "Whole red spectrum? Enter T or NIL")
     (red-default "NIL")
     (green?-prompt  "Whole green spectrum? Enter T or NIL")
     (green-default "NIL")
     (ncol-prompt "Number of colors")
     (ncol-default "256")
     (items
      (list
       (cons blue?-prompt blue-default)
       (cons red?-prompt red-default)
       (cons green?-prompt green-default)
       (cons ncol-prompt ncol-default)))
     blue-val red-val green-val ncol
     new-info)
    (flet
      ((test-val-for-num (read-val)
         (if (and (symbolp read-val)
                  (not (boundp read-val)))
           :wrong
           (progn (setf read-val (eval read-val))
                  (if (not (numberp read-val))
                    :wrong
                    read-val))))
       (test-val-for-boole (read-val)
         (if (and (symbolp read-val)
                  (not (boundp read-val)))
           :wrong
           (if (eval read-val)
             T
             NIL))))
      (setf new-info
            (loop for pair in
                  (wb:collect-input
                   items
                   )
                  collect
                  (let ((read-val (read-from-string (cdr pair))))
                    (if (or
                         (string= (car pair) blue?-prompt)
                         (string= (car pair) red?-prompt)
                         (string= (car pair) green?-prompt)
                         )
                      (cons (car pair)
                            (do
                              ((ans
                                (test-val-for-boole read-val)
                                (test-val-for-boole
                                 (read-from-string (wb:prompt-user
                                                    :prompt-string
                                                    (concatenate
                                                     'string
                                                     (car pair)
                                                     " MUST BE T OR NIL")
                                                    :type 'string)))))
                              ((not (eq ans :wrong)) ans)))
                      (cons (car pair)
                            (do
                              ((ans
                                (test-val-for-num read-val)
                                (test-val-for-num
                                 (read-from-string  (wb:prompt-user
                                                     :prompt-string
                                                     (concatenate
                                                      'string
                                                      (car pair)
                                                      " MUST BE A NUMBER")
                                                     :type 'string)))))
                              ((not (eq ans :wrong)) ans))))))
            )
      
      (setf blue-val
            (cdr (assoc blue?-prompt
                        new-info :test #'string=)))
      
      
      (setf red-val
            (cdr (assoc red?-prompt
                        new-info :test #'string=)))
      
      (setf green-val
            (cdr (assoc green?-prompt
                        new-info :test #'string=)))
      
      (setf ncol
            (cdr (assoc ncol-prompt
                        new-info :test #'string=)))
      
      (setf (color-table-of self)
            (wb:make-color-table 
             :number ncol :red? red-val :green? green-val :blue? blue-val
             :fast-colors? NIL))
      (setf (fast-color-table-of self)
            (wb:make-color-table 
             :number ncol :red? red-val :green? green-val :blue? blue-val
             :fast-colors? T))
      
      )
    )
  )

