;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               image.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(2d-image set-image-colour-map set-highlight-colour-map set-image-colour-values
           image-plot lasso)))


(defclass 2d-image (one-per-case-mixin 3d-view compound-view)
  ((dimensions :initform NIL :initarg :dimensions :reader dimensions-of
               :documentation "Dimensions of the image matrix")
   (image-matrix :initform nil :documentation "Image matrix of the imagels")
   (image-colour-map :initform nil :reader image-colour-map-of)
   (highlight-colour-map :initform nil :reader highlight-colour-map-of)
   )
  (:documentation "The 2-dimensional image class.")
  (:default-initargs :initform-fn #'get-data-inits-3
    :case-view 'imagel))

(defmethod dimensions-of ((self 2d-image))  ;should be a better way of doing this
  (or (slot-value self 'dimensions)
      (setf (slot-value self 'dimensions)
            `(,(+ (- (apply #'max (x-coords-of self))
                     (apply #'min (x-coords-of self))) 1)
              ,(+ (- (apply #'max (y-coords-of self))
                     (apply #'min (y-coords-of self))) 1)
              ))))
#|
(defmethod sort-image-subviews ((self 2d-image)) 
  ;first by x or column and secondly by y or row
  (setf (subviews-of self) 
        (sort (subviews-of self) 
              #'(lambda(a b) (cond ((= (value-of (viewed-object-of a) 0)
                                       (value-of (viewed-object-of b) 0))
                                    (<= (value-of (viewed-object-of a) 1) 
                                        (value-of (viewed-object-of b) 1)))
                                   (t
                                    (<= (value-of (viewed-object-of a) 0) 
                                        (value-of (viewed-object-of b) 0)))))
              )))
|#

(defmethod image-matrix-of ((self 2d-image))
  (or (slot-value self 'image-matrix)
      (let* ((max-col (apply #'max (x-coords-of self)))
             (min-col (apply #'min (x-coords-of self)))
             (max-row (apply #'max (y-coords-of self)))
             (min-row (apply #'min (y-coords-of self)))
             ;(dim (dimensions-of self)) ;want matrix column row to match plot coordinates
             (mat (make-array (list (+ max-row 1) (+ max-col 1)) :initial-element nil))
             )
        (loop for col from min-col to max-col do
              (loop for row from min-row to max-row do
                    (loop for sv in (subviews-of self)
                          for sv-x = (q::eref (case-data-of (viewed-object-of sv)) 0)
                          for sv-y = (q::eref (case-data-of (viewed-object-of sv)) 1) 
                          do
                          (when (and (= sv-x col) (= sv-y row))
                            (setf (q::eref mat col row) sv)))))
        (setf (slot-value self 'image-matrix) mat)
        )))

(defmethod new-variable :after  ((self 2d-image) &key)
  (let* ((image-values (z-coords-of self))
         (max-image-value (apply #'max image-values))
         (min-image-value (apply #'min image-values)))
    (setf (q::eref (image-colour-map-of self) 2 1) 
          (cons min-image-value max-image-value))
    (setf (q::eref (highlight-colour-map-of self) 2 1) 
          (cons min-image-value max-image-value))
    (set-image-colour-values self)
    ;need to change image-plot title as well
    ))

(defmethod initialize-instance :after ((self 2d-image) &key)
  (declare (special *default-highlight-color* wb::*default-canvas-pen-color*))
  (let* ((image-values (z-coords-of self))
         (max-image-value (apply #'max image-values))
         (min-image-value (apply #'min image-values))
         (image-colour-map  
          (make-array '(3 3)
                      :initial-contents        
                      (list '(nil 0.05 0.95) 
                            `(,(wb::hue-of wb::*default-canvas-pen-color*) nil nil)
                            `(1 ,(cons min-image-value max-image-value) :lightness))))
         (highlight-colour-map 
          (make-array '(3 3)
                      :initial-contents 
                      (list '(nil 0.05 0.95) 
                            `(,(wb::hue-of *default-highlight-color*) nil nil)
                            `(1 ,(cons min-image-value max-image-value) :lightness))))
         )
    (setf (slot-value self 'image-colour-map) image-colour-map)
    (setf (slot-value self 'highlight-colour-map) highlight-colour-map)
    (set-image-colour-values self)
    (image-matrix-of self) ;put loading time when 2d-image is first initialized
    ;(sort-image-subviews)
    ))

(defmethod set-image-colour-values ((self 2d-image))  
  ;to be modified &key image-values (or image-values (z-coords-of self))
  (map NIL #'(lambda (thing val) 
               (progn 
                 (set-draw-style (drawing-style-of thing) 
                                 :color 
                                 (map-with-lhs-matrix val 
                                                      (image-colour-map-of self)))
                 (set-draw-style (drawing-style-of thing) 
                                 :highlight-color 
                                 (map-with-lhs-matrix val 
                                                      (highlight-colour-map-of self)))
                 ))
       (subviews-of self) (z-coords-of self))
  )

(defmethod set-image-colour-map ((self 2d-image) &key map)
  (if map
    (setf (image-colour-map-of self) map)
    (user-pick-hue-light-range self 'image-colour-map-of
                               :prompt-user-string "Pick Range of Image Colours"))
  )

(defmethod set-highlight-colour-map ((self 2d-image) &key map)
  (if map
    (setf (highlight-colour-map-of self) map)
    (user-pick-hue-light-range self 'highlight-colour-map-of 
                               :prompt-user-string "Pick Range of Highlight Image Colours"))
  )

(defmethod (setf image-colour-map-of) (new-map (self 2d-image))
  (let ((old-map (image-colour-map-of self))
        )
    (if (not (equal old-map new-map))
      (progn 
        (loop for sv in (subviews-of self) 
              for image-value = (elt (z-coords-of self 
                                                  :cases (list (viewed-object-of sv))) 0) 
              do
              (set-draw-style (drawing-style-of sv) 
                              :color (map-with-lhs-matrix image-value new-map)))
        (setf (slot-value self 'image-colour-map) new-map)
        (loop for vp in (viewports-of self) do
              (draw-view self :viewport vp))))))


(defmethod (setf highlight-colour-map-of) (new-map (self 2d-image))
  (let ((old-map (highlight-colour-map-of self))
        )
    (if (not (equal old-map new-map))
      (progn
        (loop for sv in (subviews-of self)
              for image-value = (elt (z-coords-of self 
                                                  :cases (list (viewed-object-of sv))) 0) 
              do
              (set-draw-style (drawing-style-of sv) 
                              :highlight-color (map-with-lhs-matrix image-value new-map)))
        (setf (slot-value self 'highlight-colour-map) new-map)
        (loop for vp in (viewports-of self) do
              (draw-view self :viewport vp))))))

(defconstant *image-margin-percent* 0.05)

(defmethod draw-view :before ((self 2d-image) &key viewport)
  ;;(setq foo self vp viewport) 
  (let* ((image-margin-width (truncate (* *image-margin-percent* (width-of viewport))))
         (image-margin-height (truncate (* *image-margin-percent* (height-of viewport))))
         (vp-width (- (width-of viewport) (* 2 image-margin-width)))
         (vp-height (- (height-of viewport) (* 2 image-margin-height)))
         (image-width (car (dimensions-of self)))
         (image-height (cadr (dimensions-of self)))
         (width (truncate vp-width image-width))
         (height (truncate vp-height image-height))
         )
    (loop for sb in (subviews-of self) do
          (set-rectangle-width sb width)
          (set-rectangle-height sb height))
    ))

(defmethod subview-location-string ((self 2d-image) (subview imagel))
  (let ((c (butlast
            (car (coords-of self :cases (list (viewed-object-of subview)))))))
    (if c (format nil "(~{~A~^,~})" c))))


(defmethod get-draw-portion ((self 2d-image) viewport)
  (let* ((image-margin-width (truncate (* *image-margin-percent* (width-of viewport))))
         (image-margin-height (truncate (* *image-margin-percent* (height-of viewport))))
         (vp-width (- (width-of viewport) (* 2 image-margin-width)))
         (vp-height (- (height-of viewport) (* 2 image-margin-height)))
         (image-width (car (dimensions-of self)))
         (image-height (cadr (dimensions-of self)))
         (width (truncate vp-width image-width)) 
         (height (truncate vp-height image-height)))
    
    (make-viewport (window-of viewport) 
                   (+ (left-of viewport) image-margin-width) 
                   ;try to get rid of truncation lines
                   (+ (left-of viewport) (* width image-width))
                   (+ (bottom-of viewport) image-margin-height)
                   (+ (bottom-of viewport) (* height image-height)))))

;;if i got rid of this method, the middle-menu would pick up a lot more
(defmethod get-menu-items :around ((self 2d-image) (slot-name (eql 'middle-menu)))
  (let* (
         ;(bounds (bounds-menu-item-list self))
         (vm (var-menu-items self))
         (var-menu-items (if vm
                           (append 
                            vm
                            `(("-" nil)
                              ("LinkVars?" (set-link-vars-p :toggle))))
                           `(("LinkVars?" (set-link-vars-p :toggle)))))
         )
    `(( "Toggle hi" (toggle-highlight ))
      ;or should I rename this hue somehow
      ( "Color" nil "" :sub-items ,(color-menu-items))  
      ( "Invisible?" (set-invisible-subviews t :highlit? *selected-subviews-only* ))
      ( "AllVisible?" (set-invisible-subviews nil :highlit? nil))
      ("-" nil)
      ;( "Bounds" nil "" :sub-items ,bounds)
      ;("-" nil)
      ("Variables" nil "" :sub-items ,var-menu-items) ;not sure about this one
      ("Flip-x" (set-flip-x-p :toggle :draw? t))
      ("Flip-y" (set-flip-y-p :toggle :draw? t))
      )))

(defmethod image-plot ((self 2d-image)
                       &rest keyword-args
                       &key
                       (draw? NIL)
                       (viewport NIL)
                       (title (dataset-name self))
                       &allow-other-keys)
  ;;; displays the image analysis workplace
  (let* ((vp (if viewport viewport (viewport-of self)))
         (image-colour-button 
          (rounded-button :viewed-object self
                          :text "Image Colour"
                          :button-color wb::*gray-colour*
                          :left-fn
                          #'(lambda()
                              (set-image-colour-map self))))
         (highlight-colour-button 
          (rounded-button :viewed-object self
                          :text "Highlight Colour"
                          :button-color wb::*gray-colour*
                          :left-fn
                          #'(lambda()
                              (set-highlight-colour-map self))))
         (data-subset-button 
          (rounded-button :viewed-object self
                          :text "Data Subset"
                          :button-color wb::*gray-colour*
                          :left-fn
                          #'(lambda()
                              (let* ((case-list (loop for sv in (selected-views) collect
                                                      (viewed-object-of sv))))
                                (wb::save-value (make-dataset nil
                                                              :cases case-list
                                                              :variates (variates-of self)
                                                              :save? t))))))
         (deselect-all-button 
          (rounded-button :viewed-object self
                          :text "Deselect All"
                          :button-color wb::*gray-colour*
                          :left-fn
                          #'(lambda()
                              (deselect-view self))))
         
         (lasso-button 
          (rounded-button :viewed-object self
                          :text "Lasso"
                          :button-color wb::*gray-colour*
                          :left-fn
                          #'(lambda()
                              (deselect-view self)
                              (lasso self))))
         
         (button-list (list image-colour-button highlight-colour-button data-subset-button 
                            deselect-all-button lasso-button))
         ;        (image-title (text-view :text (format NIL "~A: ~A" (or title "Image") 
         ;                                              (z-variate-of self)) :draw? nil))
         (image-title (text-view :text (format NIL "~A" (or title "Image")) :draw? nil))
         image-view-layout button-layout)
    
    (defmethod shift-left-button-fn ((thing (eql lasso-button)) &key viewport)
      (declare (ignore viewport))
      (control-start thing)
      (lasso self)
      (control-done thing))
    
    (setf button-layout (apply #'grid-layout :ncols 1
                               :subviews button-list
                               :box-views? NIL :gap-x .05 :gap-y .05
                               :viewed-object self
                               :draw? NIL
                               keyword-args))
    (setf image-view-layout 
          (apply #'view-layout :viewed-object self
                 :subviews (list image-title self button-layout)
                 :positions '((0 100 100 110) (0 100 0 100) (100 130 0 100))
                 :draw? nil
                 keyword-args))
    (if draw?
      (draw-view image-view-layout :viewport vp))))

(defun lasso (self)
  (flet ((inside-lasso (sub lasso-list)
           (let ((sub-x (qk::eref sub 0))
                 (sub-y (qk::eref sub 1))
                 (left nil)
                 (right nil)
                 (top nil) 
                 (bottom nil))
             (loop for lasso-subs in lasso-list
                   for lasso-x = (qk::eref lasso-subs 0)
                   for lasso-y = (qk::eref lasso-subs 1) do
                   
                   (if (not (find sub lasso-list))
                     (progn
                       ;check left
                       (when (= sub-x lasso-x) ;same column
                         (progn
                           (if (< lasso-y sub-y) (setf top t))
                           (if (> lasso-y sub-y) (setf bottom t))))
                       (when (= sub-y lasso-y) ;same row
                         (progn
                           (if (< lasso-x sub-x) (setf left t))
                           (if (> lasso-x sub-x) (setf right t)))))
                     ))
             (and left right bottom top))))
    
    (let (lasso)
      (loop until (not (wb::mouse-down-p)))
      (loop until (wb::mouse-down-p))
      (when (wb::mouse-down-p)
        (let* ((win (window-of (viewport-of self)))
               (mouse-pos (wb::mouse-position win))
               (x (wb::position-x mouse-pos))
               (y (wb::position-y mouse-pos))
               (xy (vector x y)))
          (do ((i 1 (+ i 1)))
              ((not (wb::mouse-down-p)))
            (loop for sv in (subviews-of self) 
                  for vp = (viewport-of sv) 
                  for vo = (viewed-object-of sv) do
                  (if (contains-p vp xy)
                    (progn (select-view sv) ;difference between toggle-select-view
                           (if lasso 
                             (if (not (member vo lasso)) (push vo lasso))
                             (setf lasso (list vo))))) )
            (setf mouse-pos (wb::mouse-position win))
            (setf x (wb::position-x mouse-pos))
            (setf y (wb::position-y mouse-pos))
            (setf xy (vector x y)))
          ))
      (setf lasso (coords-of self :cases lasso))
      (let ((max-x (- (apply #'max (mapcar #'(lambda(x) (qk::eref x 0)) lasso)) 1))
            (min-x (+ (apply #'min (mapcar #'(lambda(x) (qk::eref x 0)) lasso)) 1))
            (max-y (- (apply #'max (mapcar #'(lambda(x) (qk::eref x 1)) lasso)) 1))
            (min-y (+ (apply #'min (mapcar #'(lambda(x) (qk::eref x 1)) lasso)) 1)))
        (loop for i from min-x to max-x do
              (loop for j from min-y to max-y
                    for sv = (qk::eref (image-matrix-of self) i j)
                    for vo = (elt (coords-of self :cases (list (viewed-object-of sv))) 0)
                    do
                    (when (inside-lasso vo lasso)
                      (select-view sv)))))
      ))
  )

#|
(defun thresholding (self)
  (let* ((image-value-range (q::eref (image-colour-map-of self) 2 1))
         (low (car image-value-range))
         (high (cdr image-value-range))
         (lower (wb::prompt-user :prompt-string (format NIL "Enter lower end of range:")
                                 :type 'number :read-type :eval
                                 :initial-string (format NIL "~s" low)))
         (upper (wb::prompt-user :prompt-string (format NIL "Enter higher end of range:")
                                 :type 'number :read-type :eval
                                 :initial-string (format NIL "~s" high)))
         )
    (loop for sv in (subviews-of self)
          for val = (image-value-of sv) do
          (when (and (<= lower val) (>= upper val))
            (select-view sv )))))
|#
;these remaining 5 method should perhaps not be 2d-image methods, but
;they will be for now - nw
(defmethod  get-subview-width  ((self 2d-image) new)
  (let* ((sub (car (subviews-of-type self 'fixed-size-rectangle)))
         (old (rectangle-width-of sub))
        (inc (rectangle-width-increment sub)))
    (case new
      (:larger (+  old inc)) 
      (:smaller (-  old inc))
      (:prompt (wb:prompt-user :type 'number 
                               :read-type :eval
                               :prompt-string 
                               (format nil "Change width from ~A" old)))
      (t old))))

(defmethod  get-subview-height  ((self 2d-image) new)
  (let* ((sub (car (subviews-of-type self 'fixed-size-rectangle)))
        (old (rectangle-height-of sub))
        (inc (rectangle-height-increment sub)))
   (case new
      (:larger (+  old inc)) 
      (:smaller (-  old inc))
      (:prompt (wb:prompt-user :type 'number 
                               :read-type :eval
                               :prompt-string 
                               (format nil "Change height from ~A" old)))
      (t old))))

(defmethod set-subview-size :around ((self 2d-image) new &key (draw? t) highlit?)
  (if (car (subviews-of-type self 'fixed-size-rectangle))
    (progn 
      (set-subview-width self new draw? highlit?)
      (set-subview-height self new draw? highlit?))
    (call-next-method)))

(defmethod set-subview-width ((self 2d-image) new &key (draw? t) highlit?)
  (let ((height (rectangle-height-of (car (subviews-of-type self 'fixed-size-rectangle)))))
    (unless (numberp new)
      (setq new (get-subview-width self new)))
    (set-draw-style self :width new)
    (loop for v in (some-subviews self :highlit? highlit?) do 
          (if (typep v 'fixed-size-rectangle) 
            (set-rectangle-width v new :draw? draw?)
            (loop for vp in (viewports-of v) 
                  for vp-copy = (copy-region vp)
                  do
                  (set-viewport-width-height vp-copy (+ 2 new) height)
                  (reshape-viewport v vp :new-location 
                                    vp-copy :draw? nil))))
    (init-position-subviews self)
    (if draw?
      (progn (erase-view self :viewport (car (viewports-of self)))
             (draw-view self :viewport (car (viewports-of self)))))
    ))

(defmethod set-subview-height ((self 2d-image) new &key (draw? t) highlit?)
  (let ((width (rectangle-width-of (car (subviews-of-type self 'fixed-size-rectangle)))))
    (unless (numberp new)
      (setq new (get-subview-height self new)))
    (set-draw-style self :height new)
    (loop for v in (some-subviews self :highlit? highlit?) do 
          (if (typep v 'fixed-size-rectangle) 
            (set-rectangle-height v new :draw? draw?)
            (loop for vp in (viewports-of v) 
                  for vp-copy = (copy-region vp)
                  do
                  (set-viewport-width-height vp-copy width (+ 2 new))
                  (reshape-viewport v vp :new-location 
                                    vp-copy :draw? nil))))
    (init-position-subviews self)
    (if draw?
      (progn (erase-view self :viewport (car (viewports-of self)))
             (draw-view self :viewport (car (viewports-of self)))))
    ))

