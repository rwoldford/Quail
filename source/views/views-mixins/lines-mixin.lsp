;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               lines-mixin.lisp
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
;;;     C.B. Hurley 1988-1992 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( basic-lines-mixin 
            single-style-lines-mixin multi-style-lines-mixin
            lines-mixin draw-lines lines-coords-for-viewport )))



(defgeneric compute-lines-coords-for-viewport (self  vp ))
  
  

;; the mixin viewport-coords-cache-mixin added on 4-20-95. If this
;; is removed also remove other changes below: see ***

(defclass basic-lines-mixin (viewport-coords-cache-mixin linkable-mixin)
  ((middle-menu :allocation :class :initform nil)
   (style-keys :initform '(:dashing :width :color) :allocation :class)
   (bounding-region 
    :initarg  :plot-region
    :initform nil 
    :accessor bounding-region-of 
    :documentation "A region in the local coordinates of view used to define its bounds")
   (clip-draw? :initform t :initarg :clip-draw? :accessor clip-draw-p)
   (lines-coords :initarg :lines-coords
                 :initform nil
                 :accessor lines-coords-of))
  (:default-initargs :dashing nil :width 1 :color *default-curve-color* :linkable? nil
    :viewport-compute-method #'compute-lines-coords-for-viewport))


(defclass multi-style-lines-mixin (multiple-draw-style-mixin basic-lines-mixin) ())

(defclass lines-mixin ( basic-lines-mixin) ())  ;; the default mixin



(defgeneric lines-coords-for-viewport (lines-mixin viewport)
 (:documentation "Returns the viewport coordinates of line segments~
                   as a list of cons pairs"))

(defgeneric draw-lines (lines-mixin viewport &key &allow-other-keys)
 (:documentation "Draws the connected line segments in viewport"))

;;------------------------------------------------------------------------------------





;; *** this method needed for added mixin viewport-coords-cache-mixin
(defmethod compute-lines-coords-for-viewport ((self basic-lines-mixin)  vp )
  
  (let ( 
         (c-list  (lines-coords-of self))
         (map (select-map-to-viewport self vp) )
         ox oy sx sy)
    
    (setq ox (x-shift map) sx (x-scale map)
          oy (y-shift map) sy (y-scale map))
    
    (loop for c in c-list 
          collect (if (and (numberp  (car c)) (numberp  (cadr c)) )
                    (cons (truncate (+ ox (* sx (car c))))
                        (truncate (+ oy (* sy (cadr c)))))))))

;; *** this method needed for added mixin viewport-coords-cache-mixin
(defmethod (setf lines-coords-of) :after (new (self  basic-lines-mixin))
  (declare (ignore new))
  (remove-viewport-coords self))


;; *** this method needed for added mixin viewport-coords-cache-mixin
(defmethod lines-coords-for-viewport ((self basic-lines-mixin)  vp )
  
  (viewport-coords-of self :viewport vp))




(defmethod erase-view ((self basic-lines-mixin) 
                       &key viewport delta-width )

  (unless delta-width
    (setq delta-width (if (draw-style self :highlight?)
                      (highlight-delta self)
                      (erase-delta self))))
  
  (with-exposed-viewports self viewport vp
    (draw-lines self vp :operation :boole-andc1 :delta-width delta-width)))

(defmethod invert-view ((self basic-lines-mixin) 
                        &key viewport (delta-width 1))
  (with-exposed-viewports self viewport vp
    (draw-lines self vp :operation :boole-xor :delta-width delta-width)))

(defmethod draw-view ((self basic-lines-mixin) &key viewport)
 
  (with-exposed-viewports self viewport vp
    (draw-lines  self vp)))

(defmethod erase-delta ((self basic-lines-mixin))
  0)

(defmethod downlight-view ((self basic-lines-mixin)
                           &key viewport operation) 
  (setq operation (or operation (highlight-operation self)))
  (if (eql operation :boole-xor)
    (highlight-view self :viewport viewport :operation operation)
    (progn
      (erase-view self :viewport viewport :delta-width (highlight-delta self))
      (unless (draw-style self :invisible?)
        (draw-view self :viewport viewport :erase? nil)))))

(defmethod highlight-delta ((self basic-lines-mixin))
  0)

(defmethod highlight-operation ((self basic-lines-mixin))
  :default)

(defun window-draw-line (x y x1 y1  bw wb-region clip? color operation width)
  (declare (ignore y))
  (wb:with-pen-values  bw color width operation
  (cond ((and x x1)
           (if clip? 
             (wb:clipped-draw-to  bw  x1 y1 wb-region)
             (wb:canvas-draw-to  bw  x1 y1)))
          (x1
           (wb:canvas-move-to   bw  x1 y1))
          (t nil))))



(defmethod simple-draw-lines ((self basic-lines-mixin)  vp &key (operation :default) 
                              (delta-width 0)
                              (width (+ (draw-style self :width) delta-width))
                              (color (draw-style self :color)))
  (let ((coords (lines-coords-for-viewport self vp))
        (bw (window-of vp))
        (clip? (clip-draw-p self))
        (wb-region (wb-region vp))
        ;;(dashing (draw-style self :dashing))
        )
    (when coords
       (wb:with-pen-values  bw color width operation
        (if clip?
      (loop for (x . y) in (cons nil coords)
            for (x1 . y1) in coords do
             (cond ((and x x1)
                  (wb:clipped-draw-to  bw  x1 y1 wb-region))
                  (x1 (wb:canvas-move-to   bw  x1 y1))
                  (t nil)))

    (loop for (x . y) in (cons nil coords)
            for (x1 . y1) in coords do 
             (cond ((and x x1)
          (wb:canvas-draw-to  bw  x1 y1))
          (x1   (wb:canvas-move-to   bw  x1 y1))
          (t nil))))))))





(defmethod is-brushed-p((self basic-lines-mixin) (brush brush) viewport)
  (or (contains-p brush viewport)
      (let* ((a2 (brush-x brush))
             (b1 (brush-y brush))
             (a1 (- a2 (brush-width brush)))
             (b2 (+ b1 (brush-height brush)))
             (coords (lines-coords-for-viewport self viewport)))
        (loop for (x1 . y1) in coords
              for (x2 . y2) in (cdr coords)
               thereis (and x1 x2 
                           (or (line-segment-intersectp x1 y1 x2 y2 a1 b1 a1 b2)
                          (line-segment-intersectp x1 y1 x2 y2  a1 b2 a2 b2)
                          (line-segment-intersectp x1 y1 x2 y2   a2 b2 a2 b1)
                          (line-segment-intersectp x1 y1 x2 y2   a2 b1 a1 b1)))))))


(defmethod is-brushed-p((self basic-lines-mixin) (brush angled-brush) viewport)
  (if (zerop (brush-angle self))
    (call-next-method)
    (or (contains-p brush viewport)
    (destructuring-bind (wc ws hc hs c s)
                        (brush-cache-pars brush)
      (declare (ignore c s))
    (let* ((x (brush-x self)) (y (brush-y self))
           (x3  (- x wc)) (y3  (- y ws))
           (x1 (- x hs)) (y1 (+ y hc))
           (x2 (- x1 wc))  (y2 (- y1 ws))
         (coords (lines-coords-for-viewport self viewport)))
     (if (and coords (caar coords))
       (loop for (a1 . b1) in coords
                for (a2 . b2) in (cdr coords)
                thereis (and a1 a2 
                             (or (line-segment-intersectp a1 b1 a2 b2  x y x1 y1)
                            (line-segment-intersectp a1 b1 a2 b2 x2 y2 x1 y1)
                            (line-segment-intersectp a1 b1 a2 b2  x2 y2 x3 y3)
                            (line-segment-intersectp a1 b1 a2 b2  x y x3 y3))))))))))
     
;;----------------------------------------------------------------------


(defun line-default-middle-items ()
  (let ((size-change-list 
         '(("fatter" (set-drawing-style :width :fatter :highlit?  *selected-subviews-only*))
           ("thinner" (set-drawing-style :width :thinner :highlit?  *selected-subviews-only*))
           ("prompt" (set-drawing-style :width :prompt :highlit?  *selected-subviews-only*)))))
    (list `( "Width" nil "" :sub-items ,size-change-list))))

(defmethod style-menu-items ((self basic-lines-mixin) )
  (line-default-middle-items))




;;----------------------------------------------------------------------




(defmethod draw-lines ((self lines-mixin)  viewport &key (operation :default) 
                       (delta-width 0)
                       (width (+ (draw-style self :width) delta-width))
                       (color (draw-style self :color)))
  (simple-draw-lines self viewport :operation operation 
                     :delta-width :delta-width 
                     :width width :color color))


(defmethod highlight-view ((self lines-mixin)
                           &key viewport (delta-width (highlight-delta self)))
  (with-exposed-viewports self viewport 
    vp
    (draw-lines self vp :delta-width delta-width :color *default-highlight-color*
                :operation (highlight-operation self))))



    

(defmethod draw-lines ((self multi-style-lines-mixin)  vp 
                       &key (operation :default) (delta-width 0 ))
  
  (let* ((styles (drawing-styles-of self))
         (coords (lines-coords-for-viewport self vp))
         (bw (window-of vp))
         (clip? (clip-draw-p self))
         (wb-region (wb-region vp))
         (alpha (/ 1 (max 1 (length styles))))
         (n (length coords))
         )
    
    (when coords
      (if (and (>= (length styles) n) (not (= n 2)))
        (loop 
          for (x . y) in (cons nil coords)
          for (x1 y1) in coords
          for ds in (cons nil styles)
           do (if (and ds (draw-style ds :invisible))
                (setq x nil))
            (window-draw-line x y x1 y1 bw wb-region clip?
                              (draw-style ds :color)  
                              operation (+ delta-width (draw-style ds :width ) )))
        (loop with dx and dy
          for (x . y) in coords
          for (x1 . y1) in (cdr coords )
          when (and x x1) do
          (wb:canvas-move-to bw x y)
          (setq dx  (- x1 x)  dy  (- y1 y))
           (loop for ds in styles
                do
                 (window-draw-line (and (not (draw-style ds :invisible? )) x) y 
                                   (incf x (* alpha dx)) (incf y (* alpha dy)) 
                                   bw wb-region clip?
                                    (draw-style ds :color)  
                                    operation (+ delta-width (draw-style ds :width )))))))) )





(defmethod highlight-view ((self multi-style-lines-mixin)
                           &key viewport (delta-width (highlight-delta self)))
  (let* ((clip? (clip-draw-p self))
        (styles (drawing-styles-of self))
        (alpha (/ 1 (max 1 (length styles))))
        (operation (highlight-operation self)))
    (with-exposed-viewports self viewport 
      vp
      (let* (
             (coords (lines-coords-for-viewport self vp))
             (bw (window-of vp))
             (wb-region (wb-region vp))
             (n (length coords)))

       (when coords
         (if (and (>= (length styles) n) (not (= n 2)))
            (loop 
              for (x . y) in (cons nil coords)
          for (x1 y1) in coords
          for ds in (cons nil styles)
          do (if (and ds (not (draw-style ds :highlight? )))
                (setq x nil))
               (window-draw-line x y x1 y1 bw wb-region clip?
                                  *default-highlight-color*  
                                  operation (+ delta-width  (draw-style ds :width ))))
            (loop with dx and dy
              for (x . y) in coords 
              for (x1 . y1) in (cdr coords )
              when (and x x1) do
          (wb:canvas-move-to bw x y)
            (setq dx  (- x1 x)  dy  (- y1 y))   
              (loop for ds in styles
                    do
                    (window-draw-line (and (draw-style ds :highlight? ) x) y 
                                   (incf x (* alpha dx)) (incf y (* alpha dy)) 
                                   bw wb-region clip?
                                    *default-highlight-color*   
                                    operation (+ delta-width (draw-style ds :width )))))))))))




(defmethod use-x-axis-p ((self basic-lines-mixin))
  t)

(defmethod use-y-axis-p ((self basic-lines-mixin))
  t)


(defmethod distance-to-location ((self basic-lines-mixin) viewport location)
  ;; computes squared euclidean distance
  (let* ((coords (lines-coords-for-viewport self viewport))
           (locn (if (region-p location)
                   (centre-of location) location))
           (x (2d-position-x locn))
           (y (2d-position-y locn))
           )
      (if coords
        (sqrt
         (labels ((d2 (x1 y1 x2 y2)
                   (+ (expt ( - x1 x2) 2) (expt ( - y1 y2) 2)))
                 (dt (x1 y1 x2  y2 x y)
                   (let ((d12 (+ (* (- x1 x) (- x2 x)) (* (- y1 y) (- y2 y)))))
                     (and (< d12 (d2 x1 y1 x y)) (< d12 (d2 x2 y2 x y)))))
                 (dp (x1 y1 x2  y2 x y)
                   (/ (abs (+ (* x (- y2 y1)) (* y (- x1 x2)) ( * x2 y1) (* y2 (- x1)))) 
                      (sqrt   (d2 x1 y1 x2 y2)))))
          (loop for (x1 . y1) in coords
                for (x2 . y2) in (cdr coords)
                 when (and x1 x2)
                minimize
                
                
                
                (cond
                 ((and (= x2 x1) (= y2 y1))
                  (d2 x y x1 y1))
                 ((dt x1 y1 x2  y2 x y) 
                  (expt (dp x1 y1 x2 y2 x y) 2))
                 (t (min (d2 x y x1 y1) (d2 x y x2 y2)))))))
        10000)))


          
(defmethod selected-p ((self basic-lines-mixin) viewport location)
  ;; is view selected with mouse at location?
  ;; if the viewport has
  
  (and (active-viewport-p viewport) 
      ;; (or (contains-p viewport location)
        (< (distance-to-location self viewport location) 10)))
        
        