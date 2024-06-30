;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               clip.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1991
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(clip-code clipped-bitblt clipped-bltshade clipped-draw-line
           clipped-draw-between clipped-draw-to clipped-relative-draw-to
           clipped-plot-at clipped-draw-string
           )))


;; Clipped display functions


(defun clip-code (x y left right top bottom)
       
  ;; Cohen-Sutherland clip codes.  Assumes integer args RIGHT and
  ;; TOP are one past the region.
  (let ((abovebit (if (> y top) 8 0))
	(belowbit (if (> bottom y) 4 0))
	(rightbit (if (> x right) 2 0))
	(leftbit (if (> left x) 1 0)))
    (logior abovebit belowbit rightbit leftbit)))

(defun clipped-bitblt (canvas bitmap clipping-region
		       &key
		       (bitmap-left 0)
		       (bitmap-bottom 0)
		       (canvas-left 0)
		       (canvas-bottom 0)
		       operation
		       (width (bitmap-width bitmap))
		       (height (bitmap-height bitmap)))
       
  ;; Assume that source must be a window or a bitmap -- and hence
  ;; has scale 1 Process defaults 
  (let* ((clip-left (region-left clipping-region))
	 (clip-bottom (region-bottom clipping-region))
	 (clip-width (region-width clipping-region))
	 (clip-height (region-height clipping-region))
	 (new-left canvas-left)
	 (new-bottom canvas-bottom)
	 (new-width width)
	 (new-height height)
	 (clipped-p nil))
    (when (> clip-left new-left)
      (setq clipped-p t)
      (setq new-width (- new-width (- clip-left new-left)))
      (setq new-left clip-left))
    (when (> clip-bottom new-bottom)
      (setq clipped-p t)
      (setq new-height (- new-height (- clip-bottom new-bottom)))
      (setq new-bottom clip-bottom))
    (when (> (+ new-left new-width)
	     (+ clip-left clip-width))
      (setq clipped-p t)
      (setq new-width (- (+ clip-left clip-width) new-left)))
    (when (> (+ new-bottom new-height)
	     (+ clip-bottom clip-height))
      (setq clipped-p t)
      (setq new-height (- (+ clip-bottom clip-height) new-bottom)))
    (cond ((null clipped-p)
	   ;; No clipping
	   (canvas-bitblt canvas bitmap :bitmap-left 
			  bitmap-left :bitmap-bottom bitmap-bottom 
			  :canvas-left canvas-left :canvas-bottom 
			  canvas-bottom :width width :height height 
			  :operation operation))
	  ((or (<= new-width 0)
	       (<= new-height 0))
                    
	   ;; Gross clipping
	   nil)
	  (t 
	   ;; Adjusted bitblt
	   (canvas-bitblt canvas bitmap
			  :bitmap-left
			  (+ bitmap-left (- new-left canvas-left))
			  :bitmap-bottom
			  (+ bitmap-bottom (- new-bottom canvas-bottom))
			  :canvas-left new-left :canvas-bottom 
			  new-bottom :width new-width :height 
			  new-height :operation operation)))))

(defun clipped-bltshade (canvas texture clipping-region &key
                               (canvas-left 0)
                               (canvas-bottom 0)
                               (width (canvas-width canvas))
                               (height (canvas-height canvas))
                               operation)

       (let ((clip-left (region-left clipping-region))
             (clip-bottom (region-bottom clipping-region))
             (clip-width (region-width clipping-region))
             (clip-height (region-height clipping-region))
             (new-left canvas-left)
             (new-bottom canvas-bottom)
             (new-width width)
             (new-height height))
            (when (> clip-left new-left)
                (setq new-width (- new-width (- clip-left new-left)))
                (setq new-left clip-left))
            (when (> clip-bottom new-bottom)
                (setq new-height (- new-height (- clip-bottom new-bottom)))
                (setq new-bottom clip-bottom))
            (if (> (+ new-left new-width)
                   (+ clip-left clip-width))
                (setq new-width (- (+ clip-left clip-width) new-left)))
            (if (> (+ new-bottom new-height)
                   (+ clip-bottom clip-height))
                (setq new-height (- (+ clip-bottom clip-height) new-bottom)))
            (if (or (>= 0 new-width) (>= 0 new-height))
                ;; Gross clipping
                nil
                ;; Adjusted bitblt
                (canvas-bltshade canvas texture :canvas-left new-left 
                       :canvas-bottom new-bottom :width new-width 
                       :height new-height :operation operation))))

 
;;; Clip against CLIPPINGREGION and draw in STREAM.  Implements
;;; Cohen-Sutherland clipping.  From Foley and Van Dam, pg.  146 

(defun clipped-draw-line (canvas x1 y1 x2 y2 clipping-region
                             &key width operation color
                             dashing)
  (with-focused-canvas canvas
    (let* ((clip-left (region-left clipping-region))
           (clip-bottom (region-bottom clipping-region))
           (clip-right (1- (+ clip-left (region-width clipping-region))))
           (clip-top (1- (+ clip-bottom (region-height clipping-region))))
           (old-x2 x2) (old-y2 y2)
           (accept-p nil) (done-p nil)
           outcode-1 outcode-2)
      (loop
        (if done-p (return nil))
        (setq outcode-1 (clip-code x1 y1 clip-left clip-right clip-top clip-bottom))
        (setq outcode-2 (clip-code x2 y2 clip-left clip-right clip-top clip-bottom))
        (if (= 0 (logand outcode-1 outcode-2))
          ;; Possible accept
          (if (setq accept-p (= 0 (logior outcode-1 outcode-2)))
            (setq done-p t) ;; accept
            (progn ;; Find intersections
              (when (= 0 outcode-1)
                ;; Swap points so (X1 Y1) is guaranteed to be outside 
                (rotatef x1 x2) (rotatef y1 y2)
                (rotatef outcode-1 outcode-2))
              (cond
               ((/= 0 (logand outcode-1 8)) ;; divide line at top
                (incf x1 (truncate (* (- x2 x1) (- clip-top y1)) (- y2 y1)))
                (setq y1 clip-top))
               ((not (= 0 (logand outcode-1 4))) ;; divide line at bottom
                (incf x1 (truncate (* (- x2 x1) (- clip-bottom y1)) (- y2 y1)))
                (setq y1 clip-bottom))
               ((/= 0 (logand outcode-1 2)) ;; divide line at right
                (incf y1 (truncate (* (- y2 y1) (- clip-right x1)) (- x2 x1)))
                (setq x1 clip-right))
               (t ;; divide line at left
                (incf y1 (truncate (* (- y2 y1) (- clip-left x1)) (- x2 x1)))
                (setq x1 clip-left)))))
          ;; Reject
          (setq done-p t)))
      (when accept-p  ;; actually draw a line if one accepted
        (canvas-draw-line canvas x1 y1 x2 y2 :width width
                          :operation operation :color color :dashing dashing))
      ;; Correctly update position in stream
      (canvas-move-to canvas old-x2 old-y2))))

(defun clipped-draw-between (canvas position-1 position-2 
                                    clipping-region &key width
                                    operation
                                    color dashing)
  (clipped-draw-line canvas 
                     (position-x position-1)
                     (position-y position-1)
                     (position-x position-2)
                     (position-y position-2)
                     clipping-region :width width :operation operation
                     :color color :dashing dashing))

(defun clipped-draw-to (canvas x y clipping-region
                               &key width
                               operation
                               color
                               dashing)
  (clipped-draw-line canvas (canvas-x canvas)
                     (canvas-y canvas)
                     x y clipping-region :width width :operation 
                     operation :color color :dashing dashing))

(defun clipped-relative-draw-to (canvas dx dy clipping-region &key 
                                    width operation
                                    color  dashing)
       (let ((x (canvas-x canvas))
             (y (canvas-y canvas)))
            (clipped-draw-line canvas x y (+ x dx)
                   (+ y dy)
                   clipping-region :width width :operation 
                   operation :color color :dashing dashing)))

(defun clipped-plot-at (canvas position-x position-y glyph clipping-region
			&key operation)
  (let* ((glyph-width (bitmap-width glyph))
	 (glyph-height (bitmap-height glyph))
	 (newx (- position-x 
		  (truncate glyph-width 2)))
	 (newy (- position-y 
		  (truncate glyph-height 2))))
    (clipped-bitblt glyph canvas clipping-region :canvas-left
                    newx :canvas-bottom newy :width glyph-width :height
                    glyph-height :operation operation)))

(defun clipped-draw-string (canvas string clipping-region)
  "Draw the string clipped to the given region of the canvas."
  (let
    ((string-region (canvas-string-region canvas string)))
    
    (if  (subregion-p clipping-region string-region)
      (canvas-princ-string canvas string)
      (let
        ((iregion (intersect-regions string-region clipping-region)))
        
        (if  (and iregion
                  (= (region-height iregion)
                     (region-height string-region)))
          
          
          ;; Some chars visible
          (let* ((minx (region-left clipping-region))
                 (maxx (+ minx (region-width clipping-region)))
                 (x (canvas-x canvas))
                 (y (canvas-y canvas)))
            (dotimes (i (length string))
              (let* ((char (char string i))
                     (charwidth (canvas-character-width canvas char))
                     (nextx (+ x charwidth)))
                (if (not (or (< x minx)
                             (> nextx maxx)))
                  (canvas-draw-character canvas char)
                  (canvas-move-to canvas nextx y))
                (setq x nextx)))))))))
