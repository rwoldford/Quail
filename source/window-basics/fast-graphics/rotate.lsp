;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              rotate.lsp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1991 George Washington University
;;;     
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
(in-package :wb)
(export '(rotate-point-cloud draw-point-cloud scale-data-for-region 
          mapply-rotation! rotate-line-segments draw-line-segments
          make-rotation-transform make-shift-transform))
(defun x-shift (region)
  #-:sbcl(declare 
           (optimize (speed 3) (safety 0)
            (space 0) (compilation-speed 0))
           )
  (declare (inline + aref round /))
  (round (+ (aref region 0) (/ (aref region 2) 2))))
(defun y-shift (region)
  #-:sbcl(declare 
           (optimize (speed 3) (safety 0)
            (space 0) (compilation-speed 0))
           )
  (declare (inline + aref round /))
  (round (+ (aref region 1) (/ (aref region 3) 2))))
 
(defun make-shift-transform (region)
  #-:sbcl(declare (optimize (speed 3) (safety 0)
            (space 0) (compilation-speed 0))
           )
  (make-instance '2d-shift 
                   :x-shift (x-shift region) :y-shift (y-shift region)))
(defun make-rotation-transform (region direction  )
  #-:sbcl(declare (optimize (speed 3) (safety 0)
            (space 0) (compilation-speed 0))
           )
  (let* ((xhift (x-shift region))
         (yshift (y-shift region)))
    
    (ecase direction
      (:x (make-instance '3d-x-rotate&2d-shift :angle 0 
                         :x-shift xhift :y-shift yshift))
      (:y (make-instance '3d-y-rotate&2d-shift :angle 0 
                         :x-shift xhift :y-shift yshift))
      (:z (make-instance '3d-z-rotate&2d-shift :angle 0 
                         :x-shift xhift :y-shift yshift)))))
#|   This version does not contain code used by views involving draw-rate
;;;  see the version which follows this ;;14JUL2023
(defun rotate-point-cloud (c points 
                                  &key axes axis-color
                                  (steps 100) (increment (/ pi 30)) (direction :y) 
                                  size fill? symbol color invisible?
                                  ( plot-rgn (canvas-region c) ) 
                                  
                                  (standardize? nil) (center? nil) (viewport-coords? nil)
                                  erase-points erase-axes
                                  stop-fn)
  "Rotates a point-cloud using plotting traps. The point cloud is in list form~
   with each sublist an x,y,z observation. !!"
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline canvas-move-axes
                   canvas-move-symbols
                   mapply-transform-store
                   )
           )
  (declare (inline rotatef incf))
  
  (let ((data (append points axes )))
        
    (if viewport-coords?
      (progn
        (if (and (not standardize?) center?)
          (setq data (center-data-lists data))
          (if standardize?
            (setq data (standardize-data-lists data))))
        (setq data (scale-data-for-region data plot-rgn))))
                         
   
    (let* ((old-points (copy-tree points)) (old-axes (copy-tree axes))
           (new-points (copy-tree points)) (new-axes (copy-tree axes))
           (rot (make-rotation-transform plot-rgn direction  ))
           (single-color? (every #'(lambda(c) (eq-colors c (car color))) color)))
      (if (and (colored-canvas-p c) (not single-color?))
        (setq color (rgb-colors color)))
      
      (mapply-transform-store rot data (append old-points old-axes))
      (with-pen-values-restored c
        (unless erase-points
          (canvas-draw-symbols c old-points   :symbol symbol
                               :color color :fill? fill? :size size 
                               :invisible? invisible? :single-color? single-color? ))
        (unless erase-axes
          (canvas-draw-axes c  old-axes :color axis-color))
        
        (incf (angle-of rot) increment)
        (mapply-transform-store rot data (append new-points new-axes))
        (canvas-move-symbols c (or erase-points old-points)  new-points  
                             :symbol symbol :color color :fill? fill? 
                             :size size :invisible? invisible?
                             :rgb-color? t :single-color? single-color? )
        (canvas-move-axes c (or erase-axes old-axes) new-axes :color axis-color )
        
        ;;; loop with the do keyword - better than "loop". CW 03/1997.
        ;   (loop for k from 1 below steps
        ;         until (if (functionp stop-fn) (funcall stop-fn))
        ;         do
        (do ((k 1 (incf k)))
            ((or (= k steps) (if (functionp stop-fn) (funcall stop-fn))))
          (incf (angle-of rot) increment)
          (rotatef old-axes new-axes) (rotatef old-points new-points)
          (mapply-transform-store rot data (append new-points new-axes))
          (canvas-move-symbols c old-points new-points  :symbol symbol
                               :color color :fill? fill? :size size
                               :invisible? invisible?
                               :rgb-color? t :single-color? single-color?)
          (when axes
            (canvas-move-axes c old-axes new-axes :color axis-color)))
        )
      (values rot new-points new-axes)
      )))
|#
;;; Version from QUAIL-2009-ACER which does contain the draw-rate material
;;; 14JUL2023

(defun rotate-point-cloud (c points 
                                  &key axes axis-color
                                  (steps 100) (increment (/ pi 30)) (direction :y) 
                                  size fill? symbol color invisible?
                                  ( plot-rgn (canvas-region c) ) 
                                  (draw-rate nil)
                                  (standardize? nil) (center? nil) (viewport-coords? nil)
                                  erase-points erase-axes
                                  stop-fn)
  "Rotates a point-cloud using plotting traps. The point cloud is in list form~
   with each sublist an x,y,z observation. !!"
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline canvas-move-axes
                   ;rotatef
                   canvas-move-symbols
                   mapply-transform-store
                   ;incf
                   )
           )
  
  (let ((data (append points axes )))
        
    (if viewport-coords?
      (progn
        (if (and (not standardize?) center?)
          (setq data (center-data-lists data))
          (if standardize?
            (setq data (standardize-data-lists data))))
        (setq data (scale-data-for-region data plot-rgn))))
                         
   
    (let* ((old-points (copy-tree points)) (old-axes (copy-tree axes))
           (new-points (copy-tree points)) (new-axes (copy-tree axes))
           (rot (make-rotation-transform plot-rgn direction  ))
           (bg-color  (canvas-background-color c))
           (pen-color  (pen-color-of c))
           (single-color? (every #'(lambda(c) (eq-colors c (car color))) color))
           (draw-time (if draw-rate (round (/ internal-time-units-per-second draw-rate))))
           start-time step-time)


      (if (and (colored-canvas-p c) (not single-color?))
        (setq color (rgb-colors color pen-color )))
      
      (mapply-transform-store rot data (append old-points old-axes))

      (unless erase-points
        (with-pen-values-restored c
          (canvas-draw-symbols c old-points   :symbol symbol
                               :color color :fill? fill? :size size 
                               :invisible? invisible? :single-color? single-color? )
          (canvas-set-pen c :color bg-color)))
        (unless erase-axes
          (with-pen-values-restored c
            (canvas-draw-axes c  old-axes :color axis-color)
            (canvas-set-pen c :color bg-color)))
        
        (incf (angle-of rot) increment)
        (mapply-transform-store rot data (append new-points new-axes))
        (with-pen-values-restored c
          (canvas-move-symbols c (or erase-points old-points)  new-points  
                               :symbol symbol :color color :fill? fill? 
                               :size size :invisible? invisible?
                               :rgb-color? t :single-color? single-color? )
          (canvas-set-pen c :color bg-color))
        (with-pen-values-restored c
          (canvas-move-axes c (or erase-axes old-axes) new-axes :color axis-color )
          (canvas-set-pen c :color bg-color))
        
        ;;; loop with the do keyword - better than "loop". CW 03/1997.
        ;   (loop for k from 1 below steps
        ;         until (if (functionp stop-fn) (funcall stop-fn))
        ;         do
        (do ((k 1 (incf k)))
            ((or (= k steps) (if (functionp stop-fn) (funcall stop-fn))))
          (if draw-rate
            (setq start-time (get-internal-real-time)))
          (incf (angle-of rot) increment)
          (rotatef old-axes new-axes) (rotatef old-points new-points)
          (mapply-transform-store rot data (append new-points new-axes))
          (with-pen-values-restored c 
            (canvas-move-symbols c old-points new-points  :symbol symbol
                                 :color color :fill? fill? :size size
                                 :invisible? invisible?
                                 :rgb-color? t :single-color? single-color?)
            (canvas-set-pen c :color bg-color)
            )
          (when axes
            (with-pen-values-restored c
              
            (canvas-move-axes c old-axes new-axes :color axis-color)
            (canvas-set-pen c :color bg-color)))
          (when draw-rate
          (setq step-time (- (get-internal-real-time) start-time))
          (if (< step-time draw-time)
            (sleep (min 1 (/ (- draw-time step-time) internal-time-units-per-second)))))


          )
        
      (values rot new-points new-axes)
      )))


      
(defun draw-point-cloud (c points
                           &key axes
                           size fill? symbol color invisible?
                           ( plot-rgn (canvas-region c) ) 
                           (standardize? nil) (center? nil)
                           (viewport-coords?))
  
  "Draws a point-cloud using plotting traps. The point cloud is in list form~
   with each sublist an x,y,z observation. !!"
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  
  (let ((data (append points axes )))
    (if viewport-coords?
      (progn
      (if (and (not standardize?) center?)
        (setq data (center-data-lists data))
        (if standardize?
          (setq data (standardize-data-lists data))))
      (setq data (scale-data-for-region data plot-rgn))))
     
    
    (let* ((scaled-data (scale-data-for-region
                         data plot-rgn))
           (shift (make-shift-transform plot-rgn ))
           (scaled-points (subseq scaled-data 0 (length points)))
           (scaled-axes (subseq scaled-data (length points) )))
      
      
      (mapply-transform! shift scaled-data )
      (with-pen-values-restored c
      (canvas-draw-symbols c scaled-points   :symbol symbol
                           :color color :fill? fill? :size size 
                           :invisible? invisible?)
      (canvas-draw-axes c scaled-axes  )
      )
    
    )))
(defmethod mapply-rotation! ((rot 3d-rotate) (a list) &key (integer? t))
  #-:sbcl(declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))
           (inline mapply-transform! ))
  (declare (inline elt round * / float))
  (if integer?
    (mapply-transform! rot a)
    (let* ((big 10000)
           (integer-coefs
            (loop for c in a collect
                  (loop for ci in c collect
                        (round (* big ci))))))
      (mapply-transform! rot integer-coefs)
      (loop for cnew in integer-coefs 
            for cold in a do
            (loop for ci in cnew
                  for i upfrom 0
                  do
                  (setf (elt cold i) (float (/ ci big))))))))
 
(defun scale-data-for-region (data region)
  "scales  data so that when rotated and shifted it will always fit in a window~
   with minimum dimension SIZE ~
   Data should already be centered at 0"
  #-:sbcl(declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))) ; 29JUL2023 added last )
         (declare (inline round *))
  (let* ((size (min (aref region 2) (aref region 3)))
         (d (/ size
               (sqrt (loop for di in data
                           maximize (loop for dij in di
                                          sum (* dij dij))))
               2)))
    (loop for di in data collect
          (loop for dij in di collect (round (* dij d))))
    )) ; 29JUL2023 deleted last ) .. see above
(defun standardize-data-lists (data )
  "scales 3-d data so that each dimension has mean 0 and variance 1"
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let* ((mean #'(lambda(i)
              (/  (loop for di in data sum (elt di i)) (length data))))
         (sd #'(lambda(i m)
              (sqrt (/  (loop for di in data sum (expt (- (elt di i) m) 2))
                  (length data)))))
         (m0 (funcall mean  0)) (sd0 (funcall sd 0 m0))
         (m1 (funcall mean  1)) (sd1 (funcall sd 1 m1))
         (m2 (funcall mean  2)) (sd2 (funcall sd 2 m2)))
        
    (loop with means = (list m0 m1 m2)
          with sds = (list sd0 sd1 sd2)
          for d in data collect
          (loop for di in d
                for m in means
                for sd in sds 
                collect (/ (- di m) sd)))))
(defun center-data-lists (data )
  "center 3-d data so that each dimension has mean 0 "
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (let* (
         (mean #'(lambda(i)
              (/  (loop for di in data sum (elt di i)) (length data))))
         
         (m0 (funcall mean  0)) 
         (m1 (funcall mean  1)) 
         (m2 (funcall mean  2)) )
        
    (loop with means = (list m0 m1 m2)
          for d in data collect
          (loop for di in d
                for m in means
                collect (- di m)))))
          
(defun rotate-line-segments (c points 
                                  &key axes axis-color
                                  (steps 100) (increment (/ pi 30)) (direction :y) 
                                  width color invisible?
                                  ( plot-rgn (canvas-region c) ) 
                                  
                                  (standardize? nil) (center? nil) (viewport-coords? nil)
                                  erase-points erase-axes
                                  stop-fn)
  "Rotates lines using plotting traps. points gives the lines coordinates,~
   where points has even length, each pair giving the segment endpoints. "
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline canvas-move-axes
                   canvas-move-lines
                   mapply-transform-store
                   )
           )
  (declare (inline ;rotatef 
                   ;incf
                   ))
  
  (let ((data (append points axes )))
        
    (if viewport-coords?
      (progn
        (if (and (not standardize?) center?)
          (setq data (center-data-lists data))
          (if standardize?
            (setq data (standardize-data-lists data))))
        (setq data (scale-data-for-region data plot-rgn))))
                         
   
    (let* ((old-points (copy-tree points)) (old-axes (copy-tree axes))
           (new-points (copy-tree points)) (new-axes (copy-tree axes))
           (rot (make-rotation-transform plot-rgn direction  ))
           (single-color? (every #'(lambda(c) (eq-colors c (car color))) color))
           (single-width? (every #'(lambda(c) (= c (car width))) width))
           )
      (if (and (colored-canvas-p c) (not single-color?))
        (setq color (rgb-colors color)))
      
      (mapply-transform-store rot data (append old-points old-axes))
      (if (and single-color? single-width?)
        (setq color (car color)))
      (if single-width?
        (setq width (car width)))
      (with-pen-values-restored c
        (unless erase-points
          (canvas-draw-lines c old-points   
                               :color color  :width width 
                               :invisible? invisible?))
        (unless erase-axes
          (canvas-draw-axes c  old-axes :color axis-color))
        
        (incf (angle-of rot) increment)
        (mapply-transform-store rot data (append new-points new-axes))
        (canvas-move-lines c (or erase-points old-points)  new-points  
                              :color color
                             :width width :invisible? invisible?
                             :rgb-color? t )
        (canvas-move-axes c (or erase-axes old-axes) new-axes :color axis-color )
        ;   (loop for k from 1 below steps
        ;         until (if (functionp stop-fn) (funcall stop-fn))
        ;         do
        (do ((k 1 (incf k)))
            ((or (= k steps) (if (functionp stop-fn) (funcall stop-fn))))
              (incf (angle-of rot) increment)
              (rotatef old-axes new-axes) (rotatef old-points new-points)
              (mapply-transform-store rot data (append new-points new-axes))
              
              (canvas-move-lines c old-points new-points
                                   :color color :width width
                                   :invisible? invisible?
                                   :rgb-color? t :single-color? single-color?)
              
              (when axes
                (canvas-move-axes c old-axes new-axes :color axis-color))) ) 
      
            
      
      (values rot new-points new-axes)
      )))
(defun draw-line-segments (c points
                           &key axes
                           width  color invisible?
                           ( plot-rgn (canvas-region c) ) 
                           (standardize? nil) (center? nil)
                           (viewport-coords?))
  
  "Draws lines using plotting traps. points gives the lines coordinates,~
   where points has even length, each pair giving the segment endpoints. "
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  
  (let ((data (append points axes )))
    (if viewport-coords?
      (progn
      (if (and (not standardize?) center?)
        (setq data (center-data-lists data))
        (if standardize?
          (setq data (standardize-data-lists data))))
      (setq data (scale-data-for-region data plot-rgn))))
     
    
    (let* ((scaled-data (scale-data-for-region
                         data plot-rgn))
           (shift (make-shift-transform plot-rgn ))
           (scaled-points (subseq scaled-data 0 (length points)))
           (scaled-axes (subseq scaled-data (length points) )))
      
      
      (mapply-transform! shift scaled-data )
      (with-pen-values-restored c
      (canvas-draw-lines c scaled-points  
                           :color color  :width width 
                           :invisible? invisible?)
      (canvas-draw-axes c scaled-axes  )
      )
    
    )))
(defun rotate-line-segments-points (c points 
                                  &key axes axis-color
                                  (steps 100) (increment (/ pi 30)) (direction :y) 
                                  width color invisible?
                                  psize psymbol pfill?
                                   pcolor  pinvisible?
                                 (draw-rate nil)
                                  ( plot-rgn (canvas-region c) ) 
                                  
                                  (standardize? nil) (center? nil) (viewport-coords? nil)
                                  erase-points erase-axes
                                  stop-fn)
  "Rotates lines using plotting traps. points gives the lines coordinates,~
   where points has even length, each pair giving the segment endpoints. "
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline canvas-move-axes
                   canvas-move-lines
                   mapply-transform-store
                   )
           )
  (declare (inline ;rotatef 
                   ;incf
                   ))
  
  (let ((data (append points axes )))
        
    (if viewport-coords?
      (progn
        (if (and (not standardize?) center?)
          (setq data (center-data-lists data))
          (if standardize?
            (setq data (standardize-data-lists data))))
        (setq data (scale-data-for-region data plot-rgn))))
                         
   
    (let* ((old-points (copy-tree points)) (old-axes (copy-tree axes))
           (new-points (copy-tree points)) (new-axes (copy-tree axes))
           (rot (make-rotation-transform plot-rgn direction  ))
           (single-pcolor? (every #'(lambda(c) (eq-colors c (car pcolor))) pcolor))
           (single-color? (every #'(lambda(c) (eq-colors c (car color))) color))
           (single-width? (every #'(lambda(c) (= c (car width))) width))
           (bg-color  (canvas-background-color c))
           (draw-time (if draw-rate (round (/ internal-time-units-per-second draw-rate))))
           start-time step-time)
      (if (and (colored-canvas-p c) (not single-color?))
        (setq color (rgb-colors color (pen-color-of c))))
      (if (and (colored-canvas-p c) (not single-pcolor?))
        (setq pcolor (rgb-colors pcolor (pen-color-of c))))
      
      (mapply-transform-store rot data (append old-points old-axes))
      (if (and single-color? single-width?)
        (setq color (car color)))
      (if single-width?
        (setq width (car width)))
      
        (unless erase-points
          (with-pen-values-restored c
            (canvas-draw-lines c old-points   
                               :color color  :width width 
                               :invisible? invisible?)
            (canvas-set-pen c :color bg-color))
          (with-pen-values-restored c
            (canvas-draw-symbols c old-points   :symbol psymbol
                                 :color pcolor :fill? pfill? :size psize 
                                 :invisible? pinvisible? 
                                 :single-color? single-pcolor? )
            (canvas-set-pen c :color bg-color)))
        (unless erase-axes
          (with-pen-values-restored c
          (canvas-draw-axes c  old-axes :color axis-color)
          (canvas-set-pen c :color bg-color)))
        
        (incf (angle-of rot) increment)
        (mapply-transform-store rot data (append new-points new-axes))
        (with-pen-values-restored c
          (canvas-move-lines c (or erase-points old-points)  new-points  
                             :color color
                             :width width :invisible? invisible?
                             :rgb-color? t )
          (canvas-set-pen c :color bg-color))
        (with-pen-values-restored c
          (canvas-move-symbols c (or erase-points old-points)  new-points  
                               :symbol psymbol :color pcolor :fill? pfill? 
                               :size psize :invisible? pinvisible?
                               :rgb-color? t :single-color? single-pcolor? )
          (canvas-set-pen c :color bg-color))
        (with-pen-values-restored c
        (canvas-move-axes c (or erase-axes old-axes) new-axes :color axis-color )
        (canvas-set-pen c :color bg-color))
        ;   (loop for k from 1 below steps
        ;         until (if (functionp stop-fn) (funcall stop-fn))
        ;         do
        (do ((k 1 (incf k)))
            ((or (= k steps) (if (functionp stop-fn) (funcall stop-fn))))
          (if draw-rate
            (setq start-time (get-internal-real-time)))
          (incf (angle-of rot) increment)
          (rotatef old-axes new-axes) (rotatef old-points new-points)
          (mapply-transform-store rot data (append new-points new-axes))
          (with-pen-values-restored c
            (canvas-move-lines c old-points new-points
                               :color color :width width
                               :invisible? invisible?
                               :rgb-color? t :single-color? single-color?)
            (canvas-set-pen c :color bg-color))
          (with-pen-values-restored c 
            (canvas-move-symbols c old-points new-points  :symbol psymbol
                                 :color pcolor :fill? pfill? :size psize
                                 :invisible? pinvisible?
                                 :rgb-color? t :single-color? single-pcolor?)
            (canvas-set-pen c :color bg-color)
            )
          
          (when axes
            (with-pen-values-restored c
              (canvas-move-axes c old-axes new-axes :color axis-color)
              (canvas-set-pen c :color bg-color)))
          (when draw-rate
            (setq step-time (- (get-internal-real-time) start-time))
            (if (< step-time draw-time)
              (sleep (min 1 (/ (- draw-time step-time) internal-time-units-per-second)))))) 
      
            
      
      (values rot new-points new-axes)
      )))