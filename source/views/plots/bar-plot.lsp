;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               bar-plot.lisp
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
;;;     C.B. Hurley 1996 

(in-package :views)








(defmethod init-position-margin-labels :around ((self plot)
                                        outer-space inner-space
                                        &rest key-args )
  (let ((i (interior-view-of self)))
    (cond ((typep i 'bar-chart)
           (apply #'bar-plot-position-margin-labels self outer-space inner-space key-args))
          ((and (typep i 'batch-layout)
                (typep (car (subviews-of i)) 'bar-chart))
           (apply #'bar-plot-position-margin-labels self outer-space inner-space key-args))
          (t (call-next-method)))))






(defmethod bar-plot-position-margin-labels ((self plot) outer-space inner-space
                                 &key bottom-label-size top-label-size
                                 left-label-size right-label-size gap-x gap-y
                                 &allow-other-keys)
  (with-accessors ((ll  left-labels-of) (rl right-labels-of)
                   (tl top-labels-of) (bl bottom-labels-of))
                  self
    (multiple-value-bind (li ri bi ti)
                         (bounds-of inner-space)
      (multiple-value-bind (lo ro bo to)
                           (bounds-of outer-space)
        (let* ((iv (interior-view-of self))
               (bar-chart (if (typep iv 'bar-chart) iv
                              (car (descendant-views-of-type iv 'bar-chart) )))
               (orientation (if bar-chart (orientation-of bar-chart)))
               (batch-strings (if bar-chart (generate-bar-strings bar-chart)))
               tiles)
          (when ll
            (setq tiles
                  (if (and (eq orientation :horizontal)
                           (= (length ll) (reduce #'+ batch-strings :key #'length )))
                    (loop with w = (/ left-label-size (length batch-strings))
                          with f = (+ lo left-label-size)
                          for c in batch-strings
                          append  (tile-region-list 
                                   (make-region  (- f w) f bi ti)
                                   (length c) 1  nil 0 0 )
                          do (decf f w))
                    (tile-region-list 
                     (make-region lo (+ lo left-label-size)  bi ti)
                     (length ll) 1 nil 0 gap-y)))
            
            (loop for lli in ll
                  for l-region in tiles
                  do
                  (place-subview self lli l-region)))

        
        (when rl
            (setq tiles
                  (if (and (eq orientation :horizontal)
                           (= (length rl) (reduce #'+ batch-strings :key #'length )))
                    (loop with w = (/ right-label-size (length batch-strings))
                          with f = (- ro right-label-size)
                          for c in batch-strings
                          append  (tile-region-list 
                                   (make-region  f (+ f w)  bi ti)
                                   (length c) 1 nil 0 0 )
                          do (incf f w))
                    (tile-region-list 
                     (make-region  (- ro right-label-size) ro  bi ti)
                     (length rl) 1 nil 0 gap-y)))
            
            (loop for rli in rl
                  for l-region in tiles
                  do
                  (place-subview self rli l-region)))
        
        
        (when bl
          (setq tiles
                (if (and (eq orientation :horizontal)
                         (= (length bl) (reduce #'+ batch-strings :key #'length )))
                  (loop with w = (/ bottom-label-size (length batch-strings))
                        with f = (+ bo bottom-label-size)
                        for c in batch-strings
                        append  (tile-region-list 
                                 (make-region li ri (- f w) f )
                                              1 (length c) nil 0 0 )
                                 do (decf f w))
            (tile-region-list 
                 (make-region li ri bo (+ bottom-label-size bo))
                 1 (length bl) nil gap-x 0 )))
            
          (loop for bli in bl
                for l-region in  tiles do
                (place-subview self bli l-region)))
        (when tl
          (setq tiles
                (if (and (eq orientation :horizontal)
                         (= (length bl) (reduce #'+ batch-strings :key #'length )))
                  (loop with w = (/ top-label-size (length batch-strings))
                        with f = (- to top-label-size)
                        for c in batch-strings
                        append  (tile-region-list 
                                 (make-region li ri f (+ f w))
                                              1 (length c) nil 0 0 )
                                 do (incf f w))
            (tile-region-list 
                 (make-region  li ri (- to top-label-size ) to)
                 1 (length tl) nil gap-x 0 )))
            
          (loop for tli in tl
                for l-region in tiles  do
                (place-subview self tli l-region)))

        )))))