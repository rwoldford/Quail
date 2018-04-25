;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               barchart.lisp
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
;;;     C.B. Hurley 1992 George Washington University
;;;     
;;;
;;;
;;;------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(bar-chart barchart-position-subviews)))

(defgeneric barchart-position-subviews (view &key &allow-other-keys))

(defclass bar-chart(orientation-mixin batch-mixin position-key-mixin view-layout )
  ((position-keys :initform '(:gap-x :gap-y  ) :allocation :class)
   (middle-menu :allocation :class
                :initform nil)
   (filled? :allocation :class :initform nil :accessor filled-p)
   (bar-height
    :initarg :bar-height :initform nil
    :accessor bar-height-of 
    :documentation "Function (or variate) applied to batch to get height of bar ")
   
   (style-keys :initform '(:size :fill? :box-color) :allocation :class))
  (:default-initargs :subview-constructor #'barchart-subviews
    :subview-position-fn #'barchart-position-subviews
    :orientation :horizontal 
    :orientation-menu? nil
    :gap-x .15 :gap-y .15
    :box-views? t
    :color *default-bar-color*
    :fill? *default-bar-fill?*
    :box-color-menu? nil :box-views-menu? nil
    :subview-type 'bar :default-subview-type 'bar :initform-fn #'get-barchart-inits
    :link-bounds-x? nil :link-bounds-y? nil))

   


(defmethod style-menu-items ((self bar-chart))
  `(( "Color" nil "" :sub-items ,(color-menu-items))
    ("Invisible?"  (set-drawing-style :invisible? :toggle :highlit?  *selected-subviews-only*))
    ("Fill?" (set-drawing-style :fill? :toggle :highlit?  *selected-subviews-only*))
    ))

(defmethod change-variable ((self bar-chart) &rest keyword-args)
  (declare (ignore keyword-args)))

(defmethod barchart-subviews((self bar-chart) &rest keyword-pairs 
                             &key orientation  format by  &allow-other-keys)
  (setq format (if (eq orientation :horizontal) :row :col))
  (if (eq by #'list-cases)
    (progn
      (if (null (bar-height-of self))
         (setf (bar-height-of self) (choose-variable  (viewed-object-of self) 1 
                                                     "Choose a variable to use for the bar heights")))
      (apply #'batch-sub-views self :format format :by nil :batches #'list-cases  keyword-pairs))
    (apply #'batch-sub-views self :format format  keyword-pairs)
    ))

           


    
(defmethod use-y-axis-p ((self bar-chart))
  (eql (orientation-of self) :horizontal))

(defmethod use-x-axis-p ((self bar-chart))
  (eql (orientation-of self) :vertical))


(defmethod set-orientation ((self  bar-chart) orientation &key draw? )
  (declare (ignore orientation draw?)))

(defun gap2 (a b na nb)
  (let ((as (if (listp a)
              (loop repeat na append a)
              (make-list na :initial-element a))))

  (append as
   (loop repeat nb
         when (listp b) append b else collect b
             append as 
      ))))


(defun gapk(g n)
  (loop with ans = nil
        for gi in g
        for ni in n do
        (setq ans (gap2 ans gi 1 ni))
        finally (return ans)))


(defun irregular-tile-region (region gap-x gap-y )
  "Returns a list where each list element is a list~
   of regions in a row beginning at the top-left,~
   separated in the horizontal direction by gap-x and by gap-y in~
   the vertical direction.~
   Gap-x and gap-y are lists."

  (let* ((ncols (+ 1 (length gap-x)))
         (nrows (+ 1 (length gap-y)))
         (w (/ (width-of region) (+ (reduce #'+ gap-x) ncols)))
         (h (/ (height-of region) (+ (reduce #'+ gap-y) nrows))))
    
  (multiple-value-bind (l r b tp) (bounds-of region)
      (loop with gap-xs = (append gap-x (list 0))
            with y = tp
            for gy in (append (reverse gap-y) (list 0))
            collect
            (loop with x = l 
                  for gx in gap-xs collect
                  (make-region x (min r (+ x w))
                              (max b  (- y h)) y) do
                  (incf x (* w (+ 1 gx))))
            do
                  (decf y (* h (+ 1 gy)))))))

                 



(defmethod barchart-position-subviews ((self bar-chart)
                                       &key bar-length bounding-region  positions
                                        gap bar-height)
  
  ( if positions
       (view-layout-position-subviews self  :positions positions :bounding-region
                                      bounding-region)
       (let* ((subs (layout-views-of self))
             (big-region (make-region))
             (vars (if bar-height (list-variates (viewed-object-of self))))
              (orientation (orientation-of self))
              (nsubs (length subs))
             (gap-x (getf (default-positions-of self) :gap-x))
             (gap-y (getf (default-positions-of self) :gap-y))
              (batch-levels (batch-level-counts-of self))
               repeats
               sub-regions
               tiles)
         (when gap (setq gap-x gap gap-y gap)
               (setf (getf (default-positions-of self) :gap-x) gap)
               (setf (getf (default-positions-of self) :gap-y) gap))
         (when subs
           (setf positions
                 (loop for i from 0 below (length subs)
                       collect (or (and bar-length (listp bar-length) (elt bar-length i))
                                   bar-length)))
             (if (eq orientation :horizontal)
             (progn
               (unless (zerop nsubs)
                 (setf (left-of big-region) (* (left-of big-region) nsubs))
                 (setf (right-of big-region) (* (right-of big-region) nsubs)))
               (cond ((and (listp gap-x)
                           (= (length gap-x) (length batch-levels)))
                      (setq repeats
                            (nreverse
                             (loop for b in batch-levels
                                   collect (- b 1))))
                      (setq gap-x (gapk (reverse gap-x) repeats))
                      (setq tiles (irregular-tile-region big-region gap-x nil )))
                     (t (if (listp gap-x) (setq gap-x (car gap-x)))
                        (setq tiles (tile-region big-region 1 (length subs)  nil 
                                                 gap-x 0))))
               
               
               (loop for layer-list in subs
                     for pos in  positions
                     for r in (reduce #'append tiles)
                     do
                     (loop with s = (car layer-list)
                           repeat (length layer-list) 
                           for height = 
                           (cond ((numberp  pos)
                                  pos)
                                 ((functionp pos) 
                                  (funcall pos s))
                                 ((and pos (symbolp pos))
                                  (funcall (get-function pos) s))
                                 (bar-height (if (functionp bar-height)
                                        (funcall bar-height (viewed-object-of s))
                                        (value-of (viewed-object-of s) bar-height :vars vars)))    
                                 ((numberp (viewed-object-of s))
                                  (viewed-object-of s))
                                  (t nil))
                           do 
                           (unless (numberp height)
                             (setq height 
                                   (if (typep s 'rectangle-mixin)
                                     (bar-count s)
                                   (let ((vos (list-viewed-elements  s)))
                                      (if (listp vos) (length vos) 
                                          (if vos 1 0))))))
                           (push 
                            (make-region (left-of r) (right-of r) 
                                         (bottom-of r) (+ (bottom-of r) height))
                            sub-regions))))
             (progn
               (unless (zerop nsubs)
                 (setf (bottom-of big-region) (* (bottom-of big-region) nsubs))
                 (setf (top-of big-region) (* (top-of big-region) nsubs)))
               (cond ((and (listp gap-y)
                           (= (length gap-y) (length batch-levels)))
                      (setq repeats
                            (nreverse
                             (loop for b in batch-levels
                                   collect (- b 1))))
                      (setq gap-y (gapk (reverse gap-y) repeats))
                       (setq tiles (irregular-tile-region big-region gap-x nil )))
                     (t (if (listp gap-y) (setq gap-y (car gap-y)))
                        (setq tiles (tile-region big-region (length subs) 1   nil 
                                                0 gap-y ))))
               (loop for layer-list in subs
                     for pos in  positions
                     for r in (reduce #'append tiles)
                     do
                     (loop with s = (car layer-list)
                           repeat (length layer-list) 
                           for width = 
                           (cond ((numberp  pos)
                                  pos)
                                 ((functionp pos) 
                                  (funcall pos s))
                                 ((and pos (symbolp pos))
                                  (funcall (get-function pos) s))
                                 (bar-height (if (functionp bar-height)
                                        (funcall bar-height (viewed-object-of s))
                                        (value-of (viewed-object-of s) bar-height :vars vars)))
                                 ((numberp (viewed-object-of s))
                                  (viewed-object-of s))
                                 (t nil))
                           do
                           (unless (numberp width)
                             (setq width
                                   (let ((vos (list-viewed-elements  s)))
                                      (if (listp vos) (length vos) 
                                          (if vos 1 0)))))
                           (push 
                            (make-region (left-of r) (+  (left-of r) width)
                                         (bottom-of r) (top-of r))
                            sub-regions)))))
           
           (setf (sub-view-locns-of self) (nreverse sub-regions))
            (if (null bounding-region)
         (set-bounding-region self :region (compute-containing-bounding-region self)))
           ))
      ))
                        
#|
(defmethod grid-margin-region-info ((self bar-chart) 
                                    axis
                                    index)
  (declare (ignore axis index))
  (bounding-region-of self))

|#




(defmethod generate-bar-strings  ((self bar-chart) )  
  (let ( (names (batch-level-names-of self))
         times)
    (setq names
          (loop for b in names
                collect (mapcar 
                         #'(lambda(s) 
                             (if s (princ-to-string 
                                    (if (floatp s)
                                      (short-string-float s 6)
                                          s)) ""))
                         b)))
    (setq times (reverse (mapcar #'length names)))
    (setq times     (loop for ti on times
                          collect (reduce #'* (cdr ti))))
    (loop for ti in times 
          for n in (reverse names)
          collect
          (loop repeat ti append n))))

(defmethod margin-string-y  ((self bar-chart) )
  (if (eq (orientation-of self) :vertical)
      (loop for n in (generate-bar-strings self) append  n)
      (coord-string self)))

(defmethod margin-string-x  ((self bar-chart) )
  (if (eq (orientation-of self) :horizontal)
      (loop for n in (generate-bar-strings self) append  n)
      (coord-string self)))
     
(defmethod batch-string-x ((self bar-chart))
  (when (eq (orientation-of self) :horizontal)
    (loop for n in (all-tuples (batch-level-names-of self))
          for n1 = (loop for z in n
                            when z 
                            collect (if (floatp z)
                                          (short-string-float z 6)
                                          z))
          collect
          (format nil "~{~A~^ ~}" n1))))
   
(defmethod batch-string-y ((self bar-chart))
  (when (eq (orientation-of self) :vertical)
    (loop for n in (all-tuples (batch-level-names-of self))
          for n1 = (loop for z in n
                            when z 
                            collect (if (floatp z)
                                          (short-string-float z 6)
                                          z))
          collect
          (format nil "~{~A~^ ~}" n1))))


(defmethod margin-string-bottom ((self bar-chart))
  (margin-string-x self))

(defmethod margin-string-left ((self bar-chart))
  (margin-string-y self))


(defmethod coord-string ((self bar-chart))
  (plot-axis-string (bar-height-of self) nil nil  
                    ))
