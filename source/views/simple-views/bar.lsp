;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               bar.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute)
(export  '(bar rectangle set-collect-styles-p
           text-display-mixin bar-with-text rectangle-with-text weights-of set-weights)))
;;;----------------------------------------------------------------------------------
(defclass rectangle-mixin (multiple-draw-style-mixin linkable-mixin orientation-mixin)
 ((style-keys :initform '(:fill? :color ) :allocation :class)
  (weights :initform nil :initarg :weights :accessor weights-of )
  (line-width :initform 1 :initarg :line-width :accessor line-width-of )
  (collect-styles?  :initform t :initarg collect-styles? 
                     :accessor collect-styles-p)
   (middle-menu :allocation :class :initform nil))
  (:default-initargs :fill? *default-bar-fill?* :color  *default-bar-color*
    :orientation :horizontal))


(defclass bar (rectangle-mixin  simple-view) ()
  (:default-initargs :viewed-elements :expand-viewed-object)
  )

(defclass rectangle (justification-mixin fixed-size-rectangle rectangle-mixin  simple-view) 
  ((middle-menu :allocation :class :initform nil)
   )
  (:default-initargs 
    :justification :center :rectangle-width nil :rectangle-height nil))

(defclass text-display-mixin () 
  ((text  
    :initarg :text
    :accessor text-of 
    :initform nil
    :documentation "text for printing")
   (text-drawing-styles :initform nil  :accessor text-drawing-styles-of)
   )
  (:default-initargs :font *default-label-font* :string-color *default-label-color*))

(defclass bar-with-text (text-display-mixin bar) 
  ()
  )

(defclass rectangle-with-text (text-display-mixin rectangle) 
  ()
  ) 

(defmethod initialize-instance :after ((self text-display-mixin) 
                                       &rest keyword-pairs
                                       &key  text-font text-color )
  (setf (text-drawing-styles-of self) (make-drawing-style :text-color text-color  :text-font text-font)))
  




;;;----------------------------------------------------------------------------------

(defmethod get-text ((self text-display-mixin))
  (let ((text (text-of self))
        (vo (viewed-object-of self)) str)
    (cond ((stringp text)
           text)
          ((functionp text) (setq str (funcall text vo))
           (princ-to-string str))
          ((listp vo)
           (length vo))
          ((dataset-p vo)  (length (list-cases vo)))
          (vo 1)
          (t 0))))

(defmethod get-text ((self bar-with-text))
  (let ((text (text-of self))
        (vo (viewed-object-of self)) str)
    (cond ((stringp text)
           text)
          ((functionp text) (setq str (funcall text vo))
           (princ-to-string str))
          (t (bar-count self)))))

(defmethod set-text ((self text-display-mixin) text)
  (if (viewports-of self)
    (progn
      (erase-view self)
      (setf (text-of self) text)
      (draw-view self))
    (setf (text-of self) text)
    ))



(defmethod draw-view :after ((self text-display-mixin) &key viewport)
  (let* ( (font (or (draw-style (text-drawing-styles-of self) :text-font)
                 *default-label-font* ))
         (fill? (if (has-draw-style-p self :fill?)
                 (draw-style self :fill?)
                 nil ))
         (scolor
          (or (draw-style (text-drawing-styles-of self) :text-color)
                 (if fill? wb:*default-canvas-background-color* ) )
          )
         (count (get-text self)))
   (with-exposed-viewports self viewport vp
       (wb:canvas-draw-string (window-of vp)
                             (format nil "~A"  count)
                             :region (wb-region vp) 
                              :clip? t
                              :font font
                              :color scolor
                              :justification :center
                            ))))
(defmethod list-legal-justifications ((self rectangle))
  (list :center :left :right  :bottom :top ))

(defmethod fix-viewports ((self rectangle) &key viewport ) 
  (let ((h (rectangle-height-of  self))
        (w (rectangle-width-of  self))
        (just (justification-of self)))
    (loop for vp in (if viewport 
                      (list viewport) (viewports-of self) )
          do (case  just
               
               (:bottom 
                (if h 
                  (setf (top-of vp) (+ (bottom-of vp) h )))
                (if w
                  (set-viewport-width-height vp
                                           w 
                                           (height-of vp))))
               (:top (if h (setf (bottom-of vp) (- (top-of vp) h ) ))
                     (if w
                       (set-viewport-width-height vp
                                                  w 
                                                  (height-of vp))))
               (:left (if w (setf (right-of vp) (+ (left-of vp) w ) )) 
                      (if h
                       (set-viewport-width-height vp
                                                 (width-of vp) h )))
               (:right (if w (setf (left-of vp) (- (right-of vp) w ) ))
                       (if h
                       (set-viewport-width-height vp
                                                 (width-of vp) h )))
               (t (set-viewport-width-height vp
                                           (or w (width-of vp)) 
                                           (or h (height-of vp))))))))


(defmethod draw-view ((self rectangle-mixin) &key viewport ) 
  
  "Color and fill self in proportion to the colors/fill of the drawing-style."
  
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let (
            (axis (orientation-of self))
            (x-wid (- r l))
            (y-wid (- tp b)) (w (window-of vp)))
        (cond ((zerop x-wid)
               (wb:canvas-draw-line 
                w l  b l tp  :color (draw-style self :color) :width (line-width-of self)))
              ((zerop y-wid)
               (wb:canvas-draw-line 
                w l  b r b :color (draw-style self :color) :width (line-width-of self)))
              
              (t
               (loop with l1 and b1 and tp1 and  r1 and
                     last-fill = nil
                     with styles = (if (collect-styles-p self)
                                     (style-proportions self '( :highlight? :fill? :color   ) 
                                                         (weights-of self))
                                     (style-proportions-in-order self '(:highlight? :fill? :color     )
                                                                  (weights-of self)))
                     with nstyles = (length (first styles))
                     with lwid = (line-width-of self)
                     for (hi fill col ) in (first styles)
                     for (next-hi  next-fill next-col) in (append (cdr (first styles)) (list nil nil nil))
                     for prop in (second styles)
                     for i upfrom 1
                     do 
                     (if (eql axis :horizontal)
                       (setq y-wid  (round (* prop (+ 1 (height-of vp)))))
                       (setq x-wid  (round (* prop (+ 1 (width-of vp))))))
                     (setq b1 b tp1 (min tp (+ b y-wid))
                           l1 l r1 (min r (+ l x-wid))) 
                     (cond (hi 
                            (wb:canvas-highlight-rectangle w l1 r1
                                                 b1   tp1
                                                 :color *default-highlight-color*
                                                 :operation :default))
                            (fill
                            (wb:canvas-draw-filled-rectangle  w  l1 r1 b1 tp1 :color col))
                           ((eql axis :horizontal)
                             (wb:canvas-draw-line w l1 b1  l1 (1- tp1) :color col :width lwid)
                            (wb:canvas-draw-line w r1 b1 r1 (1- tp1) :color col :width lwid)
                            (unless last-fill
                              (wb:canvas-draw-line w l1 b1 r1 b1 :color col :width lwid))
                            (unless next-fill
                              (if (= nstyles i)
                                (wb:canvas-draw-line w l1 tp1 r1 tp1 :color col :width lwid )
                              (wb:canvas-draw-line w l1 (1- tp1) r1 (1- tp1) :color col :width lwid ))))
                           (t
                            
                            (wb:canvas-draw-line w l1 b1  (1-  r1) b1 :color col :width lwid)
                            (wb:canvas-draw-line w l1 tp1 (1-  r1) tp1 :color col :width lwid)
                            (unless last-fill
                              (wb:canvas-draw-line w l1 b1 l1 tp1 :color col :width lwid))
                            (unless next-fill
                              (if (= nstyles i)
                             (wb:canvas-draw-line w r1 b1 r1 tp1 :color col :width lwid)
                              (wb:canvas-draw-line w (1-  r1) b1 (1- r1) tp1 :color col :width lwid))))
                       )
                     
                     (if (eql axis :horizontal)
                       (incf b y-wid) (incf l x-wid))
                     (setq last-fill (or hi fill))
                     ))
              )))))



(defmethod highlight-operation ((self rectangle-mixin))
  :draw
 )

(defmethod highlight-view ((self rectangle-mixin) &key viewport )
  "Highlight a proportion of each color-fill combintation ~
   by examining drawing styles. "
  (with-exposed-viewports self viewport vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (let (
            (axis (orientation-of self))
            (x-wid (- r l))
            (y-wid (- tp b)) (w (window-of vp)))
        (cond ((zerop x-wid)
               (wb:canvas-draw-line 
                w l  b l tp  :color (draw-style self :color) :width (line-width-of self)))
              ((zerop y-wid)
               (wb:canvas-draw-line 
                w l  b r b :color (draw-style self :color) :width (line-width-of self)))
              
              (t
               (loop with l1 and b1 and tp1 and  r1 and
                     last-fill = nil
                     with styles = (if (collect-styles-p self)
                                     (style-proportions self '( :highlight? :fill? :color   ) (weights-of self))
                                     (style-proportions-in-order self '(:highlight? :fill? :color     ) (weights-of self)))
                     with nstyles = (length (first styles))
                     with lwid = (line-width-of self)
                     for (hi   fill col) in (first styles)
                     for (next-hi next-fill next-col ) in (append (cdr (first styles)) (list nil nil nil))
                     for prop in (second styles)
                     for i upfrom 1
                     do 
                     (if (eql axis :horizontal)
                       (setq y-wid  (round (* prop (+ 1 (height-of vp)))))
                       (setq x-wid  (round (* prop (+ 1 (width-of vp))))))
                     (setq b1 b tp1 (min tp (+ b y-wid))
                           l1 l r1 (min r (+ l x-wid))) 
                     (cond (hi 
                            (wb:canvas-highlight-rectangle w l1 r1
                                                 b1   tp1
                                                 :color *default-highlight-color*
                                                 :operation :default))
                            (fill
                            (wb:canvas-draw-filled-rectangle  w  l1 r1 b1 tp1 :color col))
                           ((eql axis :horizontal)
                            (wb:canvas-clear  w :canvas-left l1 :canvas-bottom b1 :width (- r1 l1) :height (- tp1 b1) )
                            (wb:canvas-draw-line w l1 b1  l1 (1- tp1) :color col :width lwid)
                            (wb:canvas-draw-line w r1 b1 r1 (1- tp1) :color col :width lwid)
                            (unless last-fill
                              (wb:canvas-draw-line w l1 b1 r1 b1 :color col :width lwid))
                            (unless next-fill
                              (if (= nstyles i)
                                (wb:canvas-draw-line w l1 tp1 r1 tp1 :color col :width lwid)
                              (wb:canvas-draw-line w l1 (1- tp1) r1 (1- tp1) :color col :width lwid))))
                           (t
                            (wb:canvas-clear   w :canvas-left l1 :canvas-bottom b1 :width (- r1 l1) :height (- tp1 b1) )
                            (wb:canvas-draw-line w l1 b1  (1-  r1) b1 :color col :width lwid)
                            (wb:canvas-draw-line w l1 tp1 (1-  r1) tp1 :color col :width lwid)
                            (unless last-fill
                              (wb:canvas-draw-line w l1 b1 l1 tp1 :color col :width lwid))
                            (unless next-fill
                              (if (= nstyles i)
                             (wb:canvas-draw-line w r1 b1 r1 tp1 :color col :width lwid)
                              (wb:canvas-draw-line w (1-  r1) b1 (1- r1) tp1 :color col :width lwid))))
                       )
                     
                     (if (eql axis :horizontal)
                       (incf b y-wid) (incf l x-wid))
                     (setq last-fill (or hi fill))
                     ))
              )))))


#|
(defmethod bar-count ((self bar))
  (if (listp (viewed-object-of self))
    (length (viewed-object-of self))))

|#

(defmethod weights-of ((self rectangle-mixin))
  (let ((w (slot-value self 'weights)))
    (cond ((and (listp w) (and (= (length w) (length (list-viewed-elements self)))))
            w)
          ((functionp w) 
            (mapcar w (list-viewed-elements self)))
          ((identifier-p w) 
            (loop with vars = (list-variates (viewed-object-of self))
                  for c in (list-viewed-elements self) collect 
                  (value-of c w :variates vars)))
           
           (t nil))))

(defmethod set-weights ((self rectangle-mixin) weights &key draw?)
   (setf (slot-value self 'weights) weights)
   (if draw? (draw-view self)))

(defmethod bar-count ((self bar))
  (weight-sum self))


(defmethod weight-sum ((self rectangle-mixin))
  (let ((w (weights-of self)))
    (if w
      (reduce #'+ w)
      (length (list-viewed-elements self)))))

(defmethod get-menu-items :around ((self rectangle-mixin) (slot-name (eql 'middle-menu)))

  (add-menu-items self  (call-next-method)
        '(
          ;;( "Horizontal?" (set-orientation  :toggle :draw? t))
           ( "CollectStyles?" (set-collect-styles-p  :toggle :draw? t))
           ( "Counts" (report-style-counts))
           )))


(defmethod report-style-counts ((self rectangle-mixin) )
  (let* ((props 
         (second (if (collect-styles-p self)
           (style-proportions self '( :highlight? :fill? :color   ) 
                              (weights-of self))
           (style-proportions-in-order self '(:highlight? :fill? :color     )
                                       (weights-of self)))))
        (wsum (weight-sum self))
        (counts (loop 
                  for c in props
                  collect (* wsum c))))
   (quail-print (format nil "Bar counts: ~{~A~^,~}, Total: ~A" counts wsum))
    (quail-print (format nil "Bar proportions: ~{~5,3F~^,~}" props))
    ))


(defmethod style-menu-items :around ((self rectangle-mixin))
  
  (let ((items (call-next-method)))
     (setq items (loop for it in items
                       when (and (listp it) (eq (car it) 'set-color-style) )
                       do (setf (cdr (last it)) `(:highlit?  *selected-subviews-only*))
                       collect it))
  (add-menu-items self  items
        '(( "Fill?" (set-drawing-style  :fill? :toggle :highlit?  *selected-subviews-only*))
           ))))

(defmethod set-drawing-style :around  ((self rectangle-mixin) 
                                       &rest style-pairs &key fill?)
  (if (eql fill? :toggle)
    (apply #'call-next-method self :fill? (not (draw-style  self :fill?)) style-pairs)
    
    (call-next-method)))



(defmethod set-collect-styles-p ((self rectangle-mixin) val &key draw? &allow-other-keys)
  (if (eq val :toggle)
    (setf (collect-styles-p self) (not (collect-styles-p self)))
    (setf (collect-styles-p self) val))
  (if draw? (draw-view self :erase? t)))


(defmethod update-menu-items :after  ((self rectangle-mixin) 
                                      (slot-name (eql 'middle-menu)))
  (let* ((m (slot-value self slot-name)))
    (wb:check-menu-item m "CollectStyles?" (collect-styles-p self))
    #|
(if (eq (orientation-of self) :horizontal)
      (wb:set-menu-item-string m "Horizontal?" "Vertical?")
      (wb:set-menu-item-string m  "Vertical?" "Horizontal?"))
|#
    
    ))



(defmethod description-string ((self rectangle-mixin))
  (format nil "~A, Count: ~A" (call-next-method) (weight-sum self)))
  




