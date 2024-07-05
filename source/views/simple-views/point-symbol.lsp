;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               point-symbol.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( point-symbol label-of point-symbol-type group-point-symbol )))
;;;----------------------------------------------------------------------------------


(defclass point-symbol (view-with-size linkable-mixin simple-view )
  
  ((middle-menu :allocation :class :initform nil)
   (style-keys :initform '(:symbol :size :fill? :color) :allocation :class)
   (label
    :initarg :label
    :initform #'identifier-name 
    :documentation "Function which when applied to subject gives a label, or a  label"))
  (:default-initargs :color *default-point-color*
                     :symbol *default-point-symbol*
                     :size *default-point-size*
                     :fill? *default-point-fill?*))


(defclass group-point-symbol (point-symbol) ()
   (:default-initargs :viewed-elements :expand-viewed-object
     )) 

(defgeneric label-of (point-symbol))
  
;;;----------------------------------------

(defconstant *cos-18* (cos (* pi 0.1)) )

(defconstant *cos-54* (cos (* pi 0.3)))

(defconstant *sin-18* (sin (* pi 0.1)))

(defconstant *sin-54* (sin (* pi 0.3)))



(defmethod label-of ((self point-symbol))
  (let ((l (slot-value self 'label)))
    (if (functionp l)
      (funcall l (viewed-object-of self))
      l)))

(defmethod fix-viewports ((self point-symbol) &key viewport )
  (let ((size (draw-style self :size)))
      (loop for vp in (if viewport 
                        (list viewport ) (viewports-of self) )
            do
            (set-square-viewport-size vp (+ 2 size))
            )))



(defun point-symbol-key (sym)
  "Returns the symbol key for sym, if one exists."
  (cond
   ((member sym *point-symbol-types*) sym)
   ((stringp sym)  :text)
   ((wb:bitmap-p sym) :bitmap)
   (t nil)))

(defmethod set-draw-style :after ((self point-symbol) (style (eql :size)) new &key)
  (declare (ignore new))
  (fix-viewports self))


(defmethod view-size-of ((self point-symbol))
  (draw-style self :size))

(defmethod (setf view-size-of)  (new (self point-symbol) )
  (set-draw-style self :size new))

(defmethod set-view-size ((self point-symbol) new &rest args)
  (apply #'set-drawing-style self :size new args)
  (setf (slot-value self 'view-size) (draw-style self :size)))


(defmethod view-size-increment ((self point-symbol))
 (if (>= (draw-style self :size) 3) 2 1))


(defmethod draw-view ((self point-symbol) &key viewport)
  
  (let ((symbol (draw-style self :symbol))
        sym-type)
      (setf sym-type (or (point-symbol-key symbol) :box))
        
      (with-exposed-viewports self  viewport vp
        (draw-point-symbol self sym-type vp))))


(defmethod draw-point-symbol ((self point-symbol)
                              (symbol (eql :triangle))
                              viewport)
  
  (let ((color (draw-style self :color))
        (bw (window-of viewport))
        xc yc rad  )
    (with-point-symbol-center&radius self viewport xc yc rad 
      (if  (not (draw-style self :fill?))
        (wb:canvas-draw-polygon 
         bw (list (cons (- xc rad)  (- yc rad)) (cons xc (+ yc rad))
                  (cons  (+ xc rad)  (- yc rad))) :color color :width 1)            
        (wb:canvas-draw-filled-polygon 
         bw (list (cons (- xc rad)  (- yc rad)) (cons xc (+ yc rad))
                  (cons  (+ xc rad)  (- yc rad))) :color color)))))
                                      
(defmethod min-size ((symbol t))
  1)

(defmethod min-size ((symbol (eql :circle)))
  2)

(defmethod min-size ((symbol (eql :diamond)))
  2)

(defmethod min-size ((symbol (eql :triangle)))
  2)

(defmethod min-size ((symbol (eql :star)))
  2)

(defmethod min-size ((symbol (eql :poly-star)))
  4)

(defmethod draw-style ((self point-symbol) (slot-name (eql :symbol)) &key)
  (let ((shape (call-next-method self slot-name))
        (size (draw-style self :size)))
    (if (< size (min-size shape)) :box shape)))
    
          
    


(defmethod draw-point-symbol ((self point-symbol)
                              (symbol (eql :text))
                              viewport)
  
  (let ((color (draw-style self :color))
        (bw (window-of viewport))
        xc yc rad  )
    (declare (ignorable xc yc rad))
     
    (with-point-symbol-center&radius self viewport xc yc rad 
       (wb:canvas-draw-string  bw (draw-style self :symbol) 
                              :region (wb-region viewport)
                              :color color))))


(defmethod draw-point-symbol ((self point-symbol)
                              (symbol (eql :star))
                              viewport)
  (let ((bw (window-of viewport))
        xc yc rad  )
    (with-point-symbol-center&radius self viewport xc yc rad 
      (let* ((r*c18 (round (* rad *cos-18*)))
             (r*s18 (round (* rad *sin-18*)))
             (r*c54 (round (* rad *cos-54*)))
             (r*s54 (round (* rad *sin-54*)))
             (x1  (+ xc r*c18))
             (y1 (+ yc r*s18))
             (x2 xc)
             (y2 (+ yc rad))
             (x3 (- xc r*c18))
             (x4 (- xc r*c54))
             (x5 (+ xc r*c54))
             (y5 (- yc r*s54)))
        (wb:with-pen-values bw (draw-style self :color) 1 nil
          (wb:canvas-draw-line bw xc yc x1 y1)
          (wb:canvas-draw-line bw xc yc x2 y2)
          (wb:canvas-draw-line bw xc yc x3 y1) 
          (wb:canvas-draw-line bw xc yc x4 y5)  
          (wb:canvas-draw-line bw xc yc x5 y5))))))



(defmethod draw-point-symbol ((self point-symbol)
                              (symbol (eql :diamond))
                              viewport)
  (let ((color (draw-style self :color))
        (bw (window-of viewport))
        xc yc rad  )
    (with-point-symbol-center&radius self viewport xc yc rad 
     (if (draw-style self :fill?)
       (wb:canvas-draw-filled-polygon 
        bw (list  (cons (- xc rad) yc)  (cons xc (+ yc rad))
                    (cons (+ xc rad) yc) (cons xc (- yc rad)))
        :color color)
       (wb:canvas-draw-polygon 
        bw (list  (cons (- xc rad) yc)  (cons xc (+ yc rad))
                    (cons (+ xc rad) yc) (cons xc (- yc rad)))
        :color color :width 1)))))


(defmethod draw-point-symbol ((self point-symbol)
                              (symbol (eql :cross))
                              viewport)
  
  (let ( (bw (window-of viewport))
         xc yc rad  )
    (with-point-symbol-center&radius self viewport xc yc rad 
      (wb:with-pen-values bw (draw-style self :color) 1 nil
        (wb:canvas-draw-line bw (- xc rad) yc (+ xc rad) yc)
        (wb:canvas-draw-line bw xc (+ yc rad) xc (- yc rad))))))


(defmethod draw-point-symbol ((self point-symbol)
                              (symbol (eql :circle))
                              viewport)
   (let ((color (draw-style self :color))
        (bw (window-of viewport))
        xc yc rad  )
    (with-point-symbol-center&radius self viewport xc yc rad 
     (if (draw-style self :fill?)
       (wb:canvas-draw-filled-circle bw xc yc rad :color color)
       (wb:canvas-draw-circle bw xc yc rad :color color :width 1)))))
                                      

(defmethod draw-point-symbol ((self point-symbol)
                              (symbol (eql :bitmap))
                              viewport)
 
   (let (xc yc rad)
     (declare  (ignorable rad))
    (with-point-symbol-center&radius self viewport xc yc rad 
     (wb::plot-glyph-at xc yc  (draw-style self :symbol)  (window-of viewport) ))))

(defmethod draw-point-symbol ((self point-symbol)
                              (symbol (eql :box))
                              viewport)
   (let ((color (draw-style self :color))
        (bw (window-of viewport))
        xmin xmax ymin ymax  )
    (with-point-symbol-bounds self viewport  xmin xmax ymin ymax
      (if (draw-style self :fill?)
       (wb:canvas-draw-filled-rectangle bw xmin xmax ymin ymax :color color)
       (wb:canvas-draw-inside-rectangle bw xmin xmax ymin ymax :color color :width 1)))))
                               
(defmethod draw-point-symbol ((self point-symbol)
                              (symbol (eql :poly-star))
                              viewport)
  (let ((bw (window-of viewport))
        (color (draw-style self :color))
        xc yc rad  ) 
    (with-point-symbol-center&radius self viewport xc yc rad 
      (let* ((r*c18 (round (* rad *cos-18*)))
             (r*s18 (round (* rad *sin-18*)))
             (r*c54 (round (* rad *cos-54*)))
             (r*s54 (round (* rad *sin-54*)))
             (r (truncate rad 2))
              (x1  (+ xc r*c18))
             (y1 (+ yc r*s18))
             (x2 xc)
             (y2 (+ yc rad))
             (x3 (- xc r*c18)) (y3 y1)
             (x4 (- xc r*c54))
             (y4 (- yc r*s54))
             (x5 (+ xc r*c54))
             (y5 y4)
             (u1 (+ xc (truncate r*c54 2)))
             (v1 (+ yc (truncate r*s54 2)))
             (u2 (- xc (truncate r*c54 2)))
             (v2 v1)
             (u3 (- xc (truncate r*c18 2)))
             (v3 (- yc (truncate r*s18 2)))
             (u4 xc)
             (v4 (- yc r))
             (u5 (+ xc (truncate r*c18 2)))
             (v5 (- yc (truncate r*s18 2)))
             (p1 (cons x1 y1)) (p2 (cons x2 y2)) (p3 (cons x3 y3))
             (p4 (cons x4 y4)) (p5 (cons x5 y5))
             (q1 (cons u1 v1)) (q2 (cons u2 v2)) (q3 (cons u3 v3))
             (q4 (cons u4 v4)) (q5 (cons u5 v5)))
        (if  (not (draw-style self :fill?))
        (wb:canvas-draw-polygon 
         bw (list p1 q1 p2 q2 p3 q3 p4 q4 p5 q5) :color color :width 1)            
        (wb:canvas-draw-filled-polygon 
         bw (list p1 q1 p2 q2 p3 q3 p4 q4 p5 q5) :color color))))))
     

(defmethod description-string ((self point-symbol))
  (let ((name (or (label-of self) (viewed-object-description-string self))))
    (if name
    (format nil "~A viewing ~A" self name)
    (format nil "~A" self))))
    
             

(defmethod distance-to-location ((self point-symbol) viewport location)
  (let ((locn-c (if (region-p location)
                  (centre-of location) location)))
    (distance-from viewport locn-c)))


(defmethod style-menu-items ((self point-symbol))
  (let (( shape-change-list
          (loop for s in *point-symbol-types* 
                collect
                `( ,(string-downcase s)  (set-drawing-style :symbol ,s)))))
         `(( "Fill?" (set-drawing-style :fill? :toggle))
            ( "Shape" nil "" :sub-items ,shape-change-list)
            )))



(defmethod highlight-operation ((self point-symbol))
  :default
 )




(defmethod default-left-fn ((self point-symbol) &key viewport &allow-other-keys)
  (identify-view self :viewport viewport))

(defmethod shift-left-button-fn ((self point-symbol) 
                                 &key viewport )
  
  ;;(toggle-select-view self)
  (select-view self)
  (let ((s (viewed-object-description-string self))
        (parent (find-parent self :viewport viewport)))
    (if parent
      (setq s
            (format nil "~A: At ~A"
                         s (subview-location-string  parent self)
                         )))
    (quail-print s)))

(defmethod add-viewport ((self point-symbol) vp pvp
                         &key compute-transform?)
  (declare (ignore compute-transform?))
  (call-next-method self vp pvp :compute-transform? nil))
