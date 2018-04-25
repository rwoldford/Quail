;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               draw-macros.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(with-exposed-viewports enlist-viewport 
             with-bordered-plot-subs with-update-style-cache)))

(defmacro with-point-symbol-center&radius
          ( point-sym vp xc yc rad  &body do-forms)
  (let ((c (gensym)) (s (gensym)))
    `(let* ((,s (draw-style  ,point-sym :size))
            (,c (centre-of ,vp)))
       (setq ,rad (truncate ,s 2))
       (setq ,xc (2d-position-x ,c))
       (setq ,yc (2d-position-y ,c))
       ,@do-forms)))




(defmacro with-point-symbol-bounds
          (point-sym vp   xmin xmax ymin ymax  &body do-forms)
  
  (let ((c (gensym "c"))
        (xc (gensym "xc")) 
        (yc (gensym "yc"))
        (size (gensym "size"))
        (rad (gensym "rad")))
    `(let* ((,size (draw-style  ,point-sym :size))
            (,c (centre-of ,vp))
            (,xc (2d-position-x ,c))
            (,yc (2d-position-y ,c))
            (,rad (truncate ,size 2)))
       (setq ,xmin (- ,xc ,rad))
       (setq ,xmax (+ ,xmin ,size))
       (setq ,ymin (- ,yc ,rad))
       (setq ,ymax (+ ,ymin ,size))
       ,@do-forms)))








(defmacro enlist-viewport (view viewport)
  `(if ,viewport
      (if (listp ,viewport) 
        ,viewport
        (list ,viewport))
      (viewports-of ,view)))
#|
(defmacro with-exposed-viewports ( view viewport vp &body do-forms)
  `(dolist (,vp (mapcar #'(lambda(x) (get-draw-portion ,view x))
                        (if ,viewport
                          (if (listp ,viewport) 
                            ,viewport
                            (list ,viewport))
                          (viewports-of ,view))))
    (when (active-viewport-p ,vp)
      ,@do-forms)))

|#
(defmacro with-exposed-viewports ( view viewport vp &body do-forms)
  `(if (and ,viewport (not (listp ,viewport)))
     (if (active-viewport-p ,viewport)
       (let ((,vp (get-draw-portion ,view ,viewport)))
         ,@do-forms))
  (dolist (,vp (mapcar #'(lambda(x) (get-draw-portion ,view x))
                        (or ,viewport
                          (viewports-of ,view))))
    (when (active-viewport-p ,vp)
      ,@do-forms))))

(defmacro with-bordered-plot-subs 
          (plot interior-view
          left-view right-view bottom-view top-view 
          left-label right-label bottom-label top-label &body do-forms)
  `(let ((,left-view     (left-views-of     ,plot))
         (,right-view    (right-views-of    ,plot))
         (,top-view      (top-views-of      ,plot))
         (,bottom-view   (bottom-views-of   ,plot))
         (,interior-view (interior-views-of ,plot))
         (,left-label    (left-labels-of    ,plot))
         (,right-label   (right-labels-of   ,plot))
         (,top-label     (top-labels-of     ,plot))
         (,bottom-label  (bottom-labels-of  ,plot)) )
     ,@do-forms)) 
         
(defvar *update-style-cache* nil)
(defvar *cache-updates?* nil)

(setq *update-style-cache* nil)
(setq *cache-updates?* nil)


(defmacro optional-cache(form)
  "When *cache-updates?* is non nil, certain calls to ~
   draw-view, erase-view, highlight-view and downlight-view ~
   are cached, rather than being performed right away. "
  (let((oldpair (gensym "opt-cache-oldpair"))
       (old (gensym "opt-cache-old"))
       (new (gensym "opt-cache-new"))
       (view (gensym "opt-cache-view")))
    `(let()
       (declare (special *update-style-cache* *cache-updates?*))
       (if *cache-updates?*
         (let ((,view ,(second form)))
                (when (active-viewports-p ,view)
                  (let* ( (,oldpair (assoc ,view *update-style-cache*))
                          (,old (cdr ,oldpair))
                          (,new (function ,(first form))))
                    (cond ((null ,oldpair)
                           (setq *update-style-cache* 
                                 (acons ,view ,new *update-style-cache*)))
                          ((null ,old)
                           (setf (cdr ,oldpair) ,new))
                          ((eql ,new ,old)
                           nil)
                          ((eql ,old #'erase-view)
                           (setf (cdr ,oldpair) #'draw-view))
                          ((eql ,new #'erase-view)
                           (setf (cdr ,oldpair) #'erase-view))
                          ((eql ,old #'draw-view)
                           nil)
                          ((eql ,new #'draw-view)
                           (setf (cdr ,oldpair) #'draw-view))
                          
                         ;; ((not (typep ,view 'multiple-draw-style-mixin))
                         ;;  (setf (cdr ,oldpair) nil))
                          (t (setf (cdr ,oldpair) #'draw-view)))
                    nil)))
         ,form))))

(defmacro with-update-style-cache(&body forms)
  "Certain calls to draw-view, erase-view, ~
   highlight-view and downlight-view within forms~
   are delayed. This means that multiple draws on a  ~
   view are effected with a single draw."
  `(let()
     (declare (special *update-style-cache* *cache-updates?*))
     (if (null *cache-updates?*)
       (progn
         (unwind-protect
           (progn
             (setq *cache-updates?* t)
             ,@forms
             (loop for (view . action) in *update-style-cache*
                   when action do
                   ;; safer to put downlight in here because of the boole-xor drawing
                   (if (or (eql action #'draw-view) 
                           (and
                            (or (eql action #'downlight-view)
                                (eql action #'highlight-view))
                            (eql (highlight-operation view) :boole-xor)))
                     (draw-view view :erase? t)
                     (funcall action view))))
           
           (setq *cache-updates?* nil)
           (setq *update-style-cache* nil)))
       (progn
         ,@forms))))



            
