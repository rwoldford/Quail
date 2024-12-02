;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               display-list.lisp
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
;;;----------------------------------------------------------------------------------


(in-package :views)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(display-list case-display-list variate-display-list sub-view-height
          draw-region-width draw-region-height
          display-length-of *display-items-limit* batch-display-list)))
          

(defclass display-list (scrollable-view-mixin compound-view)
  ((border :initarg :display-list-border :initform 2
           :accessor display-list-border-of)
   (subview-height :initarg :subview-height :initform nil
                   :accessor subview-height-of)
   (subview-width :initarg :subview-width :initform nil
                   :accessor subview-width-of)
   (middle-menu :allocation :class :initform nil)
   (style-keys :initform '(:font ) :allocation :class)
   )
   
  (:default-initargs :item-type 'label :items nil :default-tag "Item-" :font *default-label-font*))
                     

(defclass data-display-list (data-extract-mixin display-list)
  ()
  (:default-initargs 
    :labels #'identifier-name
    :initform-fn #'get-dataset-init 
    :item-type 'data-label :title-type 'label  ))

(defclass case-display-list (data-display-list)
  ()
  (:default-initargs 
    :default-tag "Case-"
      ))

(defclass batch-display-list (data-display-list)
  ((batches :initform nil :initarg :batches :accessor batches-of)
   (by-vars :initform nil :initarg :by :accessor by-vars-of)
   )
  (:default-initargs 
    :item-type 'group-label  
    :by :prompt
    :initform-fn #'get-batch-inits 
     :default-tag "Group-"))
                     
                     

(defclass variate-display-list (data-display-list)
  ()
  (:default-initargs 
    :labels #'variate-string-of
    :initform-fn #'get-dataset-init :default-tag "Var-"))
                     
(defmethod default-display-items-of ((self display-list))
  nil)


(defmethod default-display-items-of ((self case-display-list))
  (cases-of self))


(defmethod default-display-items-of ((self variate-display-list))
  (variates-of self))


(defmethod default-display-title-of ((self display-list))
  (dataset-name (viewed-object-of self)))


(defmethod default-display-title-of ((self case-display-list))
  (format nil "~A: Cases" (dataset-name (viewed-object-of self))))


(defmethod default-display-title-of ((self variate-display-list))
  (format nil "~A: Variates" (dataset-name (viewed-object-of self))))

(defmethod default-display-title-of ((self batch-display-list))
  (let ((b (by-vars-of self)))
    (unless (listp b)
      (setq b (list b)))
    (if b
      (format nil "~{~A~^ by ~}"  b)
      (dataset-name (viewed-object-of self)))))



(defgeneric draw-region-height (display-list)
  )

(defgeneric draw-region-width (display-list)
  )



(defmethod display-length-of ((self display-list))
  (length (subviews-of self)))



(defmethod draw-region-width ((self display-list))
  (+ (* 2 (display-list-border-of self)) (sub-view-width self)))

(defmethod draw-region-height ((self display-list))
  (+ (* 2 (display-list-border-of self)) (* (sub-view-height self)
     (display-length-of self))))

  

(defmethod make-draw-viewport ((self display-list) &optional title)
  (let* ((w (draw-region-width self))
         (h (draw-region-height self))
         (topi 40) (righti 5) (scroll 15)
         (win (make-view-window 
             :title title
             :left (max 0
                        (- (wb:screen-width) righti scroll w))
             :bottom (max 0 
                          (- (wb:screen-height) topi scroll h))
             :right (- (wb:screen-width) righti)
             :top (- (wb:screen-height) topi))))
    (make-viewport win)))

(defmethod sub-view-height ((self display-list) &optional sub)
  (or (subview-height-of self)
      (let ()
  (if (null sub)
    (setq sub (car (subviews-of self))))
  (if (and sub (typep sub 'label))
    (+  (* 2 (wb::canvas-font-leading  (draw-style sub :font )))
       (wb:canvas-font-height (draw-style sub :font )))
    20))))

(defmethod sub-view-width ((self display-list) &optional sub)
  (or (subview-width-of self)
      (let ()
  (if (and sub (typep sub 'label))
    (wb:canvas-string-width nil (get-text sub) :font (draw-style sub :font ))
    (let ((w
     (loop for s in (subviews-of self)
           when (and s (typep s 'label))
           maximize
           (wb:canvas-string-width nil
                                   (get-text s) :font (draw-style s :font )))))
      (if (or (null w) (zerop w)) 150
          w))))))


(defmethod compute-sub-viewports ((self display-list)
                                  &optional viewport new-start)
  ;(declare (ignore subviews)) ;25NOV2024
  (if (and new-start (listp new-start))
    (setq new-start (if (numberp (first new-start))
                      (first new-start) (second new-start))))

     (loop  with border = (display-list-border-of self)
            with subs = (subviews-of self)
            
        for vp in (if viewport (list viewport) (viewports-of self))
        for w = (window-of vp)
        for start = (or new-start (view-start self :viewport vp))
        do
        (multiple-value-bind (l r b tp) (bounds-of vp)
          (declare (ignorable b))
          (incf l border)
          (decf r border)
          (decf tp border)
          #|
          (loop with y = tp
                for i from (- start 1) downto 0
                for sub = (elt subs i)
                for sv-vp = (or (select-viewport sub vp) (make-viewport w))
                for h = (sub-view-height self sub)
                do (setf (bounds-of sv-vp) (list l r y (+ h y)))
                (incf y h)
                (add-viewport sub sv-vp vp))
     |#
          (loop with y = tp
                for i from (1- start) downto 0
                for sub = (elt subs i)
                for sv-vp = (or (select-viewport sub vp) (make-viewport w))
                for h = (sub-view-height self sub)
                do (setf (bounds-of sv-vp) (list l r y (+ y h) ))
                (incf y h)
                (add-viewport sub sv-vp vp))

          (loop with y = tp
                for i from start below (display-length-of self)
                for sub = (elt subs i)
                for sv-vp = (or (select-viewport sub vp) (make-viewport w))
                for h = (sub-view-height self sub)
                do (setf (bounds-of sv-vp) (list l r (- y h) y))
                (decf y h)
                (add-viewport sub sv-vp vp)))))


(defmethod construct-sub-views  ((self display-list) &rest args
                                 &key 
                                 items item-type  subviews color)
  (declare (ignore args))
  (setq items (or items (default-display-items-of self)))
  (let* ((sub-view-args (subview-arg-list item-type 'label))
         )
    (setq sub-view-args 
          (append sub-view-args 
                  (list :orientation :horizontal  :justification :left :color color :font
                        (draw-style self :font))))
    
    (setf (subviews-of self) (or subviews
                                 (if (listp items)
                                   (loop  for l in items
                                          collect
                                          (apply #'view   :text l    sub-view-args))
                                   )))))


(defmethod construct-sub-views  ((self data-display-list) &rest args
                                 &key 
                                 items title labels item-type title-type default-tag color)
  (declare (ignore args))
  
  (let* ( (sub-vo (or items (default-display-items-of self)))
         (title-string (if title
                         (princ-to-string
                          (or (and (not (eq t title)) title)
                              (default-display-title-of self)))))
         (sub-view-args (subview-arg-list item-type 'data-label))
         (get-label 
          (cond ((and labels (listp labels))
                 #'(lambda(i)
                     (nth i labels)))
                ((functionp labels)
                 #'(lambda(i)
                     #'(lambda(vo)
                         (let ((l (funcall labels vo)))
                           (or l
                               (format nil "~A~S" default-tag i))))))
                (t #'(lambda(i)
                       (declare (ignore i))
                       labels)))))
     (setq sub-view-args 
          (append sub-view-args 
                  (list :orientation :horizontal  :justification :left :color color :font
                        (draw-style self :font))))
    
    (setf (subviews-of self)
          (if (and sub-vo (listp sub-vo))
            (loop for vo in sub-vo
                  for i upfrom 0
                  for l = (funcall get-label i)
                  collect
                  (apply #'view   :text l :clip-label? t :data vo  sub-view-args))
            (if (listp labels)
              (loop  for l in labels
                     collect
                     (apply #'view   :text l    sub-view-args))
              )))
    (when title
      (setf (getf sub-view-args :type) title-type)
      (push (apply #'view :text title-string
                    :clip-label? t
                    :data (viewed-object-of self)
                   :draw? nil sub-view-args)
            (subviews-of self)))))


(defmethod construct-sub-views  ((self batch-display-list) &rest args
                                 &key 
                                  batches by labels  order-levels )
  (declare (ignorable labels)) ; 04SEP2023
  (if (or by batches)
    (let (( subsets (remove nil (get-batches :by by :batches batches :dataset (dataset-of self) 
                                             :cases (cases-of self)
                                             :order-levels order-levels))))
      (apply #'call-next-method self :items (remove nil subsets)
                     args))
    
    (apply #'call-next-method self  args)))










(defmethod max-view-start ((self display-list) &key viewport axis)
  (declare (ignore axis))
  (if viewport 
    (let ((len (loop for s in (subviews-of self)
                     for vp = (select-viewport s viewport)
                     count (active-viewport-p vp ) )))
      (- (display-length-of self) len))                    
    (display-length-of self)))


(defmethod view-start ((self display-list) &key viewport axis)
  (declare (ignore axis))
  (if viewport 
    (loop for s in (subviews-of self)
                     for vp = (select-viewport s viewport)
                     until (or (null vp) (not (active-viewport-p vp )))
                     count (active-viewport-p vp ) ))               
    0)
(defvar *display-items-limit* 6
  "When the number of items in a display list exceeds~
   this value, the default is to add a right scroller~
   to the display")

(defun display-list (&rest args &key (display-type 'display-list)
                           (scrollable? :default) (draw? nil)
                           &allow-other-keys)
  (let (( display 
          (apply #'make-instance display-type :draw? nil args)))
    (if (eq scrollable? :default)
      (setq scrollable? 
            (> (display-length-of display) *display-items-limit*)))
    
    (if scrollable?
      (setq display
            (scrolling-display :draw? draw?
                               :data (viewed-object-of display)
                               :bottom-scroller nil
                               :display display))
      (if draw? (draw-view display)))
    display))

(defmethod style-menu-items ((self display-list))
  (font-menu-items))

(defmethod styles-to-subs ((self display-list) ) 
  (list :font))

(defun case-display-list (&rest args)
  (apply #'display-list :display-type 'case-display-list args))

(defun variate-display-list(&rest args)
  (apply #'display-list :display-type 'variate-display-list args))

(defun batch-display-list(&rest args)
  (apply #'display-list :display-type 'batch-display-list args))


    


  
