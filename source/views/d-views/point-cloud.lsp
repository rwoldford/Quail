;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               point-cloud.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(3d-one-per-case 2d-one-per-case 1d-one-per-case 
           2d-point-cloud 1d-point-cloud set-lines-to) ))
;;;----------------------------------------------------------------------------------

(defun choose-simple-view-from-menu(&key (prompt-string "Select a simple view class"))
  "Gives the user a list of simple view classes to choose from. "
   (caar (wb:prompt-for-items *simple-view-menu-list* :prompt-text prompt-string
                                        :item-function #'first)))

                    
(defclass 3d-one-per-case (one-per-case-mixin 3d-view  compound-view)
  ()
  (:default-initargs :case-view (choose-simple-view-from-menu)))


(defclass 2d-one-per-case (edge-lines-mixin one-per-case-mixin 2d-view  compound-view)
  ()
  (:default-initargs :case-view (choose-simple-view-from-menu)))

(defclass 1d-one-per-case (edge-lines-mixin one-per-case-mixin 1d-view compound-view)
  ()
  (:default-initargs :case-view (choose-simple-view-from-menu)))



(defclass 2d-point-cloud ( 2d-one-per-case)
  () 
  (:default-initargs :case-view 'point-symbol))

(defclass 1d-point-cloud (1d-one-per-case)
  () 
  (:default-initargs :case-view 'point-symbol))

(defclass edge-lines-mixin() 
  ( (lines-to :initarg :lines-to :initform nil :accessor lines-to)
    (middle-menu :allocation :class :initform nil)
    (style-keys :initform '(:size :fill? :font :color
                            :ecolor :edashing :ewidth ) :allocation :class))
  (:default-initargs :ecolor wb:*light-grey-color* :ewidth 1 :edashing '(3 . 1)))


;;;----------------------------------------------------------------------------------
(defmethod legal-lines-to ((self edge-lines-mixin))
  (list :none :left-right :top-bottom :left :right :top :bottom :x :y :prompt))

(defmethod set-lines-to ((self edge-lines-mixin) dir &key &allow-other-keys)
  
  (erase-view self)
  (if (eql dir :none) (setq dir nil))
  (if (eql dir :prompt) (setq dir
                              (choose-variable  nil 1 "Choose variable" 
                                          (loop with vars = (menu-variates-of self)
                                                for v in vars
                                                for vn in (variate-names vars)
                                                collect (list v vn)))))


  (setf (lines-to self) dir)
  (draw-view self))

(defmethod style-menu-items  ((self edge-lines-mixin))
  (declare (special  *color-menu-list* *shade-menu-list*))
  `(( "Edge lines" nil "" :sub-items
                        ,(append
                          `(("Draw" nil "" :sub-items ,(loop for dir in (legal-lines-to self)
                                                            collect `(,(string dir) (set-lines-to ,dir))))
                            ("Edge line Color" nil "" :sub-items 
                             ,(loop for c in 
                                   (if (wb:color-device-p)
                                     *color-menu-list*
                                     *shade-menu-list*)
                                   collect 
                                   (list (car c) `(set-drawing-style :ecolor ,(cadr c)))))
                            ("Edge line Width" nil "" :sub-items 
                             (("fatter" (set-drawing-style :ewidth :fatter))
                               ("thinner" (set-drawing-style :ewidth :thinner))
                               ("prompt" (set-drawing-style :ewidth :prompt))))
                            )
                          ))))

(defmethod update-menu-items :after ((self edge-lines-mixin) 
                                     (slot-name (eql 'middle-menu)))
  (let* ((m (slot-value self slot-name)))
  
  (wb:check-menu-item  m "Edge lines" (lines-to self))
  (if (lines-to self)
    (progn  (wb:enable-menu-item  m "Edge line Color")
            (wb:enable-menu-item  m "Edge line Width"))
    (progn  (wb:disable-menu-item  m "Edge line Color")
            (wb:disable-menu-item  m "Edge line Width")))
  ))
(defmethod adjusted-bounding-region ((self one-per-case-mixin) )
  
  (let* ((br (copy-region (smallest-bounding-region self))))
        (scale-sides-by br 1.1 1.1)))









(defmethod bounds-of-selected ((self 1d-one-per-case))
  (let ((selected-vos (mapcar #'viewed-object-of
                              (some-subviews self :highlit? t) )))
    (if selected-vos
      (loop  for vo in selected-vos
             for x  in (plot-coords-of self :cases selected-vos)
             when (eq (select-case-status self vo) t)
             minimize x into x-min maximize x into x-max 
             finally (if (= x-min x-max)
                       (incf x-max 0.01))
             (return (if (eq (orientation-of self) :horizontal)
                       (make-region x-min x-max 0 1)
                       (make-region 0 1 x-min x-max))))
      )))

 


(defmethod new-bounds ((self 1d-one-per-case) &key (region :compute) pretty?)
  (unless (region-p region)
    (setq region
          (case region
            (:prompt
             (apply #'make-region
                    (wb:prompt-user :result-type 'list :read-type :read
                                    :prompt-string "(min max)")))
            (:original (original-bounds self :draw? t) nil)
            (:compute (bounds-of-selected self))
            (t NIL))))
  (if (region-p region) 
    (if (eq (orientation-of self) :horizontal)
      (change-bounding-region self region :pretty? pretty? :ignore-y? t)
      (change-bounding-region self region :pretty? pretty? :ignore-x? t))))

(defmethod construct-sub-views :after ((self 1d-one-per-case) &rest initargs)
  (declare (ignore initargs))
  (loop with o = (if (eq (orientation-of self) :horizontal)
                   :vertical
                   :horizontal)
        for s in (subviews-of self)
        when (typep s 'orientation-mixin)
        do (set-orientation   s o)))
                                


(defmethod init-position-subviews ((self 1d-one-per-case) &key )
  (let* ((br (subview-position-region self))
          ;;(br-centre (centre-of br))
        ;; (x0 (2d-position-x br-centre))
        ;; (y0 (2d-position-y br-centre))
         (wid/2 (/ (width-of br)  20))
         (ht/2 (/ (height-of br)  20))
         (coords (plot-coords-of self))
         (status (case-status-of self))
         )  
         
   (setf (sub-view-locns-of self)
          (if (eq (orientation-of self) :horizontal)
            (loop 
              for x in coords
              for s in status
               for region = (cond ((invalid-status-p s)
                                  (make-region))
                                 (t
                                  (make-region (- x wid/2) (+ x wid/2)
                                               (bottom-of br) (top-of br)))
                                 
                                 )
              collect region)
            (loop 
              for y in coords
              for s in status
               
              for region = (cond ((invalid-status-p s)
                                  (make-region))
                                 (t
                                  (make-region (left-of br) (right-of br)
                                               (- y ht/2) (+ y ht/2)))
                                 
                                 )
              collect region)))))



(defmethod compute-sub-viewports ((self 1d-one-per-case)
                                  &optional viewport subviews)
  (let ((viewports (if viewport (list viewport) (viewports-of self)))
        (maps (if viewport (list (select-map-to-viewport self viewport))
                  (maps-to-viewports-of self)))
        (dir (orientation-of self))
        (subview-locns 
         (if subviews 
           (loop for s in subviews  collect 
                 (select-sub-view-locn self s))
           (sub-view-locns-of self)))
        (status
         (if subviews
           (loop for s in subviews collect (select-subview-status self s))
           (case-status-of self)))
        sv-vp)
    (setq subviews (or subviews (subviews-of self)))
    (loop with left and right and bottom and top
          for sv in subviews
          for svl in subview-locns 
          for s in status
          for sv-rad  = (if (typep sv 'view-with-size)
                          (view-size-of sv) 5)
          do
          (loop with lab-r 
                for vp in viewports for map in maps
                for xy = (apply-transform map (centre-of svl))
                for x = (2d-position-x xy)
                 for y = (2d-position-y xy)
                 for vp-win = (window-of vp)
      
                do
                (setq left (left-of vp)
                      right (right-of vp)
                      bottom (bottom-of vp)
                      top (top-of vp))
                
                (cond ((and (typep sv 'label) (eq dir :horizontal))
                       (setq lab-r (or lab-r
                                       (max 8 (/ (width-of vp) (length subviews)))))
                       (setq left (truncate (- x (/ lab-r 2))))
                       (setq right (ceiling (+ left lab-r))))
                      
                      ((typep sv 'label)
                       (setq lab-r (or lab-r
                                       (max 8 (/ (height-of vp) (length subviews)))))
                       (setq bottom (truncate (- y (/ lab-r 2))))
                       (setq top (ceiling (+ bottom lab-r))))
                      
                      ((and (typep sv 'oriented-line) (eq dir :horizontal))
                       (setq left (- x sv-rad) right (+ x sv-rad))
                       )
                      
                      ((typep sv 'oriented-line)
                       (setq bottom (- y sv-rad) top (+ y sv-rad)))
                      (t 
                       (setq left (- x sv-rad)
                             right (+ x sv-rad)
                             bottom (- y sv-rad)
                             top (+ y sv-rad))))
                (setq sv-vp (make-viewport vp-win left right bottom top))
                (unless (active-status-p s) (deactivate-viewport sv-vp))
                (add-viewport sv sv-vp vp)))))

          






#|

There are small problems with the following.
In fast-graphics, colors get changed w/o changing the pen,
so wb:with-pen-values-restored does not work as expected.
Also all nil colors should be replaced by default canvas color.


(defmethod point-styles ((self point-cloud)  subs style &optional default)
  (loop for sv in subs
        collect (draw-style (drawing-style-of sv) style :default default)))

(defmethod draw-view ((self point-cloud) &key viewport highlit?) 
  
  (let* ((subs (some-subviews self :highlit? highlit?))
         (fill? (point-styles self subs :fill?))
         (colors (point-styles self subs :color))
         (symbols (point-styles self subs :symbol))
         (invisible? (point-styles self subs :invisible?))
         (sizes (point-styles self subs :size)))
    (loop for vp in (enlist-viewport self viewport)
          for points = (loop for sv in subs
                             for sv-c = (centre-of (select-viewport sv vp))
                             collect (list (2d-position-x sv-c) (2d-position-y sv-c)))
          do 
          
          (wb:with-pen-values-restored (window-of vp)
            (wb::canvas-draw-symbols (window-of vp) points
                                     :size sizes :symbol symbols :invisible? invisible?
                                     :color colors :fill? fill?)
            (wb::set-pen-color (window-of vp) (car (last colors)))
            ))))



 
|#

(defmethod draw-view :before ((self edge-lines-mixin) &key viewport highlit?) 
  (when (lines-to self)
    (draw-edge-lines self :viewport viewport :highlit? highlit?)))

(defmethod erase-view :before ((self edge-lines-mixin) &key viewport highlit?) 
  (when (lines-to self)
    (draw-edge-lines self :viewport viewport :highlit? highlit? :operation :boole-andc1)))


(defmethod draw-edge-lines  ((self edge-lines-mixin) &key viewport highlit? (operation :default)) 
  (let* ((lt (lines-to self))
         (subs (some-subviews self :highlit? highlit?))
         (status (if (eq subs (subviews-of self))
                   (case-status-of self)
                   (loop for s in subs collect (select-subview-status self s))))
         (col (draw-style self :ecolor))
         (wid (draw-style self :ewidth))
         (dash (draw-style self :edashing))
         xy
         win xmin xmax ymin ymax )
    
    (with-exposed-viewports self viewport vp 
      (setq win (window-of vp))
      (multiple-value-setq (xmin xmax ymin ymax) (bounds-of vp))
      (setq xy
            (loop for sv in subs
                  for s in status
                  for sv-vp = (select-viewport sv vp)
                  for xy = (centre-of sv-vp)
                  when (active-status-p s) 
                  collect (list (2d-position-x xy) (2d-position-y xy))))
      (wb:with-pen-values win col wid operation
        
        
        
        (case lt
          (:left 
           (loop for (x y) in xy do
                 (canvas-draw-dashed-line win xmin y x y dash)))
          (:right (loop for (x y) in xy do
                        (canvas-draw-dashed-line win xmax y x y dash )))
          (:bottom (loop for (x y) in xy do
                         (canvas-draw-dashed-line win x ymin x y dash )))
          (:top (loop for (x y) in xy do
                      (canvas-draw-dashed-line win x ymax x y  dash)))
          (:left-right (loop for (x y) in xy do
                             (canvas-draw-dashed-line win xmax y xmin y dash)))
          (:top-bottom (loop for (x y) in xy do
                             (canvas-draw-dashed-line win x ymax x ymin dash)))
          (:x (loop with sortxy = (sort xy #'< :key #'first)
                    for (x1 y1) in sortxy by #'cdr
                    for  (x2 y2) in (cdr sortxy) by #'cdr do
                    (canvas-draw-dashed-line win x1 y1 x2 y2 dash)))
          (:y (loop with sortxy = (sort  xy #'< :key #'second)
                    for (x1 y1) in sortxy by #'cdr
                    for  (x2 y2) in (cdr sortxy) by #'cdr do
                    
                    (canvas-draw-dashed-line win x1 y1 x2 y2 dash)))
          (t
           (let ((vals (loop with f = (case-access-fn self  lt)
                             for v in (cases-of self)
                             for s in status
                             when (active-status-p s) 
                             collect (funcall f v))))
             (when (every #'numberp vals)
               (setq vals (mapcar #'cons vals xy))
               (loop with sortv = (sort vals #'< :key #'first)
                     for (v1 x1 y1) in sortv by #'cdr
                     for  (v2 x2 y2) in (cdr sortv) by #'cdr do
                     ;;(declare (ignore v1 v2))
                     
                     (canvas-draw-dashed-line win x1 y1 x2 y2 dash))
               
               ))
           ))))))


(defun canvas-draw-dashed-line(canvas x1 y1 x2  y2 dash)
  (if dash
    (let ((dash-len (first dash))
          (dash-gap (+ 1 (cdr dash))))
  (cond ((= y1 y2)
         (loop for x from (min x1 x2) to (- (max x1 x2) dash-len) by (+ dash-len dash-gap)
               do
               (wb:canvas-draw-line canvas x y1 (+ x dash-len) y1 )))
        ((= x1 x2)
         (loop for y from (min y1 y2) to (- (max y1 y2) dash-len) by (+ dash-len dash-gap)
               do
               (wb:canvas-draw-line canvas x1 y x1 (+ y dash-len))))
        (t (let* ((d (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))))
                  (c (/ (- x2 x1) d))
                  (s (/ (- y2 y1) d))
                  (lx (* c dash-len)) (ly (* s dash-len))
                  (gx (* c dash-gap)) (gy (* s dash-gap)))
             (loop with x0 = x1 and  y0 = y1
                   for i upfrom 1
                   for x = (truncate (+ x1 (* i lx) (* (- i 1) gx) ))
                   for y = (truncate (+ y1 (* i ly) (* (- i 1) gy)))
                   while (and (or (<= x1 x x2) (<= x2 x x1))
                              (or (<= y1 y y2) (<= y2 y y1))) do
                           (wb:canvas-draw-line canvas x0 y0 x y) 
                           (setq y0 (truncate (+ y1 (* i ly) (* i gy) )) )
                           (setq x0 (truncate (+ x1 (* i lx) (* i gx) )) ))))))
               
  (wb:canvas-draw-line canvas x1  y1 x2 y2 )))
  
         
 (defmethod new-style-value ((style-name (eql :ewidth)) new pair)
  (if (numberp new) 
    new
    (case new
      (:fatter (1+ (cdr pair)))
      (:thinner (1- (cdr pair)))
      (:prompt (wb:prompt-user :result-type 'number 
                                       :read-type :eval
                                       :prompt-string 
                                       (format nil "Change size from ~A" (cdr pair))))
      (t (cdr pair)))))
     
   

(defmethod new-style-value ((style-name (eql :ecolor)) new pair)
  (declare (ignore pair))
  (if (eq new :prompt)
    (wb:prompt-user-for-color)
    new))
  
