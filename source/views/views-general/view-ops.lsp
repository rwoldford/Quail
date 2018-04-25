;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               view-ops.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(fill-viewport which-viewport new-bounds switch-drawing-color
            draw-viewport viewport-of descendant-views-of-type)))

(defmethod draw-view ((self view)
                      &key viewport)
  (if (and viewport (not (listp viewport)))
    (loop for sv in (subviews-of self)
          for in? in (visible-subs-p self)
          when in?
          do
          (draw-view sv 
                     :check-viewport? nil
                     :viewport (select-viewport sv viewport)
                     :erase? nil))
    
    (loop for vp in (or viewport (viewports-of self)) do
          (loop for sv in (subviews-of self)
                for in? in (visible-subs-p self)
                when in?
                do
                (draw-view sv 
                           :check-viewport? nil
                           :viewport (select-viewport sv vp)
                           :erase? nil)))))





(defmethod draw-view :around ((self view) &key viewport (erase? nil) (highlight? t) (check-viewport? t))
  (when check-viewport?
    (if  (not (or viewport (viewports-of self)))
      (setq viewport (make-draw-viewport self (view-window-title self))))
    (when  (and viewport
                (not (member viewport (viewports-of self))))
      (add-view-to-window  (window-of viewport) self 
                           :draw? nil :viewport viewport)))
  
  (if erase?  (erase-view self :viewport viewport))
  
  (unless (draw-style self :invisible?)
    (call-next-method ))
  
  (when (and highlight? (any-highlight? self) (not (eql (highlight-operation self) :draw)))
     (highlight-view self :viewport viewport)))


#|
(defmethod draw-view :before ((self view) 
                              &key viewport (erase? nil) highlit?)
  (if  (not (or viewport (viewports-of self)))
    
    (setq viewport (make-draw-viewport self (view-window-title self))))
  (when  (and viewport
              (not (member viewport (viewports-of self))))
    (add-view-to-window  (window-of viewport) self 
                         :draw? nil :viewport viewport))
  (if erase?  (erase-view self :viewport viewport
                          :highlit? highlit?)))


(defmethod draw-view :around ((self view) &key viewport)
  (declare (ignore viewport))
  (unless (draw-style self :invisible?)
    (call-next-method )))


(defmethod draw-view :after ((self view)  &key  viewport (highlight? t))
  
  ;; ensures correct highlighting when self  is drawn
  
  (when (and highlight? (any-highlight? self))
    
    (highlight-view self :viewport viewport)
    ))
|#
(defvar *view-window-title-counts* nil)



(defun view-window-count(class-sym)
  (declare (special *view-window-title-counts*))
  (let ((x (assoc class-sym *view-window-title-counts* )))
    (if x
      (incf (cdr x) 1)
      (progn
        (setf *view-window-title-counts*
              (acons class-sym 0  *view-window-title-counts*))
        0))))


(defmethod view-window-title((self view))
  (let ((c (class-of self)))
    (format nil "~S ~S" (class-name c) 
            (view-window-count c ))))
  



(defmethod clear-view-viewport ((self view) 
                       &key viewport)
  
  ;; erases the view in VIEWPORT, if supplied, else erases  in all exposed
  ;; viewports
  
  (with-exposed-viewports self viewport vp
    (let* ((w (window-of vp))
           rl rb rw rh)
      (when vp
        (setq rl   (left-of vp))
        (setq rb  (bottom-of vp))
        (setq rw (width-of vp))
        (setq rh (height-of vp))
        (wb:canvas-clear  w
                          :canvas-left rl :canvas-bottom rb
                          :width rw :height rh)))))





(defmethod erase-view ((self view) 
                       &key viewport)
  
  ;; erases the view in VIEWPORT, if supplied, else erases  in all exposed
  ;; viewports
  
  (clear-view-viewport self :viewport viewport))

(defmethod erase-view :around ((self view)  &key viewport ) 
  (declare (ignore viewport))
  (if (viewports-of self) (call-next-method)))

(defmethod find-parent ((self view) &key viewport)
  ;; returns the parent and its viewport of self
  
  (setq viewport (or viewport (car (viewports-of self))))
  (let ((tl-view (which-toplevel-view (window-of viewport) viewport ))
        (parent-viewport (select-parent-viewport self viewport))
        p )
    (if tl-view
      (labels ((whois-parent 
                   (ancestor view)
                 (cond ((eql view ancestor) nil)
                       ((and (member view (subviews-of ancestor))
                             (member parent-viewport (viewports-of ancestor)))
                        ancestor)
                       (t (loop for v in (subviews-of ancestor)
                                thereis (whois-parent v view))))))
        (setq p (whois-parent tl-view self))
        (if p 
          (values p parent-viewport))))))



(defmethod remove-view ((self view) &key viewport (save? t ))
  
  ;; removes the view (permanently) from viewport 
  ;; if  viewport  is not provided, view is removed everywhere
  ;; note, surgery is performed on the view hierarchy
  
  (loop for vp in (or (list viewport) (viewports-of self))
        for vparent = (find-parent self :viewport vp )
        do
        (if vparent 
          (remove-subview vparent self)
          (progn
            (erase-view self :viewport viewport)
            (delete-nested-viewports self vp)
            (remove-view-from-window (window-of vp) :viewport vp))))
  (if save? (setf *selected-views* (push self *selected-views* ))))



(defmethod remove-subview ((self view) subview )
  
  ;; removes the subview (permanently) from self
  ;; redrawing self everywhere
  ;; note, surgery is performed on the view hierarchy
  
  (when (member subview (subviews-of self))
    (loop for vp in (viewports-of self)
          for vp-sub = (select-viewport subview vp) 
          do
          (erase-view subview :viewport vp-sub)
          (delete-nested-viewports subview vp-sub))
    (delete-subview self subview)))


(defmethod invert-view ((self view )   
                        &key viewport)
  
  (with-exposed-viewports self viewport 
    vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (wb:canvas-invert (window-of vp) :canvas-left l
                        :canvas-bottom b  :width (+ 1 (- r l ) )
                        :height (+ 1 (- tp b))))
    ))

(defmethod highlight-operation ((self view))
  (declare (special *default-highlight-operation*))
  *default-highlight-operation*)

(defmethod highlight-view ((self view)
                           &key viewport operation (color *default-highlight-color* )) 
  
  (setq operation (or operation (highlight-operation self)))
  (with-exposed-viewports self viewport
    vp
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (wb:canvas-highlight-rectangle (window-of vp) l r b tp  :color color
                          :operation operation))))
                         



(defmethod highlight-view :around ((self view)  &key viewport operation) 
  (declare (ignore viewport operation))
  (if (viewports-of self) (call-next-method)))


(defmethod downlight-view ((self view)
                           &key viewport operation) 
  (setq operation (or operation (highlight-operation self)))
  (if (eql operation :boole-xor)
    (highlight-view self :viewport viewport :operation operation)
    (if (draw-style self :invisible?)
      (erase-view self :viewport viewport)
      (draw-view self :viewport viewport :erase? t))))                        


(defmethod downlight-view :around ((self view)  &key viewport operation) 
  (declare (ignore viewport operation))
  (if (viewports-of self) (call-next-method)))

(defmethod selected-p ((self view) viewport location)
  ;; is view selected with mouse at location?
  ;; default looks to see if location is in viewport
  (and (active-viewport-p viewport) (contains-p viewport location)))



(defmethod reshape-viewport ((self view) viewport  
                             &key new-location transform (draw? nil))

  ;; only one of transform or new-location should be provided
  
  (if (and  transform (null new-location) )
    (progn
      (setq new-location (apply-transform transform viewport))
      (setq transform (transform-mapping-draw-portions self viewport transform)) )
    (if (and new-location   (null transform) )
      (setq transform (make-transform-for-regions 
                       (get-draw-portion self viewport) 
                       (get-draw-portion self new-location)))))

  (if draw? (erase-view self :viewport viewport))              
  (let ((map (select-map-to-viewport self viewport)))
    (if map (premultiply-transform! map transform))
    (setf (bounds-of viewport) (multiple-value-list (bounds-of new-location)))
    (reshape-sub-viewports self viewport :new-location new-location :transform transform)
    )
  
  (if draw? (draw-view self :viewport viewport :erase? nil)))


(defmethod reshape-sub-viewports ((self view) viewport  
                                  &key new-location transform )
  (declare (ignore new-location))
  
  
  (loop for sv in (subviews-of self)
        for vp = (select-viewport sv viewport)
        do 
        (reshape-viewport sv vp
                          :transform transform :draw? nil )))



(defmethod distance-to-location ((self view) viewport location)
  (if (selected-p self viewport location)
    (radius-of viewport) 10000))

(defun which-viewport (view)  
  "which viewport of the view if any, contains the mouse"
  (let* ((click-pos (make-2d-position (wb:screen-mouse-x) (wb:screen-mouse-y)))
        (w (which-window click-pos)))
    (loop for vp in (viewports-of view)
          until (eql w (window-of vp))
          finally (return vp))))

(defmethod which-subview ((self view) 
                          viewport location)
  ;; returns the view, its viewport its parent and the parent's viewport
  ;; at location
  
  (if (selected-p self viewport location)
    (let (v-sub v-parent vp-sub vp-parent candidate-subs c-best)
      
      (if (subviews-of self)
        (progn
          (loop 
                for sv in (subviews-of self) 
                for sv-vp = (select-viewport sv viewport)
                for vis in (visible-subs-p self)
                when vis
                 do
                (multiple-value-setq (v-sub vp-sub v-parent vp-parent )
                  (which-subview sv sv-vp location))
                
                (when v-sub
                  (setq v-parent (or v-parent 
                                     (if (eql sv v-sub) self sv)))
                  (setq vp-parent (or vp-parent 
                                      (if (eql sv v-sub) viewport sv-vp)))
                  (push (list v-sub vp-sub v-parent vp-parent )
                        candidate-subs)))
          
          (loop with dmin = 100000 with d 
                for c in candidate-subs
                for sub = (car c)
                when sub do
                (setq d (distance-to-location sub (second c) location))
                (if (< d dmin) (setq dmin d c-best (append c (list d))))
          )))
      (if c-best (values-list c-best)  
          (values self viewport nil nil 
                  (distance-to-location self viewport location) ))) ; end let
    ))



(defmethod remap-to-viewports ((self view) &key (erase? t) (draw? t) viewport)
  ;; recomputes viewports and redraws
  
  (loop for vp in (if viewport (list viewport) (viewports-of self)) do
        (if erase? (erase-view self :viewport vp))
        (map-to-viewport self vp)
        (if draw? (draw-view self :viewport vp))))

(defmethod remap-sub-to-viewports ((self view) subview &key (erase? t) (draw? t))
  (loop for vp in (viewports-of self) 
        for vp-sv = (select-viewport subview vp)
         do
        (if erase? 
          (erase-view subview :viewport vp-sv))
        (map-subviews-to-viewport   self vp (list subview))
        (if draw?
          (draw-view subview :viewport (select-viewport subview vp)  :erase? nil))))



(defmethod move-subview ((self view) subview new-region)
  ;; moves subview to new-region (in coords of self)
  ;; note: new-region must be contained in the bounding-region
  ;; of self
  
  (when (and (member subview (subviews-of self))
             (contains-p (bounding-region-of self) new-region))
    (place-subview self subview new-region)
    (remap-sub-to-viewports self subview :erase? t)))

(defmethod add-subview ((self view) subview new-region)
  
  (if (member subview (subviews-of self))
    (move-subview self subview new-region)
    (when (legal-subview-p self subview)
      (place-subview self subview new-region)
      (remap-sub-to-viewports self subview :erase? nil))))


(defmethod move-view ((self view) &key viewport 
                      new-location )
  
  ;; moves self to new-location (in coords of viewport)
  ;; note: new-location must be contained in the viewport
  ;; of self's parent
  ;; moves self within the viewport of parent
  
  (setq viewport (or viewport (car (viewports-of self))))
  (let ((win (window-of viewport))
        (d 10)
        axis
        parent parent-viewport)
    
    (multiple-value-setq (parent parent-viewport)
      (find-parent self  :viewport viewport))
    (setq axis 
          (if parent
            (allowed-subview-movement parent self)
            :both))
    
    (unless (eq axis :none)
      
      (if (and (null new-location) (not (wb:mouse-down-p)))
        ;; called on next mouse left button
        (progn
          (wb:draw-rect&corners win (wb-region viewport) :corner-width d :color wb:*gray-color*)
          (temporary-left-fn self (list #'move-view :viewport)  viewport))
        (progn
          (setq new-location
                (or new-location
                    
                    (let ((limit (if parent-viewport (wb-region parent-viewport)
                                     (wb:canvas-region win)) )
                          (wb-region (wb-region viewport))
                          rl rb rw rh)
                      (if (> (max (width-of viewport) (height-of viewport)) d)
                        (multiple-value-setq 
                          (rl rb rw rh)
                          (wb:reshape-canvas-rect win wb-region 
                                                  :limit-region limit 
                                                  :draw? nil
                                                  :corner-width d)))
                      (wb:erase-rect&corners win (wb-region viewport) :corner-width d)
                      
                      (if rl
                        (make-region rl (+ rl rw) rb (+ rb rh))
                        (drag-region  viewport 
                                      :window (window-of viewport)
                                      :limit parent-viewport
                                      :axis axis)))))
          
          
          (when new-location
            (unless (eq axis :both)
              (setq new-location
                    (if (eq axis :x)
                      (make-region (left-of new-location) (right-of new-location) 
                                   (bottom-of viewport) (top-of viewport))
                      (make-region (left-of viewport) (right-of viewport) 
                                   (bottom-of new-location) (top-of new-location)))))
            
            (if  parent 
              ;; obtain new-location in coords of parent
              (let ((locn-in-parent (map-region new-location parent-viewport 
                                                (bounding-region-of parent))))
                (move-subview parent self locn-in-parent))
              ;;else
              ;;move self to new-locn in window
              (reshape-viewport self  viewport :new-location new-location :draw? t  )
              )))))))


(defmethod copy-image ((self view)  &key viewport new-location 
                      ) 
  ;; copies self to new-location (in screen coords)
  ;; note: if new-location is contained in the viewport
  ;; of self's parent, this is the same as move-view
  
  (setq viewport (or viewport (car (viewports-of self))))
  (if (and (null new-location) (not (wb:mouse-down-p)))
    ;; called on next mouse left button in viewport
    (temporary-left-fn self (list #'copy-image :viewport) viewport)
    
    (progn
      (setq new-location (or new-location
                             (drag-region  viewport
                                           :window (window-of viewport)
                                           :in-window? nil)))
      (when new-location
        (let (window-at-locn sub sub-vp parent parent-vp 
                             locn-in-parent viewport-at-locn)
          (multiple-value-setq (parent parent-vp)
            (find-parent self  :viewport viewport))
          (unless (and parent-vp
                       (contains-p  parent-vp 
                                    (window-location new-location (window-of parent-vp))))
            (multiple-value-setq 
              (sub sub-vp parent parent-vp) (which-view new-location ))
            (if (and sub (legal-subview-p sub self))
              (setq parent sub parent-vp sub-vp)
              (if (not (and parent (legal-subview-p parent self)))
                (setq parent nil))))

          (cond 
           (parent  ; obtain new-location in coords of parent
            (setq viewport-at-locn 
                  (window-location new-location (window-of parent-vp)))
            (setq locn-in-parent 
                  (map-region viewport-at-locn parent-vp
                              (bounding-region-of parent)))
            (add-subview parent self locn-in-parent))
           
           ( (setq window-at-locn (which-window new-location))
             (setq viewport-at-locn 
                   (make-viewport window-at-locn
                                  (window-location new-location window-at-locn)))
             (draw-view self :viewport viewport-at-locn))
           
           (t   ;make a new window
            (setq window-at-locn (make-view-window :region new-location 
                                                   :title (view-window-title self)))
            (add-view-to-window window-at-locn self :draw? nil )
            ;;(draw-view self :viewport (make-viewport window-at-locn))
            
            )))))))




(defun draw-viewport (vp &key color operation (margin 0))
  (if (active-viewport-p vp)
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (if (zerop margin)
        (wb:canvas-draw-rectangle 
       (window-of vp) l  r b tp
       :color color :operation operation )
      (wb:canvas-draw-rectangle 
       (window-of vp) (- l margin)  (+ margin r) (- b margin) (+ tp margin)
       :color color :operation operation )))))

(defun erase-viewport (vp &key  operation (margin 0))
  (if (active-viewport-p vp)
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (wb:canvas-erase-rectangle 
       (window-of vp) (- l margin)  (+ margin r) (- b margin) (+ tp margin)
       :operation operation ))))

(defun fill-viewport (vp &key color operation)
  (if (active-viewport-p vp)
    (multiple-value-bind (l r b tp) (bounds-of vp)
      (wb:canvas-draw-filled-rectangle 
       (window-of vp) l  r b tp
       :color color :operation operation ))))

(defmethod reposition-view ((self view)
                            &key default-positions (draw? t) (bounds? nil))
  
  ;;performs initial-layout
  
  (labels ((draw-all (view viewports)
             (if (typep view 'simple-view) 
               (loop for vp in viewports 
                     for vp-sv = (select-viewport view vp)
                     do
                     (draw-view view :viewport vp-sv :erase? nil))
               
               (loop for sv in (subviews-of view)
                     for vps = (if (typep sv 'simple-view) viewports
                                   (viewports-of sv))
                     do (draw-all sv vps)))))

    (if draw? (erase-view self))

    (if bounds? 
      (original-bounds self :draw? nil))
    (setf (subview-locns-of self) nil)
    (apply #'init-position-subviews self default-positions)
    (compute-sub-viewports self)
    (loop for sv in (subviews-of self)
                   do
                   (reposition-view sv :draw? nil))
    
    (if draw? (draw-all self (viewports-of self)))))


(defmethod describe-view ((self view) )
  (describe self))

(defmethod describe-viewed-object ((self view) )
  
  (describe (viewed-object-of self)))

(defmethod short-description ((self view) )
  (quail-print (description-string self )))

(defmethod description-string ((self view) )
  (princ-to-string self))

(defmethod viewed-object-description-string ((self view) )
  (let* ((vo (viewed-object-of self))
         ans)
    (or  (dataset-name vo)
         (and (list-of-datasets-p vo)
              (setq ans (remove nil (mapcar #'dataset-name vo)))
              (format nil "~{~A~^ ~}" ans))
              
         (princ-to-string vo))))

(defmethod inspect-view ((self view))
  (inspect self))

(defmethod inspect-viewed-object ((self view))
  (inspect-data (viewed-object-of self)))


(defmethod identify-view ((self view) 
                          &key viewport )
  (setq viewport (or viewport (car (viewports-of self))))
  (select-one self)
   (let ((s (viewed-object-description-string self))
        (parent (find-parent self :viewport viewport)))
    (if parent
      (setq s
            (format nil "~A: At ~A"
                         s (subview-location-string  parent self)
                         )))
    (quail-print s)))


(defmethod print-viewed-object ((self view))
  (let ((vo (viewed-object-of self)))
    (if (list-of-datasets-p vo)
      (loop for d in vo do
            (princ (or (dataset-name d) d)) (terpri))
      (or (dataset-name vo) vo))))






(defmethod select-view ((self view))
  (pushnew self *selected-views*)
  (with-update-style-cache 
      (set-highlight-style self  t))
  )


(defmethod deselect-view ((self view))
  (set-selected-views (remove self *selected-views*))
  (with-update-style-cache 
    (set-highlight-style self  nil))
  )

#|
(defmethod select-one ((self view))
   (with-update-style-cache
      (loop for v in *selected-views* 
            do (set-highlight-style v  nil))
      (set-highlight-style  self t)
      (set-selected-views (list self))))
|#

(defmethod select-one ((self view))
  ;; toggle-select
  (let ((hi? (all-highlight? self)))
   (with-update-style-cache
      (loop for v in *selected-views* 
            do (set-highlight-style v  nil))
      (set-selected-views
      (unless hi?
        (set-highlight-style  self t)
        (list self))))))



(defun deselect-all ()
  (with-update-style-cache
    (loop for v in *selected-views* do 
          (deselect-view v ))))


(defmethod toggle-select-view ((self view))
  (with-update-style-cache
    (if (all-highlight? self)
      (deselect-view self)
      (select-view self))))



(defmethod prompt-position (self &optional ignore)
  (declare (ignore  self ignore))
  (make-2d-position 10 (- (wb:screen-height) 30)))

(defmethod prompt-position ((self view) &optional viewport)
  (declare (ignore  self ))
  (if viewport
    (let ((vp (screen-location viewport)))
      (make-2d-position (left-of vp) (top-of vp)))
    (call-next-method)))


(defmethod visible-subviews-p ((self view))
  (loop with br = (bounding-region-of self)
        for svl in (sub-view-locns-of self) 
        for sv in (subviews-of self)
        collect (if (or (and (typep sv 'point-symbol)
                             (contains-p br (centre-of svl)))
                        (contains-p br svl))
                  t nil)))


(defmethod set-aspect-ratio ((self view) 
                             &key viewport (ratio 1) (draw? t))
  (setq viewport (or viewport (car (viewports-of self))))
  (if viewport
    (let*  ((br (bounding-region-of self))
            (w (width-of viewport))
            (h (height-of viewport))
            (r (* ratio (/ (width-of br) (height-of br))))
            (new-w (min w (* r h )))
            (new-h (/ new-w r))
            )
      
      (reshape-viewport self viewport 
                        :draw? draw?
                        :new-location 
                        (make-region (left-of viewport) (+ (left-of viewport) new-w)
                                     (bottom-of viewport) (+ (bottom-of viewport) new-h))))))


(defmethod list-subviews ((self view) &key (test #'identity))
  ;; lists subviews satisfying predicate
  ;; test could be a list, where entry i corresponds to
  ;; subview i.
  ;; subviews with non-null test values are returned
  
  (let ((subs (subviews-of self)))
    (if (listp test)
      (loop for s in subs for p in test
            when p collect s)
      (loop for s in subs 
            when (funcall test s) collect s))))


(defmethod subviews-of-type ((self view) type)
  (loop for v in (subviews-of self)
        when (typep v type) collect v))

(defmethod descendant-views-of-type ((self view) type)
  (let ((views
         (loop for v in (subviews-of self)
               nconc (descendant-views-of-type v type))))
    (if (typep self type)
      (push self views))
    views))


(defmethod descendant-views-of-type ((self list) type)
  (loop for p in self
        when (typep p 'view)
        append (descendant-views-of-type p type)))


(defmethod clip&draw ((self view) region &key ignore-x? ignore-y? (pretty? t))
  (change-bounding-region self region
                          :ignore-x? ignore-x? :ignore-y? ignore-y?
                          :pretty? pretty?))


  

(defmethod new-bounds ((self view) &key (region :prompt) pretty?)
  (unless (region-p region)
    (setq region
          (case region
            (:prompt
             (apply #'make-region
                    (wb:prompt-user :type 'list :read-type :read
                                    :prompt-string "(left right bottom top)")))
            (:original (original-bounds self :draw? t) nil )
            (t nil))))
  (if (region-p region) (change-bounding-region self region :pretty? pretty?)))

(defmethod new-bounds :around ((self view) &key region (pretty? :default)) 
  (if (eq pretty? :default)
    (setq pretty? (not (region-p region))))
  (call-next-method self :region region :pretty? pretty?))

(defmethod original-bounds ((self view) &key draw? ) 
  
  (compute-bounding-region self)
  (if draw? (change-bounding-region self (bounding-region-of self)
                                    :pretty? t) ))


(defmethod layer-selected-view ((self view)  &rest arg &key viewport   )
  
  (loop for layer in *selected-views*
        unless (eq layer self)
        do
        (deselect-view layer)
        (apply #'layer-view self layer :viewport viewport arg)))


(defmethod set-view-font ((self view)  &key name size style 
                          (highlit? *selected-subviews-only*))
  (if (has-draw-style-p self :font)
    
  (let* ((font (wb:copy-canvas-font (draw-style self :font)))
         (old-size (wb:canvas-font-size font)))
    (if (null font)
      (setq font *default-label-font*)
      (let ()
        (setq size
              (case size 
                (:bigger (+ 2 old-size))
                (:smaller (-  old-size 2))
                (:prompt (prompt-for-style :font))
                (t size)))
        
        (if (eq name :prompt)
          (setq name (first (wb:prompt-for-items (wb:canvas-font-names)))))
        (if (eq style :prompt)
          (setq style (wb:prompt-for-items
                       (wb:canvas-font-styles) :selection-type :disjoint)))
        
        (if size
          (wb:set-canvas-font-size font size))
        (if name
          (wb:set-canvas-font-name font name))
        (if style
          (wb:set-canvas-font-style font style))))
    (set-drawing-style self :font font  :highlit? highlit?))))

(defmethod constrain-bounds ((self view)  &key draw? (views t)
                             region (link-bounds-x? t) (link-bounds-y? t))
  "Constrains the bounds of the subviews views of self.~
   If views is t (the default) all subviews are used."

  (if (eq t views)
    (setq views (subviews-of self)))
  (if link-bounds-x?
    (let* ((x-views (loop for v in views
                          when (linkable-bounds-x-p v) collect v))
           (x-reg (or region
                      (maximize-x-extents views))))


      (link-view-bounds x-views :x)
      (set-view-extents views :x :recompute? nil :region x-reg)))

  (if link-bounds-y?
    (let* ((y-views (loop for v in views
                          when (linkable-bounds-y-p v) collect v))
           (y-reg (or region
                      (maximize-y-extents views))))


      (link-view-bounds y-views :y)
      (set-view-extents views :y :recompute? nil :region y-reg)))
  (if (or link-bounds-y? link-bounds-x?)
    (remap-to-viewports self :erase? draw? :draw? draw?)))

(defmethod constrain-bounds ((views t)  &key draw?
                             region (link-bounds-x? t) (link-bounds-y? t))
  "Constrains the bounds of the  views."
  (if link-bounds-x?
    (let* ((x-views (loop for v in views
                          when (linkable-bounds-x-p v) collect v))
           (x-reg (or region
                      (maximize-x-extents views))))


      (link-view-bounds x-views :x)
      (set-view-extents views :x :recompute? nil :region x-reg)))

  (if link-bounds-y?
    (let* ((y-views (loop for v in views
                          when (linkable-bounds-y-p v) collect v))
           (y-reg (or region
                      (maximize-y-extents views))))


      (link-view-bounds y-views :y)
      (set-view-extents views :y :region y-reg :recompute? nil)))
  (if (or link-bounds-y? link-bounds-x?)
    (loop for v in views do
          (remap-to-viewports v :erase? t :draw? draw?))))
  

(defmethod subview-position-region ((self view ))
  (bounding-region-of self))


(defun switch-drawing-color (self color &key (draw? t))
  (if (has-draw-style-p self :color )
    (set-drawing-style  self :color color :draw? draw?))
  (loop for s in (subviews-of self) do
        (switch-drawing-color s color :draw? draw?)))


(defun viewport-of(view) 
  (car (viewports-of view)))
