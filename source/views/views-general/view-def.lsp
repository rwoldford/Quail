;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               view-def.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( view-p get-draw-portion get-map-portion  
            transform-mapping-draw-portions add-viewport select-viewport
            make-default-viewport)))

(defgeneric make-default-viewport (view)
  (:documentation "Returns a new viewport for view."))


(defgeneric get-draw-portion (self viewport)
  (:documentation "Returns the sub-region of viewport where self is drawn."))

(defgeneric get-map-portion (self region)
  (:documentation "Returns the sub-region of viewport where ~
                   bounding region of self is mapped."))

(defgeneric transform-mapping-draw-portions (self region transform)
  (:documentation "Transform maps region onto a new region.  ~
                   Return a transform mapping region onto draw portion of new region."))


(defmethod make-draw-viewport ((self view) &optional title)
  (make-viewport title))

(defmethod get-draw-portion ((self view) viewport)
  viewport)


(defmethod get-map-portion ((self view) viewport)
  (get-draw-portion self viewport))

(defmethod transform-mapping-draw-portions ((self view) region transform)
  (declare (ignore region))
  transform)

(defun view-p(view)
       "Tests for a view."
       (typep view 'view))

(defmethod subviews-of ((self view)) nil)
(defmethod (setf subviews-of) (new (self view ))
  (declare (ignore new))
  )
(defmethod visible-subs-p ((self view)) nil)

(defmethod sub-view-locns-of ((self view)) nil)

(defmethod (setf sub-view-locns-of) (new (self view))
  (declare (ignore new)))

(defmethod compute-map-to-viewport((self view) vp)
  (make-transform-for-regions (bounding-region-of self) 
                              (get-map-portion self vp)))
  

(defmethod add-viewport ((self view) vp pvp
                         &key (compute-transform? t))
  
  
  ;; adds a VIEWPORT and PARENT-VIEWPORT,  and the corresponding AFFINE-TRANSFORM  
  ;; if PVP  is already present, then install  VP to correspond with PVP 
  ;; if COMPUTE-TRANSFORM? is null, it is not installed.
  
  (let ((at (if compute-transform?
              (compute-map-to-viewport self vp)))) 
    
    (with-accessors ((parent-viewports parent-viewports-of)
                     (maps-to-viewports maps-to-viewports-of)
                     (viewports viewports-of))
                    self
      (loop for vpi in parent-viewports 
            for maps on maps-to-viewports 
            for vps on viewports 
            until (or (eql vpi pvp) (eql (car vps) vp))
            finally
            (cond ((or (eql vpi pvp) (eql (car vps) vp))
                   (rplaca maps at) (rplaca vps vp))
                  (t (push pvp parent-viewports)
                     (push at maps-to-viewports)
                     (push vp viewports)))))))

#|
(defmethod delete-viewport ((self view) viewport)
  
  ;; remove the viewport  VIEWPORT and corresponding  MAP and PARENT-VIEWPORT 
  
  (with-accessors ((viewports viewports-of)
                   (maps maps-to-viewports-of)
                   (parent-viewports parent-viewports-of))
    self
    (loop for vp in viewports 
          for m in maps 
          for pvp in parent-viewports 
          unless (eql vp viewport) collect vp into new-viewports and
           collect m into new-viewport-maps 
          and collect pvp into new-parent-viewports 
          finally
          (setf viewports new-viewports)
          (setf maps new-viewport-maps)
          (setf parent-viewports new-parent-viewports))))
|#

(defmethod delete-viewport ((self view) viewport)
  
  ;; remove the viewport  VIEWPORT and corresponding  MAP and PARENT-VIEWPORT 
  
  (with-accessors ((viewports viewports-of)
                   (maps maps-to-viewports-of)
                   (parent-viewports parent-viewports-of))
    self
    (loop for vp in viewports 
          for m in maps 
          for pvp in parent-viewports until (eql vp viewport)
          finally (when (eq vp viewport)
                    (setf viewports (delete vp viewports))
                    (setf maps (delete m maps))
                    (setf parent-viewports (delete pvp parent-viewports))))))

(defmethod delete-nested-viewports ((self view) vp)
  ;; deletes vp from self and
  ;; deletes the viewports corresponding to vp 
  ;; from all subviews of self, recursively
  
  (loop for v in (subviews-of self) 
        for sv-vp = (select-viewport v vp)  do
        (delete-nested-viewports v sv-vp)
        )
  (delete-viewport self vp))

(defmethod select-map-to-viewport ((self view) vp )
  
  ;; select the  MAP-TO-VIEWPORT  corresponding to VP 
  
  (let* ((pos (position vp (viewports-of self)))
         map)
    (when pos 
      (setq map (elt (maps-to-viewports-of self)  pos ))
      (unless map
        (setq map (compute-map-to-viewport self vp))
        (setf (elt (maps-to-viewports-of self)  pos ) map))
      map)))
                 

(defmethod select-viewport ((self view) pvp)
  
  ;;; select the VIEWPORT  corresponding to parent-viewport pvp 

  (let ((pos (position pvp (parent-viewports-of self))))
    (if pos (elt (viewports-of self) pos ))))

(defmethod select-parent-viewport ((self view) vp)
  
  ;;; select the parent viewport  corresponding to viewport vp 
  
  (let ((pos (position vp (viewports-of self) )))
    (if pos (elt (parent-viewports-of self)  pos ))))


(defmethod is-the-parent ((parent view) (sub view))
  (member sub (subviews-of parent)))

(defmethod list-viewports ((self view)
                           &optional vp)
  (typecase vp
    (viewport (list vp))
    (list vp)
    (t (viewports-of self))))

(defmethod select-sub-view-locn ((self view) (subview view))
  
  ;;; select the sub-view-locn corresponding to subview
  
  (let ((pos (position subview (subviews-of self))))
    (if pos (elt (sub-view-locns-of  self) pos ))))


(defmethod subview-location-string ((self view) (subview view))
  (let ((r (select-sub-view-locn self subview)))
  (if r
    (format nil "~A,~A, ~A, ~A" (left-of r) (right-of r)
            (bottom-of r) (top-of r)))))
;;;=======================================================================
#|
(defmethod initialize-instance :after ((self view)
                                       &rest keyword-pairs
                                       &key draw?
                                       &allow-other-keys )
  (apply #'construct-sub-views self keyword-pairs )
  (apply #'initial-layout self keyword-pairs)
  (make-menus self)
  (if draw? (draw-view self))
  )
|#

(defun disable-keyword(keyword-pairs key)
  (let ((key-list (member key keyword-pairs)))
    (if key-list (setf (car key-list) (gensym (string key))))
    keyword-pairs))
  
(defmethod initialize-instance :before ((self view)
                                        &key coord-string-x coord-string-y )
  (if coord-string-x
    (defmethod coord-string-x ((view (eql self)))
      (if (functionp coord-string-x)
        (funcall coord-string-x view) coord-string-x)))
  (if coord-string-y
    (defmethod coord-string-y ((view (eql self)))
      (if (functionp coord-string-y)
        (funcall coord-string-y view) 
        coord-string-y))))


(defmethod initialize-instance :after ((self view)
                                       &rest keyword-pairs)
  (disable-keyword keyword-pairs :initform-fn)
  (apply #'construct-sub-views self  keyword-pairs )
  (apply #'initial-layout self keyword-pairs)
  )

(defmethod auto-draw-p ((self view))
  *auto-draw-view?*)


(defmethod initialize-instance :around ((self view)
                                        &rest initargs
                                        &key (draw? :no-draw-val?)
                                        initform-fn link? (combine? t)
                                        &allow-other-keys)
  (disable-keyword initargs :link?)
  (when (and combine? (subview-keys-of self))
    (setq initargs (apply #'combine-subview-args self initargs)))
  (if (functionp initform-fn)
    (apply #'call-next-method self (append (apply initform-fn initargs) initargs))
    (apply #'call-next-method self initargs))
  (if link?
    (if (typep link? 'link-table)
      (link-view self :draw? t :link-table link?)
      (link-view self :draw? t)))
  (if (or (eq draw? t) (and (eq draw? :no-draw-val? ) (auto-draw-p self)))
    (draw-view self))
  )


(defmethod initial-layout ((self view) &rest keyword-pairs 
                           &key bounding-region &allow-other-keys)
  (if bounding-region
    ;;(apply #'set-bounding-region self :region bounding-region :pretty? nil keyword-pairs)
    (apply #'set-bounding-region self :region bounding-region 
           (append keyword-pairs (list :pretty? nil)))
    (apply #'set-bounding-region self keyword-pairs  ))
  (apply #'init-position-subviews  self keyword-pairs))


(defmethod construct-sub-views ((self view) &key)
  ;; should be specialized for compound views
  )

(defmethod set-bounding-region ((self view) &key region (pretty? t) 
                                ignore-x? ignore-y?)
  ;; if pretty? is non-nil the view may expand the bounding region to
  ;; cover a "pretty" region
  ;; if ignore-x? is non-nil the x bounds view remain unchanged
  ;; similarly ignore-y?
  
  (declare (ignore pretty?))
  
  (let ((new-region (or (and region 
                             (if (or ignore-x? ignore-y?) 
                               (make-region region)
                               region))
                        (compute-bounding-region self))))
    (if (or ignore-x? ignore-y?)
      (let ((old-bounds (or (bounding-region-of self) (make-region))))
        (if ignore-x?
          (setf (x-extent-of new-region) old-bounds))
        (if ignore-y?
          (setf (y-extent-of new-region) old-bounds))))
    (setf (bounding-region-of self) new-region)))
   

(defmethod compute-bounding-region ((self view) )

    (make-region))

(defmethod compute-bounding-region :around ((self view) )

    (let ((region (call-next-method)))
      (setq region
            (loop until (valid-region-check region nil)
                  do
                  (setq region
                        (apply #'make-region 
                         (wb:prompt-user :type 'list :read-type :read
                                         :prompt-string
                                         (format nil "Enter bounds for ~S, (left right bottom top)" self))))
                  finally (return region)))))
      








(defmethod init-position-subviews ((self view) &key)
  ;; should be specialized for compound views
  )

(defmethod place-subview ((self view)
                          (subview view)
                          locn )
  
  ;; adds the location in self for SUBVIEW
  ;; if SUBVIEW is not already in subviews-of self add it
  (with-accessors ((subviews subviews-of)
                   (subview-locns sub-view-locns-of))
    self
    (unless subview-locns
      (setf subview-locns (make-list (length subviews))))
    (loop for s in subviews 
          for l on subview-locns 
          until (eql s subview)
          finally
          (if (eql s subview) (rplaca l locn)
              ;; else
              (progn (push subview subviews)
                     (push locn subview-locns))))))

(defmethod delete-subview ((self view) subview)
  (declare (ignore self subview))
  )

;;;================================================================================


(defmethod map-to-viewport ((self view)
                            &optional viewport)
  (loop for vp in (if viewport (list viewport) (viewports-of self)) 
        do
        (add-viewport  self vp  vp)
        (map-subviews-to-viewport self vp)))


(defmethod map-subviews-to-viewport ((self view)
                                     &optional viewport
                                     subviews)
  
  ;; if VIEWPORT is not provided, this is performed for all  VIEWPORTS
  ;; if subviews is not provided, this is performed for all  subviews
  
  (loop for vp in 
        (if viewport (list viewport) (viewports-of self))
        do
        (compute-sub-viewports self vp subviews)
        (loop for sv in (or subviews (subviews-of self)) 
              for sv-vp = (select-viewport sv vp) do
              (map-subviews-to-viewport sv sv-vp))))
                                        


(defmethod compute-sub-viewports ((self view)
                                   &optional viewport subviews)
  
  ;; obtains viewports (contained in viewport) for subviews
  ;; if VIEWPORT is not provided, this is performed for all  VIEWPORTS
  
  (let ((viewports (if viewport (list viewport) (viewports-of self)))
        (maps (if viewport (list (select-map-to-viewport self viewport))
                  (maps-to-viewports-of self)))
        (subview-locns 
         (if subviews 
           (loop for s in subviews  collect 
                 (select-sub-view-locn self s))
           (sub-view-locns-of self))))
    (setq subviews (or subviews (subviews-of self)))
    (loop for sv in subviews
          for svl in subview-locns do
          (loop for vp in viewports for map in maps
                for sv-vp = (or (select-viewport sv vp)
                                (make-viewport (window-of vp)))
                do
                (setf (bounds-of sv-vp) (apply-transform map svl))
                (add-viewport sv sv-vp vp)))))



(defmethod allowed-subview-movement ((self view) subview)
  ;; may returns :x :y :both or :none
  ;; determines how subview may be moved within view

  (declare (ignore subview))
  :both)

(defmethod list-viewed-elements ((self view))
  (let ((vo (viewed-object-of self)))
    (if   vo (list vo))))
   
(defmethod legal-subview-p ((self view) subview)
  (declare (ignore subview))
  t)

(defmethod delete-all-subviews ((self view)))


(defmethod use-y-axis-p ((self view))
  nil)

(defmethod use-x-axis-p ((self view))
  nil)

(defmethod show-y-axis-p ((self view))
  (use-y-axis-p self))

(defmethod show-x-axis-p ((self view))
  (use-x-axis-p self))

(defmethod subview-locns-of ((self view))
  (sub-view-locns-of self))


(defmethod (setf subview-locns-of) (new (self view))
  (setf (sub-view-locns-of self) new))

(defmethod sub-views-of ((self view))
  (subviews-of self))



(defmethod (setf sub-views-of) (new (self view ))
  (setf (subviews-of self) new))


(defmethod view-title-string ((self view))
  nil
  )



(defmethod compute-descendant-locn((self view) (d view))
  (or (select-sub-view-locn self d)
      (loop for sv in (sub-views-of self)
            for dl = (if (typep sv 'compound-view)
                       (compute-descendant-locn sv d))
            until dl
            finally (if dl
                      (return
                       (let ((tr 
                             (make-transform-for-regions 
                              (bounding-region-of sv) 
                              (select-sub-view-locn self sv))))
                        (apply-transform tr dl)))))))
