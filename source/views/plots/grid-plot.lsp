;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               grid-plot.lisp
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



(defclass grid-plot (plot)
  ()
  (:default-initargs :link-bounds-x? t :link-bounds-y? t
    :gap-x  *default-grid-gap*  :gap-y  *default-grid-gap*
    :default-interior 'grid-layout
      ))





(defmethod init-label-text ((self grid-plot) 
                              (margin T) label)
  
    (if (typep label 'label)
      (let* ((iv  (interior-view-of self))
             (lab (text-of label)))
        (if (and (typep iv 'batch-layout) (listp (batches-of iv)))
          (loop with the-b
                for b in (batches-of iv)
                until (and (dataset-p b)
                           (equal (identifier-name b) lab) (setq the-b b))
                finally (if the-b
                          (setf (viewed-object-of label) the-b)))
           (if (typep iv 'case-layout)
          (loop with the-b
                for c in (list-case-views iv)
                for b = (viewed-object-of c)
                until (and (dataset-p b)
                           (equal (identifier-name b) lab) (setq the-b b))
                finally (if the-b
                          (setf (viewed-object-of label) the-b)))))
        (when (and lab (not (string-equal lab "")))
          (loop for v in (descendant-views-of-type iv 'd-view)
               do
               (if (equal lab (coord-string-x v))
                 (text-link v label :x) 
                 (if (equal lab (coord-string-y v)) (text-link v label :y)
                 (if (equal lab (coord-string-z v)) (text-link v label :z)))))))))




;; changes to grab the variable for the margin view. this may be dangerous... 11/2/97
;; goal of this is to use a 1d-view with case-labels in the margin.
;; it already works for scatterplot becuse it automatically gets the data
;; from the interior
(defmethod construct-margin-views ((self grid-plot) 
                                   &key  (delete-useless-axes? t)
                                   link-bounds-x?
                                   link-bounds-y?)
  "Constructs margin views using information provided by~
   the :left-views :right-views :bottom-views :top-views initargs.~
   Each  initarg should pass the legal-view-construct-p test,~
   or be a list whose elements pass the legal-view-construct-p test.~
   If an initarg is null, the corresponding view is not constructed.~
   The default view type is axis. ~
   Each legal-view-construct, together with keyword-pairs,~
   is used to construct views which are placed in the appropriate slot."
  
  
  (with-accessors ((lv  left-views-of) (rv right-views-of)
                   (iv interior-views-of)
                   (tv top-views-of) (bv bottom-views-of))
                  self
    (flet ((get-br(views)
             (let ((v (loop for vi in (flatten-views views)
                            until (typep vi '2d-view)
                            finally (return vi))))
               
               (if v (if (typep v 'd-view) 
                       (smallest-bounding-region v)
                       (bounding-region-of v))
                   )))
           (get-data(views axis)
             (let ((d (loop for vi in (if (or (eql axis :right) (eql axis :top))
                                        (reverse (flatten-views views))
                                        views)
                            until (typep vi '2d-view)
                            finally (return vi))))
               
               (cond 
                ((typep d 'd-view)
                 (append (plot-margin-br-info  self axis d)
                         (plot-margin-coord-info  self axis d) 
                         (list :data (dataset-of d))))
                
                
                (t (list :data (viewed-object-of self))))))
           
           )
    
    (multiple-value-setq (lv rv bv tv)
      (values-list
       (loop with rows = (row-views (car iv))
             with cols = (col-views (car iv))
             with nrows = (length rows)
             with ncols = (length cols)
             for v in (list lv rv bv tv)
             for axis in (list :vertical :vertical :horizontal :horizontal)
             for just in (list :right :left :top :bottom )
             for axis-info = (list :bounding-region nil :orientation axis :justification just 
                                  :ntics 2 :pretty? t)
             for n = (if (eq axis :vertical) nrows ncols)
             for slices = (if (eq axis :vertical) rows cols)
             collect 
             (cond ((null v) nil)
                   ;; added this next clause 11/2/97
                   ((and (listp v) (= (length v) n)
                         (every #'legal-view-construct-p v))
                    (loop with d
                          for r in v
                          for s in slices
                            do (setf (second axis-info) (get-br s))
                            (setq d (append axis-info (get-data s axis))) 
                            collect (list  (view-from-arg r d 'axis))))

                   ((and (listp v) (= (length v) n)
                         (every #'(lambda(x) (and (listp x)
                                                  (every #'legal-view-construct-p x)))
                                v))
                    (loop with d
                          for r in v
                          for s in slices
                            do (setf (second axis-info) (get-br s))
                            (setq d (append axis-info (get-data s axis))) 
                            collect (loop for vi in r 
                                          collect  (view-from-arg vi d 'axis))))
                         
                    
                   (t (loop with d
                            for s in slices
                            do (setf (second axis-info)  (get-br s))
                            (setq d (append axis-info (get-data s axis)))
                             collect
                            (cond 
                             ((legal-view-construct-p v)
                              (list (view-from-arg v d 'axis)))
                             ((and (listp v) (every #'legal-view-construct-p v))
                              (loop for vi in v collect  (view-from-arg vi d 'axis)))
                             (t (quail-error "Illegal subview argument ~A" v))))))))))
  
    
    (constrain-bounds self :link-bounds-x? link-bounds-x? 
                      :link-bounds-y? link-bounds-y? :recompute? t)
    (if delete-useless-axes? (delete-useless-axes self))
    
    ))

(defun flatten-views (views)
  (loop for v in views
        when (listp v) append (flatten-views v) 
        when (not (listp v)) collect v 
        ))




(defmethod nrows-of ((self grid-plot))
  (nrows-of (interior-view-of self)))


(defmethod ncols-of ((self grid-plot))
  (ncols-of (interior-view-of self)))




(defmethod compute-row-locns((self grid-plot) )
  (let* ((iv (interior-view-of self))
        (rows (if iv (row-views iv))))

  (loop for (sub) in rows
        for p = (and sub (compute-descendant-locn self sub))
        while p
        collect p into reg
        finally (return
                 (and p reg)))))

(defmethod compute-col-locns((self grid-plot) )
  (let* ((iv (interior-view-of self))
        (cols (if iv (col-views iv))))

  (loop for (sub) in cols
        for p = (and sub (compute-descendant-locn self sub))
        while p
        collect p into reg
        finally (return
                 (and p reg)))))



(defmethod init-position-margin-views ((self grid-plot)
                                       outer-space inner-space
                                       &key bottom-view-size top-view-size
                                       left-view-size right-view-size gap-x gap-y )
  (declare (ignore outer-space))
  (with-accessors ((lv  left-views-of) (rv right-views-of)
                   (tv top-views-of) (bv bottom-views-of))
                  self
    (multiple-value-bind (li ri bi ti)
                         (bounds-of inner-space)
      (let (row-locns col-locns)
      
      (if lv
        (loop with regions = (or row-locns
                                 (setq row-locns (compute-row-locns self))
                                 (tile-region-list 
                                  (make-region (- li left-view-size) li bi ti)
                                  (length lv) 1 nil 0 gap-y))
              for lvi in lv 
              for mv-region in regions
              do
              (setq mv-region (copy-region mv-region))
              (setf (left-of mv-region) (- li left-view-size))
              (setf (right-of mv-region) li)
              
              (loop for v in lvi  do
                    (place-subview self v (copy-region mv-region)))))
      
      (if rv
        (loop with regions = (or row-locns
                                 (setq row-locns (compute-row-locns self))
                                (tile-region-list 
                                  (make-region ri (+ ri right-view-size) bi ti)
                                  (length rv) 1 nil 0 gap-y))
              for rvi in rv
              for mv-region in regions
              do
              (setq mv-region (copy-region mv-region))
              (setf (left-of mv-region) ri)
              (setf (right-of mv-region) (+ ri right-view-size))
              
              (loop for v in rvi  do 
                    (place-subview self v (copy-region mv-region)))))
      
      (if bv
        (loop  with regions = (or col-locns
                                 (setq col-locns (compute-col-locns self))
                              (tile-region-list
                                   (make-region li ri (- bi bottom-view-size) bi)
                                   1 (length bv) nil gap-x 0))
               for bvi in bv
               for mv-region in regions
               do
               (setq mv-region (copy-region mv-region))
               (setf (bottom-of mv-region) (- bi bottom-view-size))
               (setf (top-of mv-region) bi)
               
               (loop for v in bvi  do 
                     (place-subview self v (copy-region mv-region)))))
      
      (if tv    
        (loop  with regions = (or col-locns
                                 (setq col-locns (compute-col-locns self))
                            (tile-region-list 
                                   (make-region li ri ti (+ ti top-view-size))
                                   1 (length tv) nil gap-x 0))
               
               for tvi in tv
               for mv-region in regions
               do
               (setq mv-region (copy-region mv-region))
               (setf (bottom-of mv-region) ti)
               (setf (top-of mv-region) (+ ti top-view-size))
               
               (loop for v in tvi  do 
                     (place-subview self v (copy-region mv-region)))))))))

(defmethod init-position-margin-labels ((self grid-plot) outer-space inner-space
                                 &key bottom-label-size top-label-size
                                 left-label-size right-label-size gap-x gap-y)
  (with-accessors ((ll  left-labels-of) (rl right-labels-of)
                   (tl top-labels-of) (bl bottom-labels-of))
                  self
    (multiple-value-bind (li ri bi ti)
                         (bounds-of inner-space)
      (multiple-value-bind (lo ro bo to)
                           (bounds-of outer-space)
        (let (row-locns col-locns) 
          (when ll
            (setq row-locns (or row-locns (compute-row-locns self))) 
            (when (and (> (length ll) 3) (< left-label-size .2 ))
              (setf left-label-size .2)
              (setf (getf  (default-positions-of self) :left-label-size)  left-label-size))
              
            (if (= (length row-locns) (length ll))
              
              (loop  for lli in ll
                    for mv-region in row-locns
                    do (setq mv-region (copy-region mv-region))
                    (setf (left-of mv-region) lo)
                    (setf (right-of mv-region) (+ lo left-label-size))
                    
                    (place-subview self lli mv-region))
              
              (loop for lli in ll
                    for l-region in 
                    (tile-region-list 
                     (make-region lo (+ lo left-label-size)  bi ti)
                     (length ll) 1 nil 0 gap-y) do
                    (place-subview self lli l-region))))
        
        (if rl
          (loop for rli in rl
                for l-region in 
                (tile-region-list 
                 (make-region (- ro right-label-size) ro bi ti)
                 (length rl) 1 nil 0 gap-y) do
                (place-subview self rli l-region)))
        
        (when bl
          (setq col-locns (or col-locns (compute-col-locns self)))
          (if (= (length col-locns) (length bl))
          
            (loop for bli in bl
                for  mv-region in col-locns do
                (setq mv-region (copy-region mv-region))
                (setf (bottom-of mv-region) bo)
                (setf (top-of mv-region) (+ bottom-label-size bo))
                (place-subview self bli mv-region))
            (loop for bli in bl
                for l-region in 
                (tile-region-list 
                 (make-region li ri bo (+ bottom-label-size bo))
                 1 (length bl) nil gap-x 0 ) do
                (place-subview self bli l-region))))
        (if tl
          (loop for tli in tl
                for l-region in 
                (tile-region-list 
                 (make-region  li ri (- to top-label-size ) to)
                 1 (length tl) nil gap-x 0 ) do
                (place-subview self tli l-region))))))))



(defmethod set-aspect-ratio ((self grid-plot) 
                             &key viewport (ratio 1) (draw? t))
  (declare (ignore viewport ratio draw? ))
  (quail-error "Does nothing"))


(defmethod delete-subview :before ((self grid-plot) view)
  (cond ((member view (interior-views-of self))
         (setf (interior-views-of self)
               (delete view (interior-views-of self))))
        ((loop for vs in (left-views-of self)
               for i upfrom 0
               until (member view vs)
               finally (if (member view vs)
                         (setf (elt (left-views-of self) i) (delete view vs))
                         nil))
         nil)
        ((loop for vs in (right-views-of self)
               for i upfrom 0
               until (member view vs)
               finally (if (member view vs)
                         (setf (elt (right-views-of self) i) (delete view vs))
                         nil))
         nil)
        ((loop for vs in (bottom-views-of self)
               for i upfrom 0
               until (member view vs)
               finally (if (member view vs)
                         (setf (elt (bottom-views-of self) i) (delete view vs))
                         nil))
         nil)
        ((loop for vs in (top-views-of self)
               for i upfrom 0
               until (member view vs)
               finally (if (member view vs)
                         (setf (elt (top-views-of self) i) (delete view vs))
                         nil))
         nil)
        ((member view (left-labels-of self))
         (setf (left-labels-of self)
               (delete view (left-labels-of self))))
        ((member view (right-labels-of self))
         (setf (right-labels-of self)
               (delete view (right-labels-of self))))
        ((member view (bottom-labels-of self))
         (setf (bottom-labels-of self)
               (delete view (bottom-labels-of self))))
        ((member view (top-labels-of self))
         (setf (top-labels-of self)
               (delete view (top-labels-of self))))
        ((eq view (title-of self)) (setf (title-of self) nil))
        (t nil)))




(defmethod delete-useless-axes ((self grid-plot)) 
  (let ((del-views nil)
        (new nil)
        (subviews (subviews-of self)))
    (when (every #'(lambda(v) (and (typep v 'axis)
                                   (hide-axis-p v)))
                 (setq new (flatten-views (left-views-of self))))
      (setq del-views (append del-views new)) 
       (setf (left-views-of self) nil))

    (when (every #'(lambda(v) (and (typep v 'axis)
                                   (hide-axis-p v)))
                 (setq new (flatten-views (right-views-of self))))
      (setq subviews (remove del-views subviews)) 
      (setf (right-views-of self) nil))

    (when (every #'(lambda(v) (and (typep v 'axis)
                                   (hide-axis-p v)))
                 (setq new (flatten-views (bottom-views-of self))))
      (setq del-views (append del-views new))
      (setf (bottom-views-of self) nil))

    (when (every #'(lambda(v) (and (typep v 'axis)
                                   (hide-axis-p v)))
                 (setq new (flatten-views (top-views-of self))))
      (setq del-views (append del-views new))
      (setf (top-views-of self) nil))
    (setq subviews (remove del-views subviews))
  (setf (subviews-of self) subviews) 
 (loop for del in del-views
            do
            (loop for v in (link-bounds-of  del) 
                  unless (eq v del) do
                  (remove-y-link v del)
                  (remove-x-link v del)))))


(defmethod row-format ((self grid-plot))
  (let* ((iv (interior-views-of self))
         (rows (if (typep (car iv) 'grid-layout)
                 (row-format (car iv))
                 (if iv (list iv))))
        (left (left-views-of self))
        (right (right-views-of self))
        (bottom (bottom-views-of self))
        (top (top-views-of self)))
    (setq rows
          (cond ((and rows left right)
                 (loop for v-mids in rows
                       for l-view in left
                       for r-view in right
                       collect
                       (append (list l-view) v-mids (list r-view))))
                ((and left right)
                 (mapcar #'list left right))
                
                
                ((and rows left)
                 (loop for v-mids in rows
                       for v in left
                       collect
                       (cons v v-mids)))
                ((and rows right)
                 (loop for v-mids in rows
                       for v in right
                       collect
                       (append v-mids (list v))))
                (t (or rows (mapcar #'list left)
                       (mapcar #'list right)))))
    (if top
      (setq rows (cons top rows)))
    (if bottom
      (setq rows (append rows (list bottom))))
    rows
      ))
  

(defmethod col-format ((self grid-plot))
  (let* ((iv (interior-views-of self))
         (cols (if (typep (car iv) 'grid-layout)
                 (col-format (car iv))
                 (if iv (list iv))))
        (left (left-views-of self))
        (right (right-views-of self))
        (bottom (bottom-views-of self))
        (top (top-views-of self)))
                 
            (setq cols
          (cond ((and cols top bottom)
                 (loop for v-mids in cols
                       for l-view in top
                       for r-view in bottom
                       collect
                       (append (list l-view) v-mids (list r-view))))
                ((and top bottom)
                 (mapcar #'list top bottom))
                
                
                ((and cols top)
                 (loop for v-mids in cols
                       for v in top
                       collect
                       (cons v v-mids)))
                ((and cols bottom)
                 (loop for v-mids in cols
                       for v in bottom
                       collect
                       (append v-mids (list v))))
                (t (or cols (mapcar #'list top)
                       (mapcar #'list bottom)))))
    (if left
      (setq cols (cons left cols)))
    (if right
      (setq cols (append cols (list right))))
    cols
    ))


(defmethod row-views ((self grid-plot))
  (let* ((iv (interior-views-of self))
         (rows (row-views (car iv)))
        (left (left-views-of self))
        (right (right-views-of self))
        (bottom (bottom-views-of self))
        (top (top-views-of self)))
   
    (setq rows
          (cond ((and rows left right) 
                 (if (= (length rows) (length left) (length right))
                   (mapcar #'append left rows right)))
                ((and left right) 
                 (if (=  (length left) (length right))
                   (mapcar #'append left right)))
                
                
                ((and rows left) 
                 (if (=  (length left) (length rows))
                   (mapcar #'append left rows)))
                ((and rows right) 
                  (if (=  (length right) (length rows))
                   (mapcar #'append  rows right)))
                (t (or rows left right))))
    
    (if top
      (setq rows (cons (reduce #'append top) rows))) 
    (if bottom
      (setq rows (append rows (list (reduce #'append bottom))))) 
    rows
      ))



(defmethod col-views ((self grid-plot))
  (let* ((iv (interior-views-of self))
         (cols (col-views (car iv)))
        (left (left-views-of self))
        (right (right-views-of self))
        (bottom (bottom-views-of self))
        (top (top-views-of self)))
    (setq cols
          (cond ((and cols top bottom)
                 (if (= (length top) (length cols) (length bottom))
                   (mapcar #'append top cols bottom)))
                ((and top bottom)
                 (if (= (length top)  (length bottom))
                   (mapcar #'append top bottom)))
                
                
                ((and cols top)
                 (if (= (length top)  (length cols))
                   
                   (mapcar #'append top cols )))
                ((and cols bottom)
                 (if (= (length bottom)  (length cols))
                   
                   (mapcar #'append  cols bottom )))
                (t (or cols top bottom))))
    (if left
      (setq cols (cons (reduce #'append left) cols)))
    (if right
      (setq cols (append cols (list (reduce #'append right)))))
    cols
      ))


(defmethod merge-rows ((self grid-plot))
  (let* ((iv (interior-views-of self))
         (mrows (merge-rows (car iv)))
         (rows-per-block (length mrows))
         (rows (row-views (car iv)))
        (left (left-views-of self))
        (right (right-views-of self))
        (bottom (bottom-views-of self))
        (top (top-views-of self)))
   
    (setq mrows
          
          (cond ((and rows left right) 
                 (if (= (length rows) (length left) (length right))
                   (loop for l in left
                         for r in right
                         for i upfrom 0
                         for j = (mod i rows-per-block) do
                         (setf (nth    j mrows) (append l (nth j mrows) r))
                         finally (return mrows))))
                ((and left right) 
                 (if (=  (length left) (length right))
                   (mapcar #'append left right)))
                
                
                ((and rows left) 
                 (if (=  (length left) (length rows))
                   (loop for l in left
                          for i upfrom 0
                         for j = (mod i rows-per-block) do
                         (setf (nth    j mrows) (append l (nth j mrows) ))
                         finally (return mrows))))
                ((and rows right) 
                  (if (=  (length right) (length rows))
                   (loop for r in right
                         for i upfrom 0
                         for j = (mod i rows-per-block) do
                         (setf (nth    j mrows) (append (nth j mrows) r))
                         finally (return mrows))))
                (t (or mrows left right))))
    
    (if top
      (setq mrows (cons (reduce #'append top) mrows))) 
    (if bottom
      (setq mrows (append mrows (list (reduce #'append bottom))))) 
    mrows
      ))


(defmethod merge-cols ((self grid-plot)) 
  (let* ((iv (interior-views-of self))
         (mcols (merge-cols (car iv)))
         (cols-per-block (length mcols))
         (cols (col-views (car iv)))
        (top (top-views-of self))
        (bottom (bottom-views-of self))
        (left (left-views-of self))
        (right (right-views-of self)))
   
    (setq mcols
          
          (cond ((and cols top bottom) 
                 (if (= (length cols) (length top) (length bottom))
                   (loop for l in top
                         for r in bottom
                         for i upfrom 0
                         for j = (mod i cols-per-block) do
                         (setf (nth    j mcols) (append l (nth j mcols) r))
                         finally (return mcols))))
                ((and top bottom) 
                 (if (=  (length top) (length bottom))
                   (mapcar #'append top bottom)))
                
                
                ((and cols top) 
                 (if (=  (length top) (length cols))
                   (loop for l in top
                          for i upfrom 0
                         for j = (mod i cols-per-block) do
                         (setf (nth    j mcols) (append l (nth j mcols) ))
                         finally (return mcols))))
                ((and cols bottom) 
                  (if (=  (length bottom) (length cols))
                   (loop for r in bottom
                         for i upfrom 0
                         for j = (mod i cols-per-block) do
                         (setf (nth    j mcols) (append (nth j mcols) r))
                         finally (return mcols))))
                (t (or mcols top bottom))))
    
    (if left
      (setq mcols (cons (reduce #'append left) mcols))) 
    (if right
      (setq mcols (append mcols (list (reduce #'append right))))) 
    mcols
      ))

