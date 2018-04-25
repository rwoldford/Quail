;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               grid-layout.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(set-format row-format col-format grid-layout batch-layout grid-position-subviews 
              *default-grid-gap*)))


(defvar *default-grid-gap* )
(setq *default-grid-gap*  0.1)



 

(defgeneric grid-position-subviews  (view &key &allow-other-keys))
(defgeneric set-format (grid-layout &key &allow-other-keys))

(defgeneric row-format (grid-layout)
  (:documentation "Returns lists of views in layout with ~
                   sublist i containing views in row i."))

(defgeneric col-format (grid-layout)
  (:documentation "Returns lists of views in layout with ~
                   sublist i containing views in column i."))


(defclass grid-layout(position-key-mixin view-layout ) 
  ((position-keys :initform '(:gap-x :gap-y :rows :cols ) :allocation :class)
   (layout-format :initform nil  :accessor layout-format-of :initarg :format)
   (right-menu :allocation :class :initform nil)
   )
  (:default-initargs :gap-x  *default-grid-gap* 
    :gap-y  *default-grid-gap*  :square? nil :format :grid
    :link-bounds-x? nil :link-bounds-y? nil :box-views? t
    :subview-position-fn #'grid-position-subviews))






(defmethod construct-sub-views :around ((self grid-layout) &rest keyword-pairs 
                                        &key rows cols nrows ncols format)
  (setq nrows
        (cond ((eq format :row) 1)
              ((and rows (listp rows)) 
               (truncate (length rows) 2))
              ((and rows (numberp rows)) rows)
              (t nrows)))
  (setq ncols
        (cond ((eq format :col) 1)
              ((and cols (listp cols)) 
               (truncate (length cols) 2))
              ((and cols (numberp cols)) cols)
              (t ncols)))
  (if (and nrows ncols)
    (apply #'call-next-method self :nsubviews (* nrows ncols) keyword-pairs)
    (call-next-method)))

(defmethod set-format ((self grid-layout) &key rows cols nrows ncols format)
  
  (let ((n (length (layout-views-of self)))
        (lf (layout-format-of self)))
    (unless (and  lf
                  (listp lf)
                  (= 2 (length lf))
                  (and (numberp (car lf)) (numberp (second lf)))
                (<= n (* (car (layout-format-of self)) (cadr (layout-format-of self)))))
       (case format
        (:row (setf (layout-format-of self) (list 1 n)))
        (:col (setf (layout-format-of self) (list n 1)))
        (t
         (setq nrows
               (cond ((and rows (listp rows)) 
                      (truncate (length rows) 2))
                     ((and rows (numberp rows)) rows)
                     (t nrows)))
         (setq ncols
               (cond ((and cols (listp cols)) 
                      (truncate (length cols) 2))
                     ((and cols (numberp cols)) cols)
                     (t ncols)))
         (if (or nrows ncols)
           (setf (layout-format-of self) (list nrows ncols)))
         (setq nrows (nrows-of self))
         (setq ncols (ncols-of self))
         (loop while (< (* nrows ncols) n)
               do (setf (layout-format-of self) nil)
               (setq nrows (nrows-of self))
               (setq ncols (ncols-of self)))
         
         (if (> (* nrows ncols) n)
           (loop repeat (- (* nrows ncols) n)
                 for view = (view)
                 collect (list view) into lviews
                 collect view into sviews
                 finally 
                 
                 (setf (subviews-of self) (append (subviews-of self) sviews))
                 (setf (layout-views-of self) (append (layout-views-of self) lviews))))
         
         )))))


(defmethod init-position-subviews :before ((self grid-layout) 
                                           &rest keyword-pairs 
                                           &key gap-x gap-y rows cols)
  (apply #'set-format self  keyword-pairs)
  (if (eq gap-y :prompt)
    (setf (getf (default-positions-of self) :gap-y)
          (if (or (and rows (listp rows)) (= (nrows-of self) 1))
            0
            (wb:prompt-user 
             :type 'number 
             :read-type :eval
             :prompt-string (format nil "Vertical gap, as a fraction of height?")))))
  (if (eq gap-x :prompt)
    (setf (getf (default-positions-of self) :gap-x)
          (if (or (and cols (listp cols)) (= (ncols-of self) 1))
            0
            (wb:prompt-user 
             :type 'number 
             :read-type :eval
             :prompt-string (format nil "Horizontal gap, as a fraction of width?"))))))







(defmethod grid-position-subviews ((self grid-layout)
                                   &key rows cols square? bounding-region)
  
  
  (let ((subs (layout-views-of self))
        (big-region (subview-position-region  self))
        sub-regions all-regions
        (r (nrows-of self)) (c (ncols-of self))
        (gap-x (getf (default-positions-of self) :gap-x))
        (gap-y (getf (default-positions-of self) :gap-y)))
    
    (when subs
     (setq sub-regions
            (tile-region-list big-region 
                              r c (if (or rows cols) nil square?) 
                              gap-x
                              gap-y))
      (if cols
        (loop for reg in sub-regions
              for i upfrom 0
              for j = (* 2 (mod i c))
              do
              (setf (left-of reg) (elt cols j))
              (setf (right-of reg) (elt cols (1+ j)))))
      (if rows
        (loop for reg in sub-regions
              for i upfrom 0
              for j = (* 2 (truncate  i c))
              do
             (setf (top-of reg) (elt rows j))
             (setf (bottom-of reg) (elt rows (1+ j)))))
      
      (loop for views in subs
            for r in sub-regions
            do (loop repeat (length views) do
                     (push (make-region r) all-regions)))
      (setf (sub-view-locns-of self) (nreverse all-regions))
      (setf (sub-views-of self) (reduce #'append subs)))
    (if (null bounding-region) 
      (set-bounding-region self :region
                           
      (cond  ((and rows cols)
              (make-region (first cols) (car (last cols))
              (car (last rows)) (first rows) ))
             (rows
             (make-region  0 1
              (car (last rows)) (first rows) ))
             (cols (make-region (first cols) (car (last cols))
              0 1))
             (t (make-region)))))
    (constrain-bounds self )))


(defmethod equate-hist-bins ((self grid-layout) &key (draw? nil))
  
  (if (link-bounds-x-p self)
    (loop for views in (case (link-bounds-x-p self)
                         (:by-row (row-format self))
                         (:by-col (col-format self))
                         (t (list (apply #'nconc (row-format self)))))
          for hists = (loop for v in (flatten-views views)
                            when (and (typep v 'histogram-view)
                                      (break-points-of v)
                                      (eql (orientation-of v) :horizontal))
                            collect v)
          when hists                 
          do
          (let* ((hist1 (car hists))
                 (br (bounding-region-of hist1))
                 (nbins (- (length (break-points-of hist1)) 1))
                 (breaks (compute-bins (left-of br) (right-of br) nbins)))
            (with-constrained-extent hists :x draw?
              (loop for h in hists do
                    (new-sub-views h :break-points  breaks)
                    (init-position-subviews h)
                    (set-bounding-region h ))))))
  
  (if (link-bounds-y-p self)
    (loop for views in (case (link-bounds-y-p self)
                         (:by-row (row-format self))
                         (:by-col (col-format self))
                         (t (list (apply #'nconc (row-format self)))))
          for hists = (loop for v in (flatten-views views)
                            when (and (typep v 'histogram-view) (break-points-of v)
                                      (eql (orientation-of v) :vertical))
                            collect v)
          when hists                
          do
          (let* ((hist1 (car hists))
                 (br (bounding-region-of hist1))
                 (nbins (- (length (break-points-of hist1)) 1))
                 (breaks (compute-bins (bottom-of br) (top-of br) nbins)))
            (with-constrained-extent hists :y draw?
            (loop for h in hists do
                  (new-sub-views h :break-points  breaks)
                  (init-position-subviews h)
                  (set-bounding-region h )))))))

(defmethod proportional-boxp-bars ((self grid-layout) &key (draw? nil))
  (let ((redraw-views nil))
    (if (link-bounds-x-p self)
      (loop for views in (case (link-bounds-x-p self)
                           (:by-row (row-format self))
                           (:by-col (col-format self))
                           (t (list (apply #'nconc (row-format self)))))
            for boxps = (loop for v in (flatten-views views)
                              when (and (typep v 'boxplot-view)
                                        (equal-areas-p v)
                                        (eql (orientation-of v) :vertical))
                              collect v)
            when boxps                 
            do
            (let ((mcpu (loop for b in boxps
                              for mc = (max-count-per-unit b)
                              when mc maximize mc)))
              (when mcpu
                (loop for b in boxps
                      do (proportional-bar-widths b mcpu))))
            (if draw? (setq redraw-views (append redraw-views boxps )))))
    
    
    (if (link-bounds-y-p self)
      (loop for views in (case (link-bounds-y-p self)
                           (:by-row (row-format self))
                           (:by-col (col-format self))
                           (t (list (apply #'nconc (row-format self)))))
            for boxps = (loop for v in (flatten-views views)
                              when (and (typep v 'boxplot-view) (equal-areas-p v)
                                        (eql (orientation-of v) :horizontal))
                              collect v)
            when boxps                
            do
            (let ((mcpu (loop for b in boxps
                              for mc = (max-count-per-unit b)
                              when mc maximize mc)))
              (when mcpu
                (loop for b in boxps
                      do (proportional-bar-widths b mcpu))))
            (if draw? (setq redraw-views (append redraw-views boxps )))))
    (if draw?       
      (loop for v in redraw-views
            do (remap-to-viewports v :erase? t :draw? draw?)))))




(defmethod constrain-bounds ((self grid-layout) &key draw? (link-bounds-x? :ignore)
                             (link-bounds-y? :ignore))
  
  (unless (eq link-bounds-x? :ignore)
    (setf (link-bounds-x-p self) link-bounds-x?))
  (unless (eq link-bounds-y? :ignore)
    (setf (link-bounds-y-p self) link-bounds-y?))
  
  (if (eq (link-bounds-x-p self) :prompt)
    (setf (link-bounds-x-p self)
          (cadar (wb:prompt-for-items '(("No link" nil)
                                        ("Link all" t)
                                        ("By row" :by-row)
                                        ("By col" :by-col))
                                      :prompt-text "Link-bounds-X?"
                                      :item-function #'first))))
  
  (if (eq (link-bounds-y-p self) :prompt)
    (setf (link-bounds-y-p self)
          (cadar (wb:prompt-for-items '(("No link" nil)
                                        ("Link all" t)
                                        ("By row" :by-row)
                                        ("By col" :by-col))
                                      :prompt-text "Link-bounds-Y?"
                                      :item-function #'first))))
  (setq link-bounds-x? (link-bounds-x-p self))
  (setq link-bounds-y? (link-bounds-y-p self))
  
  (if link-bounds-x?
    (loop for views in (case link-bounds-x?
                         (:by-row (row-views self))
                         (:by-block-row (merge-rows self))
                         (:by-block-col (merge-cols self))
                         (:by-col (col-views self))
                         (t (list (apply #'append (row-views self)))))
          for x-views = (loop for v in views
                              when (linkable-bounds-x-p v) collect v)
          for d-views = (loop for v in x-views 
                              when (and (typep v 'd-view) (use-x-axis-p v)) 
                              collect v)
          when (> (length x-views) 1)
          do
          (link-view-bounds x-views :x)
          (set-view-extents x-views :x :region (if d-views (maximize-x-extents d-views))
                            :recompute? nil )))
  (if link-bounds-y?
    (loop for views  in (case link-bounds-y?
                          (:by-row (row-views self))
                          (:by-block-row (merge-rows self))
                         (:by-block-col (merge-cols self))
                          (:by-col (col-views self))
                          (t (list (apply #'append (row-views self)))))
          for y-views = (loop for v in (flatten-views views)
                              when (linkable-bounds-y-p v) collect v)
          for d-views = (loop for v in y-views when (and (typep v 'd-view) (use-y-axis-p v)) collect v)
          when (> (length y-views) 1)
          do
          (link-view-bounds y-views :y)
          (set-view-extents y-views :y :recompute? nil :region (if d-views (maximize-y-extents d-views)))))
  (equate-hist-bins self)
  (proportional-boxp-bars self)
  (when (or link-bounds-x? link-bounds-y?)
    (remap-to-viewports self :erase? draw? :draw? draw?)))


(defmethod change-variable :after ((self grid-layout) 
                                    &key draw?)
  (proportional-boxp-bars self :draw? draw?)        
  (equate-hist-bins self :draw? draw?))


(defmethod get-view-row ((self grid-layout) row)
  (let ((ncols (ncols-of self))) 
    (subseq (layout-views-of self) (* row ncols) (min (length (subviews-of self))
                                                   (* (+ 1 row) ncols)))))


(defmethod get-view-col ((self grid-layout) col)
  (let ((ncols (ncols-of self))) 
    (loop with subs = (layout-views-of self)
          for i from col below (length subs) by ncols 
          collect (elt subs i))))



(defmethod nrows-of ((self grid-layout))
  (let ((lf (layout-format-of self)))
    (unless (listp lf)
      (setq lf (list nil nil)))
  (or (car lf)
      (let* ((n (length (layout-views-of self)))
             (ncols (second lf))
             (nrows 
              (cond ((eq ncols 0) 0)
                    (ncols (ceiling n ncols))
                    ((<= n 1) n)
                    (t
                     (wb:prompt-user 
                      :type 'number 
                      :read-type :eval
                      :prompt-string (format nil "~S views, how many rows?" n))))))
        (setf (layout-format-of self) (list nrows ncols))
        nrows))))

(defmethod ncols-of ((self grid-layout))
  (let ((lf (layout-format-of self)))
    (unless (listp lf)
      (setq lf (list nil nil)))
  (or (second lf)
      (let* ((n (length (layout-views-of self)))
             (nrows (car lf))
             (ncols
              (cond ((eq nrows 0) 0)
                    (nrows (ceiling  n nrows))
                    ((<= n 1) n)
                    (t (wb:prompt-user 
                        :type 'number 
                        :read-type :eval
                        :prompt-string (format nil "~S views, how many columns?" n))))))
        (setf (layout-format-of self) (list nrows ncols))
        ncols))))

  
       

(defmethod row-format ((self grid-layout))
  (loop with nrows = (nrows-of self) with ncols = (ncols-of self)
        with evs = (layout-views-of self)
        for i from 0 below nrows collect
        (loop for j below ncols collect (car evs) 
              do
              (setf evs (cdr evs)))))

(defmethod col-format ((self grid-layout))
  (apply #'mapcar #'list (row-format self)))


    
(defmethod allowed-subview-movement ((self grid-layout) subview)
  (declare (ignore subview))
  :none)


(defmethod use-x-axis-p ((self grid-layout))
  nil)

(defmethod use-y-axis-p ((self grid-layout))
  nil)

#|
(defmethod grid-margin-region-info ((self grid-layout) 
                                    (axis (eql :vertical))
                                    index)
  (let* ((views (get-view-row self index))
         (v (loop for vi in (flatten-views views)
                  until (typep vi '2d-view)
                  finally (return vi))))
    
    (if v (if (typep v 'd-view) 
            (smallest-bounding-region v)
            (bounding-region-of v))
        )))


(defmethod grid-margin-region-info ((self grid-layout) 
                                    (axis (eql :horizontal))
                                    index)
  (let* ((views (get-view-col self index))
         (v (loop for vi in (flatten-views views)
                  until (typep vi '2d-view)
                  finally (return vi))))
    (if v (if (typep v 'd-view) 
            (smallest-bounding-region v)
            (bounding-region-of v))
        )))
|#

(defmethod coord-string-x ((self grid-layout))
  (loop for views in (col-format self)
        for vf = (flatten-views views)
        collect
        (or (loop for vi in vf
                  thereis (and (typep vi '2d-view) (coord-string-x vi)))
            (loop for vi in vf
                  thereis (and (typep vi '1d-view) (coord-string vi)))
            (loop for vi in vf 
                  thereis (coord-string-x vi))
            "")))


(defmethod coord-string-y ((self grid-layout))
  (loop for views in (row-format self)
        for vf = (flatten-views views)
        collect
        (or (loop for vi in vf
                  thereis (and (typep vi '2d-view) (coord-string-y vi)))
            (loop for vi in vf
                  thereis (and (typep vi '1d-view) (coord-string vi)))
            (loop for vi in vf  
                  thereis (coord-string-y vi))
            "")))


(defmethod row-views ((self grid-layout))
  (if (subviews-of self)
  (loop with new
        for views in (row-format self)
        for v = (remove nil (mapcar #'row-views (reduce #'append views)))
        for n = (length (car v))
        when (every #'(lambda(x) (= (length x) n)) (cdr v))
        do (setq new  (make-list n ))
        and append
        (loop for vi in v do
              (loop for vij in vi
                    for j upfrom 0
                    do  (setf (nth    j new) (append (nth j new) vij)))
              finally (return new)))))



(defmethod col-views ((self grid-layout))
  (if (subviews-of self)
 
  (loop  with new
         for views in (col-format self)
         for v = (remove nil (mapcar #'col-views (reduce #'append views)))
         for n = (length (car v))
         when (every #'(lambda(x) (= (length x) n)) (cdr v))
         do (setq new  (make-list n ))
        and  append
         (loop for vi in v do
               (loop for vij in vi
                     for j upfrom 0
                     do  (setf (nth    j new) (append (nth j new) vij)))
               finally (return new)))))

 
(defmethod merge-rows ((self grid-layout))
  (let* ((rows (row-views self))
         (nrows (length rows))
         (nblocks (nrows-of self))
         (rows-per-block (ceiling  nrows nblocks)))
    (loop with new = (make-list rows-per-block)
          for r in rows
          for i upfrom 0
          for j = (mod i rows-per-block) do
          (setf (nth    j new) (append (nth j new) r))
          finally (return new))))

(defmethod merge-cols ((self grid-layout))
  (let* ((cols (col-views self))
         (ncols (length cols))
         (nblocks (ncols-of self))
         (cols-per-block (ceiling  ncols nblocks)))
    (loop with new = (make-list cols-per-block)
          for r in cols
          for i upfrom 0
          for j = (mod i cols-per-block) do
          (setf (nth    j new) (append (nth j new) r))
          finally (return new))))
          
                               
#|
(defmethod merge-rows ((self grid-layout))
  (loop with all = (loop for views in (row-format self)
                         collect (mapcar #'row-views (reduce #'append views)))

        with n = (loop for v in all
                       maximize (apply #'max (mapcar #'length v)))
        with new = (make-list n )
        for views in all
        do      (loop for vi in views do
                      (loop for vij in vi
                            for j upfrom 0
                            do  (setf (nth    j new) (append (nth j new) vij))))
        finally (return new)))


 


(defmethod merge-cols ((self grid-layout))
 (loop with all = (loop for views in (col-format self)
                         collect (mapcar #'col-views (reduce #'append views)))
        with n = (loop for v in all
                       maximize (apply #'max (mapcar #'length v)))
        with new = (make-list n )
        for views in all
        do      (loop for vi in views do
                      (loop for vij in vi
                            for j upfrom 0
                            do  (setf (nth    j new) (append (nth j new) vij))))
        finally (return new)))

|#
