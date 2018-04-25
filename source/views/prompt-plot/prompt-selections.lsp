;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               prompt-selections.lisp
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
;;;     C.B. Hurley 1994 George Washington University
;;;     

(in-package :vw)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(selected-view-window  top-view top-views batch-plot-cases 
           prompt-plot-args
           selected-dataset  link-selected-views unlink-selected-views list-plot-cases)))

(defun selected-view-window ()
  (let ((w (first (wb::canvases))))
    (if (and (wb:at-top-p w) (typep w 'view-window))
      w)))

(defun top-views()
  (let* (( c (selected-view-window))
        (vw  (if c (viewports-and-views-of c))))
    (if vw (mapcar #'cdr vw))))

(defun all-views()
  (loop for c in (wb::canvases)
        when (typep c 'view-window)
        append (mapcar #'cdr (viewports-and-views-of c))))
   

(defun top-view()
  (car (top-views)))

(defun selected-dataset (plots)
  (let ((p (or (loop for p in plots
                    thereis (and (typep p 'plot) (dataset-p (viewed-object-of p)) p))
               (loop for p in plots
                    thereis (and (dataset-p (viewed-object-of p)) p)))))
      (if p
        (viewed-object-of p))))

(defun current-dataset()
  (declare (special *current-dataset*))
  (or (selected-dataset (top-views))
      *current-dataset*))



(defun link-selected-views(&rest views)
  (let ((v (or views (top-views) (all-views))))
    (if v
      (if (loop for vi in v
        thereis (has-highlit-subviews? vi))
        (apply #'link-highlit-views v)
      (apply #'link-views v)))))

(defun unlink-top-views(&rest views)
  (let ((v (or views (top-views) (all-views))))
    (if v
      (if (loop for vi in v
        thereis (has-highlit-subviews? vi))
        (apply #'unlink-highlit-views v)
      (apply #'unlink-views v)))))

(defun list-plot-cases(plots &key (highlit?) )
  (if plots
    (let* ((sub-list
          (cond   (highlit?
                   (find-highlit-cases plots))
                  (t
                   (find-cases plots)))))
    
      (values-list  sub-list))))



   
;;---------------------------------------------------------------------------

(defmethod find-cases((self view))
  (let ((vo (viewed-object-of self)))
    (if (list-of-datasets-p vo)
      (list vo nil)
      (list (list vo) (list self)))))

(defmethod find-highlit-cases((self view))
  (let ((vo (viewed-object-of self)))
    (if (list-of-datasets-p vo) 
      (cond ((and (typep self 'multiple-draw-style-mixin)
                  (= (length vo) (length (drawing-styles-of self))))
             (list (loop for d in (drawing-styles-of self)
                         for e in vo
                         when (draw-style d :highlight?)
                         collect e)))
            ((any-highlight? self)
             (list (list vo) nil))
            (t nil))
      
      (when (any-highlight? self)
        (list (list vo) (list self) )))))



(defmethod find-cases((view one-per-case-mixin))
  (loop for s in (sub-views-of view)
        for vo = (viewed-object-of s)
        for status in (case-status-of view)
        unless (ignore-status-p status)
        collect vo into vos and
        collect s into views
        finally (return (list vos views))))

(defmethod find-highlit-cases((view one-per-case-mixin))
  (loop for s in (sub-views-of view)
        for vo = (viewed-object-of s)
        for status in (case-status-of view)
        unless (ignore-status-p status)
        when (any-highlight? s) 
        collect vo into vos and
        collect s into views
        finally (return (list vos views))))

(defmethod find-cases((view case-display-list))
  (loop for s in (sub-views-of view)
        for vo = (viewed-object-of s)
        collect vo into vos
        finally (return (list vos nil))))

(defmethod find-highlit-cases((view case-display-list))
  (loop for s in (sub-views-of view)
        for vo = (viewed-object-of s)
        when (any-highlight? s)
        collect vo into vos
        finally (return (list vos ))))


(defmethod find-cases((view d-view))
  (loop for vo in (cases-of view)
        for status in (case-status-of view)
        unless (ignore-status-p status)
        collect vo into vos 
        finally (return (if vos (list vos)))))

(defmethod find-highlit-cases((view d-view))
  (loop with vos 
        for s in (sub-views-of view)
        for vo = (viewed-object-of s)
        when (list-of-datasets-p vo) do
        (cond ((and (typep s 'multiple-draw-style-mixin)
                    (= (length vo) (length (drawing-styles-of s))))
               (loop for d in (drawing-styles-of s)
                     for e in vo
                     when (draw-style d :highlight?)
                     do (push e vos)))
              ((any-highlight? s)
               (setq vos (append vo vos)))
              (t nil))
        else do
        (when (any-highlight? s)
          (push vo vos ))
        finally (return (if vos (list (nreverse vos) nil)))))

(defmethod find-cases((view display-list))
  (loop for s in (sub-views-of view)
        for vo = (viewed-object-of s)
        when (list-of-datasets-p vo)
        append vo into vos
        else 
        collect vo into vos 
        finally (return (if vos (list vos nil)))))

(defmethod find-highlit-cases((view display-list))
  (loop with vos 
        for s in (sub-views-of view)
        for vo = (viewed-object-of s)
        when (list-of-datasets-p vo) do
        (cond ((and (typep s 'multiple-draw-style-mixin)
                    (= (length vo) (length (drawing-styles-of s))))
               (loop for d in (drawing-styles-of s)
                     for e in vo
                     when (draw-style d :highlight?)
                     do (push e vos)))
              ((any-highlight? s)
               (setq vos (append vo vos)))
              (t nil))
        else do
        (when (any-highlight? s)
          (push vo vos ))
        finally (return (if vos (list (nreverse vos) nil)))))

(defmethod find-cases((view plot))
  (find-cases (interior-view-of view)))

(defmethod find-highlit-cases((view plot))
  (find-highlit-cases (interior-view-of view)))

(defmethod find-highlit-cases((view compound-view))
  (loop for v in (subviews-of view)
        thereis (find-highlit-cases v)))

(defmethod find-cases((view compound-view))
  (loop for v in (subviews-of view)
        thereis (find-cases v)))

(defmethod find-highlit-cases((view pairs-layout))
  (loop for v in (subviews-of view)
        thereis (find-highlit-cases v)))

(defmethod find-cases((view pairs-layout))
  (loop for v in (subviews-of view)
        thereis (find-cases v)))


(defmethod find-cases((view view-layout))
  (loop with answer
        for v in (subviews-of view)
        for cs = (find-cases v)
        for c = (first cs)
        for s = (second cs) when c do
        (setq answer 
              (if s
                (append answer (mapcar #'cons c s))
                (append answer (mapcar #'list c))))
        finally (setq answer (remove-duplicates answer :key #'first :test #'eq-dataset))
        (return (loop for (x . y) in answer collect x into xs
                           when y collect y into ys
                           finally (return
                                    (if (= (length xs) (length ys))
                                      (list xs ys)
                                      (list xs nil)))))))


(defmethod find-highlit-cases((view view-layout))
  (loop with answer
        for v in (subviews-of view)
        for cs = (find-highlit-cases v)
        for c = (first cs)
        for s = (second cs) when c do
        (setq answer 
              (if s
                (append answer (mapcar #'cons c s))
                (append answer (mapcar #'list c))))
        finally (setq answer (remove-duplicates answer :key #'first :test #'eq-dataset))
        (return (loop for (x . y) in answer collect x into xs
                            when y collect y into ys
                           finally (return
                                    (if (= (length xs) (length ys))
                                      (list xs ys)
                                      (list xs nil)))))))


(defmethod find-cases((view table-layout))
  (list
   (loop  for v in (subviews-of view)
        append (list-viewed-elements v)) nil))

(defmethod find-highlit-cases((view table-layout))
  (list
   (loop  for v in (subviews-of view)
         append
         (if (typep v 'bar)
         (loop for e in (list-viewed-elements v)
               for d in (drawing-styles-of v)
               when ( draw-style d :highlight?)
               collect e)
        (car (find-highlit-cases v))))
   nil))


(defmethod find-cases((view bar-chart))
  (list
   (loop  for v in (subviews-of view)
        append (list-viewed-elements v)) nil))

(defmethod find-highlit-cases((view bar-chart))
  (list
   (loop  for v in (subviews-of view)
         append
         (if (typep v 'bar)
         (loop for e in (list-viewed-elements v)
               for d in (drawing-styles-of v)
               when ( draw-style d :highlight?)
               collect e)
        (car (find-highlit-cases v))))
   nil))



(defmethod find-cases((views list))
  (loop with answer
        for v in views
        for cs = (find-cases v)
        for c = (first cs)
        for s = (second cs)  when c do
        (setq answer 
              (if s
                (append answer (mapcar #'cons c s))
                (append answer (mapcar #'list c))))
        finally (setq answer (remove-duplicates answer :key #'first :test #'eq-dataset))
        (return (loop for (x . y) in answer collect x into xs
                           when y collect y into ys
                           finally (return
                                    (if (= (length xs) (length ys))
                                      (list xs ys)
                                      (list xs nil)))))))
                                     
        
(defmethod find-highlit-cases((views list))
  (loop with answer
        for v in views
        for cs = (find-highlit-cases v)
        for c = (first cs)
        for s = (second cs) when c  do
        (setq answer 
              (if s
                (append answer (mapcar #'cons c s))
                (append answer (mapcar #'list c))))
        finally (setq answer (remove-duplicates answer :key #'first :test #'eq-dataset))
        (return (loop for (x . y) in answer collect x into xs
                           when y collect y into ys
                           finally (return
                                    (if (= (length xs) (length ys))
                                      (list xs ys)
                                      (list xs nil)))))))



(defun batch-plot-cases(plots &key highlit?)
  (let* ((subject-views (append (descendant-views-of-type plots 'd-view)
                                (descendant-views-of-type plots 'case-display-list)))
         colors cases result dataset plot group-vals)
    
    (if (setq plot
              (loop for p in plots
                    thereis (and (dataset-p (viewed-object-of p)) p)))
      (setq dataset (viewed-object-of plot)))
    
    (if highlit? 
      (loop for view in subject-views
           ;; until cases 
            do
            
            (if (typep view '(or one-per-case-mixin case-display-list))
              (loop with status = (if (typep view 'd-view) (case-status-of view))
                    for s in (sub-views-of view)
                    for vo = (viewed-object-of s)
                    for i upfrom 0
                    unless (and status (ignore-status-p (elt status i)))
                    when (any-highlight? s)
                    do (push vo cases)
                    (push (draw-style s :color) colors))
              
              (loop  for s in (sub-views-of view)
                     for vo = (viewed-object-of s)
                     for i upfrom 0
                     when (list-of-datasets-p vo) do
                     (cond ((and (typep s 'multiple-draw-style-mixin)
                                 (= (length vo) (length (drawing-styles-of s))))
                            
                            (loop for d in (drawing-styles-of s)
                                  for e in vo
                                  when (draw-style d :highlight?)
                                  do (push e cases) (push (draw-style d :color) colors)))
                           ((and (typep s 'multiple-draw-style-mixin) (any-highlight? s))
                            
                            (setq cases (append vo cases))
                            (setq colors 
                                  (append (loop for c in cases
                                                collect (draw-style  s :color :element c ))
                                          colors)))
                           ((and (typep s 'single-draw-style-mixin) (draw-style s :highlight?))
                            (push (draw-style s :color) colors)
                            (push (cons 'xxlist vo) cases))
                           (t nil))
                     else do
                     (when (any-highlight? s)
                       (push vo cases )
                       (push (draw-style s :color) colors)))))
      
      
      (loop for view in subject-views
           ;; until cases
            do
            (if (typep view '(or one-per-case-mixin case-display-list))
              (loop  with status = (if (typep view 'd-view) (case-status-of view))
                     for s in (sub-views-of view)
                     for vo = (viewed-object-of s)
                     for i upfrom 0
                     unless (and status (ignore-status-p (elt status i))) do
                     
                     (push vo cases)
                     (push (draw-style s :color) colors))
              
              (loop   for s in (sub-views-of view)
                      for vo = (viewed-object-of s)
                      for i upfrom 0
                      when (list-of-datasets-p vo) do
                      (cond ((and (typep s 'multiple-draw-style-mixin)
                                  (= (length vo) (length (drawing-styles-of s))))
                             
                             (loop for d in (drawing-styles-of s)
                                   for e in vo
                                   do (push e cases) (push (draw-style d :color) colors)))
                            ((typep s 'multiple-draw-style-mixin)
                             (setq cases (append vo cases))
                             (setq colors 
                                   (append (loop for c in cases
                                                 collect (draw-style  s :color :element c ))
                                           colors)))
                            ((typep s 'single-draw-style-mixin)
                             (push (draw-style s :color) colors)
                             (push (cons 'xxlist vo) cases))
                            )
                      else do
                      (push vo cases )
                      (push (draw-style s :color) colors)))))
    
    
    (setq group-vals (remove-duplicates colors :test #'wb:eq-colors))
    (setq result
          (loop with groups = (make-list (length group-vals))
                for case in cases
                for col in colors
                for p = (position  col group-vals :test #'wb:eq-colors)
                when p
                do (if (and (listp case) (eql (car case) 'xxlist))
                     (nconc (elt groups p) (cdr case))
                     (push case (elt groups p)))
                finally (return (loop for g in groups
                                      collect (remove-duplicates g :test #'eq-dataset)))))
    
    (values (if dataset
              (loop for r in result
                    collect (make-data-subset dataset r))
              result) (list group-vals))))


(defgeneric prompt-plot-args(dataset  case-list &optional vars)
  (:documentation "Used by prompted plots to transform cases. "))


(defgeneric prompt-plot-batches(dataset  batches)
  (:documentation "Used by prompted plots to transform batches."))










(defmethod prompt-plot-args((dataset t) case-list &optional vars)
  (if (and (= (length case-list) 1)
           (dataset-with-info-p (car case-list)))
    (values (car case-list) (list-cases (car case-list)) vars)
    (values dataset (if (list-of-datasets-p case-list) case-list) vars )))

(defmethod prompt-plot-batches((dataset t) batches)
  (values dataset batches))



(defun dataset-with-info-p(d)
  (and (dataset-p d)
       (dataset-name d)
       (list-variates d)))
