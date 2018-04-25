;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               prompt-plot.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(selected-view-window view-of-selected-window top-view plot-of-selected-window
          cases-of-selected-plot single-plot group-plot prompt-plot
          prompt-single-plot prompt-plot-by
          prompt-variable-display prompt-case-display prompt-case-display-by
          *auto-share-case-views?* plot-batch-fn-by-color)))

(defvar *auto-share-case-views?* t)

(defgeneric cases-of-selected-plot(plot)
  (:documentation "Returns the cases of the selected plot as a list."))

(defgeneric plot-batch-fn-by-color (plot &optional subviews subjts)
  (:documentation "Returns a function that will batch the cases in~
                   plot by color"))

(defun selected-view-window ()
  (let ((w (first (wb::canvases))))
    (if (typep w 'view-window)
      w)))

(defun view-of-selected-window()
  (let* ((c (selected-view-window))
        (vw (if c (viewports-and-views-of c))))
    (if vw (cdar vw))))


(defun top-view()
  (let* (( c (first (ccl::windows :class 'view-window)))
        (vw  (viewports-and-views-of c)))
    (cdar vw)))



(defun plot-of-selected-window()
  (let ((p (view-of-selected-window)))
    (and (typep p 'plot) p)))
    



(defmethod cases-of-selected-plot(sel-plot)
  (if (null sel-plot)
    (setq sel-plot (plot-of-selected-window)))
  (if (typep sel-plot 'plot)
    (cases-of-selected-plot sel-plot )))


(defmethod cases-of-selected-plot((self plot))
  (let* ((one-per-case-view (car (descendant-views-of-type self 'one-per-case-mixin)))
           (views
            (if one-per-case-view 
              (or (loop for sub in (subviews-of one-per-case-view)
                            when 
                    (draw-style sub :highlight?)
                    collect sub)
                  (loop for sub in (subviews-of one-per-case-view)
                    for s in (case-status-of one-per-case-view)
                    when 
                    (active-status-p s)
                    collect sub)))))
    
      ;;(if views (values (mapcar #'viewed-object-of views) views))
      (if views (values (loop for v in views
                              append (list-viewed-elements v)) views))
))

(defun single-plot (dataset vars &optional sel-plot)
  "Constructs a plot from dataset and vars using the cases in~
   sel-plot if provided."
  (let* ((cases (cases-of-selected-plot sel-plot))
         (args (append (list :data dataset :cases cases )
                       (if *auto-share-case-views?* (list :case-views-from sel-plot ))))
         n)
    (if (numberp vars)
      (setq n vars  vars nil)
      (setq n (length vars)))
    (cond 
     ((= n 0)
      (let ((interior (choose-views-from-menu  :menu-list
                                               (view-menu-list '(and simple-view (not d-view)
                                                                 (not control-mixin))))))
        (if (and (= (length interior) 1)
                 (eq (car interior) 'label))
          (case-display-list :data dataset :cases cases)
          (plot :data dataset :interior-view 
                interior :cases cases))))
     ((= n 1) (let ((plot (car (wb:prompt-for-items (list "Default" "1d-plot")))))
                (cond ((string-equal plot "Default")
                       (if (fboundp 'default-1d-plot)
                         (apply #'default-1d-plot :var (first vars)  args)
                         (apply #'1d-plot :var (first vars) :interior-view :default args)))
                      ((string-equal plot "1d-plot")
                       (apply #'1d-plot :var (first vars) :interior-view :prompt args))
                      
                      (t nil))))
     ((= n 2) (let ((plot (car (wb:prompt-for-items (list "Default" "2d-plot" "Parallel")))))
                (cond ((string-equal plot "Default")
                       (if (fboundp 'default-2d-plot)
                         (apply #'default-2d-plot :x (first vars) :y (second vars)   args)
                         (apply #'2d-plot  :x (first vars) :y (second vars)  :interior-view :default
                                args )))
                      ((string-equal plot "2d-plot")
                       (apply #'2d-plot  :x (first vars) :y (second vars) :interior-view :prompt args ))
                      ((string-equal plot "Parallel")
                       (apply #'1d-layout-plot  :nvars 2 :vars vars args ))
                      (t nil))))
     ((>= n 3) 
      (let ((plot 
             (car (wb:prompt-for-items  
                   (append (if (fboundp 'default-d-plot) '("Default"))
                           (append (if (= n 3) '("Rotating Plot"))
                                   (list "Scatterplot Matrix" "Parallel"
                                         "Andrews' Trace" "Tukeys' Trace")))))))
        (cond ((string-equal plot "Default")
               (apply #'default-d-plot  :vars vars args))
              ((string-equal plot "Rotating Plot")
               (apply #'rotating-plot  :x (first vars) :y (second vars)
                      :z (third vars) args))
              ((string-equal plot "Scatterplot Matrix")
               (apply #'scat-mat  :vars vars args))
              ((string-equal plot "Parallel")
               (apply #'1d-layout-plot  :vars vars args ))
              ((string-equal plot "Andrews' Trace")
               (apply #'prompt-projection-trace  :andrews :vars vars  args))
              (plot (apply #'prompt-projection-trace  :tukey :vars vars  args))
              (t nil))))
     )))


(defun group-plot(dataset vars byvars &optional sel-plot (overlay-groups? nil))
  "Constructs a plot from dataset and vars for each value of byvars,~
   using the cases in sel-plot (if provided)."
  (let* ((dataset-vars (list-variates dataset))
         (x (second byvars))
         (y (first byvars))
         (cases (cases-of-selected-plot sel-plot))
         (batches (construct-batch-fn byvars sel-plot))
         (title (if (and x y) (format nil "~A by ~A" x y )
                    (format nil "~A" y)))
         (plot-args (list :title title :data dataset  
                          :left-label t :bottom-label t 
                          :gap-x 0.02 :gap-y 0.02))
         (batch-args (list :type 'batch-layout 
                           :cases cases
                           :data dataset 
                           :batches batches))
         n
         
         )
    (if (numberp vars)
      (setq n vars  vars nil)
      (setq n (length vars)))
    
    (flet ((gxf (self)
             (let ((c (car (cases-of self))))
               (if (and c x)
                 (format nil "~S" (value-of  c  x :vars dataset-vars))
                
                 )))
           (gyf (self)
             (let ((c (car (cases-of self))))
               (if (and c y)
                 (format nil "~S" (value-of  c  y :vars dataset-vars))
                 ))))
      
      
      (cond 
       ((= n 0)
        (let* (;;(menu-list (view-menu-list '(and simple-view (not d-view)
               ;;       (not control-mixin))))
               (menu-list (loop for l in '(label  bar-with-count rectangle-with-count)
                                collect (list (format nil "~A" l) l))) 
               (interior (choose-views-from-menu  :menu-list
                                                  menu-list)
                         ))
          (apply #'grid-plot 
                   :interior-view 
                   `(,@batch-args
                     :subview-type  ,interior
                     :subviews ((:coord-string-y ,#'gyf :coord-string-x ,#'gxf 
                                                 :text ,#'(lambda(vo)
                                                            (length (list-cases vo))))))
                   plot-args)))
       ((and (= n 1) overlay-groups?)
        )
       ((= n 1)
        (apply #'grid-plot 
               :interior-view   
               `(,@batch-args
                 :subview-superclass  1d-view
                 :subviews ((:coord-string-y ,#'gyf  :coord-string-x ,#'gxf 
                                          ,@(if vars  (list :var (car vars)))
                                             :case-views-from
                                             ,(if *auto-share-case-views?* 
                                               sel-plot))))
               plot-args))
       ((and (= n 2) overlay-groups?)
        )

       ((= n 2) 
        (apply #'grid-plot 
               :interior-view 
               `(,@batch-args
                 :subview-superclass 2d-view
                 :subviews ((:coord-string-y ,#'gyf 
                                             :coord-string-x ,(if (= 2 (length byvars)) #'gxf)
                                           ,@(if vars  (list :x (first vars) :y (second vars)))
                                             :case-views-from ,(if *auto-share-case-views?* 
                                               sel-plot))))
                 plot-args))
       (t nil)))))



(defmethod plot-batch-fn-by-color ((self plot) &optional subviews subjts)
  (if (null subjts)
    (multiple-value-setq (subjts subviews) 
        (cases-of-selected-plot  self)))
    #'(lambda(data &key cases)
            (declare (ignore  cases))
            (let* ((case-vals  (loop for sub in subviews
                                     collect (draw-style sub :color)))
                   (group-vals (remove-duplicates case-vals :test #'wb:eq-colors))
                   (result (loop with groups = (make-list (length group-vals) )
                                 for c in subjts
                                 for cval in case-vals
                                 for p = (position  cval group-vals :test #'wb:eq-colors)
                                 when p
                                 do (push c (elt groups p))
                                 finally (return groups))))
              (values
               (loop for r in result 
                     collect (make-data-subset data  r))
               (length result)))))




(defun construct-batch-fn(byvars &optional (sel-plot (plot-of-selected-window)))
  (let (subjts subviews)
    (if sel-plot
      (multiple-value-setq (subjts subviews) 
        (cases-of-selected-plot sel-plot)))
    (if (and (listp byvars) (member :view-color byvars))
      (plot-batch-fn-by-color sel-plot subviews subjts)
      
      (data-subsets-fn byvars))))


(defgeneric default-group-variable(obj)
  )
  

(defmethod default-group-variable ((self t)) nil)



(defun prompt-plot(&optional dim)
  "Prompts the user for inputs to construct a plot~
   of dimension dim (if provided)."
  (let* ((sel-plot (plot-of-selected-window))
         dataset vars byvars menu-list sel-byvar)
    (if (and  (typep sel-plot 'plot) (viewed-object-of sel-plot))
      (setq dataset (viewed-object-of sel-plot))
      (setq sel-plot nil
            dataset (choose-dataset)))
    (unless (numberp dim) (setq dim nil))
    (setq vars (cond
                  ((null dim) (choose-some-variables dataset 0 "Choose variables"))
                  (t dim)))
    (setq dim (if (numberp dim) dim (length vars)))
    (when (<= dim 2)
      (setq menu-list (variable-menu-items dataset))
      (if sel-plot (setq menu-list   (cons  (list "color" :view-color) menu-list))))
    (setq sel-byvar (default-group-variable dataset))
    (setq byvars 
          (if (zerop dim)
                (or (if sel-byvar
                      (list sel-byvar))
                    (choose-some-variables 
                     dataset 1  "Choose 1 or 2 by variables" menu-list))
                (if (<= dim 2)
                  (or (if sel-byvar
                      (list sel-byvar))
                  (choose-some-variables 
                   dataset 0 "Choose upto 2 by variables" menu-list)))))
    
    (if (zerop (length byvars))
      (single-plot dataset vars sel-plot)
      (group-plot dataset vars byvars sel-plot))))



(defun prompt-single-plot(&optional dim)
  "Prompts the user for inputs to construct a single plot~
   of dimension dim (if provided)."
  (let* ((sel-plot (plot-of-selected-window))
         dataset vars )
    (if (and  (typep sel-plot 'plot) (viewed-object-of sel-plot))
      (setq dataset (viewed-object-of sel-plot))
      (setq sel-plot nil
            dataset (choose-dataset)))
    (unless (numberp dim) (setq dim nil))
    (setq vars (cond
                ((null dim) (choose-some-variables dataset 0 "Choose plot variables"))
                (t dim)))
    (single-plot dataset vars sel-plot)))


(defun prompt-plot-by(&optional (dim nil))
  "Prompts the user for inputs to construct a group plot~
   of dimension dim (if provided)."
  (unless (numberp dim) (setq dim nil))
  (let* ((sel-plot (plot-of-selected-window))
         dataset vars byvars menu-list)
    (if (and  (typep sel-plot 'plot) (viewed-object-of sel-plot))
      (setq dataset (viewed-object-of sel-plot))
      (setq sel-plot nil
            dataset (choose-dataset)))
    (setq menu-list (variable-menu-items dataset))
    (if sel-plot (setq menu-list   (cons  (list "color" :view-color)
                                             menu-list)))

    (setq byvars 
          (if (default-group-variable dataset)
            (list (default-group-variable dataset))
            (if (eql 0 dim)
              (choose-some-variables 
               dataset 1  "Choose 1 or 2 by variables" menu-list)
              (choose-some-variables 
               dataset 0 "Choose upto 2 by variables" menu-list))))
    
    
    (setq vars (cond
                ((null dim) (choose-some-variables dataset 0 "Choose plot variables"))
                (t (min 2 dim))))
    
    (setq menu-list (cons  (if sel-plot  (list "color" :view-color))
                                             (variable-menu-items dataset)))
    
    
    (if (zerop (length byvars))
      (single-plot dataset vars sel-plot)
      (group-plot dataset vars byvars sel-plot))))



(defun prompt-variable-display(&optional ignore)
  "Prompts the user for inputs to construct a variable display"
  (declare (ignore ignore))
  (let* ((sel-plot (plot-of-selected-window))
         dataset)
    (if (and  (typep sel-plot 'plot) (viewed-object-of sel-plot))
      (setq dataset (viewed-object-of sel-plot))
      (setq sel-plot nil
            dataset (choose-dataset)))
    (variate-display-list :data dataset :draw? t)))


(defun prompt-case-display(&optional ignore)
  "Prompts the user for inputs to construct a case display"
  (declare (ignore ignore))
  (let* ((sel-plot (plot-of-selected-window))
         dataset)
    (if (and  (typep sel-plot 'plot) (viewed-object-of sel-plot))
      (setq dataset (viewed-object-of sel-plot))
      (setq sel-plot nil
            dataset (choose-dataset)))
    (case-display-list :data dataset :draw? t
                       :cases (cases-of-selected-plot sel-plot)
                       :labels #'(lambda(c) 
                                   (identifier-name (car (list-viewed-elements c)))))))


(defun prompt-case-display-by(&optional ignore)
  "Prompts the user for inputs to construct~
   a case display by group."
  (declare (ignore ignore))
  (let* ((sel-plot (plot-of-selected-window))
         dataset menu-list byvar batches dataset-vars)
    (if (and  (typep sel-plot 'plot) (viewed-object-of sel-plot))
      (setq dataset (viewed-object-of sel-plot))
      (setq sel-plot nil
            dataset (choose-dataset)))
    (setq dataset-vars (list-variates dataset))
    (setq menu-list (variable-menu-items dataset))
    (if sel-plot (setq menu-list   (cons  (list "color" :view-color) menu-list)))
    (setq byvar 
          (or (default-group-variable dataset)
              (choose-variable 
               dataset 1  "Choose group variable" menu-list)))
    (setq batches (construct-batch-fn byvar sel-plot))
    
    (case-display-list 
     :data dataset :draw? t :draw? t
     :labels #'(lambda(c) 
                 (value-of (car (list-viewed-elements c)) byvar :vars dataset-vars ))
     :cases (funcall batches dataset :cases (cases-of-selected-plot sel-plot)))))

