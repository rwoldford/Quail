;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               make-plots.lsp
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



(make-view-constructor-with-doc 'plot 'plot)
(make-view-constructor-with-doc 'standard-plot 'standard-plot)
(make-view-constructor-with-doc 'grid-plot 'grid-plot)
(make-view-constructor-with-doc 'd-plot 'd-plot)
(make-view-constructor-with-doc '1d-plot '1d-plot)
(make-view-constructor-with-doc '2d-plot '2d-plot)
(make-view-constructor-with-doc '3d-plot '3d-plot)


(let ((pie-local (make-view-constructor-fn 'plot 
                                           :initform-fn #'get-batch-inits
                                
                                            :default-interior 'pie
                                            :size nil)))
  (setf (get 'pie-plot 'view-class) 'plot)
  (pushnew 'pie-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'pie-plot))
  (defun pie-plot (&rest args   &key data  &allow-other-keys)
    "Produces a pie plot. ~
     (:key ~
     (:arg data :prompt The dataset to be displayed. ))~
     (:examples (:files 
     (general eg:Views;Plots;general.lsp)))~
     "
     (declare (ignore  data by batches var))
     (apply pie-local args))
  
  )


(let ((hist-local (make-view-constructor-fn '1d-plot 
                                            :default-interior 'histogram-view)))
  (setf (get 'histogram 'view-class) '1d-plot)
  (pushnew 'histogram *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'histogram))
  (defun histogram(&rest args   &key data  var &allow-other-keys)
    "Produces a histogram. ~
     (:key ~
     (:arg data :prompt The dataset whose values are to be plotted. ~
     If  :prompt, then var is to construct the plot.)~
     (:arg var :prompt The variate.))~
     (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
     (general eg:Views;Plots;general.lsp)))~
     (:see-also 
     (boxplot :function)~
     (dot-plot :function)~
     (scatterplot :function)~
     (rotating-plot :function)~
     (scat-mat :function))"
    (declare (ignore data var))
    (apply hist-local args))
  )

(let ((plot-local (make-view-constructor-fn '1d-plot 
                                            :default-interior 'boxplot-view)))
  (setf (get 'boxplot 'view-class) '1d-plot)
  (pushnew 'boxplot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'boxplot))
  (defun boxplot(&rest args   &key data  var &allow-other-keys)
    "Produces a boxplot. ~
     (:key ~
     (:arg data :prompt The dataset whose values are to be plotted. ~
     If  :prompt, then var is to construct the plot.)~
     (:arg var :prompt The variate.))~
     (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
     (general eg:Views;Plots;general.lsp)))~
     (:see-also 
     (histogram :function)~
     (dot-plot :function)~
     (scatterplot :function)~
     (rotating-plot :function)~
     (scat-mat :function))"
    (declare (ignore data var))
    (apply plot-local args))
  )

(let ((plot-local (make-view-constructor-fn '1d-plot 
                                            :default-interior '1d-point-cloud)))
  (setf (get 'dot-plot 'view-class) '1d-plot)
  (pushnew 'dot-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'dot-plot))
  (defun dot-plot(&rest args   &key data  var &allow-other-keys)
    "Produces a dot plot. ~
    (:key ~
    (:arg data :prompt The dataset whose values are to be plotted. ~
    If  :prompt, then var is to construct the plot.)~
    (:arg var :prompt The variate.))~
    (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
                       (general eg:Views;Plots;general.lsp)))~
    (:see-also 
     (histogram :function)~
    (boxplot :function)~
    (scatterplot :function)~
    (rotating-plot :function)~
    (rotating-lines-plot :function)~
    (scat-mat :function))"
    (declare (ignore data var))
    (apply plot-local args))
  )




(let ((plot-local (make-view-constructor-fn '2d-plot 
                                            :default-interior '2d-point-cloud)))
  (setf (get 'scatterplot 'view-class) '2d-plot)
  (pushnew 'scatterplot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'scatterplot))
  (defun scatterplot(&rest args   &key data   x y &allow-other-keys)
    "Produces a scatterplot of y versus x. ~
     (:key ~
     (:arg data :prompt The dataset whose values are to be plotted. ~
     If  :prompt, then x and y are used to construct the plot.)~
     (:arg x :prompt The x variate.)~
     (:arg y :prompt The y variate.))~
     (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
     (general eg:Views;Plots;general.lsp)))~
     (:see-also 
     (histogram :function)~
     (boxplot :function)~
     (dot-plot :function)~
     (lines-plot :function)~
     (fitted-line-plot :function)~
     (smooth-plot :function)~
     (line-segment-2d-plot :function)~
     (rotating-plot :function)~
     (scat-mat :function))"
    (declare (ignore data x y ))
    (apply plot-local args))
  )
  
(let ((plot-local (make-view-constructor-fn '2d-plot 
                                            :default-interior 'lines)))
  (setf (get 'lines-plot 'view-class) '2d-plot)
  (pushnew 'lines-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'lines-plot))
  (defun lines-plot(&rest args   &key data  x y  &allow-other-keys)
    "Produces a lines plot of y versus x. ~
    (x,y) points are joined by line segments in order of increasing x. ~
    (:key ~
    (:arg data :prompt The dataset whose values are to be plotted. ~
    If  :prompt, then x and y are used to construct the plot.)~
    (:arg x :prompt The x variate.)~
    (:arg y :prompt The y variate.))~
    (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
                       (general eg:Views;Plots;general.lsp)))~
    (:see-also 
    (histogram :function)~
    (scatterplot :function)~
    (lines-plot :function)~
    (smooth-plot :function)~
    
      (line-segment-2d-plot :function)~
    (rotating-plot :function)~
    (scat-mat :function))"
    (declare (ignore data x y ))
    (apply plot-local args))
  ) 

(let ((plot-local 
       (make-view-constructor-fn '2d-plot 
                                 :default-interior 'line-segments-per-case
                                 :initform-fn #'get-data-inits-2lists)))
  (setf (get 'line-segment-2d-plot 'view-class) '2d-plot)
  (pushnew 'line-segment-2d-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'line-segment-2d-plot))
  (defun line-segment-2d-plot(&rest args   &key data  x y  &allow-other-keys)
    "Produces a  plot with one or more line segment for each case, ~
    with endpoints given by the x and y coordinates. ~
    (:key ~
    (:arg data :prompt The dataset whose values are to be plotted. ~
    If  :prompt, then x and y are used to construct the plot.)~
    (:arg x :prompt The x variates.)~
    (:arg y :prompt The y variates.))~
    (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
                       (general eg:Views;Plots;general.lsp)))~
    (:see-also 
     (histogram :function)~
    (scatterplot :function)~
    (lines-plot :function)~
    (smooth-plot :function)~
    (line-segment-2d-plot :function)~
    (rotating-plot :function)~
    (scat-mat :function))"
    (declare (ignore data x y ))
    (apply plot-local args))
  ) 

(let ((plot-local 
       (make-view-constructor-fn 'rotating-plot)))
  (setf (get 'rotating-plot 'view-class) 'rotating-plot)
  (pushnew 'rotating-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'rotating-plot))
  (defun rotating-plot(&rest args   &key data  x y z &allow-other-keys)
    "Produces a rotating plot of x, y and z. ~
    (:key ~
    (:arg data :prompt The dataset whose values are to be plotted. ~
    If  :prompt, then x, y and z are used to construct the plot.)~
    (:arg x :prompt The x variate.)~
    (:arg y :prompt The y variate.)
    (:arg z :prompt The z variate.))~
    (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
                       (general eg:Views;Plots;general.lsp)))~
    (:see-also 
   (histogram :function)~
    (scatterplot :function)~
    (rotating-lines-plot :function)~
    (scat-mat :function))"
    (declare (ignore data x y z ))
    (apply plot-local args))
  )  


(let ((plot-local 
       (make-view-constructor-fn 'rotating-plot 
                                 :default-interior 'rotating-line-segments
                                 :initform-fn #'get-data-inits-3lists)))
  (setf (get 'rotating-lines-plot 'view-class) 'rotating-plot )
  (pushnew 'rotating-lines-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'rotating-lines-plot))
  (defun rotating-lines-plot(&rest args   &key data  x y z  &allow-other-keys)
    "Produces a rotating lines plot of segments ~
    with endpoints given by x, y and z. ~
    (:key ~
    (:arg data :prompt The dataset whose values are to be plotted. ~
    If  :prompt, then x, y and z are used to construct the plot.)~
    (:arg x :prompt The x variates.)~
    (:arg y :prompt The y variates.)
    (:arg z :prompt The z variates.))~
    (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
                       (general eg:Views;Plots;general.lsp)))~
    (:see-also 
     (histogram :function)~
    (scatterplot :function)~
    (lines-plot :function)~
    (line-segment-2d-plot :function)~
    (rotating-plot :function)~
    (scat-mat :function))"
    (declare (ignore data x y z ))
    (apply plot-local args))
  )  

  

(let ((plot-local (make-view-constructor-fn '2d-plot 
                                            :default-interior '(2d-point-cloud fitted-line))))
  (setf (get 'fitted-line-plot 'view-class) '2d-plot)
  (pushnew 'fitted-line-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'fitted-line-plot))
  (defun fitted-line-plot(&rest args   &key data  x y &allow-other-keys)
    "Produces a fitted line plot of y versus x. ~
    (:key ~
    (:arg data :prompt The dataset whose values are to be plotted. ~
    If  :prompt, then x and y are used to construct the plot.)~
    (:arg x :prompt The x variate.)~
    (:arg y :prompt The y variate.))~
    (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
                       (general eg:Views;Plots;general.lsp)))~
    (:see-also 
     (histogram :function)~
    (boxplot :function)~
    (dot-plot :function)~
     (lines-plot :function)~
    (smooth-plot :function)~
    
      (line-segment-2d-plot :function)~
    (rotating-plot :function)~
    (scat-mat :function))"
    (declare (ignore data x y))
    (apply plot-local args))
  )  
  
  
(let ((plot-local (make-view-constructor-fn '2d-plot 
                                            :default-interior '(2d-point-cloud smooth))))
  (setf (get 'smooth-plot 'view-class) '2d-plot)
  (pushnew 'smooth-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'smooth-plot))
  (defun smooth-plot(&rest args   &key data  x y &allow-other-keys)
     "Produces a smooth plot of y versus x. ~
    (:key ~
    (:arg data :prompt The dataset whose values are to be plotted. ~
    If  :prompt, then x and y are used to construct the plot.)~
    (:arg x :prompt The x variate.)~
    (:arg y :prompt The y variate.))~
    (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
                       (general eg:Views;Plots;general.lsp)))~
    (:see-also 
     (histogram :function)~
    (boxplot :function)~
    (dot-plot :function)~
     (lines-plot :function)~
    (fitted-line-plot :function)~
    
      (line-segment-2d-plot :function)~
    (rotating-plot :function)~
    (scat-mat :function))"
    (declare (ignore data x y))
    (apply plot-local args))
  )

#|
(let ((plot-local (make-view-constructor-fn '2d-plot 
                                            :default-interior 'lines)))
  (setf (get 'lines-plot 'view-class) '2d-plot)
  (pushnew 'lines-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'lines-plot))
  (defun lines-plot(&rest args   &key data  x y &allow-other-keys)
      "Produces a lines plot of y versus x, where coordinates are joined by line segments ~
    (:key ~
    (:arg data :prompt The dataset whose values are to be plotted. ~
    If  :prompt, then x and y are used to construct the plot.)~
    (:arg x :prompt The x variate.)~
    (:arg y :prompt The y variate.))~
    (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
                       (general eg:Views;Plots;general.lsp)))~
    (:see-also 
     (histogram :function)~
    (boxplot :function)~
    (dot-plot :function)~
     (simple-lines-plot :function)~
    (fitted-line-plot :function)~
    (smooth-plot :function)~
    
      (line-segment-2d-plot :function)~
    (rotating-plot :function)~
    (scat-mat :function))"
    (declare (ignore data x y))
    (apply plot-local args))
  )
|#  

(let ((plot-local (make-view-constructor-fn '2d-plot 
                                            :default-interior 'simple-lines)))
  (setf (get 'simple-lines-plot 'view-class) '2d-plot)
  (pushnew 'simple-lines-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'simple-lines-plot))
  (defun simple-lines-plot(&rest args   &key data  x y &allow-other-keys)
    "Produces a lines plot of y versus x, where coordinates are joined by line segments ~
     (:key ~
     (:arg data :prompt The dataset whose values are to be plotted. ~
     If  :prompt, then x and y are used to construct the plot.)~
     (:arg x :prompt The x variate.)~
     (:arg y :prompt The y variate.))~
     (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
     (general eg:Views;Plots;general.lsp)))~
     (:see-also 
     (histogram :function)~
     (boxplot :function)~
     (dot-plot :function)~
     (lines-plot :function)~
     (fitted-line-plot :function)~
     (smooth-plot :function)~
     (line-segment-2d-plot :function)~
     (rotating-plot :function)~
     (scat-mat :function))"
    (declare (ignore data x y))
    (apply plot-local args))
  )


(let ((plot-local (make-view-constructor-fn 'standard-plot 
                                            :default-interior 'function-view)))
  (setf (get 'function-plot 'view-class) 'standard-plot)
  (pushnew 'function-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'function-plot))
  (defun function-plot(&rest args   &key function &allow-other-keys)
    "Produces a plot of a single variable function.~
    (:key ~
    (:arg function :prompt The function to be plotted.)) ~
    (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
                       (general eg:Views;Plots;general.lsp)))"
    (declare (ignore function))
    (apply plot-local args))
  )
  
(let ((plot-local (make-view-constructor-fn 'standard-plot 
                                            :default-interior 'line-segment)))
  (setf (get 'line-segment-plot 'view-class) 'standard-plot)
  (pushnew 'line-segment-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'line-segment-plot))
  (defun line-segment-plot(&rest args   &key  endpoints &allow-other-keys)
    "Produces a plot of line segment(s).~
    (:key ~
    (:arg endpoints '((0 0) (1 1)) The endpoint of the line segment(s))) ~
    (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
                       (general eg:Views;Plots;general.lsp)))"
    (declare (ignore  endpoints))
    (apply plot-local args))
  )


(let ((plot-local 
       (make-view-constructor-fn 'grid-plot
                                 :default-interior 'pairs-layout
                                 :left-view t :bottom-view t
                                 :link-bounds-x? :by-col :link-bounds-y? :by-row)))
  (setf (get 'scat-mat 'view-class) 'grid-plot)
  (pushnew 'scat-mat *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'scat-mat))
  (defun scat-mat(&rest args   &key data vars &allow-other-keys)
     "Produces a scatterplot matrix. ~
    (:key ~
    (:arg data :prompt The dataset whose values are to be plotted. ~
    If  :prompt, then vars  are used to construct the plot.)~
    (:arg vars :prompt The variates.))~
    (:examples (:files (grid-plot eg:Views;Plots;grid-plot.lsp)~
                       (general eg:Views;Plots;general.lsp)))~
    (:see-also 
     (scatterplot :function)~
    (1d-layout-plot :function)~
    (xy-layout-plot :function)~
    (batch-plot :function)~
    (case-layout-plot :function)~
    )"
    (declare (ignore  data vars))
    (apply plot-local args))
  )



(let ((plot-local 
       (make-view-constructor-fn 'grid-plot
                                 :default-interior '1d-layout
                                 :bottom-label t :left-view t
                                 :bottom-view t :left-label t)))
  (setf (get '1d-layout-plot 'view-class) 'grid-plot)
  (pushnew '1d-layout-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export '1d-layout-plot))
  (defun 1d-layout-plot(&rest args   &key data vars subview-type &allow-other-keys)
    "Produces displays of variables in a grid. ~
     (:key ~
     (:arg data :prompt The dataset whose values are to be plotted. ~
     If  :prompt, then vars  are used to construct the plot.)~
     (:arg vars :prompt The variates.)
     (:subview-type :prompt The type of display to be used for each panel.))~
     (:examples (:files (grid-plot eg:Views;Plots;grid-plot.lsp)~
     (general eg:Views;Plots;general.lsp)))~
     (:see-also 
     (scatterplot :function)~
     (scat-mat :function)~
     (xy-layout-plot :function)~
     (batch-plot :function)~
     (case-layout-plot :function)~)"
    (declare (ignore  data vars subview-type))
    (apply plot-local args))
  )


(let ((plot-local 
       (make-view-constructor-fn 'grid-plot
                                 :default-interior 'xy-layout
                                 :bottom-label t :left-view t
                                 :bottom-view t :left-label t
                                 :link-bounds-x? :by-col :link-bounds-y? :by-row)))
  (setf (get 'xy-layout-plot 'view-class) 'grid-plot)
  (pushnew 'xy-layout-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'xy-layout-plot))
  (defun xy-layout-plot(&rest args   &key data x-vars y-vars subview-type &allow-other-keys)
    "Produces displays of pairs of x and y variables in a grid. ~
  (:key ~
  (:arg data :prompt The dataset whose values are to be plotted. ~
  If  :prompt, then x-vars and y-vars are used to construct the plot.)~
  (:arg x-vars :prompt The x variates.)
  (:arg y-vars :prompt The y variates.)
  (:subview-type :prompt The type of display to be used for each panel.))~
  (:examples (:files (grid-plot eg:Views;Plots;grid-plot.lsp)~
  (general eg:Views;Plots;general.lsp)))~
  (:see-also 
  (scatterplot :function)~
  (scat-mat :function)~
  (1d-layout-plot :function)~
  (batch-plot :function)~
   (case-layout-plot :function)~)"
    (declare (ignore  data x-vars y-vars subview-type))
    (apply plot-local args))
  )
  
  
(let ((plot-local 
       (make-view-constructor-fn 'grid-plot
                                 :initform-fn #'get-batch-inits
                                 :default-interior 'batch-layout
                                 :title-text nil
                                 :left-label t :bottom-label t :top-label nil
                                 :right-label nil :left-label-size .2
                                 :left-view t :bottom-view t)))
  (setf (get 'batch-plot 'view-class) 'grid-plot)
  (pushnew 'batch-plot *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'batch-plot))
  (defun batch-plot(&rest args   &key data by batches subview-type &allow-other-keys)
    "Produces displays of batches in a grid. ~
     (:key ~
     (:arg data :prompt The dataset whose values are to be plotted. )~
     (:arg by :prompt A batch is constructed using values of by. )~
     (:arg batches nil Batches if supplied should be a list of datasets,~
     in which case the by value is ignored.)~
     
     (:subview-type :prompt The type of display to be used for each panel.))~
     (:examples (:files (grid-plot eg:Views;Plots;grid-plot.lsp)~
     (general eg:Views;Plots;general.lsp)))~
     (:see-also 
     (scatterplot :function)~
     (scat-mat :function)~
     (1d-layout-plot :function)~
     (xy-layout-plot :function)~
     (case-layout-plot :function)~
     )"
    (declare (ignore  data by batches subview-type))
    (apply plot-local args))
  )  
  

(let ((plot-local 
       (make-view-constructor-fn 'grid-plot
                                 :default-interior 'case-layout
                                 :left-label t :bottom-label t :top-label :if-string
                                 :right-label :if-string
                                 :left-view t :bottom-view t)))
  
  (setf (get 'case-layout-plot  'view-class) 'grid-plot)
  (pushnew 'case-layout-plot  *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'case-layout-plot))
  (defun case-layout-plot (&rest args   &key data subview-type &allow-other-keys)
    "Produces one display per case in a grid. ~
     (:key ~
     (:arg data :prompt The dataset whose values are to be plotted. )~
     
     (:subview-type :prompt The type of display to be used for each panel.))~
     (:examples (:files (grid-plot eg:Views;Plots;grid-plot.lsp)~
     (general eg:Views;Plots;general.lsp)))~
     (:see-also 
     (scatterplot :function)~
     (scat-mat :function)~
     (1d-layout-plot :function)~
     (xy-layout-plot :function)~)"
    (declare (ignore  data subview-type))
    (apply plot-local args)))  
  

(let ((plot-local 
       (make-view-constructor-fn 'grid-plot
                                 :title-text nil
                                 :left-label t :bottom-label t
                                 :initform-fn #'get-barchart-inits
                                 :gap-x  *default-grid-gap*  :gap-y  *default-grid-gap*
                                 :default-interior 'bar-chart
                                 :left-view t :left-label-size .2
                                 :left-view-size 0.05 :bottom-view-size 0.05)))
  
  (setf (get 'bar-plot  'view-class) 'grid-plot)
  (pushnew 'bar-plot  *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'bar-plot))
  (defun bar-plot (&rest args   &key data by batches var &allow-other-keys)
    "Produces a barplot. ~
     (:key ~
     (:arg data :prompt The dataset whose values are to be plotted. )~
     (:arg by :prompt There is one bar per batch. ~
      A batch is constructed using values of by. )~
     (:arg batches nil Batches if supplied should be a list of datasets,~
     in which case the by value is ignored.)~
     (:arg var nil If var is non-nil, then batches and by are ignored.~
     The values of var are used for bar heights.))~
     (:examples (:files (scatterplot eg:Views;Plots;scatterplot.lsp)~
     (general eg:Views;Plots;general.lsp)))~
     (:see-also 
     (scatterplot :function)~
     (batch-plot :function)~
     (case-layout-plot :function)~)"
     (declare (ignore  data by batches var))
     (apply plot-local args)))






(let ((plot-local 
       (make-view-constructor-fn 'grid-plot
                                 :title-text nil
                                 :left-label t :bottom-label t :left-label-size .2
                                 :default-interior 'table-layout)))
  
  (setf (get 'table-plot  'view-class) 'grid-plot)
  (pushnew 'table-plot  *view-constructors*)
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'table-plot))
  (defun table-plot (&rest args   &key data by batches subview-type &allow-other-keys)
    "Produces displays of batches in a grid, where displays can be resized. ~
     (:key ~
     (:arg data :prompt The dataset whose values are to be plotted. )~
     (:arg by :prompt A batch is constructed using values of by. )~
     (:arg batches nil Batches if supplied should be a list of datasets,~
     in which case the by value is ignored.)~
     
      (:subview-type :bar-with-text The type of display to be used for each panel.))~
     (:examples (:files (grid-plot eg:Views;Plots;grid-plot.lsp)~
     (general eg:Views;Plots;general.lsp)))~
     (:see-also 
     (scatterplot :function)~
     (batch-plot :function)~)"
    (declare (ignore  data by batches subview-type ))
    (apply plot-local args)))


  
                 






         

