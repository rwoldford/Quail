;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               display.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  
;;;
;;;  Authors:
;;;     R.W. Oldford 1993
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(display)))


(defmethod display ((model generalized-linear-model) &rest grid-plot-args
                    &key
                    (draw? T)
                    (color wb::*white-color*)
                    (title NIL)
                    &allow-other-keys)
  (unless title
    (setf title
          (format NIL "Model summary: ~s." (class-name (class-of model)))))
  (let*
    ((link (link-of model))
     (formula (formula-of model))
     (family (family-of model))
     (formula-label (label :viewed-object "Formula:"
                           :draw? NIL
                           :justification '(:left :top)))
     (link-label (label :viewed-object "Link:"
                        :draw? NIL
                        :justification '(:left :top)))
     (family-label (label :viewed-object "Family:" :draw? NIL
                          :justification '(:left :top)))
     (formula-value (text-view
                     :viewed-object formula
                     :text  (literal-of formula)
                     :draw? NIL))
     (link-value (text-view
                  :viewed-object link
                  :text (name-of link)
                  :draw? NIL))
     (family-value (text-view
                    :viewed-object family
                    :text (name-of family)
                    :draw? NIL))
     (view-layout (grid-layout
                   :viewed-object model
                   :subviews (list formula-label formula-value
                                   link-label link-value
                                   family-label family-value
                                   )
                   :nrows 3  :box-views? NIL
                   :gap-x 0.1 :gap-y 0.1))
     (result
      (apply #'grid-plot
             :viewed-object model
             :interior-view view-layout :gap-x 0.1 :gap-y 0.1
             :title title
             :draw? NIL
             :color color
             grid-plot-args)))
    (when draw?
     (draw-view result))
    result))


(defmethod display ((fit generalized-linear-model-fit) &rest grid-plot-args
                    &key
                    (draw? T)
                    (color wb::*white-color*)
                    (title NIL)
                    &allow-other-keys)
  (unless title
    (setf title
          (format NIL "Summary of ~s." (class-name (class-of fit)))))
  (let*
    ((model (model-of fit))
     (formula (formula-of model))
     (response-name (response-of formula))
     (model-display (display model :draw? NIL))
     (predictor-matrix (model-matrix-of fit))
     (response (response-matrix-of fit))
     (deviance (deviance-of fit))
     (df (model-degrees-of-freedom-of fit))
     (deviance-result (array (list deviance
                                   df
                                   (- 1.0 (dist-chi deviance :df df)))
                             :dimensions '(1 3)))
     deviance-display
     (working-resids (residuals fit :type :working))
     (response-resids (residuals fit :type :response))
     (deviance-resids (residuals fit :type :deviance))
     (pearson-resids (residuals fit :type :pearson))
     (n (first (dimensions-of working-resids)))
     (pred (pred-of fit))
     (index (seq 1 n))
     (fitted-data (cglue index pred
                         working-resids
                         response-resids 
                         deviance-resids 
                         pearson-resids
                         response
                         predictor-matrix))
     big-layout
     result
     ctrl-middle-items
     (fitted-value-name "Fitted Values"))
    (dataset fitted-data
             :identifiers (list-identifiers response)
             :variates (append (list "Index" fitted-value-name
                                     "Working Residuals"
                                     "Response Residuals"
                                     "Deviance Residuals"
                                     "Pearson Residuals"
                                     response-name)
                               (list-variates predictor-matrix)))
    
    (dataset deviance-result :variates (list "Deviance" "d.f." "Significance"))
    (setf deviance-display (display deviance-result :draw? NIL :title NIL
                                    :row-labels? NIL))
     
    (setf big-layout (grid-layout :viewed-object fit
                                  :subviews (list model-display deviance-display)
                                  :nrows 2  :box-views? t
                                  :gap-x 0.1 :gap-y 0.1))

    (setf result
          (apply #'grid-plot :interior-view big-layout :gap-x 0.1 :gap-y 0.1
                 :title title
                 :draw? NIL
                 :color color
                 grid-plot-args))
    (setf ctrl-middle-items
          (list 
           (list "Residual plots" NIL 
                 "Produce residual plots for this fit."
                 :sub-items
                 (list (list "Rotating plot"
                             #'(lambda (&rest args)
                                 (declare (ignore args))
                                 (let*
                                   ((resid-plot
                                     (rotating-plot :data fitted-data
                                                    :x "Index"
                                                    :y "Deviance Residuals"
                                                    :z fitted-value-name
                                                    :draw? T
                                                    :title (literal-of formula)
                                                    :link? T))
                                    )
                                   (link-view (interior-view-of resid-plot))
                                   resid-plot
                                    )
                                 )
                             "Produce a rotating plot of the residuals.")
                       (list "Residuals vs index"
                             #'(lambda (&rest args)
                                 (declare (ignore args))
                                 (let*
                                   ((resid-plot
                                     (scatterplot
                                         :data fitted-data
                                         :x "Index"
                                         :y "Deviance Residuals"
                                         :draw? T
                                         :title (literal-of formula)))
                                    )
                                   (link-view (interior-view-of resid-plot))
                                   resid-plot
                                    )
                                 )
                             "Produce a plot of the residuals versus their index.")
                       (list "Residuals vs fit"
                             #'(lambda (&rest args)
                                 (declare (ignore args))
                                 (let*
                                   ((resid-plot
                                     (scatterplot
                                         :data fitted-data
                                         :x fitted-value-name
                                         :y "Deviance Residuals"
                                         :draw? T
                                         :title (literal-of formula)))
                                    )
                                   (link-view (interior-view-of resid-plot))
                                   resid-plot
                                    )
                                 )
                             "Produce a plot of the residuals versus the predictor.")
                       (list "Histogram"
                             #'(lambda (&rest args)
                                 (declare (ignore args))
                                 (let*
                                   ((resid-plot
                                     (histogram
                                         :data fitted-data
                                         :y "Deviance Residuals"
                                         :draw? T
                                         :title (literal-of formula))
                                     )
                                    )
                                   (link-view (interior-view-of resid-plot))
                                   resid-plot
                                    )
                                 )
                             "Histogram of the residuals.")
                       (list "QQ-gauss"
                             #'(lambda (&rest args)
                                 (declare (ignore args))
                                 (let*
                                   ((resid-plot
                                     (qq-gauss deviance-resids
                                         :bottom-label "Deviance Residuals"
                                         :draw? T
                                         :title (literal-of formula)))
                                    )
                                   (link-view (interior-view-of resid-plot))
                                   resid-plot
                                    )
                                 )
                             "Histogram of the residuals.")
                       (list "Scatterplot-matrix"
                             #'(lambda (&rest args)
                                 (declare (ignore args))
                                 (let*
                                   ((resid-plot
                                     (scat-mat
                                         :data fitted-data
                                         :draw? T
                                         :title (literal-of formula)))
                                    )
                                   (link-view (interior-view-of resid-plot))
                                   resid-plot
                                    )
                                 )
                             "Produce a scatterplot matrix of the results.")))
           (list "Diagnostics" NIL 
                 "Produce some diagnostic plots."
                 :sub-items
                 (list (list "Goodness of link"
                             #'(lambda (&rest args)
                                 (declare (ignore args))
                                 (inform-user "Unimplemented")
                                 )
                             "Produce a diagnostic plot ~
                              for assessing the link function.")
                       (list "Variance function"
                             #'(lambda (&rest args)
                                 (declare (ignore args))
                                 (inform-user "Unimplemented")
                                 )
                             "Produce a diagnostic plot ~
                              for assessing the variance function.")
                       ))
           (list "Analysis"  NIL 
                 "Produce some analytic results."
                 :sub-items
                 (list 
                  (list "Coefficients"
                             #'(lambda (&rest args)
                                 (declare (ignore args))
                                 (let*
                                   ((coefs (coef-of fit)))
                                   (dataset
                                    coefs
                                    :identifiers (list-variates predictor-matrix)
                                    :variates '("Estimate:"))
                                   (display
                                    coefs
                                    :draw? T
                                    :title "Coefficient estimates.")
                                   ))
                             "Produce a display of the estimated coefficients.")
                       (list "Analysis of Deviance"
                             #'(lambda (&rest args)
                                 (declare (ignore args))
                                 (inform-user "Unimplemented")
                                 )
                             "Produce a plot of the residuals versus their index.")
                       ))))
    (when draw?
      (draw-view result)
      (let ((view-window (window-of (first (viewports-of result)))))
        (setf (wb::middle-title-items-of view-window)
              ctrl-middle-items)
        (wb::install-title-menus view-window)
        (wb::set-up-title-menus view-window :title-middle "Model Assessment"))
      )
    result
    ))
