;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The pareto distribution                           
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1995 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;
;;;  Authors:
;;;      R.W. Oldford  1995.
;;;

(in-package :quail-user)

;;;
;;;  \subsubsection The pareto distribution
;;;
;;;  In this section, we consider the pareto distribution as represented in 
;;;  Quail.
;;;  The pareto can also be explored through the help system

(help 'pareto :topic)

;;;  The effect (on the pdf) of changing the shape parameter is illustrated
;;;  towards the end of this file.  Just search for `animate'.
;;;
;;;  The mathematical form of the distribution is as follows:
;;;
;;;  The pareto distribution is a continuous distribution positive on (0,infinity)
;;;  and 0 elsewhere.
;;;  It is parameterized by a shape parameter a > 0.
;;;
;;;                    -(a+1)
;;;   pdf:    a (1 + x)
;;;
;;;                      -a
;;;   cdf:    1 - (1 + x)
;;;
;;;
;;; 
;;;  An instance of a pareto with shape parameter 2 is created as 

(setf pareto (make-instance 'pareto :shape 2))

(display pareto)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at pareto .3)
(pdf-at pareto (list 0.0 .5 1))
(pdf-at pareto (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at pareto .3)
(cdf-at pareto (list 0.0 .5 1))
(cdf-at pareto (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at pareto .3)
(quantile-at pareto (list 0.0 .5 1))
(quantile-at pareto (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value pareto)
(random-value pareto 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value pareto 100))

;;;  draw the histogram

(<- hist (histogram :data data :var 0 :title "Pareto 2 sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at pareto x))
                           :domain '(0 5)
                           :nlines 100))

;;; For the pareto distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a pareto distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-pareto .3 :shape 2)
(density-pareto (list 0.0 .5 1) :shape 2)
(density-pareto (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)) :shape 2)

;;;   Cumulative distribution calculations:

(dist-pareto .3 :shape 2)
(dist-pareto (list 0.0 .5 1) :shape 2)
(dist-pareto (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)) :shape 2)

;;;   Quantile (inverse cdf) calculations:

(quantile-pareto .3 :shape 2)
(quantile-pareto (list 0.0 .5 1) :shape 2)
(quantile-pareto (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)) :shape 2)

;;;   Pseudo-random values

(random-pareto :shape 2)
(random-pareto :shape 2 :n 10)
;;;
;;;  The effect of changing the shape parameter can always be shown
;;;  in an animated display as follows (just execute the whole let* form):
;;;

(let*
  ((pareto (make-instance 'pareto :shape 1))
   
   ;;  Now get a plot of the moving density, but don't draw it yet.
   (fp (function-plot :function #'(lambda (x) (pdf-at pareto x))
                      :domain '(0 3)
                      
                      :nlines 100
                      :title "Animating pareto"
                      :left-label "Density"
                      :top-label "shape = 1"
                      :color wb:*yellow-color*
                      :draw? NIL))
   
   ;; and here are pointers to the pdf curve and the top-label:
   (pdf (interior-view-of fp))
   (top-label (top-label-of fp))
   
   ;; I would like to have a button to start the animation:
   (start-button
    (control-button
     :text "Start"
     ;; the animation is done by the following anonymous function.
     :left-fn
     #'(lambda ()
         (loop for i from 1 to 10
               do
               ;; erase it and the top-label
               (erase-view pdf)
               (erase-view top-label)
               ;; re-draw the axes which were partially erased
               (draw-view (left-view-of fp))
               (draw-view (bottom-view-of fp))
               ;; change degrees of freedom of the pareto
               (<- (shape-of pareto) i)
               ;; change degrees of freedom of the pareto
               (<- (shape-of pareto) i)
               (set-text top-label (format NIL "shape = ~s" i))
               ;; recompute the coordinates
               (compute-lines-coords pdf)
               ;; draw the new version of the pareto
               (draw-view top-label)
               (draw-view pdf))
         )
     )
    )
   )
  ;; Now change the extent on the y axis (the left-view-of the function-plot)
  ;; so that all of the gaussian pdf will show.
  
  (set-extent (left-view-of fp) 0 8) 
  
  ;; layout the function-plot and the button together and draw the works
  (view-layout :positions '((0 1 9 10) (1 9 0 9))
               :subviews (list start-button fp)
               :draw? T)
  )

;;; Simply click on the start button (with the left mouse button) and watch.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  You can get to the stock continuous distributions by executing

   (edit-file "eg:Probability;Distributions;stock-cts.lsp")

;;;
;;;  or to the stock distributions discussion by executing

   (edit-file "eg:Probability;Distributions;stock.lsp")

;;; 
;;;  or to the overview by executing

   (edit-file "eg:Probability;Distributions;overview.lsp")
