;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The weibull distribution                           
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
;;;  \subsubsection The weibull distribution
;;;
;;;  In this section, we consider the standard Weibull distribution as
;;;  represented in Quail.
;;;  The weibull can also be explored through the help system

(help 'weibull :topic)

;;;  The effect (on the pdf) of changing the shape parameter is illustrated
;;;  towards the end of this file.  Just search for `animate'.
;;;
;;;  The mathematical form of the distribution is as follows:
;;;
;;;  The weibull distribution is a continuous distribution positive on (0,infinity)
;;;  and 0 elsewhere.
;;;  It is parameterized by a shape parameter b > 0 
;;;
;;;              (b - 1)         b
;;;   pdf:    b x        exp{ - x  }
;;;
;;;                       b
;;;   cdf:    1 - exp{ - x  }
;;;
;;;
;;; 
;;;  An instance of a weibull with shape parameter 2 is created as 

(setf weibull (make-instance 'weibull :shape 2))

(display weibull)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at weibull .3)
(pdf-at weibull (list 0.0 .5 1))
(pdf-at weibull (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at weibull .3)
(cdf-at weibull (list 0.0 .5 1))
(cdf-at weibull (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at weibull .3)
(quantile-at weibull (list 0.0 .5 1))
(quantile-at weibull (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value weibull)
(random-value weibull 10)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value weibull 100))

;;;  draw the histogram

(<- hist (histogram :data data :var 0 :title "Weibull(2) sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at weibull x))
                           :domain '(0 3)
                           :nlines 100))

;;; For the weibull distribution, wrapper functions for these calculations exist
;;; which reuse the same instance of a weibull distribution changing parameters
;;; as requested.
;;;
;;;   Probability density calculations:

(density-weibull .3 :shape 2)
(density-weibull (list 0.0 .5 1) :shape 2)
(density-weibull (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)) :shape 2)

;;;   Cumulative distribution calculations:

(dist-weibull .3 :shape 2)
(dist-weibull (list 0.0 .5 1) :shape 2)
(dist-weibull (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)) :shape 2)

;;;   Quantile (inverse cdf) calculations:

(quantile-weibull .3 :shape 2)
(quantile-weibull (list 0.0 .5 1) :shape 2)
(quantile-weibull (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)) :shape 2)

;;;   Pseudo-random values

(random-weibull :shape 2)
(random-weibull :shape 2 :n 10)
;;;
;;;  The effect of changing the shape can always be shown
;;;  in an animated display as follows (just execute the whole let* form):
;;;

(let*
  ((weibull (make-instance 'weibull :shape 1))
   
   ;;  Now get a plot of the moving density, but don't draw it yet.
   (fp (function-plot :function #'(lambda (x) (pdf-at weibull x))
                      :domain '(0 2)
                      
                      :nlines 100
                      :title "Animating weibull"
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
         (loop for i from 1 to 15
               do
               ;; erase it and the top-label
               (erase-view pdf)
               (erase-view top-label)
               ;; re-draw the axes which were partially erased
               (draw-view (left-view-of fp))
               (draw-view (bottom-view-of fp))
               ;; change degrees of freedom of the weibull
               (<- (shape-of weibull) i)
               ;; change degrees of freedom of the weibull
               (<- (shape-of weibull) i)
               (set-text top-label (format NIL "shape = ~s" i))
               ;; recompute the coordinates
               (compute-lines-coords pdf)
               ;; draw the new version of the weibull
               (draw-view top-label)
               (draw-view pdf))
         )
     )
    )
   )
  ;; Now change the extent on the y axis (the left-view-of the function-plot)
  ;; so that all of the gaussian pdf will show.
  
  (set-extent (left-view-of fp) 0 5) 
  
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
