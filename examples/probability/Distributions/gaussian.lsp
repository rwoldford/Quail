;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The Gaussian or Normal distribution                           
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
;;;  \subsubsection The Gaussian distribution
;;;
;;;  In this section, we consider the Gaussian distribution as represented in 
;;;  Quail.
;;;  The gaussian distribution can also be explored through the help system

(help 'gaussian :topic)

;;;  The effect (on the pdf) of changing the scale is illustrated
;;;  towards the end of this file.  Just search for `animate'.
;;;
;;;  The mathematical form of the gaussian distribution with location (or mean)
;;;  u and scale (or standard deviation) s:
;;;
;;;
;;;  pdf at x:
;;;                                                  
;;;          1                                        2  
;;;      ---------------- * exp (- 0.5 * ((x - u) / s)  )        
;;;       s * sqrt(2*pi)
;;; 
;;; 
;;;
;;;  An instance of a standard gaussian (i.e. G(0,1) or N(0,1))

(setf gaussian (make-instance 'gaussian-dist))

(display gaussian)

;;;  Find its location and scale

(location-of gaussian)
(scale-of gaussian)

;;; Note also that a Gaussian is a special case of a Student t and so has
;;; degrees of freedom

(df-of gaussian)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at gaussian 3)
(pdf-at gaussian (list -3 -2 -1 0 1 2 3))
(pdf-at gaussian (array '(-3 -2 -1 1 2 3) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at gaussian 3)
(cdf-at gaussian (list -3 -2 -1 0 1 2 3))
(cdf-at gaussian (array '(-3 -2 -1 1 2 3) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at gaussian .3)
(quantile-at gaussian (list 0.0 .5 1))
(quantile-at gaussian (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value gaussian)
(random-value gaussian 2)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value gaussian 1000))

;;;  draw the histogram

(<- hist (histogram :data data :var 0 :title "Gaussian sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at gaussian x))
                           :nlines 100
                           :domain '(-3 3)))

;;; For the gaussian distribution, wrapper functions for these calculations
;;; exist which reuse the same instance of a gaussian distribution changing 
;;; parameters as requested.
;;;
;;;   Probability density calculations:

(density-gaussian 3)
(density-gaussian 1)

(density-gaussian (list 0.0 1 2))
(density-gaussian (array '(-3 -2 -1 1 2 3) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(dist-gaussian 3)
(dist-gaussian 10)

(dist-gaussian (list 0.0 5 10))
(dist-gaussian (array '(-3 -2 -1 1 2 3) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-gaussian .3)
(quantile-gaussian (list 0.0 .5 1))
(quantile-gaussian (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-gaussian)
(random-gaussian :n 10)

;;;
;;;
;;; The gaussian distribution in Quail has location and scale parameters as
;;; well.

(setf (location-of gaussian) 5)
(setf (scale-of gaussian) 5)

(cdf-at gaussian 5)
(pdf-at gaussian 3)
(random-value gaussian 10)

(dist-gaussian 5 :location 5 :scale 5)
(density-gaussian 5 :location 5 :scale 5)
(quantile-gaussian .5 :location 5 :scale 5)
(random-gaussian :n 3 :location 5 :scale 5)

;;;
;;;
;;;  Suppose that x is Gaussian(0,1) and we have n independent
;;;  realizations from this distribution.  The distribution of the arithmetic
;;;  average xbar = (x1 + x2 + ... + xn)/n is also Gaussian with location 0
;;;  but with scale (1 / sqrt(n)).  If the sample size doubles, for example,
;;;  the standard deviation (scale) of xbar will not be halved but will instead
;;;  be multiplied by (1/ sqrt(2)).
;;;
;;;  The effect of this changing scale can always be shown
;;;  in an animated display.  That which follows doubles the sample size at
;;;  each iteration ending at a sample of size 1024 (a typical size for a
;;;  survey sample, say).
;;;  The rapid concentration of the probability mass around 0 is obvious.
;;;  This effect is the principal reason that surveys work so well with only
;;;  around 1000 people interviewed.
;;;
;;;  Just execute the following let* form.

(let*
  ((gaussian (make-instance 'gaussian-dist))
   ;;  This will be the density whose scale will change.
   ;;  It will be convenient to have a base density for
   ;;  comparison.
   ;;  This will be a gaussian.
   (base-gauss (make-instance 'gaussian-dist))
   
   ;;  Now get a plot of the moving density, but don't draw it yet.
   (fp (function-plot :function #'(lambda (x) (pdf-at gaussian x))
                      :domain '(-2 2)
                      :nlines 100
                      :title "Distribution of the mean."
                      :left-label "Density"
                      :top-label "Sample size = 1"
                      :color wb:*green-color*
                      :draw? NIL))
   ;; and here are pointers to the pdf curve and the top-label:
   (pdf (interior-view-of fp))
   (top-label (top-label-of fp))
   
   ;; Now here are the stationary pdf curves
   (base-pdf
    (function-view :function 
                   #'(lambda (x) (pdf-at base-gauss x))
                   :nlines 50
                   :domain '(-2 2)
                   :draw? NIL
                   :color wb:*yellow-colour*))
   
   
   ;; I would like to have a button to start the animation:
   (start-button
    (control-button
     :text "Start"
     ;; the animation is done by the following anonymous function.
     :left-fn
     #'(lambda ()
         (let ((n 1)
               (1/root2 (/ 1 (sqrt 2))))
           ;; Initialize the scale to 1
           (<- (scale-of gaussian) 1)
           ;; the animation loop
           (loop for i from 1 to 10
                 do
                 ;; erase it and the top-label
                 (erase-view pdf)
                 (erase-view top-label)
                 ;; redraw the standards because they probably got
                 ;; partially erased.
                 (draw-view base-pdf)
                 ;; change the scale of the gaussian
                 (<- (scale-of gaussian)
                     (* 1/root2 (scale-of gaussian)))
                 (<- n (* n 2))
                 (set-text top-label (format NIL "Sample size = ~s" n))
                 ;; recompute the coordinates
                 (compute-lines-coords pdf)
                 ;; draw the new version of the gaussian
                 (draw-view top-label)
                 (draw-view pdf))
           )
         )
     )
    )
   )
  ;; Now change the extent on the y axis (the left-view-of the function-plot)
  ;; so that many of the gaussian pdfs will show, but the base pdf is still
  ;; recognizably Gaussian.
  
  (set-extent (left-view-of fp) 0 2) 
  
  ;; And set the tic marks on the x-axis
  (set-tics (bottom-view-of fp) '(-2 -1 0 1 2))
  ;; layout the function-plot and the button together and draw the works
  (view-layout :positions '((0 1 9 10) (1 9 0 9))
               :subviews (list start-button fp)
               :draw? T)
  ;; and finally add the reference density
  
  (layer-view pdf base-pdf)
  )

;;; Simply click on the start button (with the left mouse button) and watch.
;;;
;;; Note that this display is completely self-contained and interactive.
;;; For example middle mouse button selection on the horizontal axis will
;;; allow interaction with the axis.
;;; In this example, it might be of interest to focus attention on the centre
;;; of the distributions.  This could be done by selecting the `tic limits'
;;; item from the middle button menu on the axis and when prompted typing in
;;; say (-.5 .5) to focus on part of the centre (the programmatic equivalent
;;; would be to use set-extent on the axis as above).
;;;
;;; You might also like to adjust the tic-limits on the vertical axis if you
;;; would like to see the whole of the density for example.
;;;
;;; You might want to select the curves and (again with the middle mouse button)
;;; change the number of line segments used to represent the curve.
;;; Remember that the number you give is spread over the entire domain of the
;;; function (defined above to have been from -2 to 2).
;;;
;;; Once you are happy with the display, then click on the start
;;; button to start the animation again.
;;;
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
