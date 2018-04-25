;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The K distribution                           
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
;;;  \subsubsection The K-dist distribution
;;;
;;;  In this section, we consider the K-dist distribution as represented in 
;;;  Quail.
;;;  The K-dist can also be explored through the help system

(help 'K-dist :topic)

;;;  The effect (on the pdf) of changing degrees of freedom is illustrated
;;;  towards the end of this file.  Just search for `animate'.
;;;    
;;;  A K random-variable is simply a one to one transform of
;;;  a Chi-squared random variable.  If x has a Chi-squared distribution
;;;  on m degrees of freedom, then y = sqrt(x/m) has a K distribution
;;;  on m degrees of freedom.
;;;  
;;;  pdf at x:
;;;            for x > 0
;;;
;;;                   (m/2)                                      2
;;;                  m                             (m - 1)   -(my /2)
;;;              ------------------------------ * y         e
;;;                   (m/2)-1
;;;                  2         * gamma (m/2)
;;;
;;;     
;;;            0 otherwise
;;;
;;;  The K is derived from standard normal theory.  If we have a sample
;;;  of n independent realizations from a Gaussian(u, sigma) (or N(u, sigma^2))
;;;  and s is the sample standard deviation, then the distribution of
;;;  s/sigma is K on (n-1) degrees of freedom.
;;;  The principal reason for going to this distribution is that it is nearly
;;;  symmetric and nearly centred around 1 for all but the fewest degrees of
;;;  freedom.  ``Ball park'' values of critical values are easy to
;;;  remember because they don't change that much with the degrees of freedom.
;;;  Look at the animation at the end of this file.
;;;  Bottom line, if you must tabulate a distribution for normal theory results,
;;;  tabulate K, not Chi-squared.
;;;  
;;;
;;;  An instance of a standard K-dist with 10 degrees of freedom

(setf K-dist (make-instance 'K-dist :df 10))

(display K-dist)

;;;  Find its degrees of freedom

(df-of K-dist)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at K-dist 3)
(pdf-at K-dist (list 0.0 1.0 2.0))
(pdf-at K-dist (array '(0.5 0.8 0.9 1.0 1.1 1.2) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at K-dist 3)
(cdf-at K-dist (list 0.0 1.0 2.0))
(cdf-at K-dist (array '(0.5 0.8 0.9 1.0 1.1 1.2) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at K-dist .3)
(quantile-at K-dist (list 0.025 .05 .95 .975))
(quantile-at K-dist (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value K-dist)
(random-value K-dist 2)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value K-dist 1000))

;;;  draw the histogram

(<- hist (histogram :data data :var 0 :title "K-dist 10 sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at K-dist x))
                           :nlines 100
                           :domain '(0 3)))

;;; For the K-dist distribution, wrapper functions for these calculations
;;; exist which reuse the same instance of a K-dist distribution changing 
;;; parameters as requested.
;;;
;;;   Probability density calculations:

(density-K 3 :df 10)
(density-K 10 :df 10)

(density-K (list 0.0 1 2) :df 10)
(density-K (array '(0.5 0.8 0.9 1.0 1.1 1.2) :dimensions '(2 3)) :df 10)

;;;   Cumulative distribution calculations:

(dist-K 3 :df 10)
(dist-K 10 :df 10)

(dist-K (list 0.0 1 2) :df 10)
(dist-K (array '(0.5 0.8 0.9 1.0 1.1 1.2) :dimensions '(2 3)) :df 10)

;;;   Quantile (inverse cdf) calculations:

(quantile-K .3 :df 10)
(quantile-K (list 0.0 .5 1) :df 10)
(quantile-K (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)) :df 10)

;;;   Pseudo-random values

(random-K :df 10)
(random-K :n 10 :df 10)

;;;
;;;  The effect of increasing the degrees of freedom can always be shown
;;;  in an animated display as follows (just execute the whole let* form):
;;;

(let*
  ((K-dist (make-instance 'K-dist :df 1))
   
   ;; The distribution will be seen to concentrate at 1, so I'll get a
   ;; vertical line to place there.
   (vertical-line (line :orientation :vertical :draw? NIL))
   
   ;;  Now get a plot of the moving density, but don't draw it yet.
   (fp (function-plot :function #'(lambda (x)
                                    (pdf-at K-dist x))
                      :domain '(0 2)
                      
                      :nlines 100
                      :title "Animating K-dist"
                      :left-label "Density"
                      :top-label "df = 1"
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
         (loop for i from 1 to 30
               do
               ;; erase it and the top-label
               (erase-view pdf)
               (erase-view top-label)
               ;; re-draw the vertical line and axes which were partially erased
               (draw-view vertical-line)
               (draw-view (left-view-of fp))
               (draw-view (bottom-view-of fp))
               ;; change degrees of freedom of the K-dist
               (<- (df-of K-dist) i)
               ;; change degrees of freedom of the K-dist
               (<- (df-of K-dist) i)
               (set-text top-label (format NIL "df = ~s" i))
               ;; recompute the coordinates
               (compute-lines-coords pdf)
               ;; draw the new version of the K-dist
               (draw-view top-label)
               (draw-view pdf))
         )
     )
    )
   )
  ;; Now change the extent on the y axis (the left-view-of the function-plot)
  ;; so that all of the gaussian pdf will show.
  
  (set-extent (left-view-of fp) 0 3.5) 
  
  ;; And change the tics on the x axis (the bottom-view-of the function-plot)
  
  (set-tics (bottom-view-of fp) '(0 .5 .8  1  1.2 1.5 2))
  
  ;; Make the comparison line vertical
  (set-line-orientation vertical-line :value :vertical :draw? NIL)
  ;; and place it at 1 on the x -axis
  (set-line-intercept vertical-line :value 1 :draw? NIL)
  
  ;; layout the function-plot and the button together and draw the works
  (view-layout :positions '((0 1 9 10) (1 9 0 9))
               :subviews (list start-button fp)
               :draw? T)
  
  ;; add the vertical reference line to the plot
  (layer-view pdf vertical-line)
  )

;;; Simply click on the start button (with the left mouse button) and watch.
;;; You'll see that the distribution becomes more nearly symmetric and more
;;; nearly centred at 1 as the degrees of freedom increases.
;;; You might want to compare this with what happens to a chi-squared

    (edit-file "eg:Probability;Distributions;chi-squared.lsp")

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
