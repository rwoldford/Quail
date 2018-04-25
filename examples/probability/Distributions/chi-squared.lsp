;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The chi-squared distribution                           
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
;;;  \subsubsection The chi-squared distribution
;;;
;;;  In this section, we consider the chi-squared distribution as represented in 
;;;  Quail.
;;;  The chi-squared can also be explored through the help system

(help 'chi-squared :topic)

;;;  The effect (on the pdf) of changing degrees of freedom is illustrated
;;;  towards the end of this file.  Just search for `animate'.
;;;  
;;;  The mathematical form of the chi-squared distribution with v
;;;  degrees of freedom is as follows:
;;;
;;;
;;;  pdf at x:
;;;            for x > 0
;;;
;;;                          1               (v/2 - 1)    -x/2
;;;                ---------------------- * x            e
;;;                     v/2
;;;                    2    * gamma (v/2)
;;;     
;;;            0 otherwise
;;;
;;; 
;;;
;;;  An instance of a standard chi-squared with 10 degrees of freedom

(setf chi-squared (make-instance 'chi-squared :df 10))

(display chi-squared)

;;;  Find its degrees of freedom

(df-of chi-squared)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at chi-squared 3)
(pdf-at chi-squared (list 0.0 5 10))
(pdf-at chi-squared (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at chi-squared 3)
(cdf-at chi-squared (list 0.0 5 10))
(cdf-at chi-squared (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at chi-squared .3)
(quantile-at chi-squared (list 0.0 .5 1))
(quantile-at chi-squared (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value chi-squared)
(random-value chi-squared 2)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value chi-squared 1000))

;;;  draw the histogram

(<- hist (histogram :data data :var 0 :title "chi-squared 10 sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at chi-squared x))
                           :nlines 100
                           :domain '(0 20)))

;;; For the chi-squared distribution, wrapper functions for these calculations
;;; exist which reuse the same instance of a chi-squared distribution changing 
;;; parameters as requested.
;;;
;;;   Probability density calculations:

(density-chi 3 :df 10)
(density-chi 10 :df 10)

(density-chi (list 0.0 5 10) :df 10)
(density-chi (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)) :df 10)

;;;   Cumulative distribution calculations:

(dist-chi 3 :df 10)
(dist-chi 10 :df 10)

(dist-chi (list 0.0 5 10) :df 10)
(dist-chi (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)) :df 10)

;;;   Quantile (inverse cdf) calculations:

(quantile-chi .3 :df 10)
(quantile-chi (list 0.0 .5 1) :df 10)
(quantile-chi (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)) :df 10)

;;;   Pseudo-random values

(random-chi :df 10)
(random-chi :n 10 :df 10)

;;;
;;;  The effect of increasing the degrees of freedom can always be shown
;;;  in an animated display as follows (just execute the whole let* form):
;;;

(let*
  ((chi-squared (make-instance 'chi-squared :df 1))
   ;;  This will be the density whose degrees of freedom will change.
   ;;  It will be convenient to have a couple of base densities for
   ;;  comparison.
   ;;  At one end will be a Chi on 1 df, at the other end a Chi on 30 df.
   (chi-1 (make-instance 'chi-squared :df 1))
   (chi-30 (make-instance 'chi-squared :df 30))

   ;;  Now get a plot of the moving density, but don't draw it yet.
   (fp (function-plot :function #'(lambda (x)
                                    (pdf-at chi-squared x))
                      :domain '(0 50)
                      
                      :nlines 50
                      :title "Animating chi-squared"
                      :left-label "Density"
                      :top-label "df = 1"
                      :color wb:*yellow-color*
                      :draw? NIL))
   ;; and here are pointers to the pdf curve and the top-label:
   (pdf (interior-view-of fp))
   (top-label (top-label-of fp))
   
   ;; Now here are the stationary pdf curves
   (chi-1-pdf
    (function-view :function 
                   #'(lambda (x)
                       (pdf-at chi-1 x))
                   :nlines 50
                   :domain '(0 50)
                   :draw? NIL
                   :color wb:*green-colour*))
   (chi-30-pdf
    (function-view :function 
                   #'(lambda (x)
                       (pdf-at chi-30 x))
                   :nlines 50
                   :domain '(0 50)
                   :draw? NIL
                   :color wb:*green-colour*))

   ;; I would like to have a button to start the animation:
   (start-button
    (control-button
     :text "Start animation"
     ;; the animation is done by the following anonymous function.
     :left-fn
     #'(lambda ()
         (loop for i from 1 to 30
               do
               ;; erase it and the top-label
               (erase-view pdf)
               (erase-view top-label)
               ;; redraw the standards because they probably got
               ;; partially erased.
               (draw-view chi-1-pdf)
               (draw-view chi-30-pdf)
               ;; change degrees of freedom of the chi-squared
               (<- (df-of chi-squared) i)
               ;; change degrees of freedom of the chi-squared
               (<- (df-of chi-squared) i)
               (set-text top-label (format NIL "df = ~s" i))
               ;; recompute the coordinates
               (compute-lines-coords pdf)
               ;; draw the new version of the chi-squared
               (draw-view top-label)
               (draw-view pdf))
         )
     )
    )
   )
   ;; layout the function-plot and the button together and draw the works
   (view-layout :positions '((0 1 9 10) (1 9 0 9))
             :subviews (list start-button fp)
             :draw? T)
   ;; and finally add the two reference densities
   
   (layer-view pdf chi-1-pdf)
   (layer-view pdf chi-30-pdf)
   )

;;; Simply click on the start button (with the left mouse button) and watch.
;;;
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
