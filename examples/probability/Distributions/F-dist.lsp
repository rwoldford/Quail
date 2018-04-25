;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       The F distribution                           
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
;;;  \subsubsection The F distribution
;;;
;;;  In this section, we consider the F distribution as represented in 
;;;  Quail.
;;;  The F can also be explored through the help system

(help 'F-dist :topic)

;;;  The effect (on the pdf) of changing degrees of freedom is illustrated
;;;  towards the end of this file.  Just search for `animate'.
;;;  
;;;  The F usually comes up as the scaled ratio of independent Chi-squared
;;;  random variables.  That is if
;;;               2               2
;;;        w ~ Chi (m) and v ~ Chi (n)
;;;
;;;  and w and v are distributed independently of one another, then
;;;  y = (w/m)/(v/n) is distributed as F(m,n) where
;;;  m is called the numerator degrees of freedom and n is the denominator
;;;  degrees of freedom.
;;;
;;;  
;;;  Y is an F random variable on m and n degrees of freedom, then
;;;  the pdf at y is:
;;;
;;;            for y > 0
;;;                                       (m/2)
;;;                   gamma((m+n)/2)  (m/n)      (m/2 - 1)       - ((m+n)/2)
;;;                  -----------------------  *  y         * (1 + my/n)
;;;                   gamma (m/2) gamma(n/2)               
;;;                    
;;;                              (m/2)
;;;                          (m/n)                (m/2 - 1)          -((m+n)/2)
;;;              =   -----------------------  *  y         * (1 + my/n)
;;;                    Beta((m/2),(n/2))
;;;     
;;;            0 otherwise
;;;  
;;;
;;;  With the exception of the pdf, most methods described below are 
;;;  implemented by appealing to the relationship between F and Beta
;;;  random-variables.
;;;  In particular if x is beta(m/2,n/2) then y = (n * x)/(m * (1 - x)) is 
;;;  distributed as an F(m,n) random variable.
;;; 
;;;  An instance of a standard F with 10 and 7 degrees of freedom

(setf F (make-instance 'F-dist :df-num 10 :df-den 7))

(display F)

;;;  Find its degrees of freedom,
;;;  numerator

(df-num-of F)

;;;  and denominator

(df-den-of F)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at F 3)
(pdf-at F (list 0.0 5 10))
(pdf-at F (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at F 3)
(cdf-at F (list 0.0 5 10))
(cdf-at F (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at F .3)
(quantile-at F (list 0.0 .5 1))
(quantile-at F (list 0.9 .95 .975 .99 .995))
(quantile-at F (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value F)
(random-value F 2)

;;;  And to make sure these things look right
;;;  generate some data

(<- data (random-value F 100))

;;;  draw the histogram

(<- hist (histogram :data data :var 0
                     :title "F(10,7) sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at F x))
                           :nlines 100
                           :domain '(0 10)))

;;; For the F distribution, wrapper functions for these calculations
;;; exist which reuse the same instance of a F distribution changing 
;;; parameters as requested.
;;;
;;;   Probability density calculations:

(density-F 3 :df-num 10 :df-den 7)
(density-F 10 :df-num 10 :df-den 7)

(density-F (list 0.0 5 10) :df-num 10 :df-den 7)
(density-F (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3))
           :df-num 10 :df-den 7)

;;;   Cumulative distribution calculations:

(dist-F 3 :df-num 10 :df-den 7)
(dist-F 10 :df-num 10 :df-den 7)

(dist-F (list 0.0 5 10) :df-num 10 :df-den 7)
(dist-F (array '(0.0 2.5 5 7.5 9.75 10) :dimensions '(2 3))
        :df-num 10 :df-den 7)

;;;   Quantile (inverse cdf) calculations:

(quantile-F .3 :df-num 10 :df-den 7)
(quantile-F (list 0.0 .5 1) :df-num 10 :df-den 7)
(quantile-F (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
            :df-num 10 :df-den 7)

;;;   Pseudo-random values

(random-F :df-num 10 :df-den 7)
(random-F :n 10 :df-num 10 :df-den 7)

;;;
;;;  The effect of increasing the degrees of freedom can always be shown
;;;  in an animated display as follows (just execute the whole let* form):
;;;

(let*
  ((F (make-instance 'F-dist :df-num 1 :df-den 1))
   ;;  This will be the density whose degrees of freedom will change.
   ;;  Now get a plot of the moving density, but don't draw it yet.
   (fp (function-plot :function #'(lambda (x)
                                    (pdf-at F x))
                      :domain '(0 5)
                      
                      :nlines 50
                      :title "Animating F"
                      :left-label "Density"
                      :top-label "F(1,1)"
                      :color wb:*yellow-color*
                      :draw? NIL))
   ;; and here are pointers to the pdf curve and the top-label:
   (pdf (interior-view-of fp))
   (top-label (top-label-of fp))
   ;; and a couple of lines to serve as a reference
   (lines (loop for i from 1 to 4
                collect
                (line :orientation :vertical :draw? NIL :color wb:*light-grey-colour*)
                ))
   ;; I would like to have a couple of buttons to start the animation:
   (num-button
    (control-button
     :text "Numerator df"
     ;; the animation is done by the following anonymous function.
     :left-fn
     #'(lambda ()
         (<- (df-den-of F)
             (wb:prompt-user :read-type :read
                             :initial-string (format NIL "~s" (df-den-of F))
                             :prompt-string "Enter value for the denominator ~
                                             degrees of freedom."))
         (loop for i from 1 to 30
               do
               ;; erase it and the top-label
               (erase-view pdf)
               (erase-view top-label)
               ;; redraw the vertical axis because it probably got
               ;; partially erased.
               (draw-view (left-view-of fp))
               ;; same with the refence lines
               (loop for line in lines do (draw-view line))
               ;; change degrees of freedom of the F
               (<- (df-num-of F) i)
               ;; change degrees of freedom of the F
               (<- (df-num-of F) i)
               (set-text top-label (format NIL "F(~s,~s)" i (df-den-of F)))
               ;; recompute the coordinates
               (compute-lines-coords pdf)
               ;; draw the new version of the F
               (draw-view top-label)
               (draw-view pdf))
         )
     )
    )
   (den-button
    (control-button
     :text "Denominator df"
     ;; the animation is done by the following anonymous function.
     :left-fn
     #'(lambda ()
         (<- (df-num-of F)
             (wb:prompt-user :read-type :read
                             :initial-string (format NIL "~s" (df-num-of F))
                             :prompt-string "Enter value for the numerator ~
                                             degrees of freedom."))
         (loop for i from 1 to 30
               do
               ;; erase it and the top-label
               (erase-view pdf)
               (erase-view top-label)
               ;; redraw the vertical axis because it probably got
               ;; partially erased.
               (draw-view (left-view-of fp))
               ;; same with the refence lines
               (loop for line in lines do (draw-view line))
               ;; change degrees of freedom of the F
               (<- (df-den-of F) i)
               ;; change degrees of freedom of the F
               (<- (df-den-of F) i)
               (set-text top-label (format NIL "F(~s,~s)" (df-num-of F) i ))
               ;; recompute the coordinates
               (compute-lines-coords pdf)
               ;; draw the new version of the F
               (draw-view top-label)
               (draw-view pdf))
         )
     )
    )
   )
  
  ;; set the size of the vertical axis so that it will accomodate most densities
  (set-extent (left-view-of fp) 0 1)
  
  ;; And change the tics on the vertical axis (the bottom-view-of the function-plot)
  
  (set-tics (left-view-of fp)
            (loop for i from 0 to (+ 1 (length lines))
                  collect (/ i (+ 1 (length lines))))
            )
  
  ;; Place the refence lines
  (loop for line in lines
        as i from 1
        do
        (set-line-intercept line
                            :value (/ i (+ 1 (length lines)))
                            :draw? NIL)
        )
  
  ;; layout the function-plot and the button together and draw the works
  (view-layout :positions '((0 2 9 10) (0 2 7 8) (2 9 0 9))
               :subviews (list num-button den-button fp)
               :draw? T)
  ;; Add the reference lines
  (loop for line in lines
        do
        (layer-view pdf line))
   )

;;; Simply click on either button (with the left mouse button) and watch.
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
