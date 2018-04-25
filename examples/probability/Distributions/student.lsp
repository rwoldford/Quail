;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       Student's t distribution                           
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
;;;  \subsubsection The student t distribution
;;;
;;;  In this section, we consider the student t distribution as represented in 
;;;  Quail.
;;;  The student distribution can also be explored through the help system

(help 'student :topic)

;;;  The effect (on the pdf) of changing degrees of freedom is illustrated
;;;  towards the end of this file.  Just search for `animate'.
;;;
;;;  The mathematical form of the student distribution with v
;;;  degrees of freedom is as follows:
;;;
;;;
;;;  pdf at x:
;;;                                                  
;;;           gamma ((v + 1)/2)           1             2     -(v +1)/2
;;;      --------------------------- * ------- * (1 + (x /v))        
;;;       gamma (1/2) * gamma (v/2)    sqrt(v)
;;; 
;;; 
;;;
;;;  An instance of a standard student with 10 degrees of freedom

(setf student (make-instance 'student :df 10))

(display student)

;;;  Find its degrees of freedom

(df-of student)

;;;
;;;  We can now have this distribution produce the usual calculations:
;;;
;;;   Probability density calculations:

(pdf-at student 3)
(pdf-at student (list -3 -2 -1 0 1 2 3))
(pdf-at student (array '(-3 -2 -1 1 2 3) :dimensions '(2 3)))

;;;   Cumulative distribution calculations:

(cdf-at student 3)
(cdf-at student (list -3 -2 -1 0 1 2 3))
(cdf-at student (array '(-3 -2 -1 1 2 3) :dimensions '(2 3)))

;;;   Quantile (inverse cdf) calculations:

(quantile-at student .3)
(quantile-at student (list 0.0 .5 1))
(quantile-at student (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3)))

;;;   Pseudo-random values

(random-value student)
(random-value student 2)

;;;  And to make sure these things look right
;;;  generate lots of data

(<- data (random-value student 1000))

;;;  draw the histogram

(<- hist (histogram :data data :var 0 :title "student 10 sample"
                     :histogram-scale :density))

;;;  and overlay a view of the density function

(layer-view (interior-view-of hist)
            (function-view :function 
                           #'(lambda (x) (pdf-at student x))
                           :nlines 100
                           :domain '(-3 3)))

;;; For the student distribution, wrapper functions for these calculations
;;; exist which reuse the same instance of a student distribution changing 
;;; parameters as requested.
;;;
;;;   Probability density calculations:

(density-student 3 :df 10)
(density-student 1 :df 10)

(density-student (list 0.0 1 2) :df 10)
(density-student (array '(-3 -2 -1 1 2 3) :dimensions '(2 3)) 
                 :df 10)

;;;   Cumulative distribution calculations:

(dist-student 3 :df 10)
(dist-student 10 :df 10)

(dist-student (list 0.0 5 10) :df 10)
(dist-student (array '(-3 -2 -1 1 2 3) :dimensions '(2 3))
              :df 10)

;;;   Quantile (inverse cdf) calculations:

(quantile-student .3 :df 10)
(quantile-student (list 0.0 .5 1) :df 10)
(quantile-student (array '(0.0 .025 .5 .75 .975 1.0) :dimensions '(2 3))
                  :df 10)

;;;   Pseudo-random values

(random-student :df 10)
(random-student :n 10 :df 10)

;;;
;;;
;;; The student distribution in Quail has location and scale parameters as
;;; well.

(setf (location-of student) 5)
(setf (scale-of student) 5)

(cdf-at student 5)
(pdf-at student 3)
(random-value student 100)

(dist-student 5 :df 2 :location 5 :scale 5)
(density-student 5 :df 2 :location 5 :scale 5)
(quantile-student .5 :df 2 :location 5 :scale 5)
(random-student :n 3 :df 2 :location 5 :scale 5)

;;;
;;;
;;;  The effect of increasing the degrees of freedom can always be shown
;;;  in an animated display as follows (just execute the whole let* form):
;;;

(let*
  ((student (make-instance 'student :df 1))
   ;;  This will be the density whose degrees of freedom will change.
   ;;  It will be convenient to have a couple of base densities for
   ;;  comparison.
   ;;  At one extreme will be a student on 1 df (i.e. a Cauchy), 
   ;;  at the other end a gaussian (or student on infinite df).
   (Cauchy (make-instance 'cauchy-dist))
   (Gaussian (make-instance 'gaussian-dist))

   ;;  Now get a plot of the moving density, but don't draw it yet.
   (fp (function-plot :function #'(lambda (x)
                                    (pdf-at student x))
                      :domain '(-10 10)
                      
                      :nlines 75
                      :title "Animating student"
                      :left-label "Density"
                      :top-label "df = 1"
                      :color wb:*yellow-color*
                      :draw? NIL
                      :viewed-object student))
   ;; and here are pointers to the pdf curve and the top-label:
   (pdf (interior-view-of fp))
   (top-label (top-label-of fp))
   
   ;; Now here are the stationary pdf curves
   (Cauchy-pdf
    (function-view :function 
                   #'(lambda (x)
                       (pdf-at Cauchy x))
                   :nlines 75
                   :domain '(-10 10)
                   :draw? NIL
                   :viewed-object cauchy
                   :color wb:*green-colour*))
   (Gaussian-pdf
    (function-view :function 
                   #'(lambda (x)
                       (pdf-at Gaussian x))
                   :nlines 75
                   :domain '(-10 10)
                   :draw? NIL
                   :viewed-object Gaussian
                   :color wb:*green-colour*))

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
               ;; redraw the standards because they probably got
               ;; partially erased.
               (draw-view Cauchy-pdf)
               (draw-view Gaussian-pdf)
               ;; change degrees of freedom of the student
               (<- (df-of student) i)
               ;; change degrees of freedom of the student
               (<- (df-of student) i)
               (set-text top-label (format NIL "df = ~s" i))
               ;; recompute the coordinates
               (compute-lines-coords pdf)
               ;; draw the new version of the student
               (draw-view top-label)
               (draw-view pdf))
         )
     )
    )
   )
  ;; Now change the extent on the y axis (the left-view-of the function-plot)
  ;; so that all of the gaussian pdf will show.
  
  (set-extent (left-view-of fp) 0 .5) 

  ;; And change the extent on the x axis (the bottom-view-of the function-plot)
  ;; so that only the central part of the pdf will show.
  
  (set-extent (bottom-view-of fp) -3 3) 
  
   ;; layout the function-plot and the button together and draw the works
   (view-layout :positions '((0 1 9 10) (1 9 0 9))
             :subviews (list start-button fp)
             :draw? T)
   ;; and finally add the two reference densities
   
   (layer-view pdf Cauchy-pdf)
   (layer-view pdf Gaussian-pdf)
   
   )

;;; Simply click on the start button (with the left mouse button) and watch.
;;;
;;; Note that this display is completely self-contained and interactive.
;;; For example middle mouse button selection on the horizontal axis will
;;; allow interaction with the axis.
;;; In this example, it might be of interest to focus attention on the tail
;;; behaviour of the student.  This could be done by selecting the `tic limits'
;;; item from the middle button menu on the axis and when prompted typing in
;;; say (2.5 5) to focus on part of the right tail (the programmatic equivalent
;;; would be to use set-extent on the axis as above).  To achieve maximal
;;; separation between the curves in the display, you will need to adjust the
;;; `Tic limits' on the y axis as well.
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
