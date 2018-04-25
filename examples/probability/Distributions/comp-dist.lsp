;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       Comparing distributions                           
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

;;; *** under construction ***
(let*
  ((student (make-instance 'student :df 1))
   (Cauchy (make-instance 'cauchy-dist))
   (Gaussian (make-instance 'gaussian-dist))
   (Chi (make-instance 'chi-squared :df 1))
   (K (make-instance 'K-dist :df 1))
   (p-low 0.01)
   (p-high 0.99)
   (p-by 0.01)
   (percentiles (seq p-low p-high p-by))
   ;;  Now get a plot of the moving density, but don't draw it yet.
   (fp-x (function-plot
          :function #'(lambda (x)
                        (pdf-at gaussian x))
          :viewed-object gaussian
          :domain (let ((m (location-of gaussian))
                        (s (scale-of gaussian)))
                    (list (- m (* 3 s))
                          (+ m (* 3 s))))
          :title NIL
          :nlines 75
          :color wb:*light-blue-color*
          :draw? NIL))
   (fp-y (function-plot
          :function #'(lambda (x)
                        (pdf-at student x))
          :viewed-object student
          :domain (let ((m (location-of student))
                        (s (scale-of student)))
                    (list (- m (* 3 s))
                          (+ m (* 3 s))))
          :title NIL
          :nlines 75
          :color wb:*magenta-color*
          :draw? NIL))
   (qq-plot (qq-plot
             gaussian student
             :title "Quantile quantile plot"
             :percentiles percentiles
             :draw? NIL))
   ;; and here are pointers to the pdf curve and the axes:
   (x-axis (bottom-view-of qq-plot))
   (y-axis (left-view-of qq-plot))
   (x-label (bottom-label-of qq-plot))
   (y-label (left-label-of qq-plot))
   #|
   ;; I would like to have a button to start the animation:
   (start-button
    (control-button
     :text "Start"
     ;; the animation is done by the following anonymous function.
     :left-fn
     #'(lambda ()
         (loop for i from 1 to 30
               do
               ;; erase it and the title
               (erase-view pdf)
               (erase-view title)
               ;; redraw the standards because they probably got
               ;; partially erased.
               (draw-view Cauchy-pdf)
               (draw-view Gaussian-pdf)
               ;; change degrees of freedom of the student
               (<- (df-of student) i)
               ;; change degrees of freedom of the student
               (<- (df-of student) i)
               (set-text title (format NIL "df = ~s" i))
               ;; recompute the coordinates
               (compute-lines-coords pdf)
               ;; draw the new version of the student
               (draw-view title)
               (draw-view pdf))
         )
     )
    )
   |#
   ;; layout the plots and the button together
   (result
    (view-layout :positions '((0 1 1 2)
                              (1 2 1 2)
                              (1 2 0 1))
             :subviews (list fp-y qq-plot fp-x)
             :draw? NIL))
   from-x from-y to-x to-y from to
   )
  ;; Now change the extent on the y axis (the left-view-of each function-plot)
  ;; so that all of the pdf will show and on the same scale.
  
  (multiple-value-setq (from-x to-x) (vw::extent-of (left-view-of fp-x)))
  (multiple-value-setq (from-y to-y) (vw::extent-of (left-view-of fp-y)))
  (setf from (min from-x from-y))
  (setf to (max to-x to-y))
  (set-extent (left-view-of fp-x) from to) 
  (set-extent (left-view-of fp-y) from to) 

  
   ;;(set-drawing-style x-axis :color (draw-style pdf-fp :color))
   ;;(set-drawing-style y-axis :color (draw-style f-view :color))
   (set-text x-label "Gaussian quantiles")
   (set-text y-label "Student 1 quantiles")
   (draw-view result)
   
   result
