;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               show.lisp
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




(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(show)))

(defmethod show ((distribution prob-measure) &rest grid-plot-args
                    &key
                    (draw? T)
                    (cdf? T)
                    (pdf? T)
                    (color wb::*white-color*)
                    (title NIL)
                    (nlines 30)
                    (from NIL)
                    (to NIL)
                    &allow-other-keys)
  (unless (numberp from) (setf from (max -5.0 (lower-bound-of distribution))))
  (unless (numberp to) (setf to (min 5.0 (upper-bound-of distribution))))
  (unless title (setf title (format NIL "The ~s."  (class-name (class-of distribution)))))
  (let
    (the-plot)
    (setf
     the-plot
     (cond
      ((and pdf? cdf?)
       (let*
         ((pdf
           (function-plot
            :function
            #'(lambda (x) (pdf-at distribution x))
            :domain 
            (list from to)
            :nlines nlines
            :left-view t :bottom-view t
            :left-label "density" :bottom-label "y"
            :viewed-object distribution
            :title "Density"
            :color color
            :draw? NIL
            :link? T
            ))
          (cdf
           (function-plot
            :function
            #'(lambda (x) (cdf-at distribution x))
            :domain 
            (list from to)
            :nlines
            nlines
            :left-view t :bottom-view t
            :left-label "probability" :bottom-label "y"
            :viewed-object distribution
            :title "Cumulative distribution"
            :color color
            :draw? NIL
            :link? T
            )
           )
          
          (view-layout (grid-layout :subviews (list pdf cdf)
                                    :nrows 1 :box-views? t
                                    :gap-x 0.1 :gap-y 0.1)))
         (apply #'grid-plot :interior-view view-layout :gap-x 0.1 :gap-y 0.1
                :title title
                :draw? draw?
                grid-plot-args)))
      (pdf?
       (let*
         ((pdf
           (function-plot
            :function
            #'(lambda (x) (pdf-at distribution x))
            :domain 
            (list from to)
            :nlines nlines
            :left-view t :bottom-view t
            :left-label "density" :bottom-label "y"
            :viewed-object distribution
            :title "Density"
            :color color
            :draw? NIL
            :link? T
            ))
          (view-layout (grid-layout :subviews (list pdf)
                                    :nrows 1 :box-views? t
                                    :gap-x 0.1 :gap-y 0.1)))
         (apply #'grid-plot :interior-view view-layout :gap-x 0.1 :gap-y 0.1
                :title title
                :draw? draw?
                grid-plot-args))
       )
      (cdf?
       (let*
         ((cdf
           (function-plot
            :function
            #'(lambda (x) (cdf-at distribution x))
            :domain 
            (list from to)
            :nlines nlines
            :left-view t :bottom-view t
            :left-label "probability" :bottom-label "y"
            :viewed-object distribution
            :title "Cumulative distribution"
            :color color
            :draw? NIL
            :link? T
            ))
          (view-layout (grid-layout :subviews (list cdf)
                                    :nrows 1 :box-views? t
                                    :gap-x 0.1 :gap-y 0.1)))
         (apply #'grid-plot :interior-view view-layout :gap-x 0.1 :gap-y 0.1
                :title title
                :draw? draw?
                grid-plot-args))
       )
      ))
    (loop for s-v in (sub-views-of (interior-view-of the-plot))
          do (draw-view (interior-view-of s-v)
                        :color color))
    )
  )


