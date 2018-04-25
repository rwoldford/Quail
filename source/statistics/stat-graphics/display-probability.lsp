;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           display-probability.lsp
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


(defmethod display :around
           ((distribution prob-measure)
            &rest grid-plot-args
            &key
            (draw? T)
            (cdf? T)
            (pdf? T)
            (color vw::*DEFAULT-CURVE-COLOR*)
            (title NIL)
            (nlines 30)
            (from NIL)
            (to NIL)
            &allow-other-keys)
  "Sorts out the from, to, and title arguments. ~
   (:rest (:arg grid-plot-args Any keyword acceptable to a grid-plot is acceptable ~
   here.)) ~
   (:key ~
   (:arg draw? T Flag to indicate whether the display is to be drawn or not.) ~
   (:arg cdf? T If non-NIL the cumulative distribution function will be displayed.) ~
   (:arg pdf? T If non-NIL the probability, or probability density, function will be ~
   displayed.) ~
   (:arg color vw::*DEFAULT-CURVE-COLOR* The draw color.) ~
   (:arg title NIL Title of the display.) ~
   (:arg nlines 30 The number of lines to be used in displaying each function.) ~
   (:arg from NIL The lower bound to use in the display of the functions. If not a ~
   number, then the lowest numerical percentile will be selected.) ~
   (:arg to NIL The upper bound to use in the display of the functions.  If not a ~
   number, then the highest numerical percentile will be selected.) ~
   ) ~
   "
  (declare (ignore draw? cdf? pdf? color nlines))
  (unless from
    (setf from (lower-bound-of distribution)))
  (unless to
    (setf to (upper-bound-of distribution)))
  (if (>= from to)
    (quail-error "Display: To display a probability-measure (~s), the lower bound of ~
                  the display range (here from = ~s) ~
                  must be less than the upper bound of the display range ~
                  (here to = ~s).~&"
                 distribution from to))
  (unless (numberp (- to from))
    (cond
     ((not (or (numberp from) (numberp to)))
      ; (loop for i from 1 to 99
      ;      until (and (numberp from)
      ;                 (numberp to)) do
      (do ((i 1 (incf i)))
          ((or (= i 100) (and (numberp from) (numberp to))))
        (unless (numberp from)
          (setf from (quantile-at distribution (/ i 100))))
        (unless (numberp to)
          (setf to (quantile-at distribution (/ (- 100 i) 100))))
        )
      )
     
     ((numberp to)
      ; (loop for i from 1 to 99
      ;     until (numberp from) do
      (do ((i 1 (incf i)))
          ((or (= i 100) (numberp from)))
        (setf from (quantile-at distribution (/ i 100)))))
     
     ((numberp from)
      ; (loop for i from 99 downto 1
      ;      until (numberp to) do
      (do ((i 99 (decf i)))
          ((or (= i 0) (numberp to)))
        (setf to (quantile-at distribution (/ i 100)))))
     )
    )
  ;;(unless (numberp from) (setf from (max -5.0 (lower-bound-of distribution))))
  ;;(unless (numberp to) (setf to (min 5.0 (upper-bound-of distribution))))
  (cond
   ((not (and (numberp from) (numberp to)))
    (quail-error "To display this distribution (~s), it will be necessary ~
                  to provide numerical values for the range parameters from ~
                  and to (not from = ~s and to = ~s)."
                 distribution from to)
    )
   ((>= from to)
    (setf to (+ from 1.0))))
  (unless title
    (setf title (format NIL "The ~s."
                        (class-name (class-of distribution)))))
  (apply #'call-next-method distribution :title title :from from :to to grid-plot-args)
  )

(defmethod display ((distribution prob-measure)
                    &rest grid-plot-args
                    &key
                    (draw? T)
                    (cdf? T)
                    (pdf? T)
                    (color vw::*DEFAULT-CURVE-COLOR*)
                    (title NIL)
                    (nlines 30)
                    (from NIL)
                    (to NIL)
                    &allow-other-keys)
  (declare (ignore grid-plot-args
                    draw? cdf? pdf?
                    color title nlines
                    from to)))


(defmethod display ((distribution continuous-dist)
                    &rest grid-plot-args
                    &key
                    (draw? T)
                    (cdf? T)
                    (pdf? T)
                    (color vw::*DEFAULT-CURVE-COLOR*)
                    (title NIL)
                    (nlines 30)
                    (from NIL)
                    (to NIL)
                    &allow-other-keys)
  "Display a distribution as a view. ~
   (:rest (:arg grid-plot-args Any keyword acceptable to a grid-plot is acceptable ~
   here.)) ~
   (:key ~
   (:arg draw? T Flag to indicate whether the display is to be drawn or not.) ~
   (:arg cdf? T If non-NIL the cumulative distribution function will be displayed.) ~
   (:arg pdf? T If non-NIL the probability, or probability density, function will be ~
   displayed.) ~
   (:arg color vw::*DEFAULT-CURVE-COLOR* The draw color.) ~
   (:arg title NIL Title of the display.) ~
   (:arg nlines 30 The number of lines to be used in displaying each function.) ~
   (:arg from NIL The lower bound to use in the display of the functions. If not a ~
   number, then the lowest numerical percentile will be selected.) ~
   (:arg to NIL The upper bound to use in the display of the functions.  If not a ~
   number, then the highest numerical percentile will be selected.) ~
   ) ~
   "
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
                                    :nrows 1 :box-views? NIL
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
                                    :nrows 1 :box-views? NIL
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
                                    :nrows 1 :box-views? NIL
                                    :gap-x 0.1 :gap-y 0.1)))
         (apply #'grid-plot :interior-view view-layout :gap-x 0.1 :gap-y 0.1
                :title title
                :draw? draw?
                grid-plot-args))
       )
      ))
    (loop for s-v in (sub-views-of (interior-view-of the-plot))
          do (set-drawing-style (interior-view-of s-v)
                        :color color))

    the-plot
    )
  )


(defmethod display ((distribution discrete-dist) &rest grid-plot-args
                    &key
                    (draw? T)
                    (cdf? T)
                    (pdf? T)
                    (color vw::*DEFAULT-CURVE-COLOR*)
                    (title NIL)
                    (from NIL)
                    (to NIL)
                    &allow-other-keys)
  "Display a discrete distribution as a view. ~
   (:rest (:arg grid-plot-args Any keyword acceptable to a grid-plot is acceptable ~
   here.)) ~
   (:key ~
   (:arg draw? T Flag to indicate whether the display is to be drawn or not.) ~
   (:arg cdf? T If non-NIL the cumulative distribution function will be displayed.) ~
   (:arg pdf? T If non-NIL the probability, or probability density, function will be ~
   displayed.) ~
   (:arg color vw::*DEFAULT-CURVE-COLOR* The draw color.) ~
   (:arg title NIL Title of the display.) ~
   (:arg from NIL The lower bound to use in the display of the functions. If not a ~
   number, then the lowest numerical percentile will be selected.) ~
   (:arg to NIL The upper bound to use in the display of the functions.  If not a ~
   number, then the highest numerical percentile will be selected.) ~
   ) ~
   "
 
  (let
    (the-plot)
    (setf
     the-plot
     (cond
      ((and pdf? cdf?)
       (let*
         ((ys (loop for i from from to to by 1 collect i))
          (probs (loop for y in ys
                       collect (pdf-at distribution y)))
          (pdf
           (plot
            :interior-view
            (loop for prob in probs
                  as  y in ys
                  collect
                  (line-segment :color color
                                :endpoints
                                (list (list y 0)
                                      (list y prob)))
                  )
            :left-view t :bottom-view t
            :left-label "probability" :bottom-label "y"
            :viewed-object distribution
            :title "Density"
            :color color
            :draw? NIL
            :link? T
            ))
          (cum-probs (loop for y in ys
                           collect (cdf-at distribution y)))
          (cdf
           (plot
            :interior-view
            (loop for i from 0 to (- (length ys) 1)
                  as y in ys
                  as p in cum-probs
                  collect
                  (line-segment :color color
                                :endpoints
                                (list (list y p)
                                      (list (+ y 1) p))))
            :left-view t :bottom-view t
            :left-label "distribution" :bottom-label "y"
            :viewed-object distribution
            :title "Cumulative Distribution"
            :color color
            :draw? NIL
            :link? T
            )
           )
          
          (view-layout (grid-layout :subviews (list pdf cdf)
                                    :nrows 1 :box-views? NIL
                                    :gap-x 0.1 :gap-y 0.1)))
         (set-extent (left-view-of pdf) 0 (eref (max probs) 0))
         (set-extent (bottom-view-of pdf) (- from .1) to)
         (set-extent (left-view-of cdf) 0 1)
         (set-extent (bottom-view-of cdf) from to)
         (apply #'grid-plot :interior-view view-layout :gap-x 0.1 :gap-y 0.1
                :title title
                :draw? draw?
                grid-plot-args)))
      (pdf?
       (let*
         ((ys (loop for i from from to to by 1 collect i))
          (probs (loop for y in ys
                       collect (pdf-at distribution y)))
          (pdf
           (plot
            :interior-view
            (loop for prob in probs
                  as  y in ys
                  collect
                  (line-segment :color color
                                :endpoints
                                (list (list y 0)
                                      (list y prob)))
                  )
            :left-view t :bottom-view t
            :left-label "probability" :bottom-label "y"
            :viewed-object distribution
            :title "Density"
            :color color
            :draw? NIL
            :link? T
            ))
          (view-layout (grid-layout :subviews (list pdf)
                                    :nrows 1 :box-views? NIL
                                    :gap-x 0.1 :gap-y 0.1)))
         (set-extent (left-view-of pdf) 0 (eref (max probs) 0))
         (set-extent (bottom-view-of pdf) (- from .1) to)
         (apply #'grid-plot :interior-view view-layout :gap-x 0.1 :gap-y 0.1
                :title title
                :draw? draw?
                grid-plot-args))
       )
      (cdf?
       (let*
         ((ys (loop for i from from to to by 1 collect i))
          (cum-probs (loop for y in ys
                           collect (cdf-at distribution y)))
          (cdf
           (plot
            :interior-view
            (loop for i from 0 to (- (length ys) 1)
                  as y in ys
                  as p in cum-probs
                  collect
                  (line-segment :color color
                                :endpoints
                                (list (list y p)
                                      (list (+ y 1) p))))
            :left-view t :bottom-view t
            :left-label "distribution" :bottom-label "y"
            :viewed-object distribution
            :title "Cumulative Distribution"
            :color color
            :draw? NIL
            :link? T
            )
           )
          (view-layout (grid-layout :subviews (list cdf)
                                    :nrows 1 :box-views? NIL
                                    :gap-x 0.1 :gap-y 0.1)))
         (set-extent (left-view-of cdf) 0 1)
         (set-extent (bottom-view-of cdf) from to)
         (apply #'grid-plot :interior-view view-layout :gap-x 0.1 :gap-y 0.1
                :title title
                :draw? draw?
                grid-plot-args))
       )
      ))
    (loop for s-v in (sub-views-of (interior-view-of the-plot))
          do (draw-view (interior-view-of s-v)
                        :color color))
    the-plot
    )
  )
