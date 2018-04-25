;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               matrix.lsp
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

(defmethod display ((thing matrix) &rest grid-plot-args
                    &key
                    (draw? T)
                    (color *default-label-color*)
                    (title NIL)
                    (gap-x 0.1)
                    (gap-y 0.1)
                    &allow-other-keys)
  "A quick and dirty display for matrices."
  (let*
    ((dimensions (dimensions-of thing))
     (nrows (first dimensions))
     (ncols (second dimensions))
     (dummy-label (label :viewed-object ""
                         :draw? NIL))
     (column-labels 
      (loop for name in (list-variates thing)
            collect
            (text-view
             :viewed-object name
             :text (format NIL "~a" name)
             :draw? NIL
             :justification-mixin '(:center :top))))
     (row0
      (apply #'grid-plot
             :viewed-object thing
             :interior-view
             (grid-layout
              :viewed-object thing
              :subviews
              (cons dummy-label column-labels)
              :nrows 1
              :ncols (+ 1 ncols)
              :box-views? NIL
              :gap-x 0.0 :gap-y gap-y)
             :gap-x 0.0 :gap-y gap-y
             :draw? NIL
             :color color
             grid-plot-args))
     (remaining-rows
      (loop for name in (list-identifiers thing)
            as i from 0 to (- nrows 1)
            as vo = (ref thing i)
            collect
            (apply #'grid-plot
                   :viewed-object vo
                   :interior-view
                   (grid-layout
                    :viewed-object vo
                    :subviews
                    (cons (text-view
                           :viewed-object name
                           :text (format NIL "~a" name)
                           :draw? NIL
                           :justification-mixin '(:left :top))
                          (loop for j from 0 to (- ncols 1)
                                as value = (eref thing i j)
                                collect
                                (text-view
                                 :viewed-object value
                                 :text (format NIL "~a" value)
                                 :draw? NIL
                                 :justification-mixin '(:center :top))))
                    :nrows 1
                    :ncols (+ 1 ncols)
                    :box-views? NIL
                    :gap-x 0.0 :gap-y gap-y)
            :gap-x 0.0 :gap-y gap-y
            :draw? NIL
            :color color
            grid-plot-args)))
       )
    
    
    (apply #'grid-plot
           :viewed-object thing
           :interior-view
           (grid-layout
            :viewed-object thing
            :subviews
            (cons row0 remaining-rows)
            :nrows (+ 1 nrows)
            :ncols 1
            :box-views? NIL
            :gap-x gap-x :gap-y 0.0)
           :gap-x gap-x :gap-y 0.0
           :draw? draw?
           :color color
           :title title
           grid-plot-args)
    ))
