;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               matrix-display.lsp
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
                    (color vw::*DEFAULT-LABEL-COLOR*)
                    (title NIL)
                    (gap-x 0.1)
                    (gap-y 0.1)
                    (column-labels? T)
                    (row-labels? T)
                    (column-labels NIL)
                    (row-labels NIL)
                    (row-label-justification '(:left :top))
                    (col-label-justification '(:center :top))
                    (cell-justification '(:center :top))
                    (number-of-characters 10)
                    (decimal-places 4)
                    (fixed-point? NIL)
                    &allow-other-keys)
  "A quick and dirty display for matrices."
  (flet
    ((get-string (arg)
       (cond
        ((numberp arg)
         (let*
           ((format-directive
             (concatenate 'string
                          "~"
                          (format NIL "~a" number-of-characters)
                          ","
                          (format NIL "~a" decimal-places)
                          (if fixed-point?
                            "F"
                            "G"))))
           (format NIL format-directive arg)))
        (T
         (let*
           ((format-directive
             (concatenate 'string
                          "~"
                          (format NIL "~a" number-of-characters)
                          "@a")))
           (format NIL format-directive arg))))))
    
    (let*
      ((dimensions (dimensions-of thing))
       (nrows (first dimensions))
       (ncols (if (second dimensions)
                (second dimensions)
                1))
       row0
       remaining-rows dummy-label
       column-label-views name)
      
      (when row-labels?
        (setf row-labels (or row-labels (list-identifiers thing)))
        (setf dummy-label (text-view :viewed-object "" :draw? NIL)))
      (setf remaining-rows
            (loop for i from 0 to (- nrows 1)
                  as vo = (ref thing i)
                  collect
                  (apply #'grid-plot
                         :viewed-object vo
                         :interior-view
                         (grid-layout
                          :viewed-object vo
                          :subviews
                          (cond
                           (row-labels?
                            (setf name (nth i row-labels))
                            (cons (text-view
                                   :viewed-object name
                                   :text (format NIL "~a" name)
                                   :draw? NIL
                                   :justification-mixin row-label-justification)
                                  (loop for j from 0 to (- ncols 1)
                                        as value = (eref thing i j)
                                        collect
                                        (text-view
                                         :viewed-object value
                                         :text (get-string value)
                                         :draw? NIL
                                         :justification-mixin cell-justification))))
                           (T (loop for j from 0 to (- ncols 1)
                                    as value = (eref thing i j)
                                    collect
                                    (text-view
                                     :viewed-object value
                                     :text (get-string value)
                                     :draw? NIL
                                     :justification-mixin cell-justification))))
                          :nrows 1
                          :ncols (if row-labels? (+ ncols 1) ncols)
                          :box-views? NIL
                          :gap-x 0.0 :gap-y gap-y)
                         :gap-x 0.0 :gap-y gap-y
                         :draw? NIL
                         :color color
                         grid-plot-args)))
      
      (when column-labels?
        (setf column-labels (or column-labels (list-variates thing)))
        (setf column-label-views 
              (loop for name in column-labels
                    collect
                    (text-view
                     :viewed-object name
                     :text (format NIL "~a" name)
                     :draw? NIL
                     :justification-mixin col-label-justification)))
        
        (setf row0 
              (apply #'grid-plot
                     :viewed-object thing
                     :interior-view
                     (grid-layout
                      :viewed-object thing
                      :subviews
                      (if row-labels?
                        (cons dummy-label column-label-views)
                        column-label-views)
                      :nrows 1
                      :ncols (if row-labels? (+ ncols 1) ncols)
                      :box-views? NIL
                      :gap-x 0.0 :gap-y gap-y)
                     :gap-x 0.0 :gap-y gap-y
                     :draw? NIL
                     :color color
                     grid-plot-args)))
      
      
      
      (apply #'grid-plot
             :viewed-object thing
             :interior-view
             (grid-layout
              :viewed-object thing
              :subviews
              (if column-labels?
                (cons row0 remaining-rows)
                remaining-rows)
              :nrows
              (if column-labels?
                (+ 1 nrows)
                nrows)
              :ncols 1
              :box-views? NIL
              :gap-x gap-x :gap-y 0.0)
             :gap-x gap-x :gap-y 0.0
             :draw? draw?
             :color color
             :title title
             grid-plot-args)
      )))
