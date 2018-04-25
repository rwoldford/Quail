;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               qq-plot.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1993
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(qq-plot qq-gauss)))

(defgeneric qq-plot (dist1 dist2 
                           &rest plot-args
                           &key percentiles title draw? lines?
                           &allow-other-keys)
  (:documentation
   "Produce a quantile-quantile plot at the selected percentiles. ~
    (:required (:arg dist1 The distribution whose quantiles are ~
    plotted as the x-coordinate.) ~
    (:arg dist2 The distribution whose quantiles are ~
    plotted as the y-coordinate.)) ~
    (:key ~
    (:arg percentiles NIL The percentiles at which the quantiles are to ~
    be calculated.  This must be a refable object whose elements are between ~
    0 and 1.  If NIL, either a sequence of the 99 elements from 0.01 to 0.99 ~
    is used, or if one of the required arguments is a refable object of finite ~
    size, then the percentiles defined by its empirical distribution are used.) ~
    (:arg lines? NIL If non-NIL then a lines-plot is constructed.  If NIL ~
    then a scatterplot is constructed.)~
    (:arg title \"Quantile Quantile Plot\" A string to appear as the title of the plot.) ~
    (:arg draw? T Toggles whether the plot should be drawn once created.)) ~
    (:rest ~
    (:arg plot-args NIL Keyword arguments to be passed on to the plot at ~
    construction.))"))

(defmethod-multi qq-plot ((dist1 (array dimensioned-ref-object))
                          (dist2 T)
                          &rest plot-args
                          &key (percentiles NIL percentiles?)
                          (title "Quantile-Quantile Plot")
                          (draw? T)
                          (lines? NIL)
                          &allow-other-keys)
  (unless percentiles?
    (setf percentiles :standard))
  (let
    ((plot-fn (if lines? #'lines-plot #'scatterplot)))
    (cond
     ((eq percentiles :standard)
      (apply plot-fn
             :x (sort dist1 #'< :copy? T)
             :y (array (quantile-at dist2 
                             (let ((n (number-of-elements dist1)))
                               (setf percentiles
                                     (loop for i from 1 to n
                                           collect (/ (- i 0.5) n)))))
                       )
             :title title
             :draw? draw? plot-args))
     (percentiles
      (apply plot-fn
             :x (quantile-at dist1 percentiles)
             :y (quantile-at dist2 percentiles)
             :title title
             :draw? draw? plot-args))
     (T (setf percentiles (cdf-at dist1 dist1))
        (apply plot-fn
               :x dist1
               :y (quantile-at dist2 percentiles)
               :title title
               :draw? draw? plot-args)))))

(defmethod qq-plot ((dist1 sequence)
                    (dist2 T)
                    &rest plot-args
                    &key (percentiles NIL percentiles?)
                    (title "Quantile-Quantile Plot")
                    (draw? T)
                    (lines? NIL)
                    &allow-other-keys)
  (unless percentiles?
    (setf percentiles :standard))
  (let
    ((plot-fn (if lines? #'lines-plot #'scatterplot)))
    (cond
     ((eq percentiles :standard)
      (apply plot-fn
             :x (sort dist1 #'< :copy? T)
             :y (quantile-at dist2 
                             (let ((n (number-of-elements dist1)))
                               (setf percentiles
                                     (loop for i from 1 to n
                                           collect (/ (- i 0.5) n)))))
             :title title
             :draw? draw? plot-args))
     (percentiles
      (apply plot-fn
             :x (quantile-at dist1 percentiles)
             :y (quantile-at dist2 percentiles)
             :title title
             :draw? draw? plot-args))
     (T (setf percentiles (cdf-at dist1 dist1))
        (apply plot-fn
               :x dist1
               :y (quantile-at dist2 percentiles)
               :title title
               :draw? draw? plot-args)))))

(defmethod qq-plot ((dist1 T)
                    (dist2 sequence)
                    &rest plot-args
                    &key (percentiles NIL percentiles?)
                    (title "Quantile Quantile Plot")
                    (draw? T)
                    (lines? NIL)
                    &allow-other-keys)
  (unless percentiles?
    (setf percentiles :standard))
  (let
    ((plot-fn (if lines? #'lines-plot #'scatterplot)))
    (cond
     ((eq percentiles :standard)
      (apply plot-fn
             :x (quantile-at dist1 
                             (let ((n (number-of-elements dist2)))
                               (setf percentiles
                                     (loop for i from 1 to n
                                           collect (/ (- i 0.5) n)))))
             :y (sort dist2 #'< :copy? T)
             :title title
             :draw? draw? plot-args))
     (percentiles
      (apply plot-fn
             :x (quantile-at dist1 percentiles)
             :y (quantile-at dist2 percentiles)
             :title title
             :draw? draw? plot-args))
     (T (setf percentiles (cdf-at dist2 dist2))
        (apply plot-fn
               :x (quantile-at dist1 percentiles)
               :y dist2
               :title title
               :draw? draw? plot-args))
     )))

(defmethod-multi qq-plot ((dist1 T)
                          (dist2 (array dimensioned-ref-object))
                          &rest plot-args
                          &key (percentiles NIL percentiles?)
                          (title "Quantile Quantile Plot")
                          (draw? T)
                          (lines? NIL)
                          &allow-other-keys)
  (unless percentiles?
    (setf percentiles :standard))
  (let
    ((plot-fn (if lines? #'lines-plot #'scatterplot)))
    (cond
     ((eq percentiles :standard)
      (apply plot-fn
             :x (array (quantile-at dist1 
                             (let ((n (number-of-elements dist2)))
                               (setf percentiles
                                     (loop for i from 1 to n
                                           collect (/ (- i 0.5) n))))))
             :y (sort dist2 #'< :copy? T)
             :title title
             :draw? draw? plot-args))
     (percentiles
      (apply plot-fn
             :x (quantile-at dist1 percentiles)
             :y (quantile-at dist2 percentiles)
             :title title
             :draw? draw? plot-args))
     (T (setf percentiles (cdf-at dist2 dist2))
        (apply plot-fn
               :x (quantile-at dist1 percentiles)
               :y dist2
               :title title
               :draw? draw? plot-args))
     )))

(defmethod qq-plot ((dist1 prob-measure)
                    (dist2 prob-measure)
                    &rest plot-args
                    &key (percentiles NIL)
                    (title "Quantile Quantile Plot")
                    (draw? T)
                    (lines? NIL)
                    &allow-other-keys)
  (let
    ((plot-fn (if lines? #'lines-plot #'scatterplot)))
    (cond
     (percentiles
      (apply plot-fn
             :x (quantile-at dist1 percentiles)
             :y (quantile-at dist2 percentiles)
             :title title
             :draw? draw? plot-args))
     (T (setf percentiles (seq 0.1 .9 .05))
        (apply plot-fn
               :x (quantile-at dist1 percentiles)
               :y (quantile-at dist2 percentiles)
               :title title
               :draw? draw? plot-args)))))

(defun qq-gauss (y  &rest plot-args
                   &key (percentiles NIL)
                   (title "Gaussian QQ Plot")
                   (draw? T)
                   (lines? NIL)
                   (bottom-label "Gaussian quantiles")
                   &allow-other-keys)
  "Produce a gaussian quantile-quantile plot at the selected percentiles. ~
   (:required (:arg y The data or distribution whose quantiles are ~
   plotted as the y-coordinate against the corresponding standard ~
   gaussian quantiles ~
   on the x-coordinate.)) ~
   (:key ~
   (:arg percentiles NIL The percentiles at which the quantiles are to ~
   be calculated.  This must be a refable object whose elements are between ~
   0 and 1.  If NIL, a sequence is constructed and used.  The constructed ~
   sequence is either the 99 elements from 0.01 to 0.99 or, if the required ~
   argument is a refable object having n elements, it is the sequence ~
   of n elements whose i'th element is (/ (- i 0.5) n).) ~
   (:arg title \"GaussianQQPlot\" A string to appear as the title of the plot.) ~
   (:arg draw? T Toggles whether the plot should be drawn once created.) ~
   (:arg bottom-label \"GaussianQuantiles\" Label to appear on the x axis.)) ~
   (:rest ~
   (:arg plot-args NIL Keyword arguments to be passed on to the plot at ~
   construction.))"
  (declare (special *gaussian-dist*))
  (unless percentiles
    (setf percentiles :standard)
    )
            
  (apply #'qq-plot
         *gaussian-dist* y
         :percentiles percentiles
         :title title
         :draw? draw?
         :lines? lines?
         :bottom-label bottom-label
         plot-args))
