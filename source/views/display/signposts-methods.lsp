;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;;                            signposts-methods.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  
;;;
;;;  Authors:
;;;     R.W. Oldford 1993, 1995, 1996.
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   SIGNPOSTS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add do-nothing primary FEB 02, 1998
(defmethod signposts ((thing T)
                              &rest keyword-args
                              &key
                              (viewed-object NIL viewed-object?)
                              (ncols 5)
                              &allow-other-keys)
   (declare (ignore thing keyword-args viewed-object ncols))
   (call-next-method))




(defmethod signposts :around ((thing T)
                              &rest keyword-args
                              &key
                              (viewed-object NIL viewed-object?)
                              (ncols 5)
                              &allow-other-keys)
  (unless viewed-object? (setf viewed-object thing))
  (let ((signposts (call-next-method)))
    (if (and signposts (listp signposts))
      (apply #'grid-layout :ncols (max (length signposts) ncols)
             :subviews signposts :draw? NIL
             :box-views? NIL :gap-x .05 :gap-y .05
             :viewed-object viewed-object
             :draw? NIL
             keyword-args)
      signposts)
    )
  )

(defmethod signposts ((thing T)
                      &rest keyword-args 
                      &key (as-dataset? NIL)
                      &allow-other-keys)
  (if (dataset-p thing)
    (if as-dataset?
      (list
       (apply #'view-layout
              :subviews
              (list
               (label :viewed-object thing :text "Single variate"
                          :justification '(:bottom :center))
               (control-button :viewed-object thing
                               :text "Plot"
                               :left-fn
                               #'(lambda ()
                                   (prompt-plot :dim 1)))
               (control-button :viewed-object thing
                               :text "Plot by"
                               :left-fn
                               #'(lambda ()
                                   (prompt-plot-by :dim 1))))
              :positions
              '((0 100 80 109)
                (0 100 40 79)
                (0 100 0 39))
              :viewed-object thing
              keyword-args)
       (apply #'view-layout
              :subviews
              (list
               (label :viewed-object thing :text "Two variates"
                          :justification '(:bottom :center))
               (control-button :viewed-object thing
                               :text "Plot"
                               :left-fn
                               #'(lambda ()
                                   (prompt-plot :dim 2)))
               (control-button :viewed-object thing
                               :text "Plot by"
                               :left-fn
                               #'(lambda ()
                                   (prompt-plot-by :dim 2))))
              :positions
              '((0 100 80 109)
                (0 100 40 79)
                (0 100 0 39))
              :viewed-object thing
              keyword-args)
       (apply #'view-layout
              :subviews
              (list
               (label :viewed-object thing :text "Multiple variates"
                          :justification '(:bottom :center))
               (control-button :viewed-object thing
                               :text "Plot"
                               :left-fn
                               #'(lambda ()
                                   (prompt-plot :dim 3)))
               (control-button :viewed-object thing
                               :text "Plot by"
                               :left-fn
                               #'(lambda ()
                                   (batch-plot))))
              :positions
              '((0 100 80 109)
                (0 100 40 79)
                (0 100 0 39))
              :viewed-object thing
              keyword-args)
       
       )
      (apply #'control-button :viewed-object thing
             :text "Display as dataset"
             :left-fn
             #'(lambda ()
                 (dataset-view :data thing :draw? T))
             keyword-args)
      )
    (apply #'control-button :viewed-object thing
           :text "Inspect"
           :left-fn
           #'(lambda () (inspect thing))
           keyword-args)
    )
  )
