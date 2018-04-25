;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               single-plot.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1994 George Washington University
;;;     

(in-package :vw)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(single-plot *default-2d-plot* *default-1d-plot* 
          *AUTO-SHARE-CASE-VIEWS?*)))

(defvar *default-2d-plot* 'scatterplot)

(defvar *default-1d-plot* 'histogram)


(defvar *AUTO-SHARE-CASE-VIEWS?* t)

(setq *AUTO-SHARE-CASE-VIEWS?* t)



   

(defgeneric single-plot-by-dim (dim
                                &key
                                plot-fn 
                                dataset  cases 
                                vars  args)
  (:documentation  "A generic function to be called by single-plot that ~
                    allows dispatching by dimension dim.  dim must be an integer."))

     


(defmethod single-plot-by-dim ((dim (eql 0))
                                &key
                                plot-fn 
                                dataset  cases 
                                vars  args)
  (declare (ignore args vars plot-fn))
      (let* ((plot (car (wb:prompt-for-items (list "Case Display" "Variate Display")))))
        (if (string-equal plot "Variate Display")
          (variate-display-list :data dataset  :draw? t)
          (case-display-list :data dataset :cases cases :draw? t))))

(defmethod single-plot-by-dim ((dim (eql 1))
                                &key 
                                plot-fn 
                                dataset  cases
                                vars  args)
  (declare (ignore dataset cases))
  (let ((var (or (first vars) :prompt)))
    (cond ((eq plot-fn #'bar-plot)
           (apply #'bar-plot :by var args))
          (plot-fn (apply plot-fn :var var  args))
          (t   (apply #'1d-plot :var var :interior-view :prompt args)))))

(defmethod single-plot-by-dim ((dim (eql 2))
                               &key 
                               plot-fn 
                               dataset  cases
                               vars  args)
  (declare (ignore dataset cases))
  (let (plot 
        (x (or (first vars) :prompt))
        (y (or (second vars) :prompt)))
    (if plot-fn
      (apply plot-fn :x x :y y  args )
      (progn
        (setq plot (car (wb:prompt-for-items (list  "2d-plot" "Side by side"))))
        (cond  ((string-equal plot "Side by side")
                (if (wb:prompt-true-or-false "Common scale?")
                  
                  (apply #'1d-layout-plot   :vars (or vars :prompt) args )
                  (apply #'1d-layout-plot :left-view nil :link-bounds-x? nil
                         :bottom-view nil :link-bounds-y? nil :vars (or vars :prompt) args )))
               (t (apply #'2d-plot    :x x :y y :interior-view :prompt args )))))))
     

(defmethod single-plot-by-dim ((dim number)
                               &key
                               plot-fn
                               dataset  cases 
                               vars  args)
  (declare (ignore dataset cases))
  (if (>= dim 3) 
    (let ((x (or (first vars) :prompt))
          (y (or (second vars) :prompt))
          (z (or (third vars) :prompt))
          (v (or vars :prompt))
          (items (append (if (= dim 3) '("Rotating Plot" "Rotating Lines"))
                         (list "Scatterplot Matrix" "Plot matrix" "Side by side"))))
      (setq plot-fn (or plot-fn (car (wb:prompt-for-items items))))
      (cond ((functionp plot-fn) (apply plot-fn  :vars v args))
            ((not (stringp plot-fn)) nil)
            ((string-equal plot-fn "Scatterplot Matrix")
             (apply #'scat-mat  :vars v args))
            ((string-equal plot-fn "Plot Matrix")
             (apply #'scat-mat :pairs-view :prompt :vars v args))
            ((string-equal plot-fn "Rotating Plot")
             (apply #'rotating-plot  :x x :y y :z z args))
            ((string-equal plot-fn "Rotating Lines")
               (apply #'rotating-lines-plot  :x x :y y :z z args))
            ((string-equal plot-fn "Side by side")
             (if (wb:prompt-true-or-false "Common scale?")
               
               (apply #'1d-layout-plot   :vars v args )
               (apply #'1d-layout-plot :left-view nil :link-bounds-x? nil
                      :bottom-view nil :link-bounds-y? nil :vars v args )))
            
            (t nil))
      )))
