;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;;                               display-data.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  
;;;
;;;  Authors:
;;;     R.W. Oldford 1993, 1995, 1999
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(dataset-view case-view)))


(defun dataset-view (&rest
                     keyword-args
                     &key  (data NIL) (draw? NIL) (viewport NIL) (signposts? T)
                     &allow-other-keys)
  (unless data (setf data (choose-dataset)))
  (if (= 1 (length (list-cases data)))
    (apply #'case-view data :draw? draw? :signposts?  signposts? :vars (list-variates data)
               keyword-args)
  (let* ((cases (case-display-list    
                 :data data   :draw? NIL :scrollable? T  :link? T))
         (vars  (variate-display-list :data data :draw? NIL
                 :scrollable? T
                 :link? T))
         (name  (text-view :text (dataset-name data) :draw? NIL))
         (var-label (text-view :text
                               (format NIL "Variates (~s)" (length (list-variates data)))
                               :draw? NIL))
         (case-label (text-view :text 
                                (format NIL "Cases (~s)" (length (list-cases data)))
                                :draw? NIL))
         (title (text-view :text "Dataset:" :draw? NIL))
         
         (hline1    (line :slope 0 :intercept 0.5
                          :viewed-object data))
         (signpost-view (if signposts? (signposts data :as-dataset? T)))
         
         result)
     
    (setf result
          (cond
           (signpost-view
            (apply #'view-layout
                   :viewed-object data
                   :subviews (list title
                                   name
                                   case-label
                                   cases
                                   var-label
                                   vars
                                   hline1
                                   signpost-view)
                   :positions '((0 20 90 100)
                                (20 80 90 100)
                                (2 20 80 90)
                                (2 40 0 80)
                                (42 62 80 90)
                                (42 80 0 80)
                                 (0 100 0 2)
                                (0 100 -50 0))
                   :draw? NIL
                   keyword-args))
           (T
            (apply #'view-layout
                   :viewed-object data
                   :subviews (list title
                                   name
                                   case-label
                                   cases
                                   var-label
                                   vars
                                   hline1)
                   :positions '((0 20 90 100)
                                (20 80 90 100)
                                (2 20 80 90)
                                (2 40 0 80)
                                (42 62 80 90)
                                (42 80 0 80)
                                (0 100 0 2))
                   :draw? NIL
                   keyword-args)
            )
           
           )
          )
    (when draw?
      (unless (viewport-p viewport)
        (setf viewport 
               (make-viewport
                (make-view-window 
                 :left 10 :right (max 500 (- (wb::screen-width) 100))
                 :bottom (max 0 (- (wb::screen-height) 800))
                 :top (- (wb::screen-height) 100)
                 :title (if (dataset-name data)
                            (format NIL "Viewing ~a" (dataset-name data))
                            "Dataset View")))))
      (draw-view result :viewport viewport))
    result
    )
  ))


(defun case-view (c &rest keyword-args &key (draw? nil) name signposts? (viewport NIL) 
                    vars &allow-other-keys)
  (setq vars (or vars (list-variates c)))
  (let* ((var-display (variate-display-list :data c :scrollable? nil
                                     :title (or name (dataset-name c) "Case")
                                     :labels (mapcar #'variate-string-of vars)
                                     :display-list-border 2))
         (list-vals (values-of  c vars))
         (value-display
                       (display-list :data c :scrollable? nil
                                     :items (cons "" (mapcar #'princ-to-string list-vals))
                                     :display-list-border 2
                                     :item-type '(:justification :left)))
         var-title
         (signpost-view (if signposts? (signposts c :as-dataset? T)))
         result)
    (loop for val in list-vals
        ;;  for var in vars
          for sub in (cdr (subviews-of value-display))
          ;; do (setf (viewed-object-of sub) var)
           when (numberp val) do
          (change-class sub 'numerical-label)
          (setf (slot-value sub 'number) val)
          )

            
    (if (and (member :bold (wb:canvas-font-styles)) 
             (setq var-title (first (subviews-of var-display))))
      (set-view-font var-title :style :bold))
    (setq result (apply #'scrolling-display :data c
         :right-scroller nil :bottom-scroller nil
         :data c
         :gap 5
         :displays
         (list var-display
               value-display)
         :draw? nil keyword-args))
    
   
    (cond (signposts?
           (let* ((w (max 300 (draw-region-width result)))
                 (h (+  100 (draw-region-height result)))
                 (results (view-layout :data c
                                       :subviews (list result signpost-view)
                                       :positions (list 
                                                   (make-region 0 w 100 (+ 100 (draw-region-height result)))
                                                   (make-region 0 w 0 100))
                                       :bounding-region (make-region 0 w 0 h))))
             
             (when draw?
               (unless (viewport-p viewport)
                 (setf viewport 
                       (make-viewport
                        (make-view-window 
                         :title (if (dataset-name c)
                                  (format NIL "Viewing ~a" (dataset-name c))
                                  "Case View")
                         :left 10 :right (min  (- (wb::screen-width) 40) 
                                               (+ 25 w))
                         :bottom 10 :top (min  (- (wb::screen-height) 40) 
                                               (+ 25 h))))))
                 (draw-view results :viewport viewport))
             results))
          (t (when draw?
               (unless (viewport-p viewport)
                 (setf viewport 
                       (make-viewport
                        (make-view-window 
                         :title (if (dataset-name c)
                                  (format NIL "Viewing ~a" (dataset-name c))
                                  "Case View")
                         :left 10 :right (min  (- (wb::screen-width) 40) 
                                               (+ 25 (draw-region-width result)))
                         :bottom 10 :top (min  (- (wb::screen-height) 40) 
                                               (+ 25 (draw-region-height result)))))))
                 (draw-view result :viewport viewport))
             result))))

