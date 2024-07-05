(in-package :vw)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '( plot-menu-items)))

(defun make-data-display-menu-items()
  `( ("DataInfo" nil ""
     :sub-items (
                 ("Cases"  (prompt-case-display))
                 ("Variates"  (prompt-variable-display))
                 ("By Group" (prompt-case-display-by))))
    ("Single variate" nil ""
     :sub-items (("Histogram" (prompt-plot :dim 1 :plot-fn ,#'histogram))
                 ("Barchart" (prompt-plot :dim 1 :plot-fn ,#'bar-plot))
                 ("Boxplot" (prompt-plot :dim 1 :plot-fn ,#'boxplot))
                 ("Dot plot" (prompt-plot :dim 1 :plot-fn ,#'dot-plot))
                 ("Plot"  (prompt-plot :dim 1))
                 ("-")
                 ("Histogram by" (prompt-plot-by :dim 1 :plot-fn 'histogram-view))
                 ("Barchart by" (prompt-plot-by :dim 1 :plot-fn 'bar-chart))
                 ("Boxplot by" (prompt-plot-by :dim 1 :plot-fn 'boxplot-view))
                 ("Dot plot by" (prompt-plot-by :dim 1 :plot-fn '1d-point-cloud))
                 ("Plot by" (prompt-plot-by :dim 1))))
    ("Two variates"  nil ""
     :sub-items (("Scatterplot" (prompt-plot :dim 2 :plot-fn ,#'scatterplot))
                 ("Lines plot" (prompt-plot :dim 2 :plot-fn ,#'lines-plot))
                 ("Line Segments" (prompt-plot :dim 2 :plot-fn ,#'line-segment-2d-plot))
                 ("Plot"  (prompt-plot :dim 2))
                 ("-")
                 ("Scatterplot by" (prompt-plot-by :dim 2 :plot-fn ,#'2d-point-cloud))
                 ("Lines plot by" (prompt-plot-by :dim 2 :plot-fn ,#'lines))
                 ("Plot by" (prompt-plot-by :dim 2))
                ;; ("Plot by: overlay" (prompt-plot-by :dim 2 :overlay? t))
                 ))
    ("Multiple variates"  nil ""
     :sub-items (("3D Scatterplot" (prompt-plot :dim 3 :plot-fn "Rotating Plot"))
                 ("3D Line Segments" (prompt-plot :dim 3 :plot-fn "Rotating Lines"))
                 ("Scatterplot matrix" (prompt-plot :dim 3 :plot-fn "Scatterplot Matrix"))
                 ("Andrews' Trace" (prompt-plot :dim 3 :plot-fn "Andrews' Trace"))
                 ("Tukey's Trace" (prompt-plot :dim 3 :plot-fn "Tukey's Trace"))
               
                 ("-")
                 ("Plot matrix" (prompt-plot :dim 3 :plot-fn "Plot Matrix"))
                 ("Side by side" (prompt-plot :dim 3 :plot-fn "Side by side"))
                 ("Cross plot" (prompt-plot :dim '(nil nil)))
                 ("-")
                 ("Scatterplot matrix by" (prompt-plot-by :dim 3 :plot-fn 'pairs-layout))
                 ("3D Scatterplots by"
                  (prompt-plot-by :dim 3 :plot-fn 'rotating-plot))
                 
                  
                 ))
    
    
    ("Table" (prompt-plot-by :dim 0))))

(defvar *saved-selections* nil)


(defun choose-view-set()
  (declare (special   *saved-selections*))
 (third (first (wb:prompt-for-items
                      *saved-selections* :prompt-text "Choose view set"
                      :item-function #'first))))

(defun view-set-menu-items()
  (declare (special  *selected-views* *saved-selections*))
  `(("View Sets" nil ""
     :sub-items
     (("Name" ,#'(lambda(ignore)
                   (declare (ignore ignore)) 
                   (when *selected-views*
                     (push 
                      (let ((s (list-plot-cases *selected-views*))
                            (name (wb:prompt-user :result-type 'symbol
                                            :read-type :read
                                            :prompt-string "Enter name")))
                      (list name
                            (if *current-dataset* 
                                (make-data-subset *current-dataset*   s :name name :save? nil)
                                (dataset s :cases s :name name :save? nil))
                            *selected-views*))
                      *saved-selections*))))
      
      ("Show"  
       ,#'(lambda(ignore)
           (declare (ignore ignore))   
           (when *saved-selections*
              (with-update-style-cache
                (loop for v in *selected-views* do
                      (set-highlight-style  v  nil))
                (set-selected-views (choose-view-set))
                (loop for v in *selected-views* do
                      (set-highlight-style  v  t))))))
      
      ("Union"  
       ,#'(lambda(ignore)
            (declare (ignore ignore)) 
            (when *saved-selections*
              (let ((new (choose-view-set)))
                (with-update-style-cache
                (loop for v in new do
                        (set-highlight-style v t)
                        (pushnew v *selected-views*)))))))
      ("Intersection"  
       ,#'(lambda(ignore)
            (declare (ignore ignore)) 
            (when *saved-selections*
              (let* ((old-views (remove-duplicates (highlighted-views)))
                     (sel-views (choose-view-set)))
           (loop for v in old-views
              do (set-highlight-style v  nil :not-from sel-views :draw-links? nil))
            (set-selected-views  sel-views)))))
      
      
      ("Difference"  
       ,#'(lambda(ignore)
            (declare (ignore ignore)) 
            (when *saved-selections*
              (let ((new (choose-view-set)))
                (with-update-style-cache
                  (loop for v in new
                        ;;   when (any-highlight? v)
                        do (set-highlight-style v nil)
                        (set-selected-views (delete v *selected-views*)))
                  *selected-views* )))))
      
       ))))





(let ((plot-menu-items nil)
      )
  (defun plot-menu-items ()
    
    (or plot-menu-items
        (setq plot-menu-items
              `(,@(make-data-display-menu-items)
                
                ("Function" nil ""
                 :sub-items ( ("Single variable function" (function-plot))
                              ("Two variable function" (surface-plot))))
                ("Grid Layout" ,#'(lambda(ignore)
                                   (declare (ignore ignore))
                                   (let ((subs (selected-views)))
                                     (if subs
                                       (grid-layout :subviews subs :draw? t)
                                       (grid-layout :draw? t :nsubviews 
                                                    (wb:prompt-user :result-type 'number :read-type :eval
                                :prompt-string "Enter number of subviews")
                                   )))))
                 ("View Layout" (view-layout :nsubviews 0 :draw? t))
                ("-")
                              
                 
                ("Linking" nil ""
                 :sub-items 
                 (("Link views" (link-selected-views))
                  
                 #| 
                  ("Background link table"
                   ,#'(lambda(ignore)
                         (declare (ignore ignore) (special *foreground-link-tables*))
                         (mapcar #'background-link-table 
                                 (choose-link-tables *foreground-link-tables*))))
                  ("Foreground link table"
                   ,#'(lambda(ignore)
                         (declare (ignore ignore) (special *background-link-tables*))
                         (mapcar #'foreground-link-table 
                                 (choose-link-tables *background-link-tables*))))
|#
                  ("Unlink views" (unlink-top-views))
                  ("New link table" ,#'(lambda(ignore)
                                         (declare (ignore ignore))
                                         (let ((f (wb::prompt-user :result-type  t
                                                                   :prompt-string "Enter a link test function"
                                                                   :read-type :read))
                                               name
                                               new)
                                                               
                                                    (setq new (make-link-table :test (get-function f)))
                                                    (setq name (wb:prompt-user :result-type 'symbol
                                                                               :read-type :read
                                                                               :initial-string (link-table-name-of new)
                                                                               :prompt-string "Name of table"))
                                                    (setf (link-table-name-of new) name))))
                  ("Delete all link tables" (delete-link-tables))
                           
                  ))
                 ,@(view-set-menu-items)
                )))))



;;(setq q::*quail-plot-menu-items* (plot-menu-items ))
;;(q::install-default-quail-menubar)
 
