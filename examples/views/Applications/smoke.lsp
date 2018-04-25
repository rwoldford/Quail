
(in-package :quail-user)
(load "q:Data;smoker.lsp") ;; sets up data smoke-data


(variate-display-list :data smoke-data :draw? t)




;; First we look at the interrelationships between the 3 smoking variables
(setq sm 
      (scat-mat :data smoke-data :vars '("blood carbon monoxide"
                                         "blood nicotine"
                                         "puffed smoke")))

;; Next we construct a table of counts for sex by age.

(setq b (table-plot   :data smoke-data :by '("sex"  "age") :left-label (list "male" "female")))

(rescale-entries (interior-view-of b) :justification :bottom :dimension :height)


;; By linking, we can examine how the smoking variables
;; vary across age and sex.
(link-views sm  b)


;; For instance, there is possibly a  tendency for high blood carbon monoxide
;; to be more prevalent in younger age groups.
;; To examine this more closely, plot blood carbon monoxide vs age.
;; We do this with two overlayed point clouds, one for men and one for
;; women, so we can use different :x-function arguments to separate the men and women,
;; and assign them different color values at construction.

(setq s1 (overlay-plot :data smoke-data :by "sex"
                       :x "age" :y "blood carbon monoxide"))




;; Now lets overlay on each point cloud a smooth constructed by
;; computing the median blood carbon monoxide value at each age level.
;; We first define the following function:

(defun group-summary (x y f)
  "This function groups the ys into sublists by their x values,~
   applies f to each sublist, and then returns a list of~
   fitted values, one per y element."
  (let* ((xs (remove-duplicates x))
         (ys (make-list (length xs) )))
    (loop for xi in x
          for yi in y
          do
          (push yi (elt ys (position xi xs))))
    (setq ys (mapcar f ys))
    (loop for xi in x
          collect (elt ys (position xi xs)))))

(loop for i in (interior-views-of s1)
      for c = (draw-style (first (sub-views-of i)) :color)  do
      (add-smooth i :fit-fn #'group-summary :smooth-par #'median :color c))

;; The plot does not show much of an age-blood carbon monoxide pattern
;; however we see the median response for men (red) is lower than that for
;; women.


;; We can examine the pattern for other variables using:
(change-variable s1 :y "blood nicotine")
(change-variable s1 :y "puffed smoke")
(change-variable s1 :y "blood carbon monoxide")

;; If you want to look at all 3 responses simulataneously do:


(setq s2 (copy-view s1 :draw? t))
(change-variable s2 :y "blood nicotine")

(setq s3 (copy-view s1 :draw? t))
(change-variable s3 :y "puffed smoke")

;; To plot s1 s2 s3 in a single window for printing:

(col-layout :subviews (list s1 s2 s3) :draw? t :box-views? nil)




;; Some dot plots a la Cleveland:
;; First blood nicotine by age: (here a similar plot would be obtained
;; via a scatterplot.)



(setq g1  (batch-plot :data smoke-data :by "age" :subview-type '1d-point-cloud :var "blood nicotine"
          ))

(change-variable g1 :var "blood carbon monoxide")
(setq g2 (copy-view g1))
(change-variable g2 :var "puffed smoke")


;; The following one shows mean blood nicotine by age.
;; Here each 1d-point-cloud has a single subject, which is the list of cases
;; at each age group.
;; Following Cleveland's style, the means at each group are connect by a line to the left edge.

(setq g1  (batch-plot :data smoke-data :by "age" :subview-type (list :type '1d-point-cloud :cases #'list 
                      :value-fn #'values-of :var "blood nicotine" :function #'mean :lines-to :left)
                      :box-views? nil
          ))
;; or
(setq g1  (batch-plot :data smoke-data :by "age" :subview-type (list :type '1d-point-cloud :cases #'list 
                      :value-fn #'values-of :var "blood nicotine" :function #'mean :lines-to :left-right)
                      :box-views? nil
          ))

;; then change variable and/or function via

(change-variable g1 :var "blood carbon monoxide")

;; This one shows blood nicotine by age and sex (label 1,3 is
;; men, age group 3, for instance).

(setq g1  (batch-plot :data smoke-data :by (list "age" "sex" ) :subview-type (list :type '1d-point-cloud :cases #'list 
                      :value-fn #'values-of :var "blood nicotine" :function #'mean :lines-to :left-right)
                      :box-views? nil :format :col
          ))




;; In the above, reordering the batches arguments would facilitate comparisions
;; of males and females within age groups.
;; However a nicer plot results when each 1d-point-cloud has two cases
;; which are the males and females at each age group.
;; In the following the males are colored with green and the females yellow.

(setq g1  (batch-plot :data smoke-data :by "age" :subview-type (list :type '1d-point-cloud :cases (data-subsets-fn "sex") 
                      :value-fn #'values-of :var "blood nicotine" :function #'mean 
                      :lines-to :left-right :colors (list wb:*green-color*  wb:*yellow-color* ))
                      :box-views? nil :format :col
                      
          ))










