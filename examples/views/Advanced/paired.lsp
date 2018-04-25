;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               paired.lsp
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
;;;     C.Hurley
;;;
;;;
;;;----------------------------------------------------------------------------------

#|
We consider a dataset (from \cite{JohnsonWichern}),
where eleven effluent samples are divided in two and sent to two
laboratories for testing. Two measurements, biochemical oxygen demand (BOD) and suspended
solids (SS) are obtained for each half sample.
This is a two-way dataset, where the factors are sample number and
laboratory, and each half-sample is a case. 
|#
(<- mdata (mway-dataset  (array 
                           '(6 27 25 15
                             6 23 28 13
                             18 64 36 22
                             8 44 35 39
                             11 30 15 31
                             34 75 44 64
                             28 26 42 30
                             71 124 54 64
                             43 54 34 56
                             33 30 29 20
                             29 14 39 21)
                   :dimensions '(11 2 2))
                              :factors '("Sample" "Lab")
                             :variates '("Bod" "Ss")
                             :factor-levels (list (loop for i from 1 to 11 collect i)
                                                  (list "com" "state"))
                    :name "Effluent data"))


;;Alternatively, the dataset could be regarded
;;as multivariate, where each sample is a case.



;;The plots of constructed reflect this dual nature of the data.
;;The first plots (a and b) show dotplots of the half samples where point symbol shape encodes
;;the two levels of the laboratory factor. 





(let ((p (1d-point-cloud :data mdata
          :cases '(:by "Sample")
          :var #'(lambda(d) (select-value d "Sample"))
          :orientation :vertical
          :case-view `(:type data-label
                             :justification :right 
                             :font nil
                             )  
          :flip-y? t )))
  (setq a (scatterplot :data mdata :title nil :left-label nil
                       :x "Bod" :y "Sample" :lines-to :left :left-view p :flip-y? t))
  (setq b (scatterplot :data mdata :title nil :left-label nil
                       :x "Ss" :y "Sample" :lines-to :left :left-view p :flip-y? t
                       :case-views-from a)))
       

;; Next we make a "legend" for the plots:                            



(setq legend (grid-layout :nrows 1
             :subviews (list (case-display-list :data mdata :cases '(:by "Lab") :subview-height 10)
                             (case-display-list :data mdata :cases '(:by "Lab") 
                                                :item-type 'point-symbol :subview-height 10))
              :box-views? nil))

;; and position it somewhere convenient in a...


(add-subview a legend (make-region .7 .9 .8 .9))

;; An easy way to assign different symbols to the two labs is:
;; Make a link table using test  #'contains-data-p
(setq contains-lt (make-link-table :test #'contains-data-p :name "Contains"))
(link-view a :link-table contains-lt)

;; There is no need to link b since a and b were constructed to use the same point symbols.

(set-drawing-style (second (subviews-of (second (subviews-of legend)))) 
                   :symbol :triangle :size 7 :fill? t)
(set-drawing-style (first (subviews-of (second (subviews-of legend))))
                   :symbol :box :size 5  )

;;The third plot shows a scatterplot of the laboratory
;; differences for the two response variables.
;;









(defun lab-diff(var)
  #'(lambda(d) (- (select-value d var :test '("Lab" "com")) 
                  (select-value d var :test '("Lab" "state")))))

(setq c (scatterplot :data mdata :cases '(:by "Sample") 
                    ;; :value-fn #'values-of
                     :x (lab-diff "Bod")
                     :y (lab-diff "Ss")
                     :title nil
                     :bottom-label "BOD" :left-label "SS"
             ))

(layer-view (interior-view-of c)  
                  (line :orientation :horizontal :color wb:*light-grey-color*))
(layer-view (interior-view-of c)  
                  (line :orientation :vertical :color wb:*light-grey-color*))

;; c can also be linked to the others  test  #'contains-data-p

(link-view c :link-table contains-lt)



;; Mark the two outliers in c with different colours- they are cases 3 and 8.

;; If you change the drawing styles of the point symbols in c
(set-drawing-style (interior-view-of c) :fill? t :symbol :circle :size 6)

;; This will also affect those of a and b and 
;; you will want to redo

(set-drawing-style (second (subviews-of (second (subviews-of legend)))) 
                   :symbol :triangle :size 7 :fill? t)
(set-drawing-style (first (subviews-of (second (subviews-of legend))))
                   :symbol :box :size 5  )
;; Here we are using the colors of the point symbols in a (also b) to
;; reflect a case's sample, and the choice of symbol shape encodes
;; the lab used.

;;; To prepare these plots for printing or inclusion in a document:

(draw-view (grid-layout :box-views? nil :gap 0  :nrows 1 :subviews (list a b c))
           :viewport (make-viewport (make-view-window :left 10 :right 700 :bottom 10 :top 300)))


#|
;; alternatively...
(<- sample-data  (dataset-transpose  mdata "Sample" :name "Sample data"))



(setq legend (grid-layout :nrows 1
             :subviews (list (case-display-list :data (dataset-transpose mdata "Lab") :subview-height 10)
                             (case-display-list :data (dataset-transpose mdata "Lab") 
                                                :item-type 'point-symbol :subview-height 10))
              :box-views? nil))

(defun diff(pair) (- (first pair) (second pair)))
(setq c (scatterplot :data sample-data :x '("com BOD"  "state BOD")
                     :x-function #'diff
                     :y '("com SS"  "state SS") :y-function #'diff  :title nil
                     :bottom-label "BOD" :left-label "SS"))

|#