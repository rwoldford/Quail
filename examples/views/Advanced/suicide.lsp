;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               suicide                         
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     C.B. Hurley 
;;;
;;;

#| The suicide data 
 consists of numbers of suicides in W. Germany in 1974-1977.
The suicides are classified by
age (17 groups), method (9 categories), and sex.
Therefore, this multiway dataset has one response variable (count) and
17 x 9 x 2 cases, one for each combination
of the three factor variables.

This data comes originally from Van der Heijden, P.G.M. and de Leeuw, J. (1985)
`Correspondence analysis used complementary to log-linear analysis',
{\it Psychometrika} 50, 429-47. I obtained the data from `Statistical
Analysis using Splus', by Brian Everitt.

(<- suiclist  '( 

m 10 4 0 0 247 1 17 1 6 9
m 15 348 7 67 578 22 179 11 74 175
m 20 808 32 229 699 44 316 35 109 289
m 25 789 26 243 648 52 268 38 109 226
m 30 916 17 257 825 74 291 52 123 281
m 35 1118 27 313 1278 89 299 53 78 198
m 40 926 13 250 1273 89 299 53 78 198
m 45 855 9 203 1381 71 347 68 103 190
m 50 684 14 136 1282 87 229 62 63 146
m 55 502 6 77 972 49 151 46 66 77
m 60 516 5 74 1249 83 162 52 92 122
m 65 513 8 31 1360 75 164 56 115 95
m 70 425 5 21 1268 90 121 44 119 82
m 75 266 4 9 866 63 78 30 79 34
m 80 159 2 2 479 39 18 18 46 19
m 85  70 1 0 259 16 10 9 18 10
m 90 18 0 1 76 4 2 4 6 2
w 10 28 0 3 20 0 1 0 10 6
w 15 353 2 11 81 6 15 2 43 47
w 20 540 4 20 111 24 9 9 78 67
w 25 464 6 27 125 33 26 7 86 75
w 30 530 2 29 178 42 14 20 92 78
w 35 688 5 44 272 64 24 14 98 110
w 40 566 4 24 343 76 18 22 103 86
w 45 716 6 24 447 94 13 21 95 88
w 50 942 7 26 691 184 21 37 129 131
w 55 723 3 14 527 163 14 30 92 92
w 60 820 8 8 702 245 11 35 140 114
w 65 740 8 4 785 271 4 38 156 90
w 70 624 6 4 610 244 1 27 129 46
w 75 495 8 1 420 161 2 29 129 35
w 80 292 3 2 223 78 0 10 84 23
w 85 113 4 0 83 14 0 6 34 2
w 90 24 1 0 19 4 0 2 7 0
))

(<- suic  (array suiclist :dimensions '(2 17 11)))

(<- methods '(Poison Cookgas Toxicgas Hang Drown Gun Knife Jump Other))
(<- sex '(Male Female))
(<- age (loop for i from 10 to 90 by 5 collect i))
(<- suic (ref suic t  t (iseq 2 10)))

(<- sdata (mway-dataset suic :factors (list "Sex" "Age" "Method"  )
                             :variates (list "count")
                             :factor-levels (list sex age methods)
                    :name "Suicide data"))


;;; --- The trellis plot ------------------------------------------------



(defun log1(x) (log (+ 1 x)))
(setq b
      (let* ((p (1d-point-cloud :data sdata :cases '(:by "Age") :var :iseq :orientation :vertical
                 :flip-y? t
                 :case-view `(:type label 
                                    :font ,wb:*very-small-graphics-font*
                                    :justification :right) ))
             (b (batch-plot :data sdata :by "Method" :x "count" :y :iseq :x-function #'log1 
                            :ncols 3
                            :subview-type `(:type overlay-plot :margins :none :title label :flip-y? t
                                                  ;;:lines-to :left-right   :ecolor ,wb::*light-gray-color*
                                                  :by "Sex"   )
                            :left-view (list p (copy-view p) (copy-view p )   )
                            :no-labels? t :top-view nil
                            :right-view nil :left-view-size 0.08
                            :bottom-view '(:type axis :min 0 :max 8 :pretty? nil :tic-list (0 4 8))
                            :draw? nil
                            ))
             (w (make-view-window :left 0 :bottom 0 :top 400 :right 400)))
        (draw-view b :viewport (make-viewport w))
         
         b))

(loop for v in (subviews-of  (interior-view-of b))
              when (typep v '2d-plot)
              do (set-drawing-style  (second (interior-views-of v) ) :edashing nil )
              (set-drawing-style  (second (interior-views-of v) ) :ecolor 13421772 )
              (set-lines-to  (second (interior-views-of v) ) :left-right) )

;; It might be clearer to show all the factor levels separately:


(setq g (grid-layout :data sdata :subviews 
             `((:type case-display-list :title "Age"  :cases (:by  "Age") :scrollable? nil 
                               )
                  (:type case-display-list :title "Method"  :cases (:by  "Method")  :scrollable? nil )
                  (:type case-display-list  :title "Sex"   :cases (:by   "Sex")   :scrollable? nil ))
              :cols '(.1 .28 .3 .68 .7 1.0)
              :draw? t))

(setq lt (make-link-table :test #'subset-id))

(setq lc (make-link-table :test #'contains-data-p))
(link-view b :type 'point-symbol :link-table lt)
(link-view b  :link-table lt)
(link-view g  :link-table lt)

;; Note: we can use g to select various combinations of factor levels
;; We link only the point symbols in b. Otherwise the age labels would be linkable
;;; --- Correspondence analysis computation ------------------------------------------------



(<- d (array suiclist :dimensions '(34 11)))
(<- d (ref d t (iseq 2 10)))
(<- d (/ d (sum d)))
(<- dr (collapse #'sum d :slices 0))
(<- dc (collapse #'sum d :slices 1))

(<- drdc (.* dr (tp dc))) 


(<- e (/ (- d drdc) (sqrt drdc)))
(<- esvd (svd-of e))

(<- delta (singular-values-of esvd))

(<- u (ref (left-singular-vectors-of esvd) t '(0 1)))
(<- v (ref (right-singular-vectors-of esvd) t '(0 1)))
(<- u1 (/ (* (ref u t 0) (eref delta 0)) (sqrt dr)))
(<- u2 (/ (* (ref u t 1) (eref delta 1)) (sqrt dr)))
(<- v1 (/ (* (ref v t 0) (eref delta 0)) (sqrt dc)))
(<- v2 (/ (* (ref v t 1) (eref delta 1)) (sqrt dc)))

(<- udata (array (row-major-list-elements (cglue  u1 u2)) :dimensions '(2 17 2)))

(<- vdata (cglue  v1 v2))

;; edata are the Pearson residuals assuming independece of Method with Age.Sex
;; which are fed into the SVD to compute the CA
;; udata are the CA coordinates for "rows" = Age.Sex factor, vdata are the
;; CA coordinates for the "columns" = Method factor.
;; Turn these into datasets prior to plotting:


(<- edata (mway-dataset (array e :dimensions '(2 17 9)) :factors (list "Sex" "Age" "Method"  )
                             :variates (list "resid")
                             :factor-levels (list sex age methods)
                    :name "Suicide residuals"))

(<- udata (mway-dataset  udata
                                  :factors (list "Sex" "Age"  )
                             :variates (list "CA1" "CA2")
                             :factor-levels (list sex age )
                    :name "Age.Sex score"))


(<- vdata (mway-dataset  vdata
                           :factors (list "Method" )
                             :variates (list "CA1" "CA2")
                             :factor-levels (list methods )
                    :name "Method score"))


;;; --- Residual plots ------------------------------------------------


;; First, plot the residuals used to compute the CA.


(setq eplot
      (let* ((p (1d-point-cloud :data edata :cases '(:by "Age") :var :iseq :orientation :vertical
                 :flip-y? t
                 :case-view `(:type label 
                                    :font ,wb:*very-small-graphics-font*
                                    :justification :right)))
             (b (batch-plot :data edata :by "Method"
                            :x "resid" :y :iseq 
                            :ncols 3
                            :subview-type `(:type overlay-plot :margins :none :title label :flip-y? t
                                                  ;; :lines-to :left-right   :ecolor ,wb::*light-gray-color*
                                                  :by "Sex")
                            :left-view (list p (copy-view p) (copy-view p )   )
                            :no-labels? t :top-view nil
                            :right-view nil :left-view-size 0.08
                            :bottom-view '(:type axis :min -.08 :max .08 :pretty? nil :tic-list (-.08 0 .08))
                            :draw? nil
                            ))
             (w (make-view-window :left 0 :bottom 0 :top 400 :right 400)))
        (draw-view b :viewport (make-viewport w))
         
         b))

        
(loop for v in (subviews-of  (interior-view-of eplot))
              when (typep v '2d-plot)
              do (set-drawing-style  (second (interior-views-of v) ) :edashing nil )
              (set-drawing-style  (second (interior-views-of v) ) :ecolor 13421772 )
              (set-lines-to  (second (interior-views-of v) ) :left-right) )
        
(loop for v in (subviews-of  (interior-view-of eplot))
      when (typep v '2d-plot)
      do  
      (layer-view  (interior-view-of v) 'oriented-line 
                           :orientation :vertical :intercept 0 :viewed-object nil))
        

;; eplot and b show "the same" cases, in the sense that the cases represent the same
;; factor combinations, but the cases are not automatically "eq-dataset".
;; To change this, one could change eq-dataset, or, define a new link test::




(defun eq-idt(a b)
  (or (eq-dataset a b) (eq-identifiers (identifier-of a) (identifier-of b))))
      

;; Then link the plots..

(setq lt (make-link-table :test #'eq-id))

(link-view b :to-views :old :draw? nil :type 'point-symbol)
(link-view eplot :to-views :old :draw? nil :type 'point-symbol)

;; or, parallel link for speed
(fast-link2-views  eplot b :draw? nil :type '(and linkable-mixin point-symbol))


;; We have not linked the age and method labels.
;; Instead, we draw the labels in a separate display below.

;;; --- Correspondence analysis plots ------------------------------------------------


;; The plot the CA scores.

(setq ca 
      (let ((s
             (scatterplot :data udata :x "CA1" :y "CA2"
                          :left-view (list :type 'axis :tic-format "~4,1F")
                          :bottom-view   (list :type 'axis :tic-format "~3,1F"))))
        
        (layer-view (interior-view-of s) (2d-point-cloud :data vdata  :x "CA1" :y "CA2"
                                          :case-view 'data-label))
        
        (set-aspect-ratio s )
        (layer-view (interior-view-of s)  'oriented-line :orientation  :vertical :intercept 0 :data nil)
        (layer-view (interior-view-of s)  'oriented-line :orientation  :horizontal :intercept 0 :data nil)
        s))


(defun subset-id(a b)
  (or (eq-dataset a b) 
      (let ((ia (identifier-of a))
            (ib (identifier-of b)))
        (or (eq-identifiers ia ib)
            (cond ((and (listp ia) (listp  ib))
                   (or
                    (subsetp ia ib :test #'eq-identifiers)
                    (subsetp ib ia :test #'eq-identifiers)))
                  ((listp ia)
                   (member ib ia :test #'eq-identifiers))
                  ((listp ib)
                   (member ia ib :test #'eq-identifiers))
                  (t nil))))))

(set-link-table-test #'subset-id :link-table  lt :rebuild-links? nil)
(link-view  ca :link-table lt :draw? t :type 'point-symbol)
(link-view  ca :link-table lt :draw? t )


(loop for s in (descendant-views-of-type b 'plot)
      when (title-of s) do
      (setf (vw::linkable-view-p (title-of s)) t))

(loop for s in (descendant-views-of-type eplot 'plot)
      when (title-of s) do
      (setf (vw::linkable-view-p (title-of s)) t))

    


(setq ltm (make-link-table  :name "Method" :test #'id-test))

(link-view b  :type 'label :link-table ltm)
(link-view eplot  :type 'label :link-table ltm)
(link-view ca  :type 'label :link-table ltm)




#|
;; It might be clearer to show all the factor levels separately:


(setq g (grid-layout :data sdata :subviews 
             `((:type case-display-list  :cases (:by  "Age") :scrollable? nil 
                               )
                  (:type case-display-list  :cases (:by  "Method")  :scrollable? nil )
                  (:type case-display-list    :cases (:by   "Sex")   :scrollable? nil ))
              :cols '(.1 .2 .3 .6 .7 1.0)
              :draw? t))




(link-view  g :link-table lt :draw? nil )




|#