
(in-package :q-user)

(<- arms-data
    (array '((5.6  29)
             (6.1  125)
             (6.0  100)
             (4.8  4)
             (5.2  10)
             (5.8  60)
             (5.4  10)
             (6.0  125)
             (5.7  40)
             (5.9  90)
             (5.5  16)
             (5.3  12)
             (5.5  23)
             (5.4  16)
             (5.1  6)
             (5.0  8)
             (4.9  2)
             (6.1  165)
             (6.0  140))))
(<- arms-vars (list "west" "soviet" "num" "ignore1"
                    "fit" "ignore1" "fit-low" "fit-up" "resid"))
(<- arms-output 
    (array
     '((1     5.6000      5.5792      0.0203      5.5364      5.6221      0.0208)
       (2     6.1000      6.0279      0.0319      5.9607      6.0952      0.0721)
       (3     6.0000      5.9594      0.0293      5.8976      6.0211      0.0406)
       (4     4.8000      4.9709      0.0353      4.8963      5.0454     -0.1709)
       (5     5.2000      5.2523      0.0250      5.1995      5.3050     -0.0523)
       (6     5.8000      5.8025      0.0241      5.7516      5.8535    -.002515)
       (7     5.4000      5.2523      0.0250      5.1995      5.3050      0.1477)
       (8     6.0000      6.0279      0.0319      5.9607      6.0952     -0.0279)
       (9     5.7000      5.6780      0.0214      5.6329      5.7230      0.0220)
       (10     5.9000      5.9270      0.0281      5.8677      5.9863     -0.0270)
       (11     5.5000      5.3966      0.0215      5.3512      5.4420      0.1034)
       (12     5.3000      5.3083      0.0234      5.2588      5.3577    -0.00826)
       (13     5.5000      5.5081      0.0203      5.4653      5.5508    -.008054)
       (14     5.4000      5.3966      0.0215      5.3512      5.4420    .0033932)
       (15     5.1000      5.0954      0.0304      5.0313      5.1595    .0046037)
       (16     5.0000      5.1837      0.0272      5.1264      5.2411     -0.1837)
       (17     4.9000      4.7580      0.0446      4.6639      4.8522      0.1420)
       (18     6.1000      6.1132      0.0353      6.0386      6.1877     -0.0132)
       (19     6.0000      6.0627      0.0333      5.9925      6.1329     -0.0627))))




(<- arms (cglue arms-data arms-output))
(dataset arms :variates arms-vars :name "Arms race")

#|

(defun log10(x) (log x 10))

(setq s4 (scatterplot :data arms :x "soviet" :x-function 'log10  :y "west" ))
(add-lines s4 :data arms :x "soviet" :x-function 'log10 :y "fit")
;; or (layer-view (interior-view-of s4) 'lines :x "soviet" :x-function 'log10 :y "fit")


(setq s1 (scatterplot :data arms :x "soviet" :y "west" :draw? nil :title nil))

(setq s2 (scatterplot :data arms :x "soviet" :x-function 'log10  :y "west" 
                      :draw? nil :title nil))


(setq s3 (scatterplot :data arms :x "soviet" :y "west"  :title nil))
(setq l3 (lines :data arms :x "soviet" :y "fit"))
(layer-subview s3 l3 (interior-view-of s3)  )


(setq s4 (scatterplot :data arms :x "soviet" :x-function 'log10
                      :y "west"  :title nil))
(setq l4 (lines :data arms :x "soviet" :x-function 'log10 :y "fit"))
(layer-subview s4 l4 (interior-view-of s4) )

(row-layout :subviews (list s3 s4) :draw? t :gap-x 0.1)

(setq s5 (scatterplot :data arms :x "soviet" :y "west" ))

(layer-subview s5 l3 (interior-view-of s5))
(setq l3u (lines :data arms :x "soviet"  :y "fit-up"))
(layer-subview s5 l3u (interior-view-of s5))
(setq l3l (lines :data arms :x "soviet"  :y "fit-low"))
(layer-subview s5 l3l (interior-view-of s5))

(setq s6 (scatterplot :data arms :x "soviet" :x-function 'log10  :y "west" :draw? nil :title nil))
(setq l4 (lines :data arms :x "soviet" :x-function 'log10 :y "fit"))
(layer-subview s6 l4 (interior-view-of s6))
(setq l4u (lines :data arms :x "soviet" :x-function 'log10 :y "fit-up"))
(layer-subview s6 l4u (interior-view-of s6))
(setq l4l (lines :data arms :x "soviet" :x-function 'log10 :y "fit-low"))
(layer-subview s6 l4l (interior-view-of s6))

(setq s7 (scatterplot :data arms :x "fit"  :y "resid" :draw? nil :title nil))
(setq s8 (scatterplot :data arms :x "num"  :y "resid" :draw? nil :title nil))

(setq p (grid-layout :subviews (list s1 s2 s3 s4 s5 s6 s7 s8)
                          :gap-x 0.05 :gap-y 0.05
                          :draw? nil :nrows 4 ))

(setq w (make-view-window  :left 20 :bottom 20 :right 600 :top 300))
(draw-view p :viewport (make-viewport w))

(loop for s in (sub-views-of p) do
      (set-drawing-style (left-label-of s) :font :smaller)
      (set-drawing-style (bottom-label-of s) :font :smaller))


(setq p2 (row-layout :subviews (list s7 s8)
                          :gap-x 0.08 :gap-y 0.08
                          :draw? t  ))
|#