(defun meta-plot (means sds
                        &rest plot-keywords
                        &key
                        (plot-title "Meta-analysis")
                        (time (iseq (length means)))
                        (probable-error? NIL)
                        (t-value 2.0)
                        (bar-width (float (/ (range time) 30)))
                        (draw? T)
                        (point-size 3)
                        (test-value 0.0)
                        (y-min NIL)
                        (y-max NIL)
                        &allow-other-keys)
  (if probable-error?
    (setf sds (loop for s in sds collect (/ s 0.6745))))
  (let* ((weights 
          (loop for s in sds collect (/ 1.0 (* s s))))
         local-means
         local-weights
         sum-local-weights
         local-sd
         (ci-lower (make-list (length means)))
         (ci-upper (make-list (length means)))
         (cum-means (make-list (length means)))
         (cum-sds (make-list (length means)))
         (lower (make-list (length means)))
         (upper (make-list (length means)))
         (half-bar-width (/ bar-width 2))
         j sd*t
         ci-plot
         cum-plot
         plot
         )
    (unless y-min
      (setf y-min (elt means 0)))
    (unless y-max
      (setf y-max (elt means 0)))
    ;; calculate end-points of all error bars
    (loop for i from 1 to (length means)
          do
          (setf j (- i 1))
          ;; First the regular confidence intervals
          (setf sd*t (* t-value (elt sds j)))
          (setf (elt ci-lower j)
                (- (elt means j) sd*t))
          (setf (elt ci-upper j)
                (+ (elt means j) sd*t))
          ;; Now the cumulative ones
          (setf local-means (subseq means 0 i))
          (setf local-weights (subseq weights 0 i))
          (setf sum-local-weights (sum local-weights))
          (setf (elt cum-means j)
                (/ (sum (* local-means local-weights))
                   (sum local-weights)))
          (setf local-sd (/ 1.0
                            (sqrt sum-local-weights)))
          (setf (elt cum-sds j) local-sd)
          (setf (elt lower j) (- (elt cum-means j)
                                 (* t-value local-sd)))
          (setf (elt upper j) (+ (elt cum-means j)
                                 (* t-value local-sd)))
          ;; Get y axis min and max
          (setf y-min (min y-min (elt ci-lower j) (elt lower j)))
          (setf y-max (max y-max (elt ci-upper j) (elt upper j)))
          )

    (flet ((generate-error-bars (time lower upper)
             (let ((error-bars NIL))
               (loop for moment in time
                     as up in upper
                     as low in lower
                     do
                     ;; lower-bar
                     (push
                      (list :type 'line-segment
                            :endpoints
                            (list (list
                                   (- moment half-bar-width)
                                   low)
                                  (list
                                   (+ moment half-bar-width)
                                   low))
                            )
                      error-bars)
                     ;; draw connecting line
                     (push
                      (list :type 'line-segment
                            :endpoints
                            (list (list
                                   moment
                                   low)
                                  (list
                                   moment
                                   up))
                            )
                      error-bars)
                     
                     ;; draw upper-bar
                     (push
                      (list :type 'line-segment
                            :endpoints
                            (list (list
                                   (- moment half-bar-width)
                                   up)
                                  (list
                                   (+ moment half-bar-width)
                                   up))
                            )
                      error-bars)
                     )
               error-bars))
           )
      
      
      (setf ci-plot
            (apply #'scatterplot
                   :y means
                   :x time
                   :size point-size
                   :shape :circle
                   :fill? T
                   :interior-view
                   (append (list '2d-point-cloud)
                           (generate-error-bars time ci-lower ci-upper)
                           )
                   :draw? NIL
                   (append plot-keywords
                           (list :title "Raw confidence intervals"))))
      (setf cum-plot
            (apply #'scatterplot
                   :y cum-means
                   :x time
                   :size point-size
                   :shape :circle
                   :fill? T
                   :interior-view
                   (append (list '2d-point-cloud)
                           (generate-error-bars time lower upper)
                           )
                   :draw? NIL
                   ;;:right-view y-axis
                   ;;:bottom-view x-axis
                   ;;:left-view NIL
                   ;;:left-label NIL
                   (append plot-keywords
                           (list :title "Cumulative confidence intervals")))
            )
      (link-view-bounds (append (sub-views-of ci-plot)
                                (sub-views-of cum-plot))
                        :x)
      (link-view-bounds (append (sub-views-of ci-plot)
                                (sub-views-of cum-plot))
                        :y)
      (set-extent (or (left-view-of ci-plot)
                      (right-view-of ci-plot))
                  y-min y-max)
      (setf plot (col-layout :subviews (list (label :viewed-object plot-title)
                                             ci-plot cum-plot)
                             :rows '(1.0 0.95 .92 .47 .45 0)
                             :draw? draw?
                             :box-views? NIL))
      (when test-value
        (set-extent (or (left-view-of ci-plot)
                        (right-view-of ci-plot))
                    (min y-min test-value)
                    (max y-max test-value))
        (add-line (interior-view-of ci-plot)
                  :orientation :horizontal
                  :intercept test-value
                  :draw? draw?)
        (add-line (interior-view-of cum-plot)
                  :orientation :horizontal
                  :intercept test-value
                  :draw? draw?)
        )
      
      (values (cglue time ci-lower means ci-upper lower cum-means upper) plot)
      )
    )
  )

(load "q:Data;light-speeds.lsp")

;;; All studies
(setf time (collect-slices (i (ref light-speeds '(:c 0 1 2 3 7 21
                                                  21 22 23 24 25 
                                                  26 27 28 29 30 
                                                  31 32 33 34 35 
                                                  36 37 38 39 40
                                                  41
                                                  58 59
                                                  73)
                                   0)) i))
(setf speed (collect-slices (i (ref light-speeds '(:c 0 1 2 3 7 21
                                                  21 22 23 24 25 
                                                  26 27 28 29 30 
                                                  31 32 33 34 35 
                                                  36 37 38 39 40
                                                  41
                                                  58 59
                                                  73)
                                    3)) i))
(setf sds (collect-slices (i (ref light-speeds '(:c 0 1 2 3 7 21
                                                  21 22 23 24 25 
                                                  26 27 28 29 30 
                                                  31 32 33 34 35 
                                                  36 37 38 39 40
                                                  41
                                                  58 59
                                                  73)
                                  4)) i))

;;; Rotating-mirror studies
(setf time (collect-slices (i (ref rotating-mirror '(:c 1) 0)) i))
(setf speed (collect-slices (i (ref rotating-mirror '(:c 1) 3)) i))
(setf sds (collect-slices (i (ref rotating-mirror '(:c 1) 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title "Rotating mirror studies"
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))

;;; Michelson's rotating-mirror studies with flat fixed mirror
(setf time (collect-slices (i (ref michelson-flat-mirror '(3 4 8 9 10) 0)) i))
(setf speed (collect-slices (i (ref michelson-flat-mirror '(3 4 8 9 10) 3)) i))
(setf sds (collect-slices (i (ref michelson-flat-mirror '(3 4 8 9 10) 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title "Michelson's Rotating mirror studies (flat fixed mirror)."
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))

;;; Michelson's rotating-mirror studies with flat fixed mirror (1879 removed)
(setf time (collect-slices (i (ref michelson-flat-mirror '(4 8 9 10) 0)) i))
(setf speed (collect-slices (i (ref michelson-flat-mirror '(4 8 9 10) 3)) i))
(setf sds (collect-slices (i (ref michelson-flat-mirror '(4 8 9 10) 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title
             "Michelson's Rotating mirror studies (flat fixed mirror and 1879 removed)."
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))

;;; Kerr Cell
(setf time (collect-slices (i (ref kerr-cell T 0)) i))
(setf speed (collect-slices (i (ref kerr-cell t 3)) i))
(setf sds (collect-slices (i (ref kerr-cell t 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title "Kerr Cell studies"
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))

;;; Radar
(setf time (collect-slices (i (ref radar T 0)) i))
(setf speed (collect-slices (i (ref radar t 3)) i))
(setf sds (collect-slices (i (ref radar t 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title "Radar studies"
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))

;;; Cavity Resonator
(setf time (collect-slices (i (ref Cavity-Resonator T 0)) i))
(setf speed (collect-slices (i (ref Cavity-Resonator t 3)) i))
(setf sds (collect-slices (i (ref Cavity-Resonator t 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title "Cavity Resonator Studies"
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))

;;; Geodimeter
(setf time (collect-slices (i (ref geodimeter '(:c 7) 0)) i))
(setf speed (collect-slices (i (ref geodimeter '(:c 7) 3)) i))
(setf sds (collect-slices (i (ref geodimeter '(:c 7) 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title "Geodimeter studies"
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))

;;; radio interferometry
(setf time (collect-slices (i (ref radio-interferometry t 0)) i))
(setf speed (collect-slices (i (ref radio-interferometry t 3)) i))
(setf sds (collect-slices (i (ref radio-interferometry t 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title "Radio Interferometry studies"
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))

;;; spectral lines
(setf time (collect-slices (i (ref spectral-lines t 0)) i))
(setf speed (collect-slices (i (ref spectral-lines t 3)) i))
(setf sds (collect-slices (i (ref spectral-lines t 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title "Spectral Lines studies"
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))

;;; tellurometer
(setf time (collect-slices (i (ref tellurometer '(:c 0) 0)) i))
(setf speed (collect-slices (i (ref tellurometer '(:c 0) 3)) i))
(setf sds (collect-slices (i (ref tellurometer '(:c 0) 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title "Tellurometer studies"
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))

;;; stabilized lasers
(setf time (collect-slices (i (ref stabilized-laser t 0)) i))
(setf speed (collect-slices (i (ref stabilized-laser t 3)) i))
(setf sds (collect-slices (i (ref stabilized-laser t 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title "Stabilized Laser studies"
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))

;;; stabilized lasers sans outlier in 1972
(setf time (collect-slices (i (ref stabilized-laser '(:c 0) 0)) i))
(setf speed (collect-slices (i (ref stabilized-laser '(:c 0) 3)) i))
(setf sds (collect-slices (i (ref stabilized-laser '(:c 0) 4)) i))

(multiple-value-setq 
  (results s)
  (meta-plot speed sds
             :plot-title "Stabilized Laser studies (1972 outlier removed)"
             :time (collect-slices
                     (s (+ time (random-uniform :from -.5 :to .5)))
                     s)
             :probable-error? T
             :test-value 299792.458
             :left-label "Speed"
             :bottom-label "Year"))



