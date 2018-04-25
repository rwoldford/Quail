;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       A general collection of plots.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     C.B. Hurley 1994.
;;;     R.W. Oldford 1994.
;;;
;;;

(in-package :quail-user)

(load "q:Data;squid.lsp")


;; The basic single variable plots are:

(histogram)
(boxplot)
(dotplot)


;; To specify the data use:

(setq h (histogram :data squids :var "weight"))
(boxplot :data squids :var "weight")
(dot-plot :data squids :var "weight")


;; To specify the number of histogram bins:

(histogram :data squids :var "weight" :nbins 10)

;; You may not get exactly 10 bins: it tries to produce "nice" bins.
;; To specify the bins end points use:

(histogram :data squids :var "weight" :break-points '(0 1 2 3 4 5 6 7 8 9 10 11))

;; When you give unequal bins widths the y axis is on a density scale.

(histogram :data squids :var "weight" :break-points '(0 1 2 3 4 5 6 7 8 11))


;;--------------------------------------------------------------

;;The basic 2 variable plots are

(scatterplot :data squids :x "weight" :y "width")

;;;coordinates for the ith case are (i, width)
(scatterplot :data squids :x :iseq :y "width")


(scatterplot :data (vw::select-data-subset squids '(> "wing" 1))
:x "weight" :y "width")


(lines-plot :data squids :x "weight" :y "width")


(fitted-line-plot :data squids :x "weight" :y "width" )

;; For a running median smooth:
(smooth-plot :data squids :x "weight" :y "width" )

;; To specify the smoothing parameter:
(smooth-plot :data squids :x "weight" :y "width" :smooth-par 3)

;; To change to a running average
(smooth-plot :data squids :x "weight" :y "width" :fit-fn (run-fn 'mean))

;; To dispense with the points use

(2d-plot :data squids :x "weight" :y "width" :interior-view 'smooth)

;; To compare smoothing parameters

(2d-plot :data squids :x "weight" :y "width"
 :interior-view `(2d-point-cloud
                   (:type smooth :smooth-par 3  :color ,wb:*yellow-color*)
                  (:type smooth :smooth-par 9  :color ,wb:*green-color*)))

;; or smoothing methods


(2d-plot :data squids :x "weight" :y "width"
 :interior-view `(2d-point-cloud
                   (:type smooth :smooth-par 3  :color ,wb:*yellow-color*)
                  (:type smooth :fit-fn ,(run-fn 'mean) :color ,wb:*green-color*)))

(2d-plot :data squids :x "weight" :y "width"
 :interior-view `((:type 2d-point-cloud  :color ,wb:*red-color* :size 5) 
                   (:type smooth :smooth-par 3  :color ,wb:*yellow-color*)
                  (:type smooth :smooth-par 9  :color ,wb:*green-color*)))

;; To use something other than a point symbol

(scatterplot :data squids :x "wing" :y "width" :case-view 'arrow)

(scatterplot :data squids :x "wing" :y "width" :case-view 'line-segment)

;; For line segments its more appropriate to specify the end points of the
;; line segment or segments



(line-segment-2d-plot :data squids
                      :x "wing"
                      :x-function #'(lambda(s) (list (- s .05) s (+ s .05)))
                      :y "rostral"
                      :y-function #'(lambda(s) (list (- s .02)  (+ s .02) (- s .02))))

;; or use multiple x variables

(line-segment-2d-plot :data squids
                      :x (list "wing" "width") :y (list "rostral" "weight"))

(line-segment-2d-plot :data squids
                      :case-view 'vw::connected-points
                      :x (list "wing" "width") :y (list "rostral" "weight"))

(line-segment-2d-plot :data squids
                      :x (list "wing" "width" "rostral-wing")
                      :y (list "rostral" "weight" "rostral-wing" "notch-wing"))


;;--------------------------------------------------


(rotating-plot :data squids :x "weight" :y "width"  :z  "rostral-wing")


(rotating-lines-plot :data squids :x '("weight" "rostral")
                     :case-view 'vw::connected-points
                     :y '("width" "rostral-wing")
                     :z  '("rostral-wing" "notch-wing"))

;;--------------------------------------------------

(scat-mat :data squids :vars (list "weight"  "width"  "rostral-wing") )


(scat-mat :data squids :vars (list-variates squids))


(1d-layout-plot :data squids :vars  '("rostral-wing" "notch-wing") :subview-type 'boxplot-view)
(1d-layout-plot :data squids :vars  '("rostral-wing" "notch-wing") :subview-type '1d-point-cloud)
(1d-layout-plot :data squids :vars  '("rostral-wing" "notch-wing") :subview-type 'histogram-view)




(xy-layout-plot :data squids :x-vars '("rostral-wing" "notch-wing")
                :y-vars '("weight" "width")
                :subview-type '2d-point-cloud)

(xy-layout-plot :data squids :x-vars '("rostral-wing" "notch-wing")
                :y-vars '("weight" "width")
                :subview-type '(2d-point-cloud smooth))


;;------------------------------------------------------------------------------------
(load "q:Data;ais.lsp")


(case-display-list :data bodymass :draw? t)

(variate-display-list :data bodymass :draw? t)

(setq b (batch-display-list 
     :data bodymass  :draw? t 
     :by "Sport"))

(batch-display-list 
     :data bodymass  :draw? t 
     :by (list "Sport" "Sex"))

(bar-plot   :data bodymass :by "Sport"  )  ;; one bar for each sport value
(bar-plot   :data bodymass :by "Wt"  )  ;; too many weight values!

;; use 4 weight levels, constructed by binning the weight range into 4 equal width bins
(bar-plot   :data bodymass :by "Wt" :level-function '(:equisize-bins 4  ))

;; Display the median of each batch on the bottom...
(bar-plot   :data bodymass :by "Wt" :level-function '(:equisize-bins 4 median ))


;; use 4 weight levels, constructed by binning the weight range into 4 equal count bins
;; Display the mean of each
(bar-plot   :data bodymass :by "Wt" :level-function '(:equicount-bins 4  mean))

(bar-plot   :data bodymass :by "Sport" :bar-height #'(lambda(d) (mean (values-of d "RCC" ))))



(defun sport-type(sport)
  (case sport
    (Netball 'Team)
    (T_Sprnt 'Athletics)
    (Gym 'Other)
    (Swim 'Water)
    (Row 'Water)
    (B_Ball 'Team)
    (T_400m 'Athletics)
    (Field 'Athletics)
    (W_Polo 'Water )
    (t 'Other)))


(bar-plot   :data bodymass :by "Sport"  )  
(bar-plot   :data bodymass :by "Sport"  :level-function #'sport-type)

(bar-plot   :data bodymass :by "Sport" :orientation :vertical )

(bar-plot   :data bodymass :by (list "Sport" "Sex") )
(bar-plot   :data bodymass :by (list "Sport" "Sex") :gap '(.5 .1 ) :bottom-label-size .12)




(setq b (table-plot   :data bodymass :by (list "Sport" "Sex") )) 

;; by default all entries are the same size.

;; to change this do ..

(rescale-entries (interior-view-of b) :justification :bottom :dimension :height)

(rescale-entries (interior-view-of b) :justification :left :dimension :width)

(rescale-entries (interior-view-of b) :justification :center :dimension :area)

;; changing cell labels
(setq b (table-plot   :data bodymass :by "Sport" ))

(set-cell-text   (interior-view-of b)  #'(lambda(d) (format nil "~4,2F" (if d 
                                                                          (sd (values-of d "SSF" )) 0))))

;; make bar length proportional to SSF

(rescale-entries (interior-view-of b) :justification :left :dimension :width
                 :scale #'(lambda(d) (if d (sd (values-of d "SSF" )) 0)))
;;--------------------------------------------------------------



(batch-plot :data bodymass :by "Sex" :subview-type '1d-point-cloud
          )



(batch-plot :data bodymass :by "Wt" :level-function '(:equicount-bins 3) 
            :subview-type '1d-point-cloud :var "Ferr"
            :left-label '("Low" "Mid" "High")
           )



(batch-plot :data bodymass :by (list "Sport" "Sex" ) :subview-type '1d-point-cloud
       :var "Ferr"   )

(batch-plot :data bodymass :by "Sex" :subview-type '1d-point-cloud
          :var "Ferr")



(batch-plot :data bodymass :by (list  "Sex"  "Wt") :level-function (list nil '(:equicount-bins 3))

            :subview-type '2d-point-cloud
          :x "Ferr" :y "BMI" :bottom-label '("Low" "Mid" "High") :left-label-size .06
          :top-label t)


(batch-plot :data bodymass :by "Sex" :subview-type '(:type histogram-view :nbins 10 )
          :var "Ferr" :left-label-size .1)
(batch-plot :data bodymass :by "Sex" :subview-type '2d-point-cloud
          :x "Ferr" :y "BMI")


(batch-plot :data bodymass :by "Sex" :subview-type '(2d-point-cloud smooth)
          :x "Ferr" :y "BMI")


(batch-plot :data bodymass :by "Sex" 
            :subview-type `(2d-point-cloud
                   (:type smooth :smooth-par 3  :color ,wb:*yellow-color*)
                  )
          :x "Ferr" :y "BMI")




;; this doesn't turn out right because there are different male and female sports
(batch-plot :data bodymass :by "Sex" :subview-type '(:type bar-chart :by "Sport")
        )





;; specify an ordering for Sport

(defmethod order-levels ((var string))
  (if (equal var "Sport")
    '(NETBALL T_SPRNT GYM SWIM ROW B_BALL T_400M FIELD
          TENNIS W_POLO)
    (call-next-method)
    ))

(batch-plot :data bodymass :by "Sex" :subview-type '(:type bar-chart :by "Sport"))




;; the interior to batch-plot is flexible


(batch-plot :data bodymass :by "Sex"  :subview-type 'rotating-plot 
             )



;; the following is not quite right: by default the batch-plot makes
;; the bounding regions of all panels the same:

(batch-plot :data bodymass :by "Sex" :subview-type 'pairs-layout
             :bottom-label nil)

;; need to make bounding regions the same in every row and column
(batch-plot :data bodymass :by "Sex" :subview-type 'pairs-layout
            :link-bounds-x? :by-col
            :link-bounds-y? :by-block-row
              :bottom-label nil)


(batch-plot :data bodymass :by "Sex" :subview-type 'pairs-layout
            :link-bounds-x? :by-col
            :link-bounds-y? :by-block-row
              :bottom-label nil)









(batch-plot :data bodymass :by "Sex"
            :subview-type  '(:type 1d-layout :subview-type boxplot-view :vars ( "RCC" "WCC" "Hc")
                        :link-bounds-x? nil) 
 :box-views? nil
 :link-bounds-x? :by-col)




