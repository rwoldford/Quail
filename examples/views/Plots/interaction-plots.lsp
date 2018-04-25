;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               interaction-plots                       
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     C.B. Hurley 1997
;;;
;;;

(in-package :quail-user)



(<- bact (array '(39.77 40.23 
                   39.19 38.95 
                   40.37 41.71 
                   40.21 40.78)
           :dimensions '(2 2 2)))


(<- bill (mway-dataset bact :factors (list "Temp" "Bact" "Run"  )
                             :variates (list "length")
                             :factor-levels '((cold warm)
                                              (ctrl myco)
                                              (run-1 run-2))
                    :name "Bacteria"))

(setq s (scatterplot :data bill
             :x "Temp"
             :x-function #'(lambda(x) (position x '(cold warm)))
             :y "length"))

(loop for d in (funcall (data-subsets-fn  (list "Run" "Bact")) bill) do
      (layer-view (interior-view-of s)  
                  'simple-lines :data d))

;; 

;; or
(setq b (grid-layout :subviews (list (batch-display-list 
                                      :data bill
                                      :by "Temp")
                                     (batch-display-list 
                                     :data bill
                                      :by "Bact")
                                     (batch-display-list 
                                      :data bill
                                     :by "Run"))
                     :nrows 1 :draw? t
                     :box-views? nil))

;; Select labels. Move the mouse cursor to the scatterplot
;; and paste (right menu) a line-segment (or simple-lines) on the pointcloud.
;; Simple-lines has the advantage over the line segment in that transforming
;; the pointcloud coordinates via sqrt will also transform the lines coordinates.


(defun treat(c)
  (let ((vals (values-of c (list "Bact" "Temp")))) 
    (cond ((equal vals '(ctrl cold)) 1)
           ((equal vals '(myco cold))
                2)
           ((equal vals '(ctrl warm))
                3)
           ((equal vals '(myco warm))
                4)
           (t nil))))
(setq s1 (scatterplot :data bill
             :x #'treat
             :y "length"))

(loop for d in (funcall (data-subsets-fn "Run") bill) do
      (layer-view (interior-view-of s1)  
                  'simple-lines :data d))



