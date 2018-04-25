;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               selection                        
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


(load "q:Data;cigs.lsp")


(setq a (scatterplot :data cigs :x "tar" :y "nicotine"))

(setq b (scatterplot :data cigs :x "weight" :y "carbon-monoxide"))



;; link the scatterplots via, or 
;; link each in turn by invoking `link' from the Plots menu from the
;; menubar on the op of the screen.
(link-views a b)

(setq c (case-display-list :data cigs :draw? t))

;; to link c to the scatterplots..
(link-view c)

;; LEFT = SELECT
;; Left mouse on a view (try the point symbol or label) highlights it
;; and downlights everything else.
;; (if it happens that the view is already highlighted, then the
;; left mouse button downlights it).
;; Groups of point symbols can be highlighted  by dragging
;; out the rectangle provided with a left mouse on the point cloud.
;; Groups can also be highlighed via brushing




;; The colors of the highlighted point symbols can be changed via
;; the middle menu on the point cloud

(set-drawing-style (interior-view-of a) :color *red-color* :highlit? t)

;; and the shape via

(set-drawing-style (interior-view-of a) :symbol :circle :highlit? t)

;; or all at once 

(set-drawing-style (interior-view-of a) :color *red-color* 
                   :fill? t :symbol :circle :highlit? t)


;;; now make histograms and link those.

(col-layout :subviews (loop for v in (list-variates cigs)
                             collect (histogram  :data cigs :title " " :left-label nil :var v :draw? nil))
             :draw? t
             :box-views? nil
             :link? t
             )
;; MIDDLE = UNION
;;; Hitting Shift along with the left mouse buttom (shift-mouse on the Mac)
;;; does extended selection. Unlike the MAC UI this does a union (OR) rather than
;;; XOR.

;;; eg Left mouse on the right-most bar in the tar histogram
;;; highlights high tar cigarettes.
;;; Then shift-left on the right-most bar in the weight histogram
;;; highlights those who have high weight or tar.


;; SHIFT MIDDLE = INTERSECTION

;;; Hitting Shift along with the middle mouse buttom (shift-option-mouse on the Mac)
;;; does an intersection. 

;;; eg Highlight the left-most bar on the tar histogram
;;; Then shift-middle on the left-most bars in the other histogram
;;; This highlights those who have low weight, and low tar, and low carbon monoxide



;; SHIFT RIGHT = DIFFERENCE

;;; Hitting Shift along with the RIGHT mouse buttom (shift-command-mouse on the Mac)
;;; does a difference: ie removes the current selection from the previous selection.

;;; eg Highlight the right-most bar on the weight histogram.
;;; Then shift-right on the right-most bar on the nicotine histogram.
;;; This identifies 2 brands as having high weight but middling values on the
;;; other 3 variables.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Another example


(load "q:Data;ais.lsp")

;; The dataset has two categorical variables, "Sport" and Sex".
;; The following shows the factor levels..

(setq b (grid-layout :subviews (list (batch-display-list 
                                      :data bodymass 
                                      :by "Sport")
                                     (batch-display-list 
                                      :data bodymass  
                                      :by "Sex"))
                     :nrows 1 :draw? t
                     :box-views? nil))

;; Get rid of the old links with
(delete-link-table)


;; This starts off a fresh link table
(link-view b)

;; Select female-- and see that there are no female w_polo participants.
;; There are no male netball or gym athletes.

(setq h (histogram   :data bodymass :var "%Bfat" :function #'log))


(setq s (scatterplot :data bodymass :x "SSF" :y "BMI"))

(link-view h)
(link-view s)

;; Color the females red.

(set-drawing-style (interior-view-of h) :fill? t)

;; Select female-- then intersection-select T_400M.
;; Female 400m runners belong to low-fat categories.

;; Select those in the scatterplot with the highest 6 SSF values.
;; They are all Female, and Five of the six are netball players.

;; Select the 2.5-3.0 Bfat category- there are high Bfat values for men.
;; The labels show there are no sprinters in this category.
;; Intersect-select male label: 
;; Then netball and gym are downlit, because they are not played by
;; males. However, the highlit labels are NOT the sports played by high fat
;; males.
;; If the selection order is reversed:
;; Select males, and intersect-select the 2.5-3.0 Bfat category
;; the first action shows all sports played by males, and the second downlights
;; sports which are not played by males in the in the high fat category,
;; thus showing that the high fat males are 
;; all Rowers, B_ball, W_polo or field athletes.


;; Check this via the following:

(setq sports '(NETBALL GYM SWIM ROW B_BALL T_400M FIELD T_SPRNT W_POLO TENNIS))
(defun sport-num(s) 
  (position s sports))

(setq sport-labels (vw::descendant-views-of-type (car (subviews-of b)) 'label))


(setq s1 (scatterplot :data bodymass :y "Sport" :y-function #'sport-num :x "BMI"
                     :left-labels (reverse sport-labels)
                     :left-label-size .2
                     :left-view nil
                     :case-views-from s
                      ))

