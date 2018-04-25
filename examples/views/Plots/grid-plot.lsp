;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               grid-plot                              
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

(<- x (array (random-gaussian :n 40) :dimensions '(20 2)))
(<- (ref x (iseq 7) 0)
    (+ (ref x (iseq 7) 0) 2))
(<- y (random-gaussian :n 20))
(<- z (+ (* x x) (* .1 y)))
(<- w (append (make-list 7 :initial-element 'a) 
              (make-list  13 :initial-element 'b)))
(<- w2 (append (make-list 10 :initial-element 'c) 
              (make-list  10 :initial-element 'd)))

(<- d (cglue x z w w2))

;;;================================================================================
;;; Plotting multiple variables.
;;;--------------------------------------------------------------------------------

;;; First, displaying each variable separately:  1d-layout-plot

;;; Here you get prompted for the type of view to use for the variables:

(1d-layout-plot :data d :vars '(0 1 2))

;;; To supply the type of view for each variable, use
(1d-layout-plot :data d :vars '(0 1 2) :subview-type 'boxplot-view)
(1d-layout-plot :data d :vars '(0 1 2) :subview-type '1d-point-cloud)
(1d-layout-plot :data d :vars '(0 1 2) :subview-type 'histogram-view)


(1d-layout-plot :data d :vars '(0 1 2) :subview-type 'boxplot-view
 :orientation :horizontal
 :box-views? nil)

;;; The parameter :box-views? nil gets rid of the boxes, and the parameters
;;; :gap-x and :gap-y specify the gap sizes in the x and y directions
;;; gap-x of .5 means the gaps between panels in the horizontal direction is half
;;; the panel width.
(1d-layout-plot :data d :vars '(0 1 2)
 :gap-x .5
 :subview-type `(:type 1d-point-cloud :color ,*red-color*))

;;; cols specifies the column positions: here the first col extends from
;;; .2 to .4, the second from .5 to .6 and the third from .8 to .9
;;; and the entire region allocated to the columns extends from .2 to .9.

(1d-layout-plot :data d :vars '(0 1 2)
 :cols '(.2 .4 .5 .6 .8 .9)
 :subview-type `(:type 1d-point-cloud :color ,*red-color*))

;;; Other parameters can be specified for the subview type:

(1d-layout-plot :data d :vars '(0 1 2) 
 :subview-type `(:type 1d-point-cloud :color ,*red-color*
                       :case-view (:type oriented-line )))

;;; Arguments to subview-type can be supplied like those of :interior-view to plots,
;;; See (edit-file "q:Examples;Views;scatterplot.lsp") for examples of this.

;;; Notice that the case-views are linked by default across the
;;; panels. This is because the same case-view objects are used in 
;;; each of the panels. Specifying :common-case-views? with a nil value
;;; removes this feature.

(1d-layout-plot :data d :vars '(0 1 2) 
 :common-case-views? nil
 :subview-type `(:type 1d-point-cloud :color ,*red-color*
                       :case-view (:type oriented-line )))

;;; If you want to use a different display type for the panels
;;; use the :subviews keyword parameter: (there should be as many
;;; elements in the subviews list as there are variables.)

(1d-layout-plot :data d :vars '(0 1 2) 
 :subviews `(1d-point-cloud (:type 1d-point-cloud :color ,*red-color*)
             boxplot-view))


;;; The :subviews parameter can be used in conjunction with :subview-type:
;;;; then the values in subview-type are given precendence over those in 
;;; subviews. 
(1d-layout-plot :data d :vars '(0 1 2) 
 :subview-type `1d-point-cloud
 :subviews `(( :color ,*red-color*) ( :color ,*blue-color*)
             ( :color ,*green-color*)))

;;; 1d-layout-plot constructs a plot which has a 1d-layout as its interior-view.
;;; A 1d-layout can be constructed without its plot wrapper. The convention is
;;; that only plots are automatically drawn: to draw the 1d-layout supply the
;;; :draw? parameter with a value of t.

(1d-layout :data d :vars '(0 1 2) :subview-type 'histogram-view
 :draw? t)

;;;--------------------------------------------------------------------------------
;; Second, displaying pairs of variables...xy-layout-plot, scat-mat
;; The following shows the y variables plotted agains the x variables.

(xy-layout-plot :data d :x-vars '(0 1)
                :y-vars '(2))
;; To supply the type or types  of view for each variable pair, use
(xy-layout-plot :data d :x-vars '(0 1)
                :y-vars '(2)
                :subview-type '2d-point-cloud)
;;; Notice you can select multiple types of view from the menu for overlays.

;;; Other parameters can be specified for the subview type:

(xy-layout-plot :data d :x-vars '(0 1)
                :y-vars '(2)
                :subview-type `(:type 2d-point-cloud :color ,*red-color*
                       :symbol :circle :size 6 :fill? t))

;;; Subview types can be layered

(xy-layout-plot :data d :x-vars '(0 1)
                :y-vars '(2)
                :subview-type `(2d-point-cloud lines))

;;; or

(xy-layout-plot :data d :x-vars '(0 1)
                :y-vars '(2)
                :subview-type `(2d-point-cloud smooth))

;;; To specify extra parameters for either or both of the layers use


(xy-layout-plot :data d :x-vars '(0 1)
                :y-vars '(2)
                :subview-type `((:type 2d-point-cloud :color ,*red-color*
                                       :symbol :circle :size 6 :fill? t) 
                                smooth))
(xy-layout-plot :data d :x-vars '(0 1)
                :y-vars '(2)
                :subview-type `((:type 2d-point-cloud :color ,*red-color*
                                       :symbol :circle :size 6 :fill? t) 
                                (:type smooth  :color ,*blue-color*)))

;;; Notice that the case-views are linked by default across the
;;; panels. This is because the same case-view objects are used in 
;;; each of the panels. Specifying :common-case-views? with a nil value
;;; removes this feature.


;;; If you want to use a different parameters for the panels
;;; use the :subviews keyword parameter: (there should be as many
;;; elements in the subviews list as there are pairs of x and y variables.)

(xy-layout-plot :data d :x-vars '(0 1)
                :y-vars '(2 2)
                :subviews '((2d-point-cloud smooth) (2d-point-cloud smooth)
                (2d-point-cloud fitted-line) (2d-point-cloud fitted-line)))

(xy-layout-plot :data d :x-vars '(0 1)
                :y-vars '(2 2)
                :subview-type '(2d-point-cloud smooth)
                :subviews '((:smooth-par 3) (:smooth-par 3) (:smooth-par 7) (:smooth-par 7)))


;;; xy-layout-plot constructs a plot which has an xy-layout as its interior-view.
;;; An xy-layout can be constructed without its plot wrapper. The convention is
;;; that only plots are automatically drawn: to draw the xy-layout supply the
;;; :draw? parameter with a value of t.


(xy-layout :data d :x-vars '(0 1)
                :y-vars '(2)
                :subview-type '2d-point-cloud
                :draw? t)


;;; A scatterplot matrix is similar to an xy-plot and is obtained with:

(scat-mat)

;;; or to specify the data arguments
(scat-mat :data d :vars '(0 1 2))

;;; and the  display type 

(scat-mat :data d :vars '(0 1 2) :pairs-view '(2d-point-cloud  smooth))

;;; 2d-point-cloud is the default pairs-view.
;;; Other parameters can be specified for the point-cloud and smooth:

(scat-mat :data d :vars '(0 1 2) :pairs-view `((:type 2d-point-cloud :color ,*green-color*)
                                               (:type smooth :color ,*yellow-color*)))


;;; The view used along the diagonal is specified using the :diag-view parameter:

(scat-mat :data d :vars '(0 1 2) :diag-view 'histogram-view)
(scat-mat :data d :vars '(0 1 2) :diag-view 'boxplot-view )

;;; The default orientation is :vertical for the boxplot-view and
;;; :horizontal for the histogram, the orientation can be specified with



(scat-mat :data d :vars '(0 1 2) 
          :diag-view '(:type boxplot-view :orientation :horizontal))


;;; Notice that the case-views are linked by default across the
;;; off-diagonal panels. This is because the same case-view objects are used in 
;;; each of the off-diagonal panels. A :link? argument of t will link all
;;; linkable views in the scat-mat to each other.

(scat-mat :data d :vars '(0 1 2) :diag-view 'boxplot-view :link? t)

;;; By default, the x bounds in every column and the y bounds in every row are the same:
;;; that is :link-bounds-x? is :by-col and  :link-bounds-y? is :by-row
;;;To use the same x-bounds in all views, and the same y-bounds in all views:
       
(scat-mat :data d :vars '(0 1 2) ::link-bounds-x? t :link-bounds-y? t)


;;;================================================================================
;;; Plotting groups of cases
;;;--------------------------------------------------------------------------------

;;; Here you get prompted for the dataset, grouping variable, 
;;; and the display type (and plotting variables as necessary) to use:

(batch-plot)

;;; Here you get prompted for the the display type only:
(batch-plot :data d :by 4 :var 0)

(batch-plot :data d :by 4 :x 0 :y 1)

(batch-plot :data d :by 4 :x 0 :y 1 :z 2)



;;; Here :format :col is unecessary because it is the default.
;;; Notice that :format :col has the meaning "position the views in
;;; a column", rather than the opposite "position the views in columns".
(batch-plot :data d :subview-type 'boxplot-view
                    :by 4 :format :col
                    :var 0)

(batch-plot :data d :subview-type 'boxplot-view
                    :by 4 :format :row
                    :var 0)

;;;Supplying extra parameters to subview-type..
(batch-plot :data d :subview-type `(:type boxplot-view :color ,*red-color* :symbol :star)
                    :by 4  :var 0
                    )




;;;Layering views in panels
(batch-plot :data d :subview-type '(2d-point-cloud smooth)
                    :by 4 :x 0 :y 1)

;;;Supplying extra parameters to the layers
(batch-plot :data d :by 4 :x 0 :y 1
                :subview-type `((:type 2d-point-cloud :color ,*red-color*
                                       :symbol :circle :size 6 :fill? t) 
                                (:type smooth  :color ,*blue-color*)))



;;; If you want to use a different parameters for the panels
;;; use the :subviews keyword parameter: (there should be as many
;;; elements in the subviews list as there are batches)

(batch-plot :data d :by 4 :x 0 :y 1
                :subviews `((:color ,*red-color* :smooth-par 2) 
                            (:color ,*blue-color* :smooth-par 4))
                :subview-type '(2d-point-cloud smooth))


(batch-plot :data d :by 4 :x 0 :y 1
                :subviews `((2d-point-cloud smooth)
                            (2d-point-cloud fitted-line)))


(batch-plot :data d :by 4 :x 0 :y 1
                :subviews `((2d-point-cloud (:type smooth :color ,*red-color*))
                            (2d-point-cloud (:type fitted-line :color ,*blue-color*))))

;;; There can be multiple by variables...

(batch-plot :data d :subview-type '2d-point-cloud
                    :by '(4 5) :x 0 :y 1)

;;; and the subview-type is arbitary
(batch-plot :data d :subview-type 'label
                    :by '(4 5) :x 0 :y 1)

(batch-plot :data d 
                    :by 4 :format :col
                    :x 0 :y 1 :z 2 
                    :subview-type 'rotating-plot)

;;; This makes a pairs layout for each of the levels of var 4.
;;; :link-bounds-x? :by-col makes the x-bounds fixed for a column,
;;; :link-bounds-y? :by-block-row makes the y-bounds fixed for row i of
;;; both blocks.
(batch-plot :data d :by 4 :subview-type 'pairs-layout
            :vars '(0 1 2)
            :link-bounds-x? :by-col
            :link-bounds-y? :by-block-row
              :bottom-label nil)

;;; If you want to keep separate bounding regions for the blocks
;;; you will need to put separate axes for the two groups.

(batch-plot :data d :by 4 :subview-type 'scat-mat
            :vars '(0 1 2) :box-views? nil
            :link-bounds-x? nil
            :link-bounds-y? nil
              :bottom-label nil :left-view nil :bottom-view nil
              )
;;; See (edit-file "q:Examples;Views;general.lsp") for other examples.

;;;--------GRID-PLOT ---------------------------------------------

;;; The above plots are all grid plots. Each of the above plots
;;; could be produced by using the grid-plot function directly.
;;; Here are some examples:

;;;--------------------------------------------------------------------------------


(grid-plot :subviews (list  (boxplot-view :data d :var 0 :color *red-color*)
                            (boxplot-view :data d :var 1 :color *blue-color*)
                            (boxplot-view :data d :var 2 :color *green-color*))
           :format :row
           :left-view t
           :bottom-label t)



(grid-plot :subviews (list  (list (2d-point-cloud :data d :x 0 :y 2)
                                  (smooth :data d :x 0 :y 2))
                            (list (2d-point-cloud :data d :x 1 :y 2)
                                  (smooth :data d :x 1 :y 2)))
           :format :row
           :left-view t :bottom-view t
           :bottom-label t :left-label t)

;;; By default grid-plot has no margin views and margin labels. Hence we specify
;;; :left-view t etc.
;;; By default grid-plots uses the same bounds for all the panels.
;;; If we want to override this, say by using the same horizontal bounds for
;;; panels in a column and by using the same vertical bounds for
;;; panels in a row use

(grid-plot :subviews (list  (list (2d-point-cloud :data d :x 0 :y 2)
                                  (smooth :data d :x 0 :y 2))
                            (list (2d-point-cloud :data d :x 1 :y 2)
                                  (smooth :data d :x 1 :y 2)))
           :format :row
           :left-view t :bottom-view t
           :bottom-label t :left-label t
           :link-bounds-x? :by-col
           :link-bounds-y? :by-row)


;;; When the subviews all use the same type, one can provide a :subview-type just once
;;; Similarly the :data can be specified once.
(grid-plot   :data d
           :subview-type 'boxplot-view
           :subviews `((:var 0 :color ,*red-color*) 
                       (:var 1 :color ,*blue-color*)
                       (:var 2 :color ,*green-color*))
         
           :format :row
           :left-view t
           :bottom-label t)

(grid-plot :data d
           :subview-type '(2d-point-cloud  smooth)
         
           :subviews '((:x 0 :y 2)
                       (:x 1 :y 2))
           :format :row
           :left-view t :bottom-view t
           :bottom-label t :left-label t
           :link-bounds-x? :by-col
           :link-bounds-y? :by-row)

;;; 1d-layout and xy-layout are typically used for plotting different
;;; variables with the same cases. Therefore :common-case-views? for
;;; 1d-layout and xy-layout has the default-value of t. However grid-layouts
;;; just position views in a grid and make no assumptions about the type of
;;; data. Therefore with grid-layouts one should specify :common-case-views? to be
;;; t if required. Of couse, this option will have no effect if the
;;; panel views are constructed prior to constructing the grid-layout.


;;; grid-plot constructs a plot which has an grid-layout as its interior-view.
;;; A grid-layout can be constructed without its plot wrapper. The convention is
;;; that only plots are automatically drawn: to draw the grid-layout supply the
;;; :draw? parameter with a value of t.


;;; Useful parameters for any grid-layout or grid-plot are
;;; gap-x gap-y cols rows link-bounds-x? link-bounds-y?
;;; See examples above.

