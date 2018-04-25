;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               scatterplots                              
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
;;; Some data
;;;
;;; First some fake data.

(<- x (random-gaussian :n 10))
(<- y (random-gaussian :n 10))
(<- z (+ (* x x) (* .1 y)))
(<- d (cglue x y z))


;;; scatterplot takes either a dataset, d, and specified variates x and y
;;; or the x and y values themselves.

;;; X and Y given directly to scatterplot

(scatterplot :x x :y y)

;;;
;;; Title and labels can be given directly to scatterplot.

(scatterplot :x x :y z
             :title "Example"
             :bottom-label "The value x"
             :left-label "Response y")


;;; The dataset d is given; X is the 0'th variate (or column) of d
;;; and Y is the 1'th.

(scatterplot :data d :x 0 :y 1)

;;; If X and Y are not specified, the user is prompted to choose variates
;;; from the dataset d.

(scatterplot :data d)

;;; If no data is specified at all then the user will be prompted
;;; for the dataset d.

(scatterplot)

;;;
;;; Selection functions can be given in place of X and Y.
;;; These are called on each case of the dataset to produce
;;; the x and y values for that case.
;;;
;;; First define a couple of functions.

(defun select-x (a)
  "Selects x from the case a. It must return a number."
  (+ (eref a 0) (eref a 2)))

(defun select-y (a)
  "Selects y from the case a. It must return a number."
  (- (eref a 0) (eref a 1)))

(scatterplot :data d :x #'select-x :y #'select-y)

;;;
;;; or
;;;

(scatterplot :data d :x #'mean :y #'sd)

;;;
;;; Functions can also be given to scatterplot.
;;; These are called on the corresponding x and y values before plotting.
;;;
;;; First define a couple of functions.

(defun square (a)
  "Returns the square of a."
  (* a a))

(defun cube (a)
  "Returns the cube of a."
  (* a a a))

(scatterplot :data d :x 0 :y 1
             :x-function #'square
             :y-function #'cube)


;;;
;;; And the x and/or y coordinates can be flipped.

(scatterplot :x x :y z
             :title "Flipping"
             :flip-x? T 
             :flip-y? T)
;;;
;;; Functions which can also be given to scatterplot.
;;; These are called on the corresponding x and y values before plotting.
;;;
;;; First define a couple of functions.

(defun copy-permute (a)
  "Randomly permute the elements of a copy of a."
  (permute a :copy? T))

(scatterplot :data d
             :x 0 :y 2
             :y-transform #'copy-permute)

;;; By default a scatterplot has left and bottom labels and
;;; left and bottom margin views which are axes.
;;; The following adds  top and right labels and suppresses
;;; the left-label.
             
(scatterplot :x x :y z
             :title "Example"
             :bottom-label "The value x"
             :top-label "A top label"
             :right-label "Response y"
             :left-label nil)

;;; The following adds top and right axes and suppresses
;;; the left and bottom axes.
             
(scatterplot :x x :y z
             :title "A funny plot"
             :left-label nil :bottom-label nil
             :left-view nil :bottom-view nil
             :right-view t
             :top-view t)


;;;
;;; Because a scatterplot is just a kind of plot its subviews
;;; (title left-label left-view top-label top-view
;;;        right-label right-view bottom-label bottom-view
;;;  and   interior-view)
;;; can be given other kinds of views at instantiation.
;;;
;;; This is done by specifying the selected keyword argument (:title etc.)
;;; in one of two ways:
;;;     either a view is created directly and given as the value
;;;     or a specification of the view is given which scatterplot then
;;;     interprets and constructs the desired view.
;;;
;;; The latter approach is usually the more desirable and will be illustrated
;;; first.


;;; The following choose different kinds of views for the bottom-view
;;; left-view, top-view, and right-view. Here the specification passed
;;; is a the name of a function that will create the view or the name of
;;; the class of the view.

(scatterplot :x x :y z
             :left-view 'boxplot-view :bottom-view 'histogram-view
             )
(scatterplot :x x :y z
             :left-view 'axis :bottom-view 'fringe-view
             )

(scatterplot :x x :y z
             :right-view 'fringe-view :top-view 'fringe-view
             )

(scatterplot :x x :y z
             :right-view 'histogram-view :top-view 'histogram-view
             )

(scatterplot :x x :y z
             :interior-view 'lines
             )

(scatterplot :x x :y z
             :interior-view 'simple-lines
             )

;;;
;;;  If we want to pass more arguments to the functions
;;;  we pass these in a list.  Note that the elements of this list must
;;;  be evaluated before being passed on.
;;;  First a simple one-- equivalent to the last above.



(scatterplot :x x :y z
             :interior-view '(:type lines :simple? T)
             )

;;;Another simple example..

(scatterplot :x x :y z
             :right-view '(:type histogram-view :fill? T)
             :top-view 'histogram-view
             )

;;;
;;; or equivalently

(scatterplot :x x :y z
             :right-view (list :type 'histogram-view :fill? T)
             :top-view 'histogram-view
             )

;;;
;;; Or choosing a color where it is important to have the color evaluated.

(scatterplot :x x :y z
             :right-view (list :type 'histogram-view :fill? T :color wb:*green-color*)
             :top-view 'histogram-view
             )

;;;
;;; Note that missing from the above examples was any mention of the data
;;; arguments :data, :x and :y. That is because scatterplot has that information
;;; and can pass it on to its subviews.

;;;
;;; Sometimes we may want to have more than one view in a single position
;;; in the plot.  Then a list of legal specifications is given.

(scatterplot :x x :y z
             :interior-view '(lines 2d-point-cloud)
             :bottom-view '((:type fringe-view
                                   :size .1)
                            (:type axis
                                   :justification :bottom))
             )


;;;
;;; which overlays a lines view and a 2d-point-cloud for the interior-view.
;;;
;;; Because 2d-point-cloud is the default interior-view of scatterplot,
;;; rather than replace it by the pair of overlayed views it is
;;; more natural to overlay the lines view directly on the default interior-view.
;;; This is achieved as follows:

(scatterplot :x x :y z
             :interior-view '(lines :default)
             :bottom-view '(axis histogram-view)
             )

(scatterplot :data d :x 0 :y 2
             :interior-view (lines :data d :x 0 :y 2)
             )

;;;
;;;  Or you can be prompted for the data.
;;;

(scatterplot :x x :y z
             :interior-view (lines)
             )



(scatterplot :x x :y z
             :interior-view '(2d-point-cloud lines)
             )


;;; If you want to have lines from each point symbol extending to the axis
;;; use


(scatterplot :x x :y z
             :interior-view '(:type 2d-point-cloud :lines-to :left)
             )

;;; The :lines-to argument can also be :left-right :top-bottom :left :right :top :bottom :x :y,
;;; and nil (the default)
;;; Use :ecolor :edashing :ewidth to specify color, dashing (cons dash gap) and line width, as in


(scatterplot :x x :y z
             :interior-view `(:type 2d-point-cloud :lines-to :top-bottom :ecolor ,wb:*red-color*
                              :ewidth 2 :edashing (4 . 4))
             )

;;; If you want to specify a drawing style for each edge line, use a 
;;; 2d-point-cloud where the case-view is a justified-line...

(scatterplot :x x :y z
             :interior-view `(:type 2d-point-cloud 
                                    :case-view (:type justified-line :justification :left))
             )

;;; To overlay this with a regular point-cloud...

(scatterplot :x x :y z
             :interior-view `(:default (:type 2d-point-cloud 
                                    :case-view (:type justified-line :justification :left))
             ))

(scatterplot :x x :y z
             :interior-view
             '(2d-point-cloud
               (:type smooth :smooth-par 4)   ;; running-median of 4
               )
             )


;;; The above kind of view is so useful that there is a built-in
;;; function for it: (Note, it suffices to give the the smooth-par 
;;; argument to smooth-plot because arguments to the plot are
;;; automaically passed to the interior-view)

(smooth-plot :x x :y z :smooth-par 4)

;;; There are analgous functions lines-plot, simple-lines-plot
;;; and fitted-line-plot.


;;;
;;; One can also pass on an instance of a view directly. Of course, then some
;;; care should be taken that the same data is used.
;;;

(scatterplot :data d :x 0 :y 2
             :right-view
             (case-display-list :data d ))
;;; equivalent to:

(scatterplot :data d :x 0 :y 2
             :right-view
             'case-display-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Programmatic use of scatterplots.
;;;
;;;  Scatterplots also produce instances that can be manipulated
;;;  programmatically. All of these things can be done via mouse interaction,
;;;  nevertheless, ...

(<- s (scatterplot :data d :x 0 :y 2
             :right-view 'fringe-view :top-view 'fringe-view
             :title "Example"
             :bottom-label "The value x"
             :left-label "Response y"))

;;;
;;; s and all of its subviews are added to the set of linked views
;;;

(link-view s)

;;; And the reverse.

(unlink-view s)

;;;  So now you might change the x axis limits
;;;

(set-extent (bottom-view-of s)  -4  4)
(set-ntics (bottom-view-of s)  3)           
(set-tics (bottom-view-of s)  '(-4 0 5/2 4))
(set-tic-format (bottom-view-of s)  "~D")
(set-tics (bottom-view-of s)  '(-1 1))
(set-tics (bottom-view-of s)  '((-1 "-one") (1 "one")))


(set-extent (bottom-view-of s)  -4  4)

;;;
;;;  Or change the variables plotted.
;;;

(change-variable s :x-function 'square)
(change-variable s :x-function 'identity)
(change-variable s :y-function 'square)
(change-variable s :y-function 'identity)

;;;
;;; And because the dataset viewed by the scatterplot has more than one
;;; variable

(change-variable s :y 1)
(change-variable s :x 2)

;;;
;;; or equivalently


(change-variable (interior-view-of s) :x 0)
(change-variable (interior-view-of s) :y 2)
(change-variable (bottom-view-of s) :var 1)
(change-variable (bottom-view-of s) :function 'square)
(change-variable (bottom-view-of s) :var 0 :function 'identity)

;;;
;;;  The region can be given new bounds.
;;;  If pretty? is non-NIL, then these bounds are used in conjunction
;;;  with the data to choose a new region to display the data.



(new-bounds (interior-view-of s)
            :region (make-region -5 3 -1 4)
            :pretty? T)

(new-bounds (interior-view-of s)
            :region :original
            :pretty? T)

;;;  The following will force the extent.
;;; 
(set-extent (bottom-view-of s) :min -1 :max 3)
(set-extent (left-view-of s) :min 0 :max 4)

;;;
;;;  You can also layer some views on s or its subviews etc.
;;;  Here are some built-in functions.

(add-lines s)
(remove-subview s (first (subviews-of-type s 'lines))) ;; more easily and naturally
                                                        ;; done using the mouse.

(add-fitted-line s)
(remove-subview s (first (subviews-of-type s 'fitted-line)))

;;;
;;; We also have add-simple-lines add-smoothed-lines add-smoothed-simple-lines
;;; add-line add-fitted-line
;;; 

;;;
;;; Cases can also be temporarily removed from the plot's consideration 
;;; by deactivating them.
;;; 
;;; For example, first go to the plot, s, and select some cases with the mouse.
;;;

  
(deactivate-cases s :selected)

;;; will cause them to disappear.  Moreover they and their values are
;;; no longer involved in any display computations.
;;; And reactivated using

(activate-all-cases s :rescale? NIL)

;;;
;;; This is more interesting when there is a fitted line on the plot, say,
;;; and if we don't allow the plot to be rescaled each time.

(add-fitted-line s)
(deactivate-cases s :selected :rescale? NIL)
(activate-all-cases s :rescale? NIL)

;;; This feature is better illustrated on some real data.
;;; Here chemical measurements on some brands of US cigarettes.

(load "q:Data;cigs.lsp")
(<- cig-plot (scatterplot :data cigs :x "tar" :y "carbon-monoxide"
             :right-view 'fringe-view :top-view 'fringe-view))
(link-view cig-plot)
(add-fitted-line cig-plot)

;;; Named cases can be deactivated.

(deactivate-cases cig-plot (list "Bull Durham" "Old Gold")
                  :rescale? NIL)
(activate-all-cases cig-plot)

;;;
;;; case identifiers can be seen using a case-display-list

(<- cd (case-display-list :data cigs :draw? t))
(link-view cd)

;;;
;;;  We'll examine the effect of the brand "Bull Durham" on the fit
;;;  while keeping the original fitted line on the plot.

(setq f (add-fitted-line cig-plot :color wb:*green-color*))
(deactivate-cases f (list "Bull Durham") :link? nil)


;;;
;;; 
;;;
;;; And nearly any view can be layered on any other.
;;;

(layer-view (interior-view-of s)
            (lines :x x
                   :y (* x x)))
(remove-subview s (first (subviews-of-type s 'lines)))


(layer-view (interior-view-of s)
            (function-view :function 'square ))

;;;
;;; As was the case with the 
(layer-view (interior-view-of s)
            'function-view :function 'square)

(reposition-view s :title-width 0.5 :title-height 0.07)


;;;
;;; Operations on labels:
;;; The title of a plot  is is a label and is accessed by (title-of s),
;;; similarly the left label as (left-label-of s) and the
;;; bottom label as (bottom-label-of s).
;;; See (edit-file "q:Examples;Views;labels.lsp") for examples of operating on labels

(remove-subview s (title-of s))

;;;
;;;  And of course you don't have to draw the plot immediately.
;;;

(<- snd (scatterplot :x x :y z
                     :title "Example"
                     :bottom-label "The value x"
                     :left-label "Response y"
                     :draw? NIL))
;;;
;;; A place to draw it
;;;

(<- w (make-viewport))
(draw-view snd :viewport w)

;;;
;;; Another place to draw it
;;;

(draw-view snd :viewport (make-viewport))

;;;
;;;
;;;  If we built a plot through the menu operations (or programmatically)
;;;  and want to create a function that will reproduce that plot on new data,
;;;  all we need do is clone it!

(<- new-plot
    (scatterplot :x x :y z
                 :interior-view
                 '(2d-point-cloud
                   (:type smooth :smooth-par 4)   ;; running-median of 4
                   )
                 :right-view 'fringe-view
                 :top-view 'fringe-view
                 :title "Running median smooth."
                 
                 ))

(clone-view-fn new-plot 'neat-plot)

(<- dd (array (random-gaussian :n 100) :dimensions '(50 2)))

(neat-plot :data dd :x 0 :y 1)

(neat-plot :data dd :x 0 :y 1 
             :title "Another title"
             :bottom-view 'histogram-view)
