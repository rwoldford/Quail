;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        Viewports and view windows                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1995 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     R.W. Oldford 1995.
;;;
;;;
;;;
;;;  Every view is drawn in a rectangular region of a view-window
;;;  called a viewport.  Every view can appear in more than one viewport
;;;  and consequently in more than one view-window.
;;;
;;;  In what follows, we illustrate the construction
;;;  and use of viewports and views.
;;;  
;;;  The basic idea is that when drawn, a view is displayed in a rectangular
;;;  region within a window.
;;;  The window is called view-window and the rectangular region is called a
;;;  viewport.
;;;
;;;  To be concrete, let's first make a view-window,  say win.
;;;  The constructor is called make-view-window and can be called with no arguments
;;;  -- the result would be a window in default position, of default colour, etc.
;;;  Here we make win in a particular position in screen coordinates
;;;  (in Quail all coordinate systems have the origin (0,0) in the lower left
;;;  and increasing in magnitude from left to right and bottom to top).
;;;  Note that the unit is a single pixel.

(setf win (make-view-window :left 50 :right 450 :bottom 100 :top 300
                            :background-color wb::*gray-colour*))

;;; to draw a view in this window, we first need a viewport.
;;; This is constructed with
;;;     (make-viewport w xmin xmax ymin ymax) 
;;;
;;; where w is the target window and the remaining arguments specify the
;;; rectangular coordinates of the viewport expressed in the local coordinates
;;; of the window.  All arguments are optional.

(setf vp (make-viewport win 50 150 10 60))

;;;  To see it 

(draw-viewport vp)

;;;  or in colour (note US spellings of keywords are used).

(draw-viewport vp :color wb:*blue-color*)

;;;  Usually however, we have little interest in the viewports except as
;;;  places where views can be drawn.
;;;  So here's a simple view -- a label

(setf lab (label :viewed-object "hi there"))

;;;  which can now be drawn in the specified viewport using draw-view

(draw-view lab :viewport vp)

;;;  The mouse can now be placed within the region of the viewport and so
;;;  used to interact with the label in the usual way.
;;;  The viewport is the rectangular region that is highlighted when the
;;;  label is selected with the mouse.  Note that reshaping the view-window will
;;;  reshape the viewport as well.  The size and location of the viewport are
;;;  defined relative to the size of the view-window at the time of the viewport's
;;;  creation.
;;;
;;;  The same view can be drawn in many different viewports (in the same or in
;;;  different view-windows).  For example,

(setf new-vp (make-viewport win 160 250 50 75))
(draw-view lab :viewport new-vp)

;;;  Note that this is the SAME view -- it's just drawn in two different 
;;;  viewports -- and so any changes to the view will be displayed in both places.
;;;  (Selection for example will cause both viewports to be highlighted.)
;;;
;;;  Many functions take a viewport as an argument as in 

(erase-view lab :viewport new-vp)

;;; and 
(draw-view lab  :viewport new-vp)

;;;  or even
(loop for i from 1 to 10 do
      (invert-view  lab :viewport vp)
       (sleep .1)
      (invert-view  lab :viewport vp)
      (invert-view  lab :viewport new-vp)
      (sleep .1)
      (invert-view  lab :viewport new-vp)
      )
      

;;;  If not given, then a reasonable default is to apply the procedure to
;;;  all viewports in which the view appears, as in

(erase-view lab)

;;; Of course the viewports are still there as can be seen by redrawing the view.

(draw-view lab)


;;; The set of viewports in which a view is currently displayable is returned
;;; as a list from the view itself by

(viewports-of lab)

;;; Or the (viewport . view) pairs from a view-window with

(viewports-and-views-of win)

;;;  An example of the use of the latter might be

(loop for vp-v in (viewports-and-views-of win)
      do
      (highlight-view (cdr vp-v) :viewport (car vp-v))
      (sleep .1)
      (downlight-view (cdr vp-v) :viewport (car vp-v)))

;;; or again (to illustrate the loop macro)
(loop for (vp . v) in (viewports-and-views-of win)
      do
      (highlight-view v :viewport vp)
      (sleep .1)
      (downlight-view v :viewport vp))

