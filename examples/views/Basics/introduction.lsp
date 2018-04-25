;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               introduction                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1995 Statistical Computing Laboratory, University Of Waterloo
;;;
;;; 
;;;  Authors:
;;;     C.B. Hurley 1995.
;;;     R.W. Oldford 1995.
;;;
;;;
;;;
;;; Make a view. It is not drawn automatically.
;;;

(setq v (view))

;;; To draw the view...

;;; Since this is just a vanilla view, all you get is an empty viewport.

(draw-view v)


;;; The basic view can be highighted or not, invisible or not.
;;; These properties of a view are called its drawing style.
;;; Some kinds of views have more detailed drawing styles,
;;; but highlight and invisible styles are common to all views.

;;; To examine its drawing style do

(drawing-style-of v)

;;; To highlight it do

(set-drawing-style v :highlight? t)

;;;------------------------------------------------------------------------
;;; Different kinds of views
;;; See (edit-file "q:Examples;Views;Basics;classes.lsp") for 
;;; details on different kinds of views.
;;;------------------------------------------------------------------------

;;; To make this overview more interesting, let's construct
;;; and draw a view which produces something in the viewport.
;;; Views are divided into simple views and compound views.
;;; Compound views are composed of other views, which are themselves
;;; compound or simple views.
;;; Simple views on the other hand have no subviews, but usually will
;;; produce some figure when drawn on the screen.
;;; One can make a vanilla simple-view or compound view,

(view :type 'simple-view)

(view :type 'compound-view)

;;; but just like the vanilla view this doeesn't produce
;;; anything when drawn on the screen.
;;; point-symbol, bar and label are examples of simple views.
;;; The result of (list-view-classes 'simple-view)     
;;; lists all views that inherit from simple-view.
;;; The result of (list-view-classes 'compound-view)  
;;; lists all views that inherit from compound-view.
;;; Try them out..

(loop for v in (list-view-classes 'simple-view)
      do   (view :type v :draw? t))

;;; There are a lot of compound views to try- instead pick a few
;;; randomly.. its a good idea to first load some data, eg the apple data.
(load "q:Data;apple.lsp")

(let* ((l (list-view-classes 'compound-view))
      (v (nth (random (length l)) l)))
  (view :type v :draw? t))

;;; Most of the interesting view classes have constructor functions.
;;; For example, the function bar constructs a view of type bar.
;;; There are also constructor functions to construct views 
;;; which are small variations on existing classes.
;;; The variable *view-constructors* contains a list of the
;;; functions to construct views.

;;; Try one..

(let  ((v (nth (random (length *view-constructors*)) *view-constructors*)))
  (funcall v :draw? t))




;;; First, I introduce some view operations that work for any view.
;;;

(setq v (arrow :draw? t))  ; or any other kind of view.

;;; By default, only plots (a kind of view) are automatically drawn, 
;;; so we need :draw? to draw the arrow.

;;; ERase the view

(erase-view v)

;;; v temporarily disappears from view, but will reappear with

(draw-view v)

;;; or when a redisplay is issued to the window from the "Canvas" menu
;;; on the menubar.

;;;------------------------------------------------------------------------
;;; Graphichal user interface
;;; See (edit-file "q:Examples;Views;Basics;mouse.lsp") for 
;;; more details.
;;;------------------------------------------------------------------------


;;; Views follows Window-Basics and assumes a three-button mouse. 
;;; We identify the buttons as left, middle and right.
;;; (On the mac, hit the mouse button for left, 
;;; the mouse button + option for middle, and
;;; the mouse button + command for right.)


;;; Operations on a view may be invoked by pointing the
;;; mouse cursor at the view and hitting the mouse button.


;;; The basic behaviour common to most kinds of view is:

;;; left: SELECT
;;; Some information on v is printed. Highlighting on v is toggled.

;;; middle: MENU
;;; pops up the middle menu for the view v.
;;; The middle menu allows you to change drawing styles
;;; plus other parameters that determines the view's appearance.

;;; right: MENU
;;; pops up the right menu for the view v.
;;; The right menu is almost the same for each kind of view,
;;; it offers operations for linking, moving, removing and
;;; layering among others.


;;;------------------------------------------------------------------------
;;; Drawing styles
;;; See (edit-file "q:Examples;Views;Basics;drawing-styles.lsp") for 
;;; details on drawing styles.
;;;------------------------------------------------------------------------


;;; All views have drawing styles highlight and invisible.
;;; These values can also be set from the view's middle menu.
;;; See (edit-file "q:Examples;Views;Basics;mouse.lsp") for details on the menus.

(set-drawing-style v :highlight? t)
(set-drawing-style v :highlight? nil)
(set-drawing-style v :highlight? :toggle)

(set-drawing-style v :invisible? t)
(set-drawing-style v :invisible? nil)
(set-drawing-style v :invisible? :toggle)

;;; Any drawing style which takes values t or nil (for on and off)
;;; can also be toggled, as above.

;;; Notice an invisble view can still be highlit.


;;;------------------------------------------------------------------------
;;; Moving and Copying
;;; See (edit-file "q:Examples;Views;Basics;move-copy.lsp") for 
;;; more details.
;;;------------------------------------------------------------------------

;;; A view is drawn on a rectangular portion of a window called a viewport.
;;; By default, the viewport occupies all of the window. To move or reshape the
;;; viewport, invoke "Drag" from the right button menu or equivalently

(move-view v :new-location (make-region 40 80 20 90))

;;; The new location is a region with coordinates in order left right bottom top,
;;; which should be in the coordinate system of the window. If it happens
;;; that the new-location is outside the window, the view will not be drawn.


;;; A view can be drawn in more that one viewport. 
;;; The viewports where the view is drawn can be accessed by

(viewports-of v)
;;;
It will be useful to

(setq vp (first  (viewports-of v)))

For instance, we can draw
;;; v a second time with
(draw-view v :viewport (make-viewport))

;;; Now try (viewports-of v) again.
;;; Now
(move-view v :new-location (make-region 40 80 20 90))
;;; moves v in the new (most recently constructed) viewport by default.

;;; If we want to move v in the original viewport we must
;;; specify the viewport,

(move-view v :new-location (make-region 40 80  120 145) :viewport vp)


;;; Another way to may another image of a view uses the "DragCopy" operation
;;; on the right button, or equivalently

(copy-image v :new-location (make-region 500 600 300 400))

;;; Here the new-location should define a region in the coordinates
;;; of the screen. If it turns out that the new-location is contained
;;; in an existing window, then the view is drawn in that window,
;;; regardless of whether the view already appears in that window or not.


(copy-image v :new-location (make-region 90 130 90 120))


;;; To get rid of a view from the window  invoke "cut" from the fight menu
;;; or do

(remove-view  v :viewport vp)

;;; This is more drastic than setting the drawing style to invisible,
;;; removing the view actually removes the pointer from the window to
;;; the view.

;;;------------------------------------------------------------------------
;;; Moving and Copying with Compound views
;;; See (edit-file "q:Examples;Views;Basics;move-copy.lsp") for 
;;; more details.
;;;------------------------------------------------------------------------


(setq v (view :type 'compound-view :draw? t))

;;; V is a compound view. It has no subviews.
;;; Let's give v a few subviews.

;;; Add a label to v. The default bounding region of v is the
;;; unit square, so we specifiy the location of the new view 
;;; as a region within the bounding region.

(add-subview v (label :text "one") (make-region .2 .4 .1 .3))

(add-subview v (label :text "two") (make-region .5 .7 .1 .3))


;;; The basic compound-view is intended as a placeholder
;;; for other more interesting compound views. One such
;;; compound view is view-layout, which allows the user
;;; to specify the subviews and their location at construction time.

(setq v (view-layout :subviews (list (label :text "one")
                                     (label :text "two")
                                     (label :text "three"))
                     :positions (list (make-region .2 .3 .2 .3)
                                      (make-region .6 .8 .5 .7)
                                      (make-region .6 .8 .8 .9))
                     :bounding-region (make-region)
                     :draw? t))
;;;The default bounding region of v is made just bug enough to
;;; contain the subviews, if we want a bigger bounding-region
;;; we should specify it.
;;; unit square, so we specifiy the location of the new view 
;;; as a region within the bounding region.

(setq s1 (label :text "four"))
(add-subview v s1 (make-region .5 .7 .1 .3))

;;; Now if we want to move s1 say, we can use

(move-subview v s1 (make-region  .1 .3 .2 .4))
;;; where the new location is specified in the parent
;;; v's coordinatye system, or, do as in the previous section


(move-view s1 :new-location (make-region 40 80  120 145) )

;;; where the new location is specified in the window coordinate
;;; system. In any case, neither move-view or move-subview will
;;; allow you to move s1 to a region outside that occupied by
;;; its parent.

;;; One can remove s1 by

(remove-subview v s1)

;;; or as above by

(remove-view s1)

;;; As before copy-image, produces a new image of a view on the screen.

(vw::copy-image v :new-location (make-region 300 500 200 500) )

;;; Since a subview is restricted to appearing once in the parent,

(copy-image s2 :new-location (make-region 40 80  120 145) )

;;; is equivalent to  move-view, but with the location
;;; specified in screen coordinates.


;;; One can may a new view object with the same appearance as v
;;; using

(copy-view v)


