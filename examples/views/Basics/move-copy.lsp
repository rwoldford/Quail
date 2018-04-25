;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               move-copy                            
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
;;; First, experiment with any kind of view, simple or compound.
(setq v (arrow :draw? t))  ; or any other kind of view.

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

(setq vp (car  (viewports-of v)))

For instance, we can draw
;;; v a second time with
(draw-view v :viewport (make-viewport))

;;; Now try (viewports-of v) again.
;;; Now
(move-view v :new-location (make-region 40 80 20 90))
;;; moves v in the new viewport by default.

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

;;; We can  "decorate" the view by drawing something else on
;;; a viewport occupying the same location.

;;; We could do this in a few ways.

(setq v1 (line  :orientation :vertical))
(draw-view v1 :viewport (copy-region vp) )

;;; or

(layer-view v (line  :orientation :horizontal) :viewport vp)
;;; These are equivalent. If you use just vp instead of (copy-region vp)
;;; the first version also works, but now if you move v v1 will also move.
;;; (It is necessary to redraw the window after moving so both v and
;;; v1 are drawn in the new version of vp.)

;;; To layer a view via menus, select "paste" from the right menu of v,
;;; and choose "line" from the proferred menu. This layers a line on v,
;;; by default it is a diagonal line, but the orientation can be altered
;;; via the line's middle menu.


;;; To get rid of a view from the window  invoke "cut" from the right menu
;;; or do


(remove-view  v :viewport vp)

;;; This is more drastic than setting the drawing style to invisible,
;;; removing the view actually removes the pointer from the window to
;;; the view.


;;; Layering a view  followed by removing the underlying view amounts
;;; to replacing one view with another. When using the menus, it may be 
;;; difficult or impossible to select the underlying view. Hence views
;;; uses the convention that if the original view is selected when the
;;; layering occurs, the original view is automatically removed.
;;; Programmatically,

(select-view v)
(layer-view v (line  :orientation :vertical) :viewport vp)

;;; replaces v with a vertical line.



;;; Now we move to a compound view.


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

(setq s1 (arrow))
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

;;; Now try..
(setq s1 (arrow))
(add-subview v s1 (make-region .5 .7 .1 .3))

;;; Notice the arrow appears in both images of v.


;;; Since a subview is restricted to appearing once in the parent,

(vw::copy-image s1 :new-location (make-region 40 80  120 145) )

;;; is equivalent to  move-view, but with the location
;;; specified in screen coordinates.


;;; To add some decoration to the arrow:
(layer-view  s1 (line  :orientation :vertical))
;;; places a vertical line ontop of s1 wherever s1 is drawn on the screen.

;;; 
(layer-subview  v (line  :orientation :horizontal) s1 )
;;; places a horizontal line ontop of s1 wherever s1 appears
;;; as a subview of v.

;;; In the first version, s1 only appears once on the screen,
;;; so there is only one possible viewport where the line can be
;;; placed. If s1 appeared more than once on the screen
;;; the particular viewport of s1 should be specified after a :viewport
;;; key.
;;;In the second version, the position of s1 inside v
;;; is the intended location of the line

;;; One can may construct a new view object with the same appearance as v
;;; using

(copy-view v)

;;; By default it uses a viewport the same size as the most recent
;;; viewport constructed for v.

;;; If you prefer, you can specify a viewport for the copy

(copy-view v :viewport (make-viewport))

;;; or not draw the copy at all

