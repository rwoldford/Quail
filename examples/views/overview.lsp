;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               overview                             
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
;;; See (edit-file "q:Examples;Views;classes.lsp") for 
;;; details on drawing styles.
;;;------------------------------------------------------------------------

;;; To make this overview more interesting, let's construct
;;; and draw a view which produces something in the viewport.
;;; Views are divided into simple views and compound views.
;;; Compound views are composed of other views, which are themselves
;;; compound or simple views.
;;; Simple views on the other hand have no subviews, but usually will
;;; produce some figure when drawn on the screen.
;;; point-symbol, bar and label are examples of simple views.
;;; The result of (list-view-classes 'simple-view)     
;;; lists all views that inherit from simple-view.
;;; The result of (list-view-classes 'compound-view)  
;;; lists all views that inherit from compound-view.
;;; Try them out..

(loop for v in (list-view-classes 'simple-view)
      do (print v)  (view :type v :draw? t))

;;; There are a lot of compound views to try- instead pick a few
;;; randomly.. its a good idea to first load some data, eg the apple data.


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

;;; By default, only plots are automatically draw, so we need :draw? to
;;; draw the arrow.

;;;------------------------------------------------------------------------
;;; Drawing styles
;;; See (edit-file "q:Examples;Views;drawing-styles.lsp") for 
;;; details on drawing styles.
;;;------------------------------------------------------------------------


;;; All views have drawing styles highlight and invisible.
;;; These values can also be set from the view's middle menu.
;;; See (edit-file "q:Examples;Views;mouse.lsp") for details on the menus.

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
;;; See (edit-file "q:Examples;Views;move-copy.lsp") for 
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

(copy-view v :new-location (make-region 500 600 300 400))

;;; Here the new-location should define a region in the coordinates
;;; of the screen. If it turns out that the new-location is contained
;;; in an existing window, then the view is drawn in that window,
;;; regardless of whether the view already appears in that window or not.


(copy-view v :new-location (make-region 90 130 90 120))


;;; To get rid of a view from the window  invoke "cut" from the fight menu
;;; or do

(remove-view  v :viewport vp)

;;; This is more drastic than setting the drawing style to invisible,
;;; removing the view actually removes the pointer from the window to
;;; the view.

;;;------------------------------------------------------------------------
;;; Moving and Copying with Compound views
;;; See (edit-file "q:Examples;Views;move-copy.lsp") for 
;;; more details.
;;;------------------------------------------------------------------------


(setq v (view :type 'compound-view :draw? t))

;;; V is a compound view. It has no subviews.
;;; Let's give v a few subviews.

;;; Add a label to v. The default bounding region of v is the
;;; unit square, so we specifiy the location of the new view 
;;; as a region within the bounding region.

(setq s1 (label :text "one"))
(add-subview v s1 (make-region .2 .4 .1 .3))

;;; and another


(setq s2 (label :text "two"))
(add-subview v s2 (make-region .5 .7 .1 .3))

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

(remove-view s2)

;;; For copying the view, there is no copy-subview, just copy-view.
;;; There is no need for a copy-subview, since 
;;; a subview is restricted to appearing once in the parent.
;;; For this reason,

(copy-view s2 :new-location (make-region 40 80  120 145) )

;;; is equivalent to  move-view, but with the location
;;; specified in screen coordinates.

