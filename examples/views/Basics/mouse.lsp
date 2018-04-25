;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               mouse                            
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

;;; This file gives an introduction to the graphical user interface
;;; of views.
;;; Views follows Window-Basics and assumes a three-button mouse. 
;;; We identify the buttons as left, middle and right.
;;; (On the mac, hit the mouse button for left, 
;;; the mouse button + option for middle, and
;;; the mouse button + command for right.)


;;; Operations on a view may be invoked by pointing the
;;; mouse cursor at the view and hitting the mouse button.

;;; 
;;; 

(setq v1 (view-layout :subviews (list (label :text "one")
                                     (label :text "two")
                                     (label :text "three"))
                     :positions (list (make-region .2 .3 .2 .3)
                                      (make-region .6 .8 .5 .7)
                                      (make-region .6 .8 .8 .9))
                     :bounding-region (make-region)
                     :draw? t))


(setq v2 (2d-point-cloud))



(setq s1 (car (subviews-of v2)))



;;; The basic behaviour common to most kinds of view is:

;;; left: SELECT
;;; Some information on v is printed.
;;; If v was not selected before the left click, then  v is highlighted and
;;; highlighting is turned off on previously selected views.
;;; *selected-view* is a list containing v and no other
;;; view. 
;;; If v is already selected before the left click, then the highlighting
;;; is turned off on v and on all previously selected views.

;;; Equivalent to..

(left-button-fn s1)




;;; On views such as point-cloud, this allows you to sweep
;;; out a rectangle R on the screen.
;;; Views vi within R which are not selected before the left click, are highlighted.
;;; Views vi within R which are selected before the left click, are downlighted.
;;; At the end, *selected-view* is a list containing the highlighted views within R.

;;; Concisely, if N is the view pointed at by the mouse or views in R,
;;; and P is the previous selection, then SELECT 
;;; leaves N intersection (not P) selected.

;;; Equivalent to..

(left-button-fn v2)  ; waits for you to sweep out a rectangle

;;; middle: MENU
;;; pops up the middle menu for the view v.
;;; The middle menu allows you to change drawing styles
;;; plus other parameters that determines the view's appearance.

;;; Equivalent to..
(middle-button-fn s1)
;;; waits for you to push the mouse button and then the middle menu for s1 pops up.

;;; right: MENU
;;; pops up the right menu for the view v.
;;; The right menu is almost the same for each kind of view,
;;; it offers operations for linking, moving, removing and
;;; layering among others.

;;; Equivalent to..
(right-button-fn s1)
;;; waits for you to push the mouse button and then the middle menu for s1 pops up.

;;; The left, middle and  right buttons can be modified by the
;;; shift and ctrl keys.

;;; shift-left EXTENDED SELECT (UNION)
;;; Some information on v is printed.
;;; Also, the view v is highlighted and v is added to the
;;; list of highlighted views.
;;; Notice, unlike the unmodified left click, shift-left
;;; does not toggle the highlighting on v and previously selected views
;;; are unaffected, ie remain selected.

;;; Equivalent to..
(shift-left-button-fn s1)

;;; On views such as point-cloud, this allows you to sweep
;;; out a rectangle R on the screen.
;;; Concisely, if N is the view pointed at by the mouse or views in R,
;;; and P is the previous selection, then UNION 
;;; leaves N union P selected.


;;; shift-middle  SELECT (INTERSECTION)
;;; Some information on v is printed.
;;; Also, the view v is highlighted and
;;; highlighting is turned off on previously selected views.
;;; *selected-view* is a list containing v and no other
;;; view.
 
;;; Equivalent to..
(shift-middle-button-fn s1)



;;; On views such as point-cloud, this allows you to sweep
;;; out a rectangle R on the screen.
;;; Concisely, if N is the view pointed at by the mouse or views in R,
;;; and P is the previous selection, then INTERSECTION
;;; leaves N intersection P selected.

;;; shift-right  does nothing


;;; ctrl-left
;;; ctrl-middle
;;; ctrl-right



;;; These are intended to invoke operations on the viewed object of the
;;; view. At present they just put up an inspector on the
;;; viewed object of the view.


;;; Equivalent to..
(ctrl-xx-button-fn s1)
