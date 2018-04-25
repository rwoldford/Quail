;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               classes                            
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



;;;To examine the view hierarchy one can do (class-browse 'view)
;;;but is might be better to break the hierarchy into

(class-browse 'simple-view)

and

(class-browse 'compound-view)

;;; to get two more manageable windows.

;;; The following are important parts of the  class hierarchy.

;;; (i) simple views (simple-view)
Examples are point symbols, labels text-views and bars.

(class-browse 'simple-view)
shows these and others.


;;; (ii) d-views (d-view)
;;; Here "d" stands for data. These views obtain one or more sets
;;; of coordinates from the data to contruct the view.
;;; The coordinates are then used in drawing the view on a viewport.
;;; Examples are histogram, boxplot, axis (these are 1d-views)
;;; 2d-point-cloud, fitted-line, smooth (these are 2d-views)
;;; rotating-cloud (a 3d-view) 
;;; In the above-mentioned d-views all except axis and fitted-line
;;; are also compound views.
(class-browse 'd-view)
;;; shows these and others.


;;; (iii) layouts 
A layout is a kind of compound view where the user can specify the
subviews at their location at construction time. Different
kinds of layout are:

(class-browse 'view-layout)

(view-layout)

;;; (iv) plots (plot)


