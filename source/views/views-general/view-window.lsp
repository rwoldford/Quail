;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               view-window.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for 
;;;  interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export  '(view-window 
             system-view-window-items make-view-window 
             region-of   viewports-and-views-of
             add-view-to-window
             remove-view which-window which-view which-toplevel-view
             drag-region 
             create-region-with-mouse *current-dataset*
             )))

(defclass view-window (wb:canvas)
  ((region :accessor region-of
           :initform (make-region)))
  )


(defgeneric viewports-and-views-of (view-window)
  (:documentation "The views displayed in this view-window and their viewports.  ~
                   A list of the viewports and views."))

(defgeneric add-view-to-window (w v 
                                  &key draw? viewport)
  (:documentation " adds a view to view-window in position viewport.~
                   If viewport is not provided, a viewport on w is created ~
                   occupying all of w"))


(defgeneric remove-view-from-window (view-window &key view viewport)
  (:documentation
   "The view view and the view drawn at viewport are removed from view-window"))


(defgeneric which-view (view-window &optional location)
  (:documentation 
   "Returns the view, its viewport, its parent, and the parent's viewport ~
   at location and the distance as multiple values."))


(defgeneric which-toplevel-view (view-window &optional location)
  (:documentation 
   " Returns the top-level view and its viewport ~
     at location as multiple values."))

(defvar *current-dataset* nil)

;;;-------------------------------------------------------------------------------------
;;;
;;; Hook the viewports-and-views-of to the display slot of the canvas
;;;

(defmethod viewports-and-views-of ((c view-window))
  (wb:display-of c))

(defmethod (setf viewports-and-views-of) ( new-value (c view-window) )
  (setf (wb:display-of c) new-value))

;;;--------------------------------------------------------------------------------------

(defmethod set-bounds ((w view-window))
  (multiple-value-bind (l r b tp) (wb:canvas-bounds w)
    (setf (bounds-of (region-of w)) (list l r b tp))))

;;; 2 added forms 27SEP2024 to allow for mcclim's defining things in window-basics, and its definition
;;;  of a region, while set-bounds is in views, with its separate defintion of a region
#+:sbcl-linux(defun wb-to-vw (wb-region)
      "Input: a window-basics region such as #(10 20 30 40)
       Output: a matching views region #(REGION 10 20 30 40)"
      (vector 'region (svref wb-region 0) (svref wb-region 1) (svref wb-region 2) (svref wb-region 3)))

#+:sbcl-linux(defmethod set-bounds ((w wb::color-canvas))
               (multiple-value-bind (l r b tp) (wb::canvas-bounds w)
                                    (setf (bounds-of (wb-to-vw (wb::region-of w))) (list l r b tp))))

;;;========================================================================



#|
(defmethod add-view-to-window ((w view-window) v
                               &key (draw? t) 
                               (viewport (make-viewport w 0 1 0 1)))
  
  ;; adds a VIEW to WINDOW in position viewport
  
  
  (unless (find-if #'(lambda (elt) (or (eql (car elt) viewport)
                                       (eql (cdr elt) v)))
                   (viewports-and-views-of w))
    (setf (window-of viewport) w)
    (map-to-viewport v viewport)
    (if draw? (draw-view v :viewport viewport ))
    (setf (viewports-and-views-of w)
          (acons viewport v
                 (viewports-and-views-of w)))
    ))
|#


;;; remove the unless 6-6-95. No particular reason, just... why not.
(defmethod add-view-to-window ((w view-window) v
                               &key (draw? t) 
                               (viewport (make-viewport w 0 1 0 1)))
  
  ;; adds a VIEW to WINDOW in position viewport
  
  
     (setf (window-of viewport) w)
    (map-to-viewport v viewport)
    (if draw? (draw-view v :viewport viewport ))
    (setf (viewports-and-views-of w)
          (acons viewport v
                 (viewports-and-views-of w)))
    )
;;;-----------------------------------------------------------------------------------

(defun *view-left-button-fn* (canvas mouse-pos)
  (let ((click-pos (view-position mouse-pos)))
    (multiple-value-bind (view vp )
                         (which-view canvas  click-pos)
      (when view
        (left-button-fn view :viewport vp :position click-pos)))))

(defun *view-shift-left-button-fn* (canvas mouse-pos)
  (let ((click-pos (view-position mouse-pos)))
     (multiple-value-bind (view vp )
                         (which-view canvas  click-pos)
      (when view
        (shift-left-button-fn view :viewport vp :position click-pos)))))

(defun *view-ctrl-left-button-fn* (canvas mouse-pos)
  (let ((click-pos (view-position mouse-pos)))
     (multiple-value-bind (view vp )
                         (which-view canvas click-pos)
      (when view
        (ctrl-left-button-fn view :viewport vp :position click-pos )))))

(defun *view-middle-button-fn* (canvas mouse-pos)
  (let ((click-pos (view-position mouse-pos)))
     (multiple-value-bind (view vp )
                         (which-view canvas click-pos)
      (when view
        (middle-button-fn view :viewport vp :position click-pos)))))

(defun *view-shift-middle-button-fn* (canvas mouse-pos)
  (let ((click-pos (view-position mouse-pos)))
     (multiple-value-bind (view vp )
                         (which-view canvas click-pos)
      (when view
        (shift-middle-button-fn view :viewport vp :position click-pos)))))

(defun *view-ctrl-middle-button-fn* (canvas mouse-pos)
  (let ((click-pos (view-position mouse-pos)))
     (multiple-value-bind (view vp )
                         (which-view canvas click-pos)
      (when view
        (ctrl-middle-button-fn view :viewport vp :position click-pos)))))


(defun *view-right-button-fn* (canvas mouse-pos)
  (let ((click-pos (view-position mouse-pos)))
     (multiple-value-bind (view vp )
                         (which-view canvas click-pos)
      (when view
        (right-button-fn view :viewport vp :position click-pos)))))

(defun *view-shift-right-button-fn* (canvas mouse-pos)
  (let ((click-pos (view-position mouse-pos)))
     (multiple-value-bind (view vp )
                         (which-view canvas click-pos)
      (when view
        (shift-right-button-fn view :viewport vp :position click-pos)))))

(defun *view-ctrl-right-button-fn* (canvas mouse-pos)
  (let ((click-pos (view-position mouse-pos)))
     (multiple-value-bind (view vp )
                         (which-view canvas click-pos)
      (when view
        (ctrl-right-button-fn view :viewport vp :position click-pos)))))

;;---------------------------------------------------------------------------------

(defun view-window-size-changed-p (view-window)
  (multiple-value-bind (l r b tp) 
                       (wb:canvas-bounds view-window)
    (not (and (= (- r l) (width-of (region-of view-window)))
              (= (- tp b) (height-of (region-of view-window)))))))





(defun view-window-redisplay-fn (view-window)
  (wb:canvas-clear view-window)
  (when (view-window-size-changed-p view-window)
    (multiple-value-bind
      (l r b tp)  (wb:canvas-bounds view-window)
      
      (let ((at (make-transform-for-regions
                 (region-of view-window) (make-region l r b tp ) )))
        (dolist (vp-v (viewports-and-views-of view-window))
          (reshape-viewport (cdr vp-v) (car vp-v) :transform at :draw? nil )))
      (set-bounds view-window)))
  (dolist (vp-v (viewports-and-views-of view-window))
    (draw-view (cdr vp-v) :viewport (car vp-v))))


;;----------------------------------------------------------------------------------------

;;;
;;; NOTE:  Both plot-menu-items and system-view-window-items are not
;;;        to be used as title-menus on a canvas any more.
;;;        Both of these menus are useful when no view-window (or canvas)
;;;        exists; they are now part of a permanent quail-menubar.
;;;        Indeed, system-view-window-items no longer exists.
;;;        This makes the aclpc and the mcl version consistent.
;;;        ... rwo May 9, 2000.

(defun make-view-window (&rest canvas-keywords
                               &key left right bottom top 
                               (background-color NIL)
                               (view-window-class 'view-window)
                               title
                               (region NIL)
                               &allow-other-keys)
  (setq title (if (stringp title) 
                title 
                "View Window"))
                  
  (when  (region-p region)
    (multiple-value-setq (left right bottom top)
      (bounds-of region)))
  (let ((canvas (if (and left right bottom top)
                  (apply #'wb:make-canvas
                         :canvas-class view-window-class
                         :title title
                         :left left
                         :bottom bottom 
                         :width (1+ (- right left))
                         :background-color (or background-color
                                      wb::*default-canvas-background-color*);background-color
                         :height (1+ (- top bottom))
                         canvas-keywords)
                  (apply #'wb:make-canvas
                         :canvas-class view-window-class
                         :title title
                         :background-color (or background-color
                                      wb::*default-canvas-background-color*);background-color
                         canvas-keywords)
                  )
                )
        )
    
    (wb:set-left-button-fn canvas #'*view-left-button-fn*)
    (wb:set-middle-button-fn canvas #'*view-middle-button-fn*)
    (wb:set-right-button-fn canvas #'*view-right-button-fn*)
    (wb:set-ctrl-left-button-fn canvas #'*view-ctrl-left-button-fn*)
    (wb:set-ctrl-middle-button-fn canvas #'*view-ctrl-middle-button-fn*)
    (wb:set-ctrl-right-button-fn canvas #'*view-ctrl-right-button-fn*)
    
    (wb:set-shift-left-button-fn canvas #'*view-shift-left-button-fn*)
    (wb:set-shift-middle-button-fn canvas #'*view-shift-middle-button-fn*)
    (wb:set-shift-right-button-fn canvas #'*view-shift-right-button-fn*)
    
    (wb:set-redisplay-test canvas #'view-window-size-changed-p)
    (wb:set-redisplay-fn canvas #'view-window-redisplay-fn)
    (set-bounds canvas)
    canvas))



(defmethod remove-view-from-window ((self view-window) 
                                    &key view viewport)
  (setf (viewports-and-views-of self)
        (remove-if #'(lambda (elt)
                       (or (eql (car elt) viewport)
                           (eql (cdr elt) view)))
                   (viewports-and-views-of self))))


(defun which-window (location)
  
  (let*
    ((l (if (region-p location) (centre-of location ) location))
     (canvas  (wb:which-canvas (wb-position l))))
    (if (and canvas
               (typep canvas 'view-window)
               (or (2d-position-p location)
                   (contains-p (region-of canvas)
                               (window-location location canvas ))))
        canvas)))




(defmethod which-toplevel-view1 ((self view-window) &optional location)
  " Returns the top-level  view and its viewport ~
   at location as multiple values."
  
  (if (null location)
    (setq location (region-of self)))
  (let ((vpv (viewports-and-views-of self)))
    (if vpv
      (loop with view with viewport 
            with dmin = 100000
            for vp-v in vpv do
            (multiple-value-bind (a1 a2 a3 a4 d)
              (which-subview (cdr vp-v) (car vp-v) location)
              (declare (ignore a1 a2 a3 a4))
            (when (and d (< d dmin) )
              (setq view (cdr vp-v) viewport (car vp-v) dmin d)))
            finally (return (values view viewport))))))

 
(defmethod which-toplevel-view ((self view-window) &optional location)
 (which-toplevel-view1 self location))


(defmethod which-toplevel-view (location &optional ignore)
  ;; location is in screen coordinates
  
  (declare (ignore ignore))
  
  (let ((view-window 
         
         (if (region-p location)
           (which-window (centre-of location) )
           (which-window location))))
    
    (when view-window 
      (setq location 
            (window-location location  view-window ))
      (which-toplevel-view1 view-window location))))

 
(defmethod which-view ((self view-window) &optional location)
  (declare (special *current-dataset*))
 (if (null location)
    (setq location (region-of self)))
  (let ((vpv (viewports-and-views-of self)  ))
    (if vpv
      (loop with closest 
            for (vp . v) in vpv
            for r = (multiple-value-list (which-subview v vp location))
            do 
            (if (and (car r)
                      (or (null closest) 
                     (< (fifth r)  (fifth closest)) ))
             (setq closest r ))
             
             finally (setq *current-dataset* 
                           (if (and v (dataset-p (viewed-object-of v)))
                             (viewed-object-of v)))
                     (return (values-list closest))))))


(defmethod which-view (location &optional ignore)
  ;; location is in screen coordinates
  
  (declare (ignore ignore))
  
  (let ((view-window 
         
         (if (region-p location)
           (which-window (centre-of location) )
           (which-window location))))
    
    (when view-window 
      (setq location 
            (window-location location  view-window ))
      (which-view view-window location))))




(defun screen-location (location
                        &optional view-window)
  "Returns the position of location in screen coordinates rather ~
   than window coordinates."
  
  (if (and (null view-window) (viewport-p location))
    (setq view-window (window-of location)))
  (if (2d-position-p location)
    (make-2d-position 
     (+ (2d-position-x location)  (wb:screen-x view-window))
     (+ (2d-position-y location ) (wb:screen-y view-window)))
    (shift-by (make-region location ) 
              (wb:screen-x view-window)
              (wb:screen-y view-window))))


(defun window-location (location
                        &optional view-window)
  "Returns the position of location in window coordinates rather ~
   than screen coordinates."
  (if (and (null view-window) (viewport-p location))
    (setq view-window (window-of location)))
  (if (2d-position-p location)
    (make-2d-position 
     (+ (2d-position-x location ) (- (wb:screen-x view-window)))
     (+ (2d-position-y location) (- (wb:screen-y view-window))))
    ;;else
    (let ((new-region (make-region location)))
      (shift-by new-region
                (- (wb:screen-x view-window)) (- (wb:screen-y view-window)))
      (if (viewport-p location)
        (make-viewport view-window new-region)
        new-region))))


(defun drag-region (region
                    &key limit window (axis :both) (in-window? t))
  "Drags region to a new location, which is returned ~
   region cannot go outside limit (if provided).  ~
   If in-window? is non-nil, ~
   then region and limit and result are in window coords ~
   otherwise screen coords.  ~
   Axis contols the direction of dragging, values may be~
   :x :y :both :none."
  
  (let (wb-limit wb-location)
    (setq limit (or limit
                    (if in-window? (region-of window))))
    (if limit (setq wb-limit (wb-region limit)))
    (setq wb-location
          (if in-window?
            (wb:drag-region-on-canvas 
             window
             :region (wb-region region) 
             :limit-region wb-limit
             :axis axis)
            (wb:drag-region-on-screen 
             window
             :region (wb-region region) 
             :axis axis)))
    
    (view-region wb-location)))






(defun create-region-with-mouse (&optional window width height)
  (let (l b w h)
    (multiple-value-setq (l b w h)
      (wb:select-rectangle :canvas window
                           :width width
                           :height height))
    (make-region l (+ l w) b (+ b h))))
