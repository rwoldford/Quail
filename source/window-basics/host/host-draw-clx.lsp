;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        host-draw-clx.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     N.G. Bennett 1993
;;;     R.W. Oldford 1994
;;;
;;;--------------------------------------------------------------------------------

(in-package :host-draw)

(defun make-point (x &optional y)
  "Returns a point having x and y as its coordinates."
  (if y
    (list x y)
    x))

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(point-x point-y *hardcopy-ptr* pen-show pen-hide pen-shown-p 
	  pen-position pen-size set-pen-size pen-mode set-pen-mode pen-pattern 
	  set-pen-pattern set-pen-color move-to move line-to line draw-line
          draw-rectangle draw-inside-rectangle draw-filled-rectangle 
          erase-rect invert-rectangle draw-ellipse draw-filled-ellipse draw-arc 
          fill-arc erase-arc invert-arc fill-arc draw-polygon 
	  draw-filled-polygon make-bitmap copy-bits origin set-origin 
	  draw-string draw-char)
        ))
(defun point-x (point)
  "Returns the x coordinate of point."
  (first point))

(defun point-y (point)
  "Returns the x coordinate of point."
  (second point))

(defvar *hardcopy-ptr*
  NIL
  "A system dependent pointer to an open printer device.")
#|
(defmacro with-rectangle-arg ((var left &optional top right bottom) &body body)
  "Takes a rectangle, two points, or four coordinates and makes a rectangle. ~
   Body is evaluated with VAR bound to that rectangle."
  (let ((left-var (make-symbol "LEFT"))
        (top-var (make-symbol "TOP"))
        (right-var (make-symbol "RIGHT"))
        (bottom-var (make-symbol "BOTTOM")))
    `(let ((,left-var ,left)
           (,top-var ,top)
           (,right-var ,right)
           (,bottom-var ,bottom))
       (call-with-rectangle-arg
        #'(lambda (,var)
            (declare (dynamic-extent))
            ,@body)
        ,left-var ,top-var ,right-var ,bottom-var))))

(defun call-with-rectangle-arg (thunk left top right bottom)
  (rlet ((var :rect))
    (cond (bottom
           (setf (pref var rect.topleft) (ccl::make-point left top))
           (setf (pref var rect.bottomright) (ccl::make-point right bottom)))
          (right
           (quail-error "Illegal rectangle arguments: ~s ~s ~s ~s"
                        left top right bottom))
          (top
           (setf (pref var rect.topleft) (ccl::make-point left nil))
           (setf (pref var rect.bottomright) (ccl::make-point top nil)))
          (t (%setf-macptr var left)))
    (funcall thunk var)))
;;BOOLEAN EXPRESSIONS FOR THIS??
(defvar *32-bit-qd-pen-modes*
  '((:blend . 32)
    (:addPin . 33)
    (:addOver . 34)
    (:subPin . 35)
    (:transparent . 36)
    (:adMax . 37)
    (:subOver . 38)
    (:adMin . 39)
    (:hilite . 50)))


(defun mode-arg (thing)
  (or
   (and (fixnump thing) (<= 0 thing 64) thing)
   (position thing *pen-modes*)
   (cdr (assq thing *32-bit-qd-pen-modes*))
   (quail-error "Unknown pen mode: ~a" thing)))

|#
(defun pen-show (canvas)
  nil)

(defun pen-hide (canvas)
  nil)

(defun pen-shown-p (canvas)
  nil)
  
(defun pen-position (canvas)
  (wb::current-position canvas))

(defun pen-size (canvas)
  (xlib:gcontext-line-width (wb::gcontext canvas)))

(defun set-pen-size (canvas h &optional v)
  (unless h (setq h 1))
  (if v
    (xlib::set-gcontext-line-width (wb::gcontext canvas) h)
    (xlib::set-gcontext-line-width (wb::gcontext canvas) (car h))))

(defun pen-mode (canvas)
  (elt *pen-modes* (xlib:gcontext-function (wb::gcontext canvas))))

(defun set-pen-mode (canvas new-mode)
  (xlib::set-gcontext-function (wb::gcontext canvas) new-mode))
#|
(defun pen-pattern (canvas &optional
                        (save-pat (make-record (:pattern :storage :pointer))))
  (copy-record
   (pref (or *hardcopy-ptr* (wptr canvas))
         windowRecord.pnPat) (:pattern :storage :pointer) save-pat))
|#
(defun set-pen-pattern (canvas new-pattern)
  (set-pen-color canvas new-pattern))

(defun set-pen-color (canvas new-color)
  (let* ((gcontext (wb::gcontext canvas))
	 (display (xlib::gcontext-display gcontext))
	 (screen (xlib::display-default-screen display))
	 (colormap (xlib::screen-default-colormap screen))
	 (my-new-color
          (xlib::alloc-color colormap
                             (if new-color new-color
		                 (wb::canvas-default-draw-color canvas)))))
    (xlib::set-gcontext-foreground gcontext my-new-color))
  new-color)
; ----------------------------
(defun move-to (canvas h &optional v)
  (if v
    (setf (wb::current-position canvas) (list h v))
    (setf (wb::current-position canvas) h)))

(defun move (canvas h &optional v)
  (if v
    (setq h (+ h (point-x (wb::current-position canvas)))
	  v (+ v (point-y (wb::current-position canvas)))))
    (setq v (+ (second h) (point-y (wb::current-position canvas)))
	  h (+ (first h) (point-x (wb::current-position canvas))))
  (move-to canvas x y))

(defun line-to (canvas h &optional v)
  (let ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas))
	(x1 (point-x (wb::current-position canvas)))
	(y1 (point-y (wb::current-position canvas))))
    (unless v
      (setq v (second h) h (first h)))
  (xlib:draw-line window gcontext x1 y1 h v))
  (move-to canvas h v))

(defun line (canvas h &optional v)
  (let ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas))
	(x1 (point-x (wb::current-position canvas)))
	(y1 (point-y (wb::current-position canvas))))
    (unless v
      (setq v (second h) h (first h)))
      (xlib:draw-line window gcontext x1 y1 h v t))
  (move-to canvas h v))

(defun draw-line (canvas x1 y1 x2 y2)
  (let ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas)))
    (xlib:draw-line window gcontext x1 y1 x2 y2))
  (move-to canvas x2 y2))

(defun draw-rectangle (canvas x1 x2 y1 y2)
  (let ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas))
	(width (- x2 x1))
	(height (- y2 y1)))
    (xlib:draw-rectangle window gcontext x1 y1 width height))
  (move-to canvas x1 y1))

(defun draw-inside-rectangle (canvas left &optional top right bot)
  (let* ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas))
	(x1 (+ left 1))
	(y1 (+ top 1))
	(width (- right x1 1))
	(height (- bot y1 1)))
    (xlib:draw-rectangle window gcontext x1 y1 width height)
    (move-to canvas x1 y1)))

(defun draw-filled-rectangle (canvas left &optional top right bot)
  (let ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas))
	(width (- right left))
	(height (- bot top)))
    (xlib:draw-rectangle window gcontext left top width height t))
  (move-to canvas left top))

(defun erase-rect (canvas left &optional top right bot)
  (let* ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas))
	(width (- right left))
	(height (- bot top))
	(fore (xlib:gcontext-foreground gcontext))
	(back (xlib:gcontext-background gcontext))
	(old-fcn (xlib:gcontext-function gcontext)))
    (xlib::set-gcontext-foreground gcontext back)
    (xlib::set-gcontext-function gcontext boole-1)
    (xlib:draw-rectangle window gcontext left top width height t)
    (xlib::set-gcontext-foreground gcontext fore)
    (xlib::set-gcontext-function gcontext old-fcn))
  (move-to canvas left top))

(defun invert-rectangle (canvas left &optional top right bot)
  (let* ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas))
	(width (- right left))
	(height (- bot top))
	(old-fcn (xlib:gcontext-function gcontext)))
    (xlib::set-gcontext-function gcontext boole-c2)
    (xlib:draw-rectangle window gcontext left top width height t)
    (xlib::set-gcontext-function gcontext old-fcn))
  (move-to canvas left top))

(defun draw-ellipse (canvas left &optional top right bot)
  (let ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas))
	(width (- right left))
	(height (- bot top)))
    (xlib:draw-arc window gcontext left top width height 0 7 nil))
  (move-to canvas left top))

(defun draw-filled-ellipse (canvas left &optional top right bot)
  (let ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas))
	(width (- right left))
	(height (- bot top)))
    (xlib:draw-arc window gcontext left top width height 0 7 t))
  (move-to canvas left top))

;;;
;;;  Arcs
;;;

;;;
;;;  The following exists because we prefer to work with radii
;;; ... rwo
(defun radii-to-rect (x-centre y-centre x-radius y-radius)
  "From the ellipse defined by x-centre y-centre x-radius y-radius ~
   computes and returns ~
   multiple-values left top width height giving the coordinate ~
   information on the enclosing rectangle of the ellipse."
  (let (left top width height)
    (setf left (- x-centre x-radius))
    (setf top (- y-centre y-radius))            ;inverted y coord system
    (setf width (* 2 x-radius))
    (setf height (* 2 y-radius)) 
    (values left top width height)))


(defun draw-arc (canvas start-angle arc-angle
                        x-centre y-centre x-radius y-radius)
  (let ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas)))
    (multiple-value-bind
      (left top width height)
      (radii-to-rect x-centre y-centre x-radius y-radius)
      (xlib:draw-arc window gcontext 
	left top width height start-angle arc-angle)
      (move-to canvas left top))))

(defun fill-arc (canvas start-angle arc-angle 
                        x-centre y-centre x-radius y-radius)
  (let ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas)))
    (multiple-value-bind
      (left top width height)
      (radii-to-rect x-centre y-centre x-radius y-radius)
      (xlib:draw-arc window gcontext
	left top width height start-angle arc-angle t)
      (move-to canvas left top))))

(defun erase-arc (canvas start-angle arc-angle 
                        x-centre y-centre x-radius y-radius)
  (let* ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas))
	(fore (xlib:gcontext-foreground gcontext))
 	(back (xlib:gcontext-background gcontext))
	(old-fcn (xlib:gcontext-function gcontext)))
    (multiple-value-bind
      (left top width height)
      (radii-to-rect x-centre y-centre x-radius y-radius)
      (xlib::set-gcontext-foreground gcontext back)
      (xlib::set-gcontext-function gcontext boole-1)
      (xlib:draw-arc window gcontext 
	left top width height start-angle arc-angle t)
      (xlib::set-gcontext-foreground gcontext fore)
      (xlib::set-gcontext-function gcontext old-fcn)
      (move-to canvas left top))))

(defun invert-arc (canvas start-angle arc-angle 
                        x-centre y-centre x-radius y-radius)
  (let* ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas))
	(old-fcn (xlib:gcontext-function gcontext)))
    (multiple-value-bind
      (left top width height)
      (radii-to-rect x-centre y-centre x-radius y-radius)
      (xlib::set-gcontext-function gcontext boole-c2)
      (xlib:draw-arc window gcontext left top width height start-angle arc-angle t)
      (xlib::set-gcontext-function gcontext old-fcn)
      (move-to canvas left top))))

(defun draw-polygon (canvas list-of-points)
  (let ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas)))
    (xlib:draw-lines window gcontext list-of-points))
  (move-to c (first list-of-points)))

(defun draw-filled-polygon (canvas list-of-points)
  (let ((window (wb::host-window canvas))
	(gcontext (wb::gcontext canvas)))
    (xlib:draw-lines window gcontext list-of-points :fill-p t :shape :convex))
  (move-to c (first list-of-points)))

(defun make-bitmap (left &optional top right bottom &aux rowbytes bm)
 

)


(defun copy-bits (source-bitmap dest-bitmap source-rect dest-rect
                                &optional (mode 0) mask-region)
  )

(defun draw-string (canvas string)
  "Draws a string on the canvas at the current position with ~
   current font and colour."
  (let* ((position (wb::current-position canvas))
	 (x (point-x position))
	 (y (point-y position))
	 (hw (wb::host-window canvas))
	 (gc (wb::gcontext canvas)))
    (xlib:draw-image-glyphs hw gc x y string)
    (move-to canvas (+ x (xlib::text-extents
                          (xlib::gcontext-font
                           (wb::gcontext canvas))
                          string))
             y)
    )
  )

(defun draw-char (canvas char)
  (draw-string canvas (string char)))


(provide 'host-draw)
(pushnew :host-draw *features*)
