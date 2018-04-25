;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        host-draw-mcl.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;   Modified by C. Hurley 9/21/92 for consistency with MCL2.0
;;;   (from change history on Quickdraw.lisp:
;;
;; Earlier Mod history
;;
;; 10/16/91 bill PSZ's simplification of with-rectangle-arg
;; ------------- 2.0b3
;; 08/26/91 bill downward-function -> dynamic-extent
;; 08/17/91 bill (pset x :record.slot v) -> (setf (pref x :record.slot) v)
;;               No more (require-interface :quickdraw), autoloading is faster.
;; 07/09/91 bill rref & rset -> pref/href & pset/hset
;; ------------- 2.0b2
;; 02/20/91 bill with-pointers in copy-bits, *32-bit-qd-pen-modes* in mode-arg
;;--------------- 2.0b1
;;
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Host-Draw.lisp
;;
;;  version 2.0
;;
;;  copyright 1987-89 Apple Computer, Inc
;;
;;  This file implements a full error-checked interface to Quickdraw.
;;  It is meant to be useful both in programs and as an example of how to use
;;  the low-level interface to the Mac.
;;
;;  You can compile selected portions of this file, but if you do, make sure to
;;  include the macros and utility functions from the top.
;;
;;  These functions require a canvas-focus on every drawing command.
;;  For faster drawing you should only focus the canvas
;;  once, and then issue a series of drawing commands.  You can use
;;  this file as an example of how to call the Quickdraw traps directly
;;  in such a situation.
;;

(in-package :host-draw)

;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Shadowed symbols in defpackage
;;;

;;;(shadow '(make-point))


(defun h-draw:make-point (x &optional y)
  "Returns a point having x and y as its coordinates."
  (if y
    (ccl::make-point x y)
    (ccl::make-point x)))

;;; (export '(make-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(point-x point-y
          *hardcopy-ptr* clip-region set-clip-region clip-rect pen-show pen-hide
          pen-shown-p pen-position pen-size set-pen-size pen-mode
          set-pen-mode pen-pattern set-pen-pattern set-pen-color pen-state
          set-pen-state pen-normal move-to move line-to line
          offset-rect inset-rect intersect-rect union-rect point-in-rect-p
          points-to-rect point-to-angle equal-rect empty-rect-p frame-rect
          paint-rect erase-rect invert-rect fill-rect frame-oval paint-oval
          erase-oval invert-oval fill-oval frame-round-rect paint-round-rect
          erase-round-rect invert-round-rect fill-round-rect draw-arc
          fill-arc erase-arc invert-arc fill-arc new-region dispose-region
          copy-region set-empty-region set-rect-region open-region close-region
          offset-region inset-region intersect-region union-region
          difference-region xor-region point-in-region-p rect-in-region-p
          equal-region-p empty-region-p frame-region paint-region erase-region
          invert-region fill-region start-picture get-picture draw-picture
          kill-picture start-polygon get-polygon kill-polygon offset-polygon
          frame-polygon paint-polygon erase-polygon invert-polygon fill-polygon
          local-to-global global-to-local get-pixel scale-point map-point
          map-rect map-region map-polygon make-bitmap copy-bits scroll-rect
          origin set-origin draw-string draw-char)
        ))


(eval-when (eval compile)
  (require :deftrap))


(defun point-x (point)
  "Returns the x coordinate of point."
  (ccl::point-h point))

(defun point-y (point)
  "Returns the x coordinate of point."
  (ccl::point-v point))

(defvar *hardcopy-ptr*
  NIL
  "A system dependent pointer to an open printer device.")
 
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

(defun origin (view)
  (view-scroll-position view))

(defun set-origin (view h &optional v)
  (set-view-scroll-position view h v nil))


(defun clip-region (canvas &optional (save-region (#_NewRgn)))
  (declare (ignore canvas))
  (#_GetClip save-region)
  save-region)

(defun set-clip-region (canvas new-region)
  (declare (ignore canvas))
  (#_SetClip new-region)
  new-region)

(defun clip-rect (canvas left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot)
    (#_ClipRect r))
  nil)

(defun pen-show (canvas)
  (setf (pref (or *hardcopy-ptr* (wptr canvas))
              grafport.pnvis) 0)
  nil)

(defun pen-hide (canvas)
  (setf (pref (or *hardcopy-ptr* (wptr canvas)) grafport.pnvis) -1)
  nil)

(defun pen-shown-p (canvas)
  (> (pref (or *hardcopy-ptr* (wptr canvas)) grafport.pnvis) -1))
  

(defun pen-position (canvas)
  (pref (or *hardcopy-ptr* (wptr canvas)) windowRecord.pnloc))

(defun pen-size (canvas)
  (pref (or *hardcopy-ptr* (wptr canvas)) windowRecord.pnsize))

(defun set-pen-size (canvas h &optional v &aux (pt (ccl::make-point h v)))
  (with-port (or *hardcopy-ptr* (wptr canvas))
    (#_PenSize :long pt))
  pt)

(defun pen-mode (canvas)
  (elt *pen-modes* (pref (or *hardcopy-ptr* (wptr canvas)) windowRecord.pnmode)))

(defun set-pen-mode (canvas new-mode)
  (with-port (or *hardcopy-ptr* (wptr canvas))
    (#_PenMode (mode-arg new-mode))))

(defun pen-pattern (canvas &optional
                        (save-pat (make-record (:pattern :storage :pointer))))
  (copy-record
   (pref (or *hardcopy-ptr* (wptr canvas))
         windowRecord.pnPat) (:pattern :storage :pointer) save-pat))

(defun set-pen-pattern (canvas new-pattern)
  (with-port (or *hardcopy-ptr* (wptr canvas))
    (#_PenPat new-pattern))
  new-pattern)


(defun set-pen-color (canvas new-color)
  (ccl::set-fore-color canvas new-color)
  new-color)
; ----------------------------

(defun pen-state (canvas &optional (save-state (make-record :penstate)))
  (declare (ignore canvas))
  (#_GetPenState save-state)
  save-state)

(defun set-pen-state (canvas new-state)
  (declare (ignore canvas))
  (#_SetPenState new-state)
  new-state)

(defun pen-normal (canvas)
  (declare (ignore canvas))
  (#_PenNormal))

(defun move-to (canvas h &optional v)
  (declare (ignore canvas))
  (#_MoveTo :long (setq h (ccl::make-point h v)))
  h)

(defun move (canvas h &optional v)
  (declare (ignore canvas))
  (#_Move :long (setq h (ccl::make-point h v)))
  h)

(defun line-to (canvas h &optional v)
  (declare (ignore canvas))
  (#_LineTo :long (setq h (ccl::make-point h v)))
  h)

(defun line (canvas h &optional v)
  (declare (ignore canvas))
  (#_Line :long (setq h (ccl::make-point h v)))
  h)

(defun offset-rect (rect h &optional v)
  (#_OffsetRect :ptr rect :long (ccl::make-point h v))
  rect)

(defun inset-rect (rect h &optional v)
  (#_InsetRect :ptr rect :long (ccl::make-point h v))
  rect)

(defun intersect-rect (rect1 rect2 dest-rect)
  (#_SectRect rect1 rect2 dest-rect)
  dest-rect)

(defun union-rect (rect1 rect2 dest-rect)
  (#_UnionRect rect1 rect2 dest-rect)
  dest-rect)

(defun point-in-rect-p (rect h &optional v)
  (#_PtInRect (ccl::make-point h v) rect))

(defun points-to-rect (point1 point2 dest-rect)
  (#_Pt2Rect (ccl::make-point point1 nil) (ccl::make-point point2 nil) dest-rect)
  dest-rect)

(defun point-to-angle (rect h &optional v)
  (%stack-block ((ip 4))
    (#_PtToAngle rect (ccl::make-point h v) ip)
    (%get-word ip)))

(defun equal-rect (rect1 rect2)
  (#_EqualRect rect1 rect2))

(defun empty-rect-p (left &optional top right bot)
  (with-rectangle-arg (r left top right bot)
    (#_EmptyRect r )))

(defun frame-rect (canvas left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot) (#_FrameRect r )))

(defun paint-rect (canvas left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot) (#_PaintRect r)))

(defun erase-rect (canvas left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot) (#_EraseRect r)))

(defun invert-rect (canvas left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot) (#_InvertRect r)))

(defun fill-rect (canvas pattern left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot)
    (#_FillRect r pattern)))

(defun frame-oval (canvas left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot) (#_FrameOval r)))

(defun paint-oval (canvas left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot) (#_PaintOval r)))

(defun erase-oval (canvas left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot) (#_EraseOval r)))

(defun invert-oval (canvas left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot) (#_InvertOval r)))

(defun fill-oval (canvas pattern left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot)
    (#_FillOval r pattern)))

(defun frame-round-rect (canvas oval-width oval-height 
                                left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot)
    (#_FrameRoundRect r oval-width oval-height)))

(defun paint-round-rect (canvas oval-width oval-height 
                                left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot)
    (#_PaintRoundRect r oval-width oval-height)))

(defun erase-round-rect (canvas oval-width oval-height 
                                left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot)
    (#_EraseRoundRect r oval-width oval-height)))

(defun invert-round-rect (canvas oval-width oval-height 
                                 left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot)
    (#_InvertRoundRect r oval-width oval-height)))

(defun fill-round-rect (canvas pattern oval-width oval-height 
                               left &optional top right bot)
  (declare (ignore canvas))
  (with-rectangle-arg (r left top right bot)
    (#_FillRoundRect r oval-width oval-height pattern)))
;;;
;;;  Arcs
;;;

;;;
;;;  The following exists because of Macintosh's peculiar
;;;  orientation of angles
;;; ... rwo

(defun standard-to-mac-angle (theta-in-degrees)
  "Transforms the argument from the standard position (0 = positive x-axis; ~
   90 = positive y-axis) to Macintosh angle position (0 = positive y-axis; ~
   90 = positive x-axis)."
  (let (new-angle)
    (if (< theta-in-degrees 180)
      (setf new-angle (- 90 theta-in-degrees))
      (setf new-angle (- 450 theta-in-degrees)))
    (when (< new-angle 0)
      (setf new-angle (+ 360 new-angle)))
    new-angle))


;;;
;;;  The following exists because we prefer to work with radii
;;; ... rwo
(defun radii-to-rect (x-centre y-centre x-radius y-radius)
  "From the ellipse defined by x-centre y-centre x-radius y-radius ~
   computes and returns ~
   multiple-values left top right bottom giving the coordinate ~
   information on the enclosing rectangle of the ellipse."
  (let (left top right bottom)
    (setf left (- x-centre x-radius))
    (setf top (- y-centre y-radius))            ;inverted y coord system
    (setf right (+ x-centre x-radius))
    (setf bottom (+ y-centre y-radius))         ;inverted y coord system
    (values left top right bottom)))


(defun draw-arc (canvas start-angle arc-angle
                        x-centre y-centre x-radius y-radius)
  (declare (ignore canvas))
  (multiple-value-bind
    (left top right bottom)
    (radii-to-rect x-centre y-centre x-radius y-radius)
    (with-rectangle-arg (r left top right bottom)
      (#_FrameArc r (standard-to-mac-angle start-angle) (- arc-angle)))))

(defun fill-arc (canvas start-angle arc-angle 
                        x-centre y-centre x-radius y-radius)
  (declare (ignore canvas))
  (multiple-value-bind
    (left top right bottom)
    (radii-to-rect x-centre y-centre x-radius y-radius)
    (with-rectangle-arg (r left top right bottom)
      (#_PaintArc r (standard-to-mac-angle start-angle) (- arc-angle)))))

(defun erase-arc (canvas start-angle arc-angle 
                        x-centre y-centre x-radius y-radius)
  (declare (ignore canvas))
  (multiple-value-bind
    (left top right bottom)
    (radii-to-rect x-centre y-centre x-radius y-radius)
    (with-rectangle-arg (r left top right bottom)
      (#_EraseArc r (standard-to-mac-angle start-angle) (- arc-angle)))))

(defun invert-arc (canvas start-angle arc-angle 
                        x-centre y-centre x-radius y-radius)
  (declare (ignore canvas))
  (multiple-value-bind
    (left top right bottom)
    (radii-to-rect x-centre y-centre x-radius y-radius)
    (with-rectangle-arg (r left top right bottom)
      (#_InvertArc r (standard-to-mac-angle start-angle) (- arc-angle)))))
;;;Regions

(defun new-region ()
  (#_NewRgn))

(defun dispose-region (region)
  (#_DisposeRgn region))

(defun copy-region (region &optional (dest-region (new-region)))
  (#_CopyRgn region dest-region)
  dest-region)

(defun set-empty-region (region)
  (#_SetEmptyRgn region)
  region)

(defun set-rect-region (region left &optional top right bot)
  (with-rectangle-arg (r left top right bot)
    (#_RectRgn region r))
  region)

(defun open-region (canvas)
  (let ((wptr (or *hardcopy-ptr* (wptr canvas))))
    (unless (%null-ptr-p (pref wptr grafport.rgnSave))
      (quail-error "Region already open for window: ~a" canvas))
    (with-port wptr (#_OpenRgn))))

(defun close-region (canvas &optional (dest-region (new-region) dp))
  (let ((wptr (or *hardcopy-ptr* (wptr canvas))))
    (if (%null-ptr-p (pref wptr grafport.rgnSave))
      (progn 
        (if (not dp) (dispose-region dest-region))
        (quail-error "Region is not open for window: ~a" canvas)))
    (with-port wptr
      (#_CloseRgn dest-region)))
  dest-region)

(defun offset-region (region h &optional v)
  (#_OffsetRgn :ptr region :long (ccl::make-point h v))
  region)

(defun inset-region (region h &optional v)
  (#_InsetRgn :ptr region :long (ccl::make-point h v))
  region)

(defun intersect-region (region1 region2 &optional (dest-region (new-region)))
  (#_SectRgn region1 region2 dest-region)
  dest-region)

(defun union-region (region1 region2 &optional (dest-region (new-region)))
  (#_UnionRgn region1 region2 dest-region)
  dest-region)

(defun difference-region (region1 region2 &optional (dest-region (new-region)))
  (#_DiffRgn region1 region2 dest-region)
  dest-region)

(defun xor-region (region1 region2 &optional (dest-region (new-region)))
  (#_XorRgn region1 region2 dest-region)
  dest-region)

(defun point-in-region-p (region h &optional v)
  (#_PtInRgn (ccl::make-point h v) region))

(defun rect-in-region-p (region left &optional top right bot)
  (with-rectangle-arg (r left top right bot)
    (#_RectInRgn r region)))

(defun equal-region-p (region1 region2)
  (#_EqualRgn region1 region2))

(defun empty-region-p (region)
  (#_EmptyRgn region))

(defun frame-region (canvas region)
  (declare (ignore canvas))
  (#_FrameRgn region))

(defun paint-region (canvas region)
  (declare (ignore canvas))
  (#_PaintRgn region))

(defun erase-region (canvas region)
  (declare (ignore canvas))
  (#_EraseRgn region))

(defun invert-region (canvas region)
  (declare (ignore canvas))
  (#_InvertRgn region))

(defun fill-region (canvas pattern region)
  (declare (ignore canvas))
  (#_FillRgn region pattern))

;;;Pictures

(defun start-picture (canvas &optional left top right bottom)
  (with-macptrs (portrect)
    (let ((wptr (or *hardcopy-ptr* (wptr canvas))))
      (unless (%null-ptr-p (pref wptr windowRecord.picsave))
        (quail-error "A picture may not be started for window: ~a.
           since one is already started" canvas))
      (unless left (setq left (%setf-macptr portrect (pref wptr windowRecord.portrect)))))
    (with-rectangle-arg (r left top right bottom)
      (#_cliprect r)
      (setf (view-get canvas 'my-hPic) (#_OpenPicture r)))
    nil))

(defun get-picture (canvas)
  (let ((my-hPic (view-get canvas 'my-hPic))
        (wptr (or *hardcopy-ptr* (wptr canvas))))
    (if (and my-hPic (not (%null-ptr-p (pref wptr windowRecord.picSave))))
      (prog1
        my-hPic
        (with-port wptr (#_ClosePicture))
        (setf (view-get canvas 'my-hPic) nil))
      (quail-error "Picture for window: ~a is not started" canvas))))

(defun draw-picture (canvas picture &optional left top right bottom)
  (declare (ignore canvas))
  (cond ((not left)
         (setq left (href picture picture.picFrame.topleft)
               top (href picture picture.picFrame.bottomright)))
        ((pointerp left)
         ())  ;everythings fine
        ((and (not right)
              (not top))
         (setq top
               (add-points left
                           (subtract-points
                            (href picture picture.picframe.bottomright)
                            (href picture picture.picframe.topleft))))))
  (with-rectangle-arg (r left top right bottom)
    (#_DrawPicture picture r))
  picture)

(defun kill-picture (picture)
  (#_KillPicture picture))

(defun start-polygon (canvas)
  (let ((wptr (or *hardcopy-ptr* (wptr canvas))))
    (unless (%null-ptr-p (pref wptr windowRecord.polysave))
      (quail-error "A new polygon may not be started for window: ~a.
           since one is already started" canvas))
    (with-port wptr (setf (view-get canvas 'my-poly) (#_OpenPoly))))
  nil)

(defun get-polygon (canvas)
  (let ((my-poly (view-get canvas 'my-poly))
        (wptr (or *hardcopy-ptr* (wptr canvas))))
    (if (and my-poly (not (%null-ptr-p (pref wptr windowRecord.polysave))))
      (prog1
        my-poly
        (with-port wptr (#_ClosePoly))
        (setq my-poly nil))
      (quail-error "Polygon for window: ~a has not been started" canvas))))

(defun kill-polygon (polygon)
  (#_KillPoly polygon))

(defun offset-polygon (polygon h &optional v)
  (#_OffsetPoly :ptr polygon :long (ccl::make-point h v))
  polygon)

(defun frame-polygon (canvas polygon)
  (declare (ignore canvas))
  (#_FramePoly polygon))

(defun paint-polygon (canvas polygon)
  (declare (ignore canvas))
  (#_PaintPoly polygon))

(defun erase-polygon (canvas polygon)
  (declare (ignore canvas))
  (#_ErasePoly polygon))

(defun invert-polygon (canvas polygon)
  (declare (ignore canvas))
  (#_InvertPoly polygon))

(defun fill-polygon (canvas pattern polygon)
  (declare (ignore canvas))
  (#_FillPoly polygon pattern))



(defun local-to-global (canvas h &optional v)
  (declare (ignore canvas))
  (rlet ((p :point))
    (%put-long p (ccl::make-point h v))
    (#_LocalToGlobal p)
    (%get-long p)))

(defun global-to-local (canvas h &optional v)
  (declare (ignore canvas))
  (rlet ((p :point))
    (%put-long p (ccl::make-point h v))
    (#_GlobalToLocal p)
    (%get-long p)))

(defun get-pixel (canvas h &optional v)
  (setq h (ccl::make-point h v))
  (if (#_PtInRgn h 
       (pref (or *hardcopy-ptr* (wptr canvas))
             windowRecord.visrgn))
    (#_GetPixel :long h :boolean)))

(defun scale-point (source-rect dest-rect h &optional v)
  (rlet ((pt :point))
    (%put-long pt (ccl::make-point h v))
    (#_ScalePt pt source-rect dest-rect)
    (%get-long pt)))

(defun map-point (source-rect dest-rect h &optional v)
  (rlet ((pt :point))
    (%put-long pt (ccl::make-point h v))
    (#_MapPt pt source-rect dest-rect)
    (%get-long pt)))

(defun map-rect (source-rect dest-rect rect)
  (#_MapRect rect source-rect dest-rect)
  rect)

(defun map-region (source-rect dest-rect region)
  (#_MapRgn region source-rect dest-rect)
  region)

(defun map-polygon (source-rect dest-rect polygon)
  (#_MapPoly polygon source-rect dest-rect)
  polygon)

(defun make-bitmap (left &optional top right bottom &aux rowbytes bm)
  (with-rectangle-arg (r left top right bottom)
    (setq rowbytes 
          (logand
           #xfffe 
           (+ 2  (ash (- (pref r rect.right) (pref r rect.left) 1) -3))))
    (setq bm 
          (#_NewPtr :check-error
           (+ 14 (* rowbytes (- (pref r rect.bottom) (pref r rect.top))))))
    (setf (pref bm bitmap.bounds) r)
    (setf (pref bm bitmap.rowbytes) rowbytes)
    (setf (pref bm bitmap.baseaddr) (%inc-ptr bm 14)))
  bm)


(defun copy-bits (source-bitmap dest-bitmap source-rect dest-rect
                                &optional (mode 0) mask-region)
  (with-macptrs ((mask-region (if mask-region mask-region (%null-ptr))))
    (#_CopyBits source-bitmap
     dest-bitmap
     source-rect
     dest-rect
     (mode-arg mode)
     (or mask-region (%null-ptr)))))

(defun scroll-rect (canvas rect dh &optional dv)
  "Ignores any clipping regions"
  (declare (ignore canvas))
  (let* ((reg (#_newrgn)))
    (#_ScrollRect :ptr rect
     :long (ccl::make-point dh dv)
     :ptr reg)
    (#_invalrgn reg)
    (#_disposergn reg)))


(defun draw-string (canvas string)
  "Draws a string on the canvas at the current position with ~
   current font and colour."
  (let (font-face mode-size)
    (multiple-value-setq (font-face mode-size)
      (view-font-codes canvas))
    (with-font-codes font-face mode-size
      (with-pstrs ((pstring string))
        (#_DrawString pstring)))
    ))

(defun draw-char (canvas char)
  (draw-string canvas (string char)))


(provide 'host-draw)
(pushnew :host-draw *features*)
