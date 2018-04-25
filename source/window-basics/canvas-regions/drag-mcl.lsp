;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               drag-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1992
;;;     M.E. Lewis 1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(drag-region-on-canvas drag-region-on-screen
           select-rectangle
           select-canvas-rect select-screen-rect
           prompt-for-canvas 
           sweep-for-canvas reshape-canvas-rect)))

(defun calc-region (canvas &optional region left bottom width height)
  "Returns left bottom, width height as multiple values ~
   if region is provided, its left bottom etc are used ~
   if left or bottom are not provided, the mouse position is used instead."

 (when region
    (setq left (region-left region))
    (setq bottom (region-bottom region))
    (setq height (region-height region))
    (setq width (region-width region)))
  
  (setq left (or left (- (if canvas (mouse-x canvas) (screen-mouse-x)) width)))
  (setq bottom (or bottom (if canvas (mouse-y canvas) (screen-mouse-y))))
  (values left bottom width height))



(defun drag-region-on-canvas
       (canvas &key region (limit-region (canvas-region canvas))
               left bottom (width 50) (height 50) (axis :both))
  "With mouse already depressed, ~
   drags a region around following mouse position.   ~
   If left or bottom are not provided, the mouse position is used instead.  ~
   Axis = :x or :y limits motion to x or y axis respectively. ~
   Returns a region."
  
  (declare (special *gray-shade*))
  (multiple-value-setq (left bottom width height)
    (calc-region canvas region left bottom width height))
  
  (if (eql :none axis) ;allow no motion
    (setq limit-region (make-region left bottom width height)))
  
  (setq axis
        (ecase axis
          (:both 0) (:x 1)
          (:y 2) (:none 0)))
  
  (let* ((mouse-locn (view-mouse-position (the-scroller canvas)))
         (top-left (h-draw:make-point left (canvas-to-host-y canvas (+ bottom height))))
         (bottom-right (add-points top-left (h-draw:make-point width height)))
         result the-rgn 
         (limit-top-left 
          (h-draw:make-point (region-left limit-region)
                      (canvas-to-host-y canvas (region-top limit-region))))
         (limit-right-bottom 
          (h-draw:make-point 
           (region-right limit-region)
           (canvas-to-host-y canvas (region-bottom limit-region))))
         (actionproc (%null-ptr)))
    
    (setq the-rgn (h-draw::set-rect-region 
                   (h-draw::new-region) top-left bottom-right))
    (rlet ((limit-rect 
            :rect 
            :topleft 
            (add-points limit-top-left 
                        (subtract-points mouse-locn top-left))
            :bottomright 
            (subtract-points limit-right-bottom
                             (subtract-points bottom-right mouse-locn)))
           (slop-rect 
            :rect 
            :topleft (h-draw:make-point 0 0)
            :bottomright (view-size canvas))
           )
      (with-pen-values canvas *gray-shade* 1 NIL
        (setq result
              (with-focused-canvas canvas
                (#_DragGrayRgn :ptr the-rgn
                 :long mouse-locn
                 :ptr limit-rect
                 :ptr slop-rect
                 :word axis
                 :ptr actionproc :long) 
                ))))
    (dispose-record the-rgn) 
    (make-region (+ left (h-draw:point-x result))
                 (- bottom (h-draw:point-y result))
                 width height))
  )



 
(defun drag-region-on-screen
       (canvas &key region left bottom (width 50) (height 50) (axis :both)
               (limit-region (screen-region)))

  "With mouse already depressed, ~
   drags a region around following mouse position.   ~
   If left or bottom are not provided, the mouse position is used instead.  ~
   Axis = :x or :y limits motion to x or y axis respectively, :none => no motion. ~
   Returns a region."

  (declare (special *gray-shade*))

  (multiple-value-setq (left bottom width height)
    (calc-region canvas region left bottom width height))

  (setq left (+ left (screen-x canvas)))
  (setq bottom (+ bottom (screen-y canvas)))

  (if (eql :none axis) ;allow no motion
    (setq limit-region (make-region left bottom width height)))

  (setq axis
        (ecase axis
          (:both 0) (:x 1)
          (:y 2) (:none 0)))

  (let* ((mouse-locn (h-draw:make-point (screen-mouse-x)
                                 (screen-to-host-y (screen-mouse-y))))
         (top-left (h-draw:make-point left (screen-to-host-y (+ bottom height) )
))
         (bottom-right (add-points top-left (h-draw:make-point width height)))
         (limit-top-left
          (h-draw:make-point (region-left limit-region)
                      (screen-to-host-y (region-top limit-region) )))
         (limit-right-bottom
          (h-draw:make-point
           (region-right limit-region)
           (screen-to-host-y (region-bottom limit-region) )))
         result the-rgn
         (screen (screen-port))
         (actionproc (%null-ptr)))
    (setq the-rgn (h-draw::set-rect-region
                   (h-draw::new-region) top-left bottom-right))
    (rlet ((limit-rect
            :rect
            :topleft limit-top-left
            :bottomright limit-right-bottom)
           (slop-rect
            :rect
            :topleft (h-draw:make-point 0 0)
            :bottomright (h-draw:make-point (screen-width) (screen-height)) )
           )
      (rset screen windowRecord.pnPat *gray-pattern*)
      (setq result
            (with-port screen
              (#_DragGrayRgn
               :ptr the-rgn
               :long mouse-locn
               :ptr limit-rect
               :ptr slop-rect
               :word axis
               :ptr actionproc :long) )))
    (dispose-record the-rgn)
    (make-region (+ left (h-draw:point-x result))
                 (- bottom (h-draw:point-y result))
                 width height)
    )
  )




(defun select-rectangle (&key canvas (width 10) (height 10) limit-region)
  "Mouse-selects a rectangle on the canvas by selecting in canvas and ~
   sweeping out the region to be selected.  ~
   If no canvas is supplied the action is taken on the screen in ~
   screen coordinates.  ~
   The rectangle selected is confined to limit-region ~
   which defaults to the boundary of the canvas or screen. ~
   Returns the region information as ~
   multiple values (left bottom width height) in canvas ~
   (or screen) coordinates."
  (if canvas
    (select-canvas-rect canvas width height limit-region)
    (select-screen-rect width height limit-region)))

(defun select-canvas-rect (canvas &optional (width 0) (height 0)
                                  limit-region)
  "Mouse-selects a rectangle on the canvas by selecting in canvas and ~
   sweeping out the region to be selected.  ~
   The rectangle selected is confined to limit-region ~
   which defaults to the boundary of the canvas. ~
   Returns the region information as ~
   multiple values (left bottom width height) in canvas ~
   coordinates."

  (declare (special *gray-shade* :boole-xor))
  (setq limit-region (or limit-region (canvas-region canvas)))
  (loop until (mouse-down-p))
  (let* ((anchor-point
          (h-draw:make-point (- (mouse-x canvas) width)
                      (canvas-to-host-y canvas (+ (mouse-y canvas) height))))
         (old-mouse (view-mouse-position canvas))
         (new-mouse old-mouse)
         (limit-top-left
          (h-draw:make-point (region-left limit-region)
                      (canvas-to-host-y canvas (region-top limit-region))))
         (limit-right-bottom
          (h-draw:make-point
           (region-right limit-region)
           (canvas-to-host-y canvas (region-bottom limit-region)))))

    (rlet ((r :rect)
           (limit-rect
            :rect
            :topleft limit-top-left
            :bottomright limit-right-bottom))
      (setq new-mouse
                (#_pinrect :ptr limit-rect :long new-mouse))
      (setq anchor-point
                (#_pinrect :ptr limit-rect :long anchor-point))
      (with-focused-canvas canvas
        (with-pen-values canvas *gray-shade* 1 :boole-xor
          (#_pt2rect :long anchor-point :long new-mouse :ptr r)
          (#_FrameRect :ptr r)
          (loop
            (unless (mouse-down-p) (return))     ;return when the mouse lets up
            (unless (eq old-mouse new-mouse)
              (#_FrameRect :ptr r)
              (#_pt2rect :long anchor-point :long new-mouse :ptr r)
              (#_FrameRect :ptr r)
              (sleep 1/60)
              (setq old-mouse new-mouse))
            (setq new-mouse (view-mouse-position (the-scroller canvas)))
            (setq new-mouse
                (#_pinrect :ptr limit-rect :long new-mouse)))

          (#_FrameRect :ptr r)
          )
        )
      (values (rref r rect.left)
              (canvas-to-host-y canvas (rref r rect.bottom))
              (- (rref r rect.right) (rref r rect.left))
              (- (rref r rect.bottom) (rref r rect.top)))
      )))

(defun select-screen-rect ( &optional (width 50) (height 50) limit-region)
  "Mouse-selects a rectangle on the screen by selecting a location on the~
   screen with the mouse and ~
   sweeping out the region to be selected.  ~
   The rectangle selected is confined to limit-region ~
   which defaults to the boundary of the screen. ~
   Returns the region information as ~
   multiple values (left bottom width height) in ~
   screen coordinates."

  (setq limit-region (or limit-region (screen-region)))
  (loop until (mouse-down-p))
  (let* ((anchor-point
          (h-draw:make-point (- (screen-mouse-x) width)
                      (screen-to-host-y (+ (screen-mouse-y) height))))
         (old-mouse
          (h-draw:make-point (screen-mouse-x)
                      (screen-to-host-y (screen-mouse-y))))
         (wptr (screen-port))
         (new-mouse old-mouse)
         (limit-top-left
          (h-draw:make-point (region-left limit-region)
                      (screen-to-host-y (region-top limit-region) )))
         (limit-right-bottom
          (h-draw:make-point
           (region-right limit-region)
           (screen-to-host-y (region-bottom limit-region) ))))

    (rlet ((r :rect)
           (limit-rect
            :rect
            :topleft limit-top-left
            :bottomright limit-right-bottom)
           (old-pen-state :penstate))

      (setq new-mouse
                (#_pinrect :ptr limit-rect :long new-mouse))
      (setq anchor-point
                (#_pinrect :ptr limit-rect :long anchor-point))
      (with-port wptr
        (#_GetPenState :ptr old-pen-state)
        (#_PenMode :word (position :patXor *pen-modes*))
        (rset wptr windowRecord.pnPat *gray-pattern*)

        (#_pt2rect :long anchor-point :long new-mouse :ptr r)
        (#_FrameRect :ptr r))
      (loop
        (unless (mouse-down-p) (return))     ;return when the mouse lets up
        (unless (eq old-mouse new-mouse)
          (with-port wptr
            (#_FrameRect :ptr r)
            (#_pt2rect :long anchor-point :long new-mouse :ptr r)
            (#_FrameRect  :ptr r)
            (sleep 1/60)
            (setq old-mouse new-mouse))
          )
        (setq new-mouse
              (h-draw:make-point (screen-mouse-x)
                          (screen-to-host-y (screen-mouse-y))))
        (setq new-mouse
                (#_pinrect :ptr limit-rect :long new-mouse)))
      (with-port wptr
        (#_FrameRect :ptr r)
        (#_SetPenState :ptr old-pen-state))

      (values (rref r rect.left)
              (screen-to-host-y (rref r rect.bottom))
              (- (rref r rect.right) (rref r rect.left))
              (- (rref r rect.bottom) (rref r rect.top)))

      )))


(defun prompt-for-canvas (&rest args)
  "Prompts user to sweep out a region on the screen to determine the ~
   size and location of the canvas which will be returned."
  ;;(declare (special *box-cursor*))
  (quail-print "Sweep out a region for the canvas with the mouse.")
  ;;(with-cursor *box-cursor*
    (apply #'sweep-for-canvas args))
  ;;)

(defun sweep-for-canvas (&rest args)
  "Sweep out a region on the screen to determine the ~
   size and location of the canvas which will be returned."
  (multiple-value-bind (left bottom width height)
                       (select-rectangle)
    (apply #'make-canvas :left left :bottom bottom
                         :width width :height height
                         args)))

(defun reshape-canvas-rect (canvas rect &key (draw? t) limit-region (corner-width 10))
  "Reshapes a rectangle on the canvas using the mouse ~
   to relocate the corner of rectangle closest to the mouse. ~
   The selected corner should be within corner-width ~
   (horizontally and vertically) of the mouse location.~
   The rectangle selected is confined to limit-region ~
   which defaults to the boundary of the canvas. ~
   Returns the region information as ~
   multiple values (left bottom width height) in canvas ~
   coordinates."

  (let ((d corner-width)
        mouse-x mouse-y
        vl vb vr vt
        fix-x fix-y move-x move-y)
    (multiple-value-setq (vl vr vb vt) (region-bounds rect))
    (if draw?
      (draw-rect&corners canvas rect :corner-width d))

    (loop until (mouse-down-p))
    (setq mouse-x (mouse-x canvas) mouse-y (mouse-y canvas))

    (if draw?
      (erase-rect&corners canvas rect :corner-width d))
    (if (< mouse-x (truncate (+ vl vr) 2))
      (setq move-x vl fix-x vr)
      (setq move-x vr fix-x vl))

    (if (< mouse-y (truncate (+ vb vt) 2))
      (setq move-y vb fix-y vt)
      (setq move-y vt fix-y vb))

    (if (and (< (abs (- mouse-x move-x)) d)
             (< (abs (- mouse-y move-y)) d))

      (select-rectangle :canvas canvas
                        :limit-region limit-region
                        :width (- mouse-x fix-x)
                        :height (- fix-y mouse-y )))))
