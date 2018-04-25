;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               drag-clx.lisp
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
  
  (declare (special :boole-xor))
  (multiple-value-setq (left bottom width height)
    (calc-region canvas region left bottom width height))
  
  (if (eql :none axis) ;allow no motion
    (setq limit-region (make-region left bottom width height)))
  
  
  
  (let* ((old-mouse NIL)
         (new-mouse NIL)
         (top (canvas-to-host-y canvas
                                (+ bottom height)))
         (limit-left 
          (region-left limit-region))
         (limit-top 
          (canvas-to-host-y canvas (region-top limit-region)))
         (limit-bottom 
          (canvas-to-host-y canvas (region-bottom limit-region)))
         (limit-right 
          (region-right limit-region))
         (drawable (host-window canvas))
         (gcontext (gcontext canvas))
         left-lower left-upper
         ;;right-lower right-upper
         ;;bottom-lower bottom-upper
         top-lower top-upper
         )
    (flet ((host-mouse-position ()
             (multiple-value-bind
               (x y)
               (xlib::pointer-position drawable)
               (list x y)))
           )
      
      ;; Get width right
      ;;
      (setf width (min width (- limit-right limit-left)))
      ;; Get height right
      ;;
      (setf height (min height (- limit-bottom limit-top)))
      
      ;; bounds
      ;; Left
      (setf left-lower limit-left)
      (setf left-upper (max limit-left (- limit-right width)))
      
      ;; bounds
      ;; Right
      ;;(setf right-upper limit-right)
      ;;(setf right-lower (min limit-right (+ limit-left width)))
      
      ;; bounds
      ;; Top  ... remember we're in the upside down host mode
      (setf Top-lower limit-top)
      (setf Top-upper (max limit-top (- limit-top height)))
      
      ;; bounds
      ;; Bottom  ... remember we're in the upside down host mode
      ;;(setf bottom-upper limit-bottom)
      ;;(setf bottom-lower (min limit-bottom (+ limit-top height)))
      
      ;; starting position
      ;; Left
      (cond
       ((< left left-lower) (setf left left-lower))
       ((> left left-upper) (setf left left-upper)))
      
      ;; starting position
      ;; Top
      (cond
       ((< Top Top-lower) (setf Top Top-lower))
       ((> Top Top-upper) (setf Top Top-upper)))
      
      
      (setf old-mouse (host-mouse-position))
      (setf new-mouse (copy-tree old-mouse))
      ;;(setq the-rgn (h-draw::set-rect-region 
      ;;               (h-draw::new-region) top-left bottom-right))
      ;;(rlet ((limit-rect 
      ;;        :rect 
      ;;        :topleft 
      ;;        (add-points limit-top-left 
      ;;                    (subtract-points mouse-locn top-left))
      ;;        :bottomright 
      ;;        (subtract-points limit-right-bottom
      ;;                         (subtract-points bottom-right mouse-locn)))
      ;;       (slop-rect 
      ;;        :rect 
      ;;        :topleft (h-draw:make-point 0 0)
      ;;        :bottomright (view-size canvas))
      ;;      )
      
      (with-focused-canvas canvas
        (with-pen-values canvas NIL 1 :boole-xor
          
          ;; draw the rectangle
          (xlib::draw-rectangle drawable gcontext
                                left top
                                width height
                                )
          (ecase axis
            (:both
             (loop
               ;; until mouse button is released
               (unless (mouse-down-p canvas) (return))
               (unless (equal old-mouse new-mouse)
                 ;; erase rectangle
                 (xlib::draw-rectangle drawable gcontext
                                       left top
                                       width height
                                       )
                 ;; update to get new one
                 (setf left (max left-lower
                                 (min
                                  (first new-mouse)
                                  left-upper)))
                 (setf top (max top-lower
                                (min
                                 (second new-mouse)
                                 top-upper)))
                 ;; draw it
                 (xlib::draw-rectangle drawable gcontext
                                       left top
                                       width height
                                       )
                 ;; slow down
                 (sleep 1/60)
                 ;; update iteration
                 (setq old-mouse new-mouse))
               (setq new-mouse (host-mouse-position))
               )
             ;; Finally erase it.
             (xlib::draw-rectangle drawable gcontext
                                   left top
                                   width height
                                   )
             )
            (:x
             (loop
               ;; until mouse button is released
               (unless (mouse-down-p canvas) (return))
               (unless (equal old-mouse new-mouse)
                 ;; erase rectangle
                 (xlib::draw-rectangle drawable gcontext
                                       left top
                                       width height
                                       )
                 ;; update to get new one
                 (setf left (max left-lower
                                 (min
                                  (first new-mouse)
                                  left-upper)))
                 
                 ;; draw it
                 (xlib::draw-rectangle drawable gcontext
                                       left top
                                       width height
                                       )
                 ;; slow down
                 (sleep 1/60)
                 ;; update iteration
                 (setq old-mouse new-mouse))
               (setq new-mouse (host-mouse-position))
               )
             ;; Finally erase it.
             (xlib::draw-rectangle drawable gcontext
                                   left top
                                   width height
                                   )
             )
            (:y
             (loop
               ;; until mouse button is released
               (unless (mouse-down-p canvas) (return))
               (unless (equal old-mouse new-mouse)
                 ;; erase rectangle
                 (xlib::draw-rectangle drawable gcontext
                                       left top
                                       width height
                                       )
                 ;; update to get new one
                 
                 (setf top (max top-lower
                                (min
                                 (second new-mouse)
                                 top-upper)))
                 ;; draw it
                 (xlib::draw-rectangle drawable gcontext
                                       left top
                                       width height
                                       )
                 ;; slow down
                 (sleep 1/60)
                 ;; update iteration
                 (setq old-mouse new-mouse))
               (setq new-mouse (host-mouse-position))
               )
             ;; Finally erase it.
             (xlib::draw-rectangle drawable gcontext
                                   left top
                                   width height
                                   )
             )
            (:none
             (loop
               ;; until mouse button is released
               (unless (mouse-down-p canvas) (return))
               ;; erase rectangle
               (xlib::draw-rectangle drawable gcontext
                                     left top
                                     width height
                                     )
               ;; slow down
               (sleep 1/60)
               ;; draw it
               (xlib::draw-rectangle drawable gcontext
                                     left top
                                     width height
                                     )
               ;; slow down
               (sleep 1/60)
               )
             )
            )
          ;; Finally erase it.
          (xlib::draw-rectangle drawable gcontext
                                left top
                                width height
                                )
          )
        )
      (make-region left
                   (host-to-canvas-y canvas
                                     (+ top height))
                   width height)
      
      ;;(format *terminal-io* "~&drag-region-on-canvas unimplemented in X~%")
      ;;(make-region (+ left 10) (- bottom 10) width height)
      )))



 
(defun drag-region-on-screen
       (canvas &key region left bottom (width 50) (height 50) (axis :both)
               (limit-region NIL))
  
  "With mouse already depressed, ~
   drags a region around following mouse position.   ~
   If left or bottom are not provided, the mouse position is used instead.  ~
   Axis = :x or :y limits motion to x or y axis respectively, :none => no motion. ~
   Returns a region."
  
  (declare (special :boole-xor))
  (unless limit-region (setf limit-region (screen-region)))
  (multiple-value-setq (left bottom width height)
    (calc-region canvas region left bottom width height))
  
  (if (eql :none axis) ;allow no motion
    (setq limit-region (make-region left bottom width height)))
  
  
  
  (let* ((root-window
          (third
           (multiple-value-list
            (xlib::global-pointer-position *default-display*))))
         (old-mouse NIL)
         (new-mouse NIL)
         (top (screen-to-host-y (+ bottom height)))
         (limit-left 
          (region-left limit-region))
         (limit-top 
          (screen-to-host-y (region-top limit-region)))
         (limit-bottom 
          (screen-to-host-y (region-bottom limit-region)))
         (limit-right 
          (region-right limit-region))
         (drawable root-window)
         (gcontext (xlib:create-gcontext
                    :drawable drawable))
         left-lower left-upper
         ;;right-lower right-upper
         ;;bottom-lower bottom-upper
         top-lower top-upper
         )
    (flet ((host-mouse-position ()
             (multiple-value-bind
               (x y)
               (xlib::global-pointer-position *default-display*)
               (list x y)))
           (screen-mouse-down-p ()
             (multiple-value-bind 
               (a b c d state)
               (xlib::query-pointer root-window)
               (host-to-canvas-mouse-state state)))
           )
      
      ;; Get width right
      ;;
      (setf width (min width (- limit-right limit-left)))
      ;; Get height right
      ;;
      (setf height (min height (- limit-bottom limit-top)))
      
      ;; bounds
      ;; Left
      (setf left-lower limit-left)
      (setf left-upper (max limit-left (- limit-right width)))
      
      ;; bounds
      ;; Right
      ;;(setf right-upper limit-right)
      ;;(setf right-lower (min limit-right (+ limit-left width)))
      
      ;; bounds
      ;; Top  ... remember we're in the upside down host mode
      (setf Top-lower limit-top)
      (setf Top-upper (max limit-top (- limit-top height)))
      
      ;; bounds
      ;; Bottom  ... remember we're in the upside down host mode
      ;;(setf bottom-upper limit-bottom)
      ;;(setf bottom-lower (min limit-bottom (+ limit-top height)))
      
      ;; starting position
      ;; Left
      (cond
       ((< left left-lower) (setf left left-lower))
       ((> left left-upper) (setf left left-upper)))
      
      ;; starting position
      ;; Top
      (cond
       ((< Top Top-lower) (setf Top Top-lower))
       ((> Top Top-upper) (setf Top Top-upper)))
      
      
      (setf old-mouse (host-mouse-position))
      (setf new-mouse (copy-tree old-mouse))
      ;;(setq the-rgn (h-draw::set-rect-region 
      ;;               (h-draw::new-region) top-left bottom-right))
      ;;(rlet ((limit-rect 
      ;;        :rect 
      ;;        :topleft 
      ;;        (add-points limit-top-left 
      ;;                    (subtract-points mouse-locn top-left))
      ;;        :bottomright 
      ;;        (subtract-points limit-right-bottom
      ;;                         (subtract-points bottom-right mouse-locn)))
      ;;       (slop-rect 
      ;;        :rect 
      ;;        :topleft (h-draw:make-point 0 0)
      ;;        :bottomright (view-size canvas))
      ;;      )
      
      (with-gcontext (gcontext :function boole-xor)
        
        ;; draw the rectangle
        (xlib::draw-rectangle drawable gcontext
                              left top
                              width height
                              )
        (ecase axis
          (:both
           (loop
             ;; until mouse button is released
             (unless (screen-mouse-down-p) (return))
             (unless (equal old-mouse new-mouse)
               ;; erase rectangle
               (xlib::draw-rectangle drawable gcontext
                                     left top
                                     width height
                                     )
               ;; update to get new one
               (setf left (max left-lower
                               (min
                                (first new-mouse)
                                left-upper)))
               (setf top (max top-lower
                              (min
                               (second new-mouse)
                               top-upper)))
               ;; draw it
               (xlib::draw-rectangle drawable gcontext
                                     left top
                                     width height
                                     )
               ;; slow down
               (sleep 1/60)
               ;; update iteration
               (setq old-mouse new-mouse))
             (setq new-mouse (host-mouse-position))
             )
           ;; Finally erase it.
           (xlib::draw-rectangle drawable gcontext
                                 left top
                                 width height
                                 )
           )
          (:x
           (loop
             ;; until mouse button is released
             (unless (screen-mouse-down-p) (return))
             (unless (equal old-mouse new-mouse)
               ;; erase rectangle
               (xlib::draw-rectangle drawable gcontext
                                     left top
                                     width height
                                     )
               ;; update to get new one
               (setf left (max left-lower
                               (min
                                (first new-mouse)
                                left-upper)))
               
               ;; draw it
               (xlib::draw-rectangle drawable gcontext
                                     left top
                                     width height
                                     )
               ;; slow down
               (sleep 1/60)
               ;; update iteration
               (setq old-mouse new-mouse))
             (setq new-mouse (host-mouse-position))
             )
           ;; Finally erase it.
           (xlib::draw-rectangle drawable gcontext
                                 left top
                                 width height
                                 )
           )
          (:y
           (loop
             ;; until mouse button is released
             (unless (screen-mouse-down-p) (return))
             (unless (equal old-mouse new-mouse)
               ;; erase rectangle
               (xlib::draw-rectangle drawable gcontext
                                     left top
                                     width height
                                     )
               ;; update to get new one
               
               (setf top (max top-lower
                              (min
                               (second new-mouse)
                               top-upper)))
               ;; draw it
               (xlib::draw-rectangle drawable gcontext
                                     left top
                                     width height
                                     )
               ;; slow down
               (sleep 1/60)
               ;; update iteration
               (setq old-mouse new-mouse))
             (setq new-mouse (host-mouse-position))
             )
           ;; Finally erase it.
           (xlib::draw-rectangle drawable gcontext
                                 left top
                                 width height
                                 )
           )
          (:none
           (loop
             ;; until mouse button is released
             (unless (screen-mouse-down-p) (return))
             ;; erase rectangle
             (xlib::draw-rectangle drawable gcontext
                                   left top
                                   width height
                                   )
             ;; slow down
             (sleep 1/60)
             ;; draw it
             (xlib::draw-rectangle drawable gcontext
                                   left top
                                   width height
                                   )
             ;; slow down
             (sleep 1/60)
             )
           )
          )
        ;; Finally erase it.
        (xlib::draw-rectangle drawable gcontext
                              left top
                              width height
                              )
        )
      
      (xlib:free-gcontext gcontext)
      )
    (make-region left
                 (host-to-screen-y (+ top height))
                 width height)
    
    ;;(format *terminal-io* "~&drag-region-on-canvas unimplemented in X~%")
    ;;(make-region (+ left 10) (- bottom 10) width height)
    ))
  
  




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
  ;;(format *terminal-io* "~&select-canvas-rect not implemented in X~%")
  ;;(values 0 0 10 10)
  (loop until (mouse-down-p canvas))
  (let* ((anchor-point NIL)
         (old-mouse NIL)
         (new-mouse NIL)
         (left NIL)
         (top NIL)
         (drawable (host-window canvas))
         (gcontext (gcontext canvas)))
    (flet ((host-mouse-position ()
             (multiple-value-bind
               (x y)
               (xlib::pointer-position drawable)
               (list x y)))
           )
      (setq limit-region (or limit-region (canvas-region canvas)))
      (setf old-mouse (host-mouse-position))
      (setf anchor-point
            (list
             (max (- (first old-mouse) width)
                  (region-left limit-region))
             (max (- (second old-mouse) height)
                  (canvas-to-host-y canvas
                                    (region-top limit-region)))
             ))
      (setf new-mouse (copy-tree old-mouse))
      (setf left (first anchor-point))
      (setf top (second anchor-point))
      
      (with-focused-canvas canvas
        (with-pen-values canvas *gray-shade* 1 :boole-xor
          ;; draw the rectangle
          (xlib::draw-rectangle drawable gcontext
                                left top
                                width height
                                )
          (loop
            (unless (mouse-down-p canvas) (return))    ;return when the mouse lets up
            (unless (equal old-mouse new-mouse)
              ;; erase rectangle
              (xlib::draw-rectangle drawable gcontext
                                    left top
                                    width height
                                    )
              ;; update to get new one
              (cond
               ((> (first new-mouse) (first anchor-point))
                (setf left (first anchor-point))  
                (setf width (- (first new-mouse) left)))
               (T
                (setf left (first new-mouse))
                (setf width (- (first anchor-point) left))))
              (cond
               ((> (second new-mouse) (second anchor-point))
                (setf top (second anchor-point))  
                (setf height (- (second new-mouse) top)))
               (T
                (setf top (second new-mouse))
                (setf height (- (second anchor-point) top))))
             
              ;; draw it
              (xlib::draw-rectangle drawable gcontext
                                    left top
                                    width height
                                    )
              ;; slow down
              ;;(sleep 1/60)
              ;; update iteration
              (setq old-mouse new-mouse))
            (setq new-mouse (host-mouse-position))
            )
          ;; Finally erase it.
          (xlib::draw-rectangle drawable gcontext
                                left top
                                width height
                                )
          )
        )
      (values left
              (- (canvas-to-host-y canvas top) height)
              ;;(+ top height)
              width height)
      )))


(defun select-screen-rect ( &optional (width 50) (height 50) limit-region)
  "Mouse-selects a rectangle on the screen by selecting a location on the~
   screen with the mouse and ~
   sweeping out the region to be selected.  
   The rectangle selected is confined to limit-region ~
   which defaults to the boundary of the screen. ~
   Returns the region information as ~
   multiple values (left bottom width height) in ~
   screen coordinates."
  ;;(format *terminal-io8 "~&select-screen-rec not implemented in X~%")
  ;;(values 0 0 10 10)
  ;;)
  (let* ((root-window
          (third
           (multiple-value-list
            (xlib::global-pointer-position *default-display*))))
         (anchor-point NIL)
         (old-mouse NIL)
         (new-mouse NIL)
         (left NIL)
         (top NIL)
         (drawable root-window)
         (gcontext (xlib:create-gcontext
                    :drawable drawable))
         )
    
    (flet ((host-mouse-position ()
             (multiple-value-bind
               (x y)
               (xlib::global-pointer-position *default-display*)
               (list x y)))
           (screen-mouse-down-p ()
             (multiple-value-bind 
               (a b c d state)
               (xlib::query-pointer root-window)
               (host-to-canvas-mouse-state state)))
           )
      (setq limit-region (or limit-region (screen-region)))
      (loop until (screen-mouse-down-p))
      (setf old-mouse (host-mouse-position))
      (setf anchor-point
            (list
             (max (- (screen-mouse-x) width)
                  (region-left limit-region))
             (max (screen-to-host-y  (+ (screen-mouse-y) height))
                  (screen-to-host-y (region-top limit-region)))
             ))
      (setf new-mouse (copy-tree old-mouse))
      (setf left (first anchor-point))
      (setf top (second anchor-point))
      (xlib:with-gcontext (gcontext :function boole-xor)
        ;; draw the rectangle
        (xlib::draw-rectangle drawable gcontext
                              left top
                              width height
                              )
        (loop
          (unless (screen-mouse-down-p) (return))    ;return when the mouse lets up
          (unless (equal old-mouse new-mouse)
            ;; erase rectangle
            (xlib::draw-rectangle drawable gcontext
                                  left top
                                  width height
                                  )
            ;; update to get new one
              (cond
               ((> (first new-mouse) (first anchor-point))
                (setf left (first anchor-point))  
                (setf width (- (first new-mouse) left)))
               (T
                (setf left (first new-mouse))
                (setf width (- (first anchor-point) left))))
              (cond
               ((> (second new-mouse) (second anchor-point))
                (setf top (second anchor-point))  
                (setf height (- (second new-mouse) top)))
               (T
                (setf top (second new-mouse))
                (setf height (- (second anchor-point) top))))
            ;; draw it
            (xlib::draw-rectangle drawable gcontext
                                  left top
                                  width height
                                  )
            ;; slow down
            ;;(sleep 1/60)
            ;; update iteration
            (setq old-mouse new-mouse))
          (setq new-mouse (host-mouse-position))
          )
        ;; Finally erase it.
        (xlib::draw-rectangle drawable gcontext
                              left top
                              width height
                              )
        )
      (xlib:free-gcontext gcontext)
      
      (values left (- (screen-to-host-y top) height) width height)
      ))
  )



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
