;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      drag-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1992
;;;     M.E. Lewis 1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     G.W. Bennett 1996
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
     (setq left (or left (if canvas (mouse-x canvas) (screen-mouse-x))))
     (setq bottom (or bottom (- (if canvas (mouse-y canvas)
                                                   (screen-mouse-y))
                                                height)))
     (values left bottom width height))

(defun drag-region-on-canvas
      (canvas &key region (limit-region (canvas-region canvas))
         left bottom (width 50) (height 50) (axis :both))
     "With mouse already depressed, ~
drags a region around following mouse position.   ~
If left or bottom are not provided, the mouse position is used instead.  ~
Axis = :x or :y limits motion to x or y axis respectively. ~
Returns a region."
     ;; bounds for top ... remember we're in the upside down host mode
     ;; which means that a zero is the top of the screen and
     ;; 600 is its bottom ...  
     ;; AND we are drawing with the cursor at the bottom-right of
     ;; little region we are dragging about.
     ;; SO .. the minimum value for the bottom-right is
     ;; the top of the screen (0) + the height of the region
     ;; otherwise the region's top will poke above the top of the
     ;; screen ..  
     ;; and the maximum value for the bottom-right is 
     ;; limit-bottom or the bottom of the region
     ;; will slip below the end of the screen.
     ;; Hence gwb has re-named some things as follows
     ;; min-draw-corner -> min-draw-corner
     ;; max-draw-corner -> max-draw-corner
     ;; top -> draw-pt        
     (declare (special :boole-xor cg::invert))
     (sleep 10)
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
              (draw-pt (screen-to-host-y (+ bottom height)))
              (drawable (cg::parent canvas))
              left-lower left-upper
              ;;right-lower right-upper
              ;;bottom-lower bottom-upper
              min-draw-corner max-draw-corner
              )    
         ;; Get width right
         (setf width (min width (- limit-right limit-left)))
         ;; Get height right
         (setf height (min height (- limit-bottom limit-top)))
         ;; bounds Left
         (setf left-lower limit-left)
         (setf left-upper (max limit-left (- limit-right width)))
         ;; bounds Right
         ;;(setf right-upper limit-right)
         ;;(setf right-lower (min limit-right (+ limit-left width)))
         ;; bound draw-pt
         (setf min-draw-corner  (+ limit-top height))
         (setf max-draw-corner  limit-bottom )         
         ;; bounds Bottom ... remember we're in the upside down host mode
         ;;(setf bottom-upper limit-bottom)
         ;;(setf bottom-lower (min limit-bottom (+ limit-top height)))
         ;; starting position Left
         (cond
                   ((< left left-lower) (setf left left-lower))
                   ((> left left-upper) (setf left left-upper)))
         ;; starting position draw-pt
         (cond
                   ((< draw-pt min-draw-corner) (setf draw-pt min-draw-corner))
                   ((> draw-pt max-draw-corner) (setf draw-pt max-draw-corner)))     
       (cg::with-paint-operation 
           ;(drawable cg::invert)  19oct05
           (drawable cg::po-invert) ;19oct05
          (flet ((host-mouse-position ()
                    (let (position (cg::cursor-position canvas))
                        (list (cg::position-x position) (cg::position-y position)))
                    )
                  )
            (setf old-mouse (host-mouse-position))
            (setf new-mouse (copy-tree old-mouse))
            (with-focused-canvas canvas
               (with-pen-values canvas NIL 1 :boole-xor         
                  ;; draw the rectangle
                  (cg::draw-box canvas
                   (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                  (ecase axis
                     (:both
                      (loop
                        ;; until mouse button is released
                        (unless (mouse-down-p) (return))
                        (unless (equal old-mouse new-mouse)
                             ;; erase rectangle
                             (cg::draw-box canvas
                              (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                             ;; update to get new one
                             (setf left (max left-lower
                                               (min
                                                   (first new-mouse)
                                                   left-upper)))
                             (setf draw-pt (max min-draw-corner
                                                      (min
                                                          (second new-mouse)
                                                          max-draw-corner)))
                             ;; draw it
                             (cg::draw-box canvas
                              (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                             ;; slow down
                             (sleep 1/60)
                             ;; update iteration
                             (setq old-mouse new-mouse))
                        (setq new-mouse (host-mouse-position))
                        )
                      ;; Finally erase it.
                      (cg::draw-box canvas
                       (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                      )
                     (:x
                      (loop
                        ;; until mouse button is released
                        (unless (mouse-down-p) (return))
                        (unless (equal old-mouse new-mouse)
                             ;; erase rectangle
                             (cg::draw-box canvas
                              (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))                  
                             ;; update to get new one
                             (setf left (max left-lower
                                               (min
                                                   (first new-mouse)
                                                   left-upper)))                 
                             ;; draw it
                             (cg::draw-box canvas
                              (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                             ;; slow down
                             (sleep 1/60)
                             ;; update iteration
                             (setq old-mouse new-mouse))
                        (setq new-mouse (host-mouse-position))
                        )
                      ;; Finally erase it.
                      (cg::draw-box canvas
                       (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                      )
                     (:y
                      (loop
                        ;; until mouse button is released
                        (unless (mouse-down-p) (return))
                        (unless (equal old-mouse new-mouse)
                             ;; erase rectangle
                             (cg::draw-box canvas
                              (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                             ;; update to get new one                 
                             (setf draw-pt (max min-draw-corner
                                                      (min
                                                          (second new-mouse)
                                                          max-draw-corner)))
                             ;; draw it
                             (cg::draw-box canvas
                              (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                             ;; slow down
                             (sleep 1/60)
                             ;; update iteration
                             (setq old-mouse new-mouse))
                        (setq new-mouse (host-mouse-position))
                        )
                      ;; Finally erase it.
                      (cg::draw-box canvas
                       (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                      )
                     (:none
                      (loop
                        ;; until mouse button is released
                        (unless (mouse-down-p) (return))
                        ;; erase rectangle
                        (cg::draw-box canvas
                         (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                        ;; slow down
                        (sleep 1/60)
                        ;; draw it
                        (cg::draw-box canvas
                         (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                        ;; slow down
                        (sleep 1/60)
                        )
                      )
                     )
                  ;; Finally erase it.
                  (cg::draw-box canvas
                   (cg::make-box left draw-pt (+ left width) (+ draw-pt height)))
                  ))
            (make-region left
                (host-to-canvas-y canvas
                   (+ draw-pt height))
                width height)
            
            ))
         ))

(defun drag-region-on-screen
      (canvas &key region left bottom (width 50) (height 50) (axis :both)
         (limit-region NIL))
     "With mouse already depressed, ~
drags a region around following mouse position.   ~
If left or bottom are not provided, the mouse position is used instead.  ~
Axis = :x or :y limits motion to x or y axis respectively, :none => no motion. ~
Returns a region."
     ;; bounds for draw-pt ... remember we're in the upside down host mode
     ;; which means that a zero is the top of the screen and
     ;; 600 is its bottom ...  
     ;; AND we are drawing with the cursor at the bottom-right of
     ;; little region we are dragging about.
     ;; SO .. the minimum value for the bottom-right is
     ;; the top of the screen (0) + the height of the region
     ;; otherwise the region's top will poke above the top of the
     ;; screen ..  
     ;; and the maximum value for the bottom-right is 
     ;; limit-bottom or the bottom of the region
     ;; will slip below the end of the screen.
     ;; Hence gwb has re-named some things as follows
     ;; min-draw-corner -> min-draw-corner
     ;; max-draw-corner -> max-draw-corner
     ;; draw-pt -> draw-pt        
     (declare (special :boole-xor cg::invert))
     (unless limit-region (setf limit-region (screen-region)))
     (multiple-value-setq (left bottom width height)
         (calc-region canvas region left bottom width height))
     (if (eql :none axis) ;allow no motion
        (setq limit-region (make-region left bottom width height)))
     (let* ((old-mouse NIL)
              (new-mouse NIL)
              (draw-pt (screen-to-host-y (+ bottom height)))
              (limit-left 
               (region-left limit-region))
              (limit-top 
               (screen-to-host-y (region-top limit-region)))
              (limit-bottom 
               (screen-to-host-y (region-bottom limit-region)))
              (limit-right 
               (region-right limit-region))
              (drawable (cg::parent canvas))
              left-lower left-upper
              ;;right-lower right-upper
              ;;bottom-lower bottom-upper
              min-draw-corner max-draw-corner
              )
         ;; wrap all this in the correct painting operation
       (cg::with-paint-operation 
           ;(drawable cg::invert) 19oct05
           (drawable cg::po-invert) ;19oct05
          (flet ((host-mouse-position ()
                    (let ((pos (cg::cursor-position drawable)))
                        (list (cg::position-x pos) (cg::position-y pos))))
                  )
            ;; Get width right
            (setf width (min width (- limit-right limit-left)))
            ;; Get height right
            (setf height (min height (- limit-bottom limit-top)))
            ;; bounds Left
            (setf left-lower (+ limit-left width))
            (setf left-upper (max limit-right (+ limit-left width)))   
            ;; bounds draw-pt
            (setf min-draw-corner  (+ limit-top height))
            (setf max-draw-corner  limit-bottom )
            ;; bounds Bottom ... remember we're in the upside down host mode
            ;;(setf bottom-upper limit-bottom)
            ;;(setf bottom-lower (min limit-bottom (+ limit-top height)))
            ;; starting position Left
            (cond
                      ((< left left-lower) (setf left left-lower))
                      ((> left left-upper) (setf left left-upper)))
            ;; starting position Top
            (cond
                      ((< draw-pt min-draw-corner) (setf draw-pt min-draw-corner))
                      ((> draw-pt max-draw-corner) (setf draw-pt max-draw-corner)))
            (setf old-mouse (host-mouse-position))
            (setf new-mouse (copy-tree old-mouse))
            ;; draw the rectangle
            (cg::draw-box drawable
             (cg::make-box  (- left width) (- draw-pt height) left draw-pt))
            (ecase axis
               (:both
                (loop
                  ;; until mouse button is released
                  (unless (mouse-down-p) (return))
                  (unless (equal old-mouse new-mouse)
                       ;; erase rectangle
                       (cg::draw-box drawable
                        (cg::make-box (- left width) (- draw-pt height) left draw-pt))
                       ;; update to get new one
                       (setf left (max left-lower
                                         (min
                                             (first new-mouse)
                                             left-upper)))
                       (setf draw-pt (max min-draw-corner
                                                (min
                                                    (second new-mouse)
                                                    max-draw-corner)))
                       ;; draw it
                       (cg::draw-box drawable
                        (cg::make-box (- left width) (- draw-pt height) left draw-pt))
                       ;; slow down
                       (sleep 1/60)
                       ;; update iteration
                       (setq old-mouse new-mouse))
                  (setq new-mouse (host-mouse-position))
                  )
                ;; Finally erase it.
                (cg::draw-box drawable
                 (cg::make-box (- left width) (- draw-pt height) left draw-pt))
                )
               (:x
                (loop
                  ;; until mouse button is released
                  (unless (mouse-down-p) (return))
                  (unless (equal old-mouse new-mouse)
                       ;; erase rectangle
                       (cg::draw-box drawable
                        (cg::make-box (- left width) (- draw-pt height) left draw-pt))
                       
                       ;; update to get new one
                       (setf left (max left-lower
                                         (min
                                             (first new-mouse)
                                             left-upper)))
                       
                       ;; draw it
                       (cg::draw-box drawable
                        (cg::make-box (- left width) (- draw-pt height) left draw-pt))
                       ;; slow down
                       (sleep 1/60)
                       ;; update iteration
                       (setq old-mouse new-mouse))
                  (setq new-mouse (host-mouse-position))
                  )
                ;; Finally erase it.
                (cg::draw-box drawable
                 (cg::make-box (- left width) (- draw-pt height) left draw-pt))
                )
               (:y
                (loop
                  ;; until mouse button is released
                  (unless (mouse-down-p) (return))
                  (unless (equal old-mouse new-mouse)
                       ;; erase rectangle
                       (cg::draw-box drawable
                        (cg::make-box (- left width) (- draw-pt height) left draw-pt))
                       ;; update to get new one
                       (setf draw-pt (max min-draw-corner
                                                (min
                                                    (second new-mouse)
                                                    max-draw-corner)))
                       ;; draw it
                       (cg::draw-box drawable
                        (cg::make-box (- left width) (- draw-pt height) left draw-pt))
                       ;; slow down
                       (sleep 1/60)
                       ;; update iteration
                       (setq old-mouse new-mouse))
                  (setq new-mouse (host-mouse-position))
                  )
                ;; Finally erase it.
                (cg::draw-box drawable
                 (cg::make-box (- left width) (- draw-pt height) left draw-pt))
                )
               (:none
                (loop
                  ;; until mouse button is released
                  (unless (mouse-down-p) (return))
                  ;; erase rectangle
                  (cg::draw-box drawable
                   (cg::make-box (- left width) (- draw-pt height) left draw-pt))
                  ;; slow down
                  (sleep 1/60)
                  ;; draw it
                  (cg::draw-box drawable
                   (cg::make-box (- left width) (- draw-pt height) left draw-pt))
                  ;; slow down
                  (sleep 1/60)
                  )
                )
               )
            ;; Finally erase it.
            (cg::draw-box drawable
             (cg::make-box (- left width) (- draw-pt height) left draw-pt))
            )
          )
         ;; Since we are drawing from bottom-right there is no need to add
         ;; height to draw-pt
         (make-region left
             (host-to-screen-y draw-pt )
             width height)
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
     (declare (special *gray-shade* :boole-xor cg::invert))
     (loop until (mouse-down-p ))
     (let* ((anchor-point NIL)
              (old-mouse NIL)
              (new-mouse NIL)
              (left NIL)
              (top NIL)
              (drawable  (cg::frame-child canvas))
              )
         (flet ((host-mouse-position ()
                   (let ((pos (cg::cursor-position drawable)))
                       (list (cg::position-x pos) (cg::position-y pos))))
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
             (cg::with-paint-operation 
                 ;(drawable cg::invert) 19oct05
                 (drawable cg::po-invert) ;19oct05
               ;        (with-pen-values canvas *gray-shade* 1 :boole-xor
               ;; draw the rectangle
               (cg::draw-box drawable 
                (cg::make-box left top (+ left width) (+ top height))
                )
               (loop
                 (unless (mouse-down-p ) (return))    ;return when the mouse lets up
                 (unless (equal old-mouse new-mouse)
                      ;; erase rectangle
                      (cg::draw-box drawable 
                       (cg::make-box left top (+ left width) (+ top height))
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
                      (cg::draw-box drawable 
                       (cg::make-box left top (+ left width) (+ top height))                                    )
                      ;; slow down
                      ;;(sleep 1/60)
                      ;; update iteration
                      (setq old-mouse new-mouse))
                 (setq new-mouse (host-mouse-position))
                 )
               ;; Finally erase it.
               (cg::draw-box drawable
                (cg::make-box left top (+ left width) (+ top height))
                )
               );;end (with-pen-values
              );; end (with-focused-canvas
           (values left
              (- (canvas-to-host-y canvas top) height)
              ;;(+ top height)
              width height)
           );; end (flet ((host-mouse-position
         );; end (let* ((anchor-point
     );; end (defun select-canvas-rect

(defun select-screen-rect ( &optional (width 50) (height 50) limit-region)
     "Mouse-selects a rectangle on the screen by selecting a location on the~
screen with the mouse and ~
sweeping out the region to be selected.  
The rectangle selected is confined to limit-region ~
which defaults to the boundary of the screen. ~
Returns the region information as ~
multiple values (left bottom width height) in ~
screen coordinates."
   (declare (special :boole-xor cg::invert))
   (unless limit-region (setf limit-region (screen-region)))
     (let* ((root-window
                (cg::screen cg::*system*))
              (anchor-point NIL)
              (old-mouse NIL)
              (new-mouse NIL)
              (left NIL)
              (top NIL)
              (drawable root-window)
            (limit-left (region-left limit-region))
            (limit-top
             (screen-to-host-y (region-top limit-region)))
            (limit-bottom
             (screen-to-host-y (region-bottom limit-region)))
            (limit-right (region-right limit-region))
              )
         (flet ((host-mouse-position ()
                   (let ((pos 
                            (cg::with-device-context (hdc root-window)
                             (cg::cursor-position drawable))))
                       (list (cg::position-x pos) (cg::position-y pos))))
           (check-bound (old-list new-list)
   "Assumes old-left is current drawing left ~
    old-draw is current drawing bottom ~
    new-list comes from host-mouse-position ~
    so its car is new-left, cdr is new-bottom.~
    Checks for switches."
   (let ((old-left (first old-list))
         (old-top (second old-list)) 
         (new-left (first new-list))
         (new-top (second new-list)))
   (cond ((and (>= new-left old-left)
               (>= new-top old-top))
          (setf left old-left)
          (setf top old-top)
          (setf width (min (- new-left old-left) (- limit-right old-left)))
          (setf height (min (- new-top old-top) (- limit-bottom old-top)))
          )
         ((and (>= new-left old-left)
               (< new-top old-top))
          (setf left old-left)
          (setf width (min (- new-left old-left) (- limit-right old-left)))
          (setf top (max limit-top new-top))
          (setf height (- old-top top))
          )
         ((and (< new-left old-left)
                (>= new-top old-top))
          (setf left (max limit-left new-left))
          (setf width (- old-left left))
          (setf top old-top)
          (setf height (min (- new-top old-top) (- limit-bottom old-top)))
             )
         ((and (< new-left old-left)
               (< new-top old-top))
           (setf left (max limit-left new-left))
           (setf width (- old-left left))
           (setf top (max limit-top new-top))
           (setf height (- old-top top))
           )  
         ))
   (list left top width height)
      )
    )
           (setq limit-region (or limit-region (screen-region)))
           (loop until  (mouse-down-p))
           (setf old-mouse (host-mouse-position))
           (setf anchor-point (copy-tree old-mouse))
           (setf new-mouse (copy-tree old-mouse))
           (setf left (first anchor-point))
           (setf top (second anchor-point))
           (cg::with-paint-operation 
               ;(drawable cg::invert) 19oct05
               (drawable cg::po-invert) ;19oct05
            (cg::draw-box  drawable 
             (cg::make-box left top (+ left width) (+ top height)))
            (loop
              (unless (mouse-down-p) (return))    ;return when the mouse lets up
              (unless (equal old-mouse new-mouse)
                   ;; erase rectangle
                  (cg::with-device-context (hdc root-window)
                   (cg::draw-box drawable 
                    (cg::make-box left top (+ left width) (+ top height))))
                   ;; update to get new one
           (check-bound anchor-point old-mouse )
                   ;; draw it
                  (cg::with-device-context (hdc root-window)
                   (cg::draw-box drawable 
                    (cg::make-box left top (+ left width) (+ top height))))
                   ;; slow down
                   (sleep 1/60)
                   ;; update iteration
                   (setq old-mouse new-mouse))
              (setq new-mouse (host-mouse-position))
              );; end (loop
            ;; Finally erase it.
            (cg::with-device-context (hdc roo-window)
            (cg::draw-box drawable 
             (cg::make-box left top (+ left width) (+ top height))))
            );; end (cg::with-paint-operation 
           (values left (- (screen-to-host-y top) height) width height)
           );; end (flet ((host-mouse-position
         );; end let* ((root-window
     )

(defun sweep-for-canvas (&rest args)
     "Sweep out a region on the screen to determine the ~
size and location of the canvas which will be returned."
     (multiple-value-bind (left bottom width height)
            (select-rectangle)
           (apply #'make-canvas :left left :bottom bottom
              :width width :height height
              args)))

(defun prompt-for-canvas (&rest args)
     "Prompts user to sweep out a region on the screen to determine the ~
size and location of the canvas which will be returned."
     (declare (special *box-cursor*))
     (quail-print "Sweep out a region for the canvas with the mouse.")
     (with-cursor *box-cursor*
      (apply #'sweep-for-canvas args)))

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
        :height (- fix-y mouse-y ))))
)
