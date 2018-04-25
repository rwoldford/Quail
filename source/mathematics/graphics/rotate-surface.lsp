;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             rotate-surface.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c)  1992
;;;                Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     P. Poirier 1992
;;;     R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(rotate-surface set-increment)))
(defmethod set-increment ((self surface-view) val)
  (case val
    (:faster (setq val (* (vw::increment-of self) 1.5)))
    (:slower (setq val (/ (vw::increment-of self) 1.5)))
    (t nil))
  (setf (vw::increment-of self) val))

(defmethod views::move-points ((self surface-view) &rest args )
  (apply #'rotate-surface self args))

(defmethod stop-rotate ((self surface-view))
  (if (wb:mouse-down-p)
    #'(lambda() (not (wb:mouse-down-p)))
    #'(lambda() (wb:mouse-down-p))))

(defmethod rotate-surface ((self surface-view)
                           &key 
                           viewport (direction :y) (steps 1000) 
                           (increment (vw::increment-of self))
                           )
  (unless viewport
    (setq viewport
          (if (> (length (viewports-of self)) 1)
            (which-viewport self)
            (car (viewports-of self)))))
  
  (sleep 0.25)
  (if (and viewport (not (moving-p self))
           (active-viewport-p viewport))
    (let*
      ((stop-fn (stop-rotate self))
       (color-table (fast-color-table-of self))
       (hide? (hidden-lines-p self viewport))
       (depth-cue? (depth-cue-p self viewport))
       (fill? (surface-fill-p self viewport))
       (canvas (window-of viewport))
       (sx (cdr (assoc viewport (vp-x-of self))))
       (sy (cdr (assoc viewport (vp-y-of self))))
       (old-x (cdr (assoc viewport (vp-old-x-of self))))
       (old-y (cdr (assoc viewport (vp-old-y-of self))))
       (cx (x-cache-of self))
       (cy (y-cache-of self))
       (cz (z-cache-of self))
       (szv (cdr (assoc viewport (vp-z-pos-pars-of self))))
       (col (cdr (assoc viewport (vp-col-pars-of self))))
       (ncol (number-of-colors-of self))
       (range (truncate (* (screen-range-of self)
                           (sqrt 3))))
       (colrange (/ ncol 2 range))
       (a (cdr (assoc viewport (vp-rotation-of self))))
       (ax (update-scratch-x-of self))
       (ay (update-scratch-y-of self))
       (lower-y (cdr (assoc viewport (vp-lower-y-of self))))
       (upper-y (cdr (assoc viewport (vp-upper-y-of self))))
       (dim-x (number-of-elements cx))
       (dim-y (number-of-elements cy))
       (dim-z (number-of-elements cz))
       (max-x (width-of viewport))
       (max-y (height-of viewport))
       (vp-left (+ 1 (left-of viewport)))
       (vp-bottom (+ 1 (bottom-of viewport)))
       (dl (cdr (assoc viewport (vp-draw-line-of self))))
       (nl (cdr (assoc viewport (vp-num-lines-of self))))
       (cos-theta (cos increment))
       (sin-theta (sin increment))
       (old-a (make-array (list 3) :element-type 'single-float
                  :initial-element 0.0))
       (y-origin (bottom-of viewport))
       (x-origin (left-of viewport))
       (spin-orientation (spin-orientation-of self))
       )
      (when (not (functionp stop-fn))
        (setq stop-fn #'(lambda () NIL)))
      (unwind-protect
        (progn
          (setf (moving-p self) t)
          (case spin-orientation
            (:surface
             (loop 
               for i from 1 to steps
               until (funcall stop-fn)
               do
               (case direction
                 (:x (wb::spin-surface-x a cos-theta sin-theta old-a))
                 (:y (wb::spin-surface-y a cos-theta sin-theta old-a))
                 (:z (wb::spin-surface-z a cos-theta sin-theta old-a)))
               (wb::update-surface-data cx cy cz a sx sy 
                                        old-x old-y szv ax ay col
                                        dim-x dim-y dim-z max-x max-y
                                        range ncol)
               (cond
                (fill?
                 (wb:canvas-clear 
                  canvas :canvas-left vp-left :canvas-bottom vp-bottom
                  :width max-x :height max-y)
                 (wb:fast-surface-fill canvas
                                       x-origin y-origin
                                       sx sy cz
                                       szv col
                                       ncol
                                       (* (aref a 8) colrange)
                                       color-table)
                 (when hide?
                   (wb:fast-hide-lines canvas
                                       sx sy cz lower-y upper-y
                                       dl nl
                                       szv  x-origin y-origin
                                       max-x max-y
                                       a col
                                       ncol
                                       (* (aref a 8) colrange)
                                       NIL
                                       color-table)
                   )
                 )
                (hide?
                 (wb:canvas-clear 
                  canvas :canvas-left vp-left :canvas-bottom vp-bottom
                  :width max-x :height max-y)
                 (wb:fast-hide-lines canvas
                                     sx sy cz lower-y upper-y
                                     dl nl
                                     szv  x-origin y-origin
                                     max-x max-y
                                     a col
                                     ncol
                                     (* (aref a 8) colrange)
                                     depth-cue?
                                     color-table))
                (T
                 (wb:fast-show-lines canvas
                                     x-origin y-origin
                                     old-x old-y cz
                                     szv col
                                     ncol
                                     (* (aref a 8) colrange)
                                     NIL
                                     T
                                     color-table)
                 (wb:fast-show-lines canvas
                                     x-origin y-origin
                                     sx sy cz
                                     szv col
                                     ncol
                                     (* (aref a 8) colrange)
                                     depth-cue?
                                     NIL
                                     color-table)))
               ))
            (:screen
             (loop for i from 1 to steps
                   until (funcall stop-fn)
                   do
                   (case direction
                     (:x (wb::spin-screen-x a cos-theta sin-theta old-a))
                     (:y (wb::spin-screen-y a cos-theta sin-theta old-a))
                     (:z (wb::spin-screen-z a cos-theta sin-theta old-a)))
                   (wb::update-surface-data cx cy cz a sx sy old-x old-y szv ax ay col
                                            dim-x dim-y dim-z max-x max-y
                                            range ncol)
                   (cond
                    (fill?
                     (wb:canvas-clear 
                      canvas :canvas-left vp-left :canvas-bottom vp-bottom
                      :width max-x :height max-y)
                     (wb:fast-surface-fill canvas
                                           x-origin y-origin
                                           sx sy cz
                                           szv col
                                           ncol
                                           (* (aref a 8) colrange)
                                           color-table)
                     (when hide?
                       (wb:fast-hide-lines canvas
                                           sx sy cz lower-y upper-y
                                           dl nl
                                           szv  x-origin y-origin
                                           max-x max-y
                                           a col
                                           ncol
                                           (* (aref a 8) colrange)
                                           NIL
                                           color-table)
                       )
                     )
                    (hide?
                     (wb:canvas-clear 
                      canvas :canvas-left vp-left :canvas-bottom vp-bottom
                      :width max-x :height max-y)
                     (wb:fast-hide-lines canvas
                                         sx sy cz lower-y upper-y
                                         dl nl
                                         szv  x-origin y-origin
                                         max-x max-y
                                         a col
                                         ncol
                                         (* (aref a 8) colrange)
                                         depth-cue?
                                         color-table))
                    (T
                     (wb:fast-show-lines canvas
                                         x-origin y-origin
                                         old-x old-y cz
                                         szv col
                                         ncol
                                         (* (aref a 8) colrange)
                                         NIL
                                         T
                                         color-table)
                     (wb:fast-show-lines canvas
                                         x-origin y-origin
                                         sx sy cz
                                         szv col
                                         ncol
                                         (* (aref a 8) colrange)
                                         depth-cue?
                                         NIL
                                         color-table))
                    )
                   )
             )
            )
          )
        (setf (moving-p self) nil)
        )
      )
    )
  )
