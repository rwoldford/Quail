;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              lines-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1991 George Washington University
;;;     R.W. Oldford 1992
;;;     
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute)
(export  '(canvas-draw-lines canvas-erase-lines canvas-move-lines)))

;; coords are lists with even length (s1 e1 s2 e2...) where s1 and e1 are the start
;; and end of the first line

(defun draw-fw-lines (canvas coords &key (width 1)
                             color invisible?
                             erase? &allow-other-keys)
  
  "Draws or erases colored fixed width lines"
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  
  (with-focused-canvas canvas
    (choose-mode canvas :erase? erase?)
    (set-draw-color canvas color)
    (#_PenSize :long (ccl::make-point width width))
    (loop with h = (canvas-height canvas)
          for (xs ys) fixnum in coords by #'cddr
          for (xe ye) fixnum in (cdr coords) by #'cddr
          for i  in invisible? 
          unless i
          do (fast-draw-line canvas xs (- h ys) xe (- h ye)))))


(defun xor-move-fw-lines (canvas old-coords new-coords
                                &key (width 1) color invisible? &allow-other-keys )
  
  "Moves colored fixed width lines~
   Results with color background are undefined"
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (set-pen-color canvas color)
  (with-focused-canvas canvas
    (#_PenMode *host-xor-mode*)
    (#_PenSize :long (ccl::make-point width width))
    (loop with h = (canvas-height canvas)
          for (sx1 sy1 ) fixnum in old-coords by #'cddr 
          for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
          for (sx2 sy2 ) fixnum in new-coords by #'cddr 
          for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
          for i  in invisible? 
          unless i
          do (fast-draw-line canvas sx1 (- h sy1) ex1 (- h ey1))
          (fast-draw-line canvas sx2 (- h sy2) ex2 (- h ey2)))))
          

(defun move-fw-lines (canvas old-coords new-coords
                             &key (width 1) color invisible? &allow-other-keys )
  
  "Moves colored fixed width lines"
  
  
  (declare (special  *host-or-mode* *host-bic-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (set-draw-color canvas color)
  (with-focused-canvas canvas
    (#_PenSize :long (ccl::make-point width width))
    (loop with h = (canvas-height canvas)
          for (sx1 sy1 ) fixnum in old-coords by #'cddr 
          for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
          for (sx2 sy2 ) fixnum in new-coords by #'cddr 
          for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
          for i  in invisible? 
          unless i
          do 
          (mode-draw-line canvas sx1 (- h sy1) ex1 (- h ey1) *host-bic-mode*)
          (mode-draw-line canvas sx2 (- h sy2) ex2 (- h ey2) *host-or-mode*))))






(defun draw-multi-color-lines (canvas coords &key (width 1) color invisible? erase? &allow-other-keys)
  
  "Draws or erases lines with varying color."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (with-focused-canvas canvas
    (#_PenSize :long (ccl::make-point width width))
    (choose-mode canvas :erase? erase?)
    (loop with h = (canvas-height canvas)
          for (xs ys ) fixnum in coords by #'cddr
          for (xe ye ) fixnum in (cdr coords) by #'cddr
          for c in color
          for i  in invisible? 
          unless i
          do (set-draw-color canvas c)
          (fast-draw-line canvas xs (- ys h) xe (- ye h)))))



(defun xor-move-multi-color-lines (canvas old-coords new-coords
                                          &key (width 1) color invisible? &allow-other-keys )
  
  "Moves lines with varying color.  ~
   Results with color background are undefined."
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  
  (with-focused-canvas canvas
    (#_PenMode *host-xor-mode*)
    (#_PenSize :long (ccl::make-point width width))
    (loop with h = (canvas-height canvas)
          for (sx1 sy1 ) fixnum in old-coords by #'cddr 
          for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
          for (sx2 sy2 ) fixnum in new-coords by #'cddr 
          for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
          for c in color
          for i  in invisible? 
          unless i
          do
          (set-draw-color canvas c)
          (fast-draw-line canvas sx1 (- h sy1) ex1 (- h ey1))
          (fast-draw-line canvas sx2 (- h sy2) ex2 (- h ey2)))))


(defun move-multi-color-lines (canvas old-coords new-coords
                                      &key (width 1)
                                      color rgb-color? invisible? &allow-other-keys )
  
  "Moves lines with varying color."
  
  (declare (special *black-color* *host-or-mode* *host-bic-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (with-focused-canvas canvas
    (#_PenSize :long (ccl::make-point width width))
    (loop with h = (canvas-height canvas)
          for (sx1 sy1 ) fixnum in old-coords by #'cddr 
          for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
          for (sx2 sy2 ) fixnum in new-coords by #'cddr 
          for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
          for c in color
          for i  in invisible? 
          unless i
          do 
          (if rgb-color?
            (cc-set-rgb-color c)
            (set-draw-color canvas c))
          (mode-draw-line canvas sx1 (- h sy1) ex1 (- h ey1) *host-bic-mode*)
          (mode-draw-line canvas sx2 (- h sy2) ex2 (- h ey2) *host-or-mode*))))




(defun draw-multi-color-&-width-lines (canvas coords &key width color invisible? erase? &allow-other-keys)
  
  "Draws or erases lines with varying color and width."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (with-focused-canvas canvas
    (choose-mode canvas :erase? erase?)
    (loop with h = (canvas-height canvas)
          for (xs ys ) fixnum in coords by #'cddr
          for (xe ye ) fixnum in (cdr coords) by #'cddr
          
          for w in width for c in color
          for i  in invisible? 
          unless i
          do (set-draw-color canvas c)
          (#_PenSize :long (ccl::make-point w w))
          (fast-draw-line canvas xs (- ys h) xe (- ye h)))))



(defun xor-move-multi-color-&-width-lines (canvas old-coords new-coords
                                         &key width color invisible? &allow-other-keys )
  
  "Moves lines with varying color and width~
   Results with color background are undefined."
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
 
  (with-focused-canvas canvas
    (#_PenMode *host-xor-mode*)
    (loop with h = (canvas-height canvas)
          for (sx1 sy1 ) fixnum in old-coords by #'cddr 
          for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
          for (sx2 sy2 ) fixnum in new-coords by #'cddr 
          for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
          for w in width for c in color
          for i  in invisible? 
          unless i
          do (set-draw-color canvas c)
          (#_PenSize :long (ccl::make-point w w))
          (fast-draw-line canvas sx1 (- h sy1) ex1 (- h ey1))
          (fast-draw-line canvas sx2 (- h sy2) ex2 (- h ey2)))))


(defun move-multi-color-&-width-lines
       (canvas old-coords new-coords  &key width color rgb-color? invisible?  &allow-other-keys )
  
  "Moves lines with varying color and width"
  
  (declare (special *black-color* *host-or-mode* *host-bic-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (with-focused-canvas canvas
    (loop with h = (canvas-height canvas)
          for (sx1 sy1 ) fixnum in old-coords by #'cddr 
          for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
          for (sx2 sy2 ) fixnum in new-coords by #'cddr 
          for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
          for w in width for c in color
          for i  in invisible? 
          unless i
          do 
          (if rgb-color?
                (cc-set-rgb-color c)
                (set-draw-color canvas c))
          (#_PenSize :long (ccl::make-point w w))
          (mode-draw-line canvas sx1 (- h sy1) ex1 (- h ey1) *host-bic-mode*)
          (mode-draw-line canvas sx2 (- h sy2) ex2 (- h ey2) *host-or-mode*))))










(defun canvas-draw-lines (canvas coords
                                 &key (width 1) (erase? nil)
                                 color invisible?
                                 &allow-other-keys )
  
  
  "Draws or erases lines "
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (if (and  color (listp color))
    (if (and width (listp width))
      (draw-multi-color-&-width-lines canvas coords :width width :color color
                                      :invisible? invisible? :erase? erase?)
      (draw-multi-color-lines canvas coords :width width :color color
                              :invisible? invisible? :erase? erase?))
    (draw-fw-lines canvas coords
                   :width (if (listp width ) (car  width) width)
                   :color color 
                   :erase? erase?
                   :invisible? invisible?)
    ))



(defun canvas-erase-lines (canvas coords
                               &key (width 1) 
                               color invisible?
                                &allow-other-keys )


  "Erases lines "
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width)
           (inline canvas-draw-lines))
  (canvas-draw-lines canvas coords :width width :color color :erase? t
                     :invisible? invisible?))
    




(defun canvas-move-lines (canvas old-coords new-coords
                                 &key (width 1)  (rgb-color? nil)
                                 color invisible?
                                 &allow-other-keys )
  
  
  "Moves lines "
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (if (and  color (listp color))
    (if (and width (listp width))
      (move-multi-color-&-width-lines canvas old-coords new-coords
                                      :width width :invisible? invisible?
                                      :color (if (and (colored-canvas-p canvas) (not rgb-color? ))
                                               (rgb-colors color)
                                               color))
      (move-multi-color-lines canvas old-coords new-coords
                              :width width :invisible? invisible?
                              :color (if (and (colored-canvas-p canvas) (not rgb-color? ))
                                       (rgb-colors color (pen-color-of canvas))
                                       color)))
    (move-fw-lines canvas old-coords new-coords
                   :width (if (listp width ) (car  width) width) 
                   :invisible? invisible?
                   :color color)
    )
  )
    

(defun canvas-draw-axes (canvas axes
                                &key (width 1) (erase? nil)
                                color)
  
  "Draws or erases  axes"
  
  (declare (special  *host-or-mode* *host-bic-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  
  (when axes
    (with-focused-canvas canvas
      (if erase? (#_PenMode *host-bic-mode*)
          (#_PenMode *host-or-mode*))
      (set-draw-color canvas color)
      (#_PenSize :long (ccl::make-point width width))
      (loop with h = (canvas-height canvas)
            with xo = (caar axes) 
            with yo = (- h (cadar axes ) )
            for (x y ) fixnum in (cdr axes) 
            do (fast-draw-line canvas xo yo  x (- h y))))))

  



(defun canvas-erase-axes (canvas axes
                               &key (width 1) 
                               color) 
  "Erases axes "
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width)
           (inline canvas-draw-axes))
   (canvas-draw-axes canvas axes :width width :color color ))
    




(defun canvas-move-axes (canvas old-axes new-axes
                                &key (width 1)  
                                color 
                                &allow-other-keys )
  
  
  "Moves axes "
  (declare (special  *host-or-mode* *host-bic-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  
  (when old-axes
    (set-draw-color canvas color)
    (with-focused-canvas canvas
      (#_PenSize :long (ccl::make-point width width))
      (loop with h = (canvas-height canvas)
            with xo = (caar new-axes) 
            with yo = (- h (cadar new-axes ) )
            for (x-old y-old ) fixnum in (cdr old-axes)
            for (x y ) fixnum in (cdr new-axes)
            do 
            (mode-draw-line canvas xo yo x-old (- h y-old) *host-bic-mode*)
            (mode-draw-line canvas xo yo x(- h y) *host-or-mode*)))))


 









