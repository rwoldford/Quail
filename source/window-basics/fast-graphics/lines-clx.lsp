;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              lines-clx.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-draw-lines canvas-erase-lines canvas-move-lines)))

;; coords are lists with even length (s1 e1 s2 e2...) where s1 and e1 are the start
;; and end of the first line

(defun draw-fw-lines (canvas coords &key (width 1)
                             color
                             erase? invisible? &allow-other-keys)
  
  "Draws or erases colored fixed width lines"
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  
  (with-focused-canvas canvas
    (choose-mode canvas :erase? erase?)
    (set-draw-color canvas color)
    (xlib::set-gcontext-line-width (gcontext canvas) width)
    (xlib:draw-lines (host-window canvas) (gcontext canvas)
    (let ((h (canvas-height canvas)))
      (if invisible?
        (loop for (xs ys) fixnum in coords by #'cddr
              for i  in invisible? 
              unless i do
              collect (list xs (- h ys)))
	  (map 'list #'(lambda (x) (list (car x) (- h (cadr x)))) coords))))))


(defun xor-move-fw-lines (canvas old-coords new-coords
                                &key (width 1) color  invisible? &allow-other-keys )
  
  "Moves colored fixed width lines~
   Results with color background are undefined"
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (set-pen-color canvas color)
  (with-focused-canvas canvas
    (xlib::set-gcontext-function (gcontext canvas) *host-xor-mode*)
    (xlib::set-gcontext-line-width (gcontext canvas) width)
    (let ((window (host-window canvas))
	  (gcontext (gcontext canvas))
	  (h (canvas-height canvas)))
      (xlib:draw-lines 
       window gcontext 
       (if invisible?
         (loop for (xs ys) fixnum in old-coords by #'cddr
               for i  in invisible? 
               unless i do
               collect (list xs (- h ys)))
         (map 'list #'(lambda (x) (list (car x) (- h (cadr x)))) old-coords)))
      (xlib:draw-lines 
       window gcontext 
       (if invisible?
         (loop for (xs ys) fixnum in new-coords by #'cddr
               for i  in invisible? 
               unless i do
               collect (list xs (- h ys)))
         (map 'list #'(lambda (x) (list (car x) (- h (cadr x)))) new-coords))))))

(defun move-fw-lines (canvas old-coords new-coords
                             &key (width 1) color invisible? &allow-other-keys )
  
  "Moves colored fixed width lines"
  
  
  (declare (special  *host-or-mode* *host-bic-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (set-draw-color canvas color)
  (with-focused-canvas canvas
    (let ((window (host-window canvas))
	  (gcontext (gcontext canvas))
	  (height (canvas-height canvas)))
      (xlib::set-gcontext-line-width gcontext width)
      (xlib::set-gcontext-function gcontext *host-bic-mode*)
      (xlib:draw-lines 
       window gcontext 
       (if invisible?
         (loop for (xs ys) fixnum in old-coords by #'cddr
               for i  in invisible? 
               unless i do
               collect (list xs (- h ys)))
         (map 'list #'(lambda (x) (list (car x) (- h (cadr x)))) old-coords)))
      (xlib::set-gcontext-function gcontext *host-or-mode*)
      (xlib:draw-lines 
       window gcontext 
       (if invisible?
         (loop for (xs ys) fixnum in new-coords by #'cddr
               for i  in invisible? 
               unless i do
               collect (list xs (- h ys)))
         (map 'list #'(lambda (x) (list (car x) (- h (cadr x)))) new-coords))))))

(defun draw-multi-color-lines (canvas coords &key (width 1) color erase? invisible? &allow-other-keys)
  
  "Draws or erases lines with varying color."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (with-focused-canvas canvas
    (xlib::set-gcontext-line-width (gcontext canvas) width)
    (choose-mode canvas :erase? erase?)
    (loop with h = (canvas-height canvas)
          for (xs ys ) fixnum in coords by #'cddr
          for (xe ye ) fixnum in (cdr coords) by #'cddr
          for c in color
          for i  in invisible? 
          unless i
           do (set-draw-color canvas c)
          (xlib:draw-line (host-window canvas) 
			  (gcontext canvas) 
		          xs (- ys h) xe (- ye h)))))



(defun xor-move-multi-color-lines (canvas old-coords new-coords
                                          &key (width 1) color invisible? &allow-other-keys )
  
  "Moves lines with varying color.  ~
   Results with color background are undefined."
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  
  (with-focused-canvas canvas
    (xlib::set-gcontext-function (gcontext canvas) *host-xor-mode*)
    (xlib::set-gcontext-line-width (gcontext canvas) width)
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
          (draw-line (host-window canvas) (gcontext canvas) sx1 (- h sy1) ex1 (- h ey1))
          (draw-line (host-window canvas) (gcontext canvas) sx2 (- h sy2) ex2 (- h ey2)))))


(defun move-multi-color-lines (canvas old-coords new-coords
                                      &key (width 1)
                                      color rgb-color? invisible? &allow-other-keys )
  
  "Moves lines with varying color."
  
  (declare (special *black-color* *host-or-mode* *host-bic-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (with-focused-canvas canvas
    (xlib::set-gcontext-line-width (gcontext canvas) width)
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
            (cc-set-rgb-color canvas c)
            (set-draw-color canvas c))
	  (xlib::set-gcontext-function (gcontext canvas) *host-bic-mode*)
          (xlib:draw-line (host-window canvas) 
			  (gcontext canvas) 
			  sx1 (- h sy1) ex1 (- h ey1))
	  (xlib::set-gcontext-function (gcontext canvas) *host-or-mode*)
          (xlib:draw-line (host-window canvas)
			  (gcontext canvas)
			  sx2 (- h sy2) ex2 (- h ey2)))))




(defun draw-multi-color-&-width-lines (canvas coords &key width color erase? invisible? &allow-other-keys)
  
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
          (xlib::set-gcontext-line-width (gcontext canvas) w)
          (xlib:draw-line (host-window canvas)
			  (gcontext canvas)
			  xs (- ys h) xe (- ye h)))))



(defun xor-move-multi-color-&-width-lines (canvas old-coords new-coords
                                         &key width color invisible? &allow-other-keys )
  
  "Moves lines with varying color and width~
   Results with color background are undefined."
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
 
  (with-focused-canvas canvas
    (xlib::set-gcontext-function (gcontext canvas) *host-xor-mode*)
    (loop with h = (canvas-height canvas)
          for (sx1 sy1 ) fixnum in old-coords by #'cddr 
          for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
          for (sx2 sy2 ) fixnum in new-coords by #'cddr 
          for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
          for w in width for c in color
          for i  in invisible? 
          unless i
          do (set-draw-color canvas c)
          (xlib::set-gcontext-line-width (gcontext canvas) w)
          (xlib:draw-line (host-window canvas)
			  (gcontext canvas)
			  sx1 (- h sy1) ex1 (- h ey1))
          (xlib:draw-line (host-window canvas)
			  (gcontext canvas)
			  sx2 (- h sy2) ex2 (- h ey2)))))


(defun move-multi-color-&-width-lines
       (canvas old-coords new-coords  &key width color rgb-color? invisible? &allow-other-keys )
  
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
                (cc-set-rgb-color canvas c)
                (set-draw-color canvas c))
          (xlib::set-gcontext-line-width (gcontext canvas) w)
	  (xlib::set-gcontext-function (gcontext canvas) *host-bic-mode*)
          (xlib:draw-line (host-window canvas)
			  (gcontext canvas)
			  sx1 (- h sy1) ex1 (- h ey1))
	  (xlib::set-gcontext-function (gcontext canvas) *host-or-mode*)
          (xlib:draw-line (host-window canvas)
			  (gcontext canvas)
			  sx2 (- h sy2) ex2 (- h ey2)))))

(defun canvas-draw-lines (canvas coords
                                 &key (width 1) (erase? nil)
                                 color
                                 invisible? &allow-other-keys )
  
  
  "Draws or erases lines "
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (if (and  color (listp color))
    (if (and width (listp width))
      (draw-multi-color-&-width-lines canvas coords :width width 
                                      :invisible? invisible? :color color :erase? erase?)
      (draw-multi-color-lines canvas coords :width width 
                              :invisible? invisible? :color color :erase? erase?))
    (draw-fw-lines canvas coords
                   :width (if (listp width ) (car  width) width)
                   :color color 
                   :invisible? invisible?
                   :erase? erase?)
    ))



(defun canvas-erase-lines (canvas coords
                               &key (width 1) 
                               color
                                invisible? &allow-other-keys )


  "Erases lines "
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width)
           (inline canvas-draw-lines))
  (canvas-draw-lines canvas coords :width width :color color 
                     :invisible? invisible? :erase? t))

(defun canvas-move-lines (canvas old-coords new-coords
                                 &key (width 1)  (rgb-color? nil)
                                 color 
                                 invisible? &allow-other-keys )
  
  
  "Moves lines "
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (type fixnum width))
  (if (and  color (listp color))
    (if (and width (listp width))
      (move-multi-color-&-width-lines canvas old-coords new-coords
                                      :width width 
                                      :invisible? invisible?
                                      :color (if (and (colored-canvas-p canvas) (not rgb-color? ))
                                               (rgb-colors color)
                                               color))
      (move-multi-color-lines canvas old-coords new-coords
                              :width width 
                              :invisible? invisible?
                              :color (if (and (colored-canvas-p canvas) (not rgb-color? ))
                                       (rgb-colors color)
                                       color)))
    (move-fw-lines canvas old-coords new-coords
                   :invisible? invisible?
                   :width (if (listp width ) (car  width) width) 
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
      (if erase? (xlib::set-gcontext-function (gcontext canvas) *host-bic-mode*)
          (xlib::set-gcontext-function (gcontext canvas) *host-or-mode*))
      (set-draw-color canvas color)
      (xlib::set-gcontext-line-width (gcontext canvas) width)
      (loop with h = (canvas-height canvas)
            with xo = (caar axes) 
            with yo = (- h (cadar axes ) )
            for (x y ) fixnum in (cdr axes) 
            for i  in invisible? 
          unless i
          do (xlib:draw-line (host-window canvas) (gcontext canvas)
		 xo yo  x (- h y))))))

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
      (xlib::set-gcontext-line-width (gcontext canvas) width)
      (loop with h = (canvas-height canvas)
            with xo = (caar new-axes) 
            with yo = (- h (cadar new-axes ) )
            for (x-old y-old ) fixnum in (cdr old-axes)
            for (x y ) fixnum in (cdr new-axes)
            for i  in invisible? 
          unless i
          do 
	    (xlib::set-gcontext-function (gcontext canvas) *host-bic-mode*)
            (xlib:draw-line (host-window canvas)
			    (gcontext canvas)
			    xo yo x-old (- h y-old))
	    (xlib::set-gcontext-function (gcontext canvas) *host-or-mode*)
            (xlib:draw-line (host-window canvas)
			    (gcontext canvas)
			    xo yo x(- h y))))))
