;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              lines-pc.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1991 George Washington University
;;;     R.W. Oldford 1992
;;;     G.W. Bennett 1996
;;;     
;;;     
;;;--------------------------------------------------------------------

(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-draw-lines canvas-erase-lines canvas-move-lines)))

;; coords are lists with even length (s1 e1 s2 e2...) where s1 and e1 are the start
;; and end of the first line

(defun draw-fw-lines (canvas coords &key (width 1)
                                   (invisible? (list nil nil nil))
                                   color
                                   erase? &allow-other-keys)
     "Draws or erases colored fixed width lines"
     (declare (optimize (speed 3) (safety 0)
                      (space 0) (compilation-speed 0))
        (type fixnum width)
        (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
  (let ((mp (cg::frame-child canvas)))
     (with-focused-canvas canvas
      ;;    (choose-mode canvas :erase? erase?)
       ;(cg::set-line-width mp width) 18oct05
       (setf (cg::line-width mp) width) ;180ct05
      (cg::with-foreground-color (mp color)
       (loop with h = (canvas-height canvas)
         for (xs ys) fixnum in coords by #'cddr
         for (xe ye) fixnum in (cdr coords) by #'cddr
         for i in invisible?
         unless i
         do 
         (if erase?
            (fast-erase-line canvas xs ( - h ys) xe (- h ye))
            (fast-draw-line canvas xs (- h ys) xe (- h ye)))
         ))
       )))

(defun xor-move-fw-lines (canvas old-coords new-coords
                                           &key (width 1) color  &allow-other-keys )
     "Moves colored fixed width lines~
      Results with color background are undefined"
     (declare (special  *host-xor-mode* cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0))
        (type fixnum width))
  (let ((mp (cg::frame-child canvas)))
     (with-focused-canvas canvas
      (h-draw::set-pen-mode canvas *host-xor-mode*)
       ;(cg::set-line-width mp width) 18oct05
       (setf (cg::line-width mp) width) ;18oct05
      (cg::with-foreground-color (mp color)
       (loop with h = (canvas-height canvas)
         for (sx1 sy1 ) fixnum in old-coords by #'cddr 
         for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
         for (sx2 sy2 ) fixnum in new-coords by #'cddr 
         for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
         do 
             (cg::with-paint-operation 
                 ;(mp cg::erase) ;; Need this to avoid  19oct05
                 (mp cg::po-erase) ;19oct05
          ;; BLACK original lines
          (fast-draw-line canvas sx1 (- h sy1) ex1 (- h ey1)))
         (fast-draw-line canvas sx2 (- h sy2) ex2 (- h ey2))))
       )
      ))

(defun move-fw-lines (canvas old-coords new-coords
                                    &key (width 1) color &allow-other-keys )
     "Moves colored fixed width lines"
     (declare (special  *host-or-mode* *host-bic-mode*
                      cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0))
        (type fixnum width))
     ;;  (set-draw-color canvas color)
  (let ((mp (cg::frame-child canvas)))
     (with-focused-canvas canvas
       ;(cg::set-line-width mp width) 18oct05
       (setf (cg::line-width mp) width) ;18oct05
      (cg::with-foreground-color (mp color)
       (loop with h = (canvas-height canvas)
         for (sx1 sy1 ) fixnum in old-coords by #'cddr 
         for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
         for (sx2 sy2 ) fixnum in new-coords by #'cddr 
         for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
         do 
             (cg::with-paint-operation 
                 ;(mp cg::erase) 19oct05
                 (mp cg::po-erase) ;19oct05
          (mode-erase-line canvas sx1 (- h sy1) ex1 (- h ey1) *host-bic-mode*))
         (mode-draw-line canvas sx2 (- h sy2) ex2 (- h ey2) *host-or-mode*)))
       )
      ))

(defun draw-multi-color-lines (canvas coords &key (width 1) color invisible? 
                                                 erase? &allow-other-keys)
     "Draws or erases lines with varying color."
     (declare (optimize (speed 3) (safety 0)
                      (space 0) (compilation-speed 0))
        (type fixnum width)
        (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
     (with-focused-canvas canvas
  (let ((mp (cg::frame-child canvas)))
    ;(cg::set-line-width mp width) 18oct05
    (setf (cg::line-width mp) width) ;18oct05
      ;;    (choose-mode canvas :erase? erase?)
      (loop with h = (canvas-height canvas)
        for (xs ys ) fixnum in coords by #'cddr
        for (xe ye ) fixnum in (cdr coords) by #'cddr
        for c in color
        do 
            (cg::with-paint-operation 
                ;(mp cg::replace) 19oct05
                (mp cg::po-replace) ;19oct05
         (cg::with-foreground-color (mp c)
          (if erase?
             (fast-erase-line canvas xs (- h ys) xe (- h ye))
             (fast-draw-line canvas xs (- h ys) xe (- h ye)))
          ))
        )))
    )

(defun xor-move-multi-color-lines (canvas old-coords new-coords
                                                         &key (width 1) color 
                                                         invisible? &allow-other-keys )
     "Moves lines with varying color.  ~
Results with color background are undefined."
     (declare (special  *host-xor-mode* cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0))
        (type fixnum width))
     (with-focused-canvas canvas
  (let ((mp (cg::frame-child canvas)))
      (h-draw::set-pen-mode canvas *host-xor-mode*)
    ;(cg::set-line-width mp width) 18oct05
    (setf (cg::line-width mp) width) ;18oct05
      (loop with h = (canvas-height canvas)
        for (sx1 sy1 ) fixnum in old-coords by #'cddr 
        for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
        for (sx2 sy2 ) fixnum in new-coords by #'cddr 
        for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
        for c in color
        do
            (cg::with-paint-operation 
                ;(mp cg::erase) ;; Need this to avoid 19oct05
                 (mp cg::po-erase) ;19oct05
         ;; BLACK original lines
         (fast-erase-line canvas sx1 (- h sy1) ex1 (- h ey1)))
        (cg::with-foreground-color (canvas c)
         (fast-draw-line canvas sx2 (- h sy2) ex2 (- h ey2)))
        ))
   ))

(defun move-multi-color-lines (canvas old-coords new-coords
                                                  &key (width 1)
                                                  color rgb-color? &allow-other-keys )
     "Moves lines with varying color."
     (declare (special *black-color* *host-or-mode* *host-bic-mode*
                      cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0))
        (type fixnum width))
     (with-focused-canvas canvas
  (let ((mp (cg::frame-child canvas)))
    ;(cg::set-line-width mp width) 18oct05
    (setf (cg::line-width mp) width) ;18oct05
      (loop with h = (canvas-height canvas)
        for (sx1 sy1 ) fixnum in old-coords by #'cddr 
        for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
        for (sx2 sy2 ) fixnum in new-coords by #'cddr 
        for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
        for c in color
        do 
        (if rgb-color?
           (cc-set-rgb-color canvas c)
           (cg::with-foreground-color (mp c)
             (cg::with-paint-operation 
                 ;(mp cg::erase) 19oct05
                 (mp cg::po-erase) ;19oct05
             (mode-erase-line canvas sx1 (- h sy1) ex1 (- h ey1) *host-bic-mode*))
            (mode-draw-line canvas sx2 (- h sy2) ex2 (- h ey2) *host-or-mode*))
           )))
   ))

(defun draw-multi-color-&-width-lines (canvas coords &key width color 
                                                               invisible? erase? &allow-other-keys)
     "Draws or erases lines with varying color and width."
     (declare (optimize (speed 3) (safety 0)
                      (space 0) (compilation-speed 0))
        (type fixnum width)
        (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
     (with-focused-canvas canvas
  (let ((mp (cg::frame-child canvas)))
      ;;    (choose-mode canvas :erase? erase?)
      (loop with h = (canvas-height canvas)
        for (xs ys ) fixnum in coords by #'cddr
        for (xe ye ) fixnum in (cdr coords) by #'cddr          
        for w in width for c in color
        do 
        (cg::with-foreground-color (mp c)
          ;(cg::set-line-width mp w) 18oct05
          (setf (cg::line-width mp) w) ;18oct05
         (if erase?
            (fast-erase-line canvas xs (- h ys) xe (- h ye))
            (fast-draw-line canvas xs (- h ys) xe (- h ye))))))
   ))

(defun xor-move-multi-color-&-width-lines (canvas old-coords new-coords
                                                                       &key width color &allow-other-keys )
     "Moves lines with varying color and width~
Results with color background are undefined."
     (declare (special  *host-xor-mode* cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0))
        (type fixnum width))
     (with-focused-canvas canvas
  (let ((mp (cg::frame-child canvas)))
      (h-draw::set-pen-mode canvas *host-xor-mode*)
      (loop with h = (canvas-height canvas)
        for (sx1 sy1 ) fixnum in old-coords by #'cddr 
        for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
        for (sx2 sy2 ) fixnum in new-coords by #'cddr 
        for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
        for w in width for c in color
        do 
            ;(cg::set-line-width mp w) 18oct05
            (setf (cg::line-width mp) w) ;18oct05
            (cg::with-paint-operation 
                ;(mp cg::erase)  19oct05
                 (mp cg::po-erase) ;19oct05
         (fast-erase-line canvas sx1 (- h sy1) ex1 (- h ey1)))
        (cg::with-foreground-color (mp c)
         (fast-draw-line canvas sx2 (- h sy2) ex2 (- h ey2)))
        ))
  ))

(defun move-multi-color-&-width-lines
      (canvas old-coords new-coords  &key width color rgb-color? &allow-other-keys )
     "Moves lines with varying color and width"
     (declare (special *black-color* *host-or-mode* *host-bic-mode*
                      cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0))
        (type fixnum width))
     (with-focused-canvas canvas
  (let ((mp (cg::frame-child canvas)))
      (loop with h = (canvas-height canvas)
        for (sx1 sy1 ) fixnum in old-coords by #'cddr 
        for (ex1 ey1 ) fixnum in (cdr old-coords) by #'cddr 
        for (sx2 sy2 ) fixnum in new-coords by #'cddr 
        for (ex2 ey2 ) fixnum in (cdr new-coords) by #'cddr 
        for w in width for c in color
        do 
       ; (format t "~%[1] width is ~a " w)
        (if rgb-color?
           (cc-set-rgb-color mp c)
           (cg::with-foreground-color (mp c)        
             (cg::with-paint-operation 
                 ;(mp cg::erase) 19oct05
                 (mp cg::po-erase) ;19oct05
              ;(cg::set-line-width canvas w) 18oct05
              (setf (cg::line-width canvas) w) ;18oct05
             (mode-erase-line canvas sx1 (- h sy1) ex1 (- h ey1) *host-bic-mode*)
             )
            (mode-draw-line canvas sx2 (- h sy2) ex2 (- h ey2) *host-or-mode*)
            )
           )
        ))
    ))

(defun canvas-draw-lines (canvas coords
                                          &key (width 1) (erase? nil)
                                          color
                                          invisible?
                                          &allow-other-keys )
     "Draws or erases lines "
     (declare (optimize (speed 3) (safety 0)
                      (space 0) (compilation-speed 0))
        (type fixnum width)
        (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
     (if (and  color (listp color))
        (if (and width (listp width))
           (draw-multi-color-&-width-lines canvas coords :width width :color color :erase? erase?
            :invisible? invisible?)
           (draw-multi-color-lines canvas coords :width width :color color :erase? erase?
             :invisible? invisible?))
        (draw-fw-lines canvas coords
         :width (if (listp width ) (car  width))
         :color (if (listp color ) (car  color)) 
         :erase? erase?
          :invisible invisible?)
        ))

;;; Not done as of May 11, 1996 - see NOTES at the top of this file
(defun canvas-erase-lines (canvas coords
                                           &key (width 1) 
                                           color
                                           &allow-other-keys )
     "Erases lines "
     (declare (optimize (speed 3) (safety 0)
                      (space 0) (compilation-speed 0))
        (type fixnum width)
        (inline canvas-draw-lines)
        (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
     (canvas-draw-lines canvas coords :width width :color color :erase? t))

(defun canvas-move-lines (canvas old-coords new-coords
                                           &key (width 1)  (rgb-color? nil)
                                           color 
                                           &allow-other-keys )
     "Moves lines "
     (declare (optimize (speed 3) (safety 0)
                      (space 0) (compilation-speed 0))
        (type fixnum width)
        (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
     (if (and  color (listp color))
        (if (and width (listp width))
           (move-multi-color-&-width-lines canvas old-coords new-coords
            :width width 
            :color (if (and (colored-canvas-p canvas) (not rgb-color? ))
                         (rgb-colors color)
                         color))
           (move-multi-color-lines canvas old-coords new-coords
            :width width 
            :color (if (and (colored-canvas-p canvas) (not rgb-color? ))
                         (rgb-colors color)
                         color)))
        (move-fw-lines canvas old-coords new-coords
         :width (if (listp width ) (car  width)) 
         :color (if (listp color ) (car  color)))
        )
     )

(defun canvas-draw-axes (canvas axes
                                          &key (width 1) (erase? nil)
                                          color)
     "Draws or erases  axes"
     (declare (special  *host-or-mode* *host-bic-mode*
                      cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0))
        (type fixnum width))
     (when axes
          (with-focused-canvas canvas
  (let ((mp (cg::frame-child canvas)))
           (cg::with-foreground-color (mp color)
             ;(cg::set-line-width mp width) 18oct05
             (setf (cg::line-width mp) width) ;18oct05
            (if erase?
               (draw-fw-lines canvas axes :width width :erase? t)
               (draw-fw-lines canvas axes :width width ))
            )))
    ))

(defun canvas-erase-axes (canvas axes
                                           &key (width 1) 
                                           color) 
     "Erases axes "
     (declare (optimize (speed 3) (safety 0)
                      (space 0) (compilation-speed 0))
        (type fixnum width)
        (inline canvas-draw-axes)
        (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
     (canvas-draw-axes canvas axes :width width :color color :erase? t))

(defun canvas-move-axes (canvas old-axes new-axes
                                           &key (width 1)  
                                           color 
                                           &allow-other-keys )
     "Moves axes "
     (declare (special  *host-or-mode* *host-bic-mode*
                      cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0))
        (type fixnum width))
     (when old-axes
          (with-focused-canvas canvas
  (let ((mp (cg::frame-child canvas)))
    ;(cg::set-line-width mp width) 18oct05
    (setf (cg:line-width mp) width) ;18oct05
           (cg::with-foreground-color (mp color)
            (loop with h = (canvas-height canvas)
              with xo =  (caar new-axes)
              with yo = (- h (cadar new-axes))
              for (x-old y-old) fixnum in (cdr old-axes)
              for (x y) fixnum in (cdr new-axes)
              do
              (mode-erase-line canvas xo yo x-old (- h y-old) *host-bic-mode*)
              (mode-draw-line canvas xo yo x (- h y) *host-or-mode*)
            ))))
   ))
         
