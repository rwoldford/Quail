;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              points-pc.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1991 George Washington University
;;;     G.W. Bennett 1996
;;;     
;;;     
;;;----------------------------------------------------------------------------------

(in-package :wb)

(defun xor-draw-points (canvas points &key erase? &allow-other-keys)
   "Draws or erases single pixel points ~
Results with color background are undefined."
   (declare (special *black-color* *host-xor-mode*
              cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0))
     (ignore erase? ))
   (with-focused-canvas canvas
     (h-draw::set-pen-mode canvas *host-xor-mode*)
     (loop with h fixnum = (canvas-height canvas)
       for (x y1) fixnum in points
       for y fixnum = (- h y1)
       do 
       (fast-draw-line canvas x y x y))))

(defun xor-move-points (canvas old new &key none &allow-other-keys )
   "Moves  single pixel points.~
Results with color background are undefined."
   (declare (ignore  none)
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0))
     (special  *host-xor-mode* cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
   (with-focused-canvas canvas
     (h-draw::set-pen-mode canvas *host-xor-mode*)
     (loop with h fixnum = (canvas-height canvas)
       for (xo yo1 ) fixnum in old
       for (x y1 ) fixnum  in new
       for yo fixnum = (- h yo1)
       for y fixnum = (- h y1)
       do 
           (cg::with-paint-operation 
               ;(canvas cg::erase) 19oct05
               (canvas cg::po-erase) ;19oct05
        (cg::erase-line canvas (cg::make-position xo yo)
         (cg::make-position xo yo))
        )
       ;;          (fast-draw-line canvas xo yo xo yo)
       (fast-draw-line canvas x y x y)
       )
     ))

(defun xor-draw-boxes (canvas points &key size erase? 
                        &allow-other-keys)
   "Draws or erases  filled squares with varying size.~
    Results with color background are undefined."
   (declare (special  *host-xor-mode*
              cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0))
     (ignore erase? ))
   (let* ((h (canvas-height canvas)))
      (with-focused-canvas canvas
        (h-draw::set-pen-mode canvas *host-xor-mode*)
        (loop 
          for (x y ) fixnum in points for s fixnum in size
          do 
          (cg::draw-box canvas (cg::make-box-from-corners
                                (cg::make-position x (- h y))
                                (cg::make-position (+ x s) (- h (- y s))))) 
          ))))

(defun xor-move-boxes (canvas old new &key size (old-size size)  
                        &allow-other-keys)
   "Moves filled squares with varying size.~
    Results with color background are undefined."
   (declare (special  *host-xor-mode*
              cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0))
     )
   (let* ((h (canvas-height canvas)))
      ;;    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (h-draw::set-pen-mode canvas *host-xor-mode*)
        (loop for (xo yo ) fixnum in old
          for (x y ) fixnum in new 
          for so fixnum in old-size 
          for s fixnum in size
          do 
              (cg::with-paint-operation 
                  ;(canvas cg::erase )19oct05
               (canvas cg::po-erase) ;19oct05
           (cg::erase-box canvas (cg::make-box-from-corners
                                  (cg::make-position xo (- h yo))
                                  (cg::make-position (+ xo so) (- h (- yo so)))))
           )
          (cg::draw-box canvas (cg::make-box-from-corners
                                (cg::make-position x (- h y))
                                (cg::make-position (+ x s) (- h (- y s)))))
          ))))

(defun xor-draw-circles (canvas points &key size erase?
                          &allow-other-keys)
   "Draws or erases  filled circles with varying size.~
     Results with color background are undefined."
   (declare (special  *host-xor-mode* fill?
              cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0))
     (ignore erase? ))
   (let* ((h (canvas-height canvas)))    
      (with-focused-canvas canvas
        (h-draw::set-pen-mode canvas *host-xor-mode*)
        (loop 
          for (x y ) fixnum in points for s fixnum in size
          do  
          (fast-draw-circle canvas x (- h y) s fill?)
          ))))

(defun xor-move-circles (canvas old new  &key size (old-size size)
                          &allow-other-keys)
   "Moves  filled circles with varying size.~
    Results with color background are undefined."
   (declare (special  *host-xor-mode* fill?
              cg::po-paint cg::po-replace cg::po-invert cg::po-erase)
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0))
     )
   (let* ((h (canvas-height canvas)))
      (h-draw::set-pen-mode canvas *host-xor-mode*)
      (loop for (xo yo ) fixnum in old
        for (x y ) fixnum in new 
        for so fixnum in old-size 
        for s fixnum in size
        do  
            (cg::with-paint-operation 
                ;(canvas cg::erase)19oct05
               (canvas cg::po-erase) ;19oct05
         (fast-erase-circle canvas xo (- h yo) so fill?)
         )
        (fast-draw-circle canvas x (- h y) s fill?)
        )))

;; END of points-pc.lsp