;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              points-sblx.lsp
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
   #-:sbcl(declare 
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0)))
     (declare (ignore erase? ))
     (let ((mp (get-frame-pane canvas 'host-pane)))
        (loop
       for (x y) fixnum in points
       do 
       ;(format t "~%x and y are ~d ~d " x y)
       (medium-draw-point* mp  x y)
       )
      ))
;;;
(defun xor-move-points (canvas old new &key none &allow-other-keys )
   "Moves  single pixel points.~
Results with color background are undefined."
   #-:sbcl(declare 
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0)))
   (declare (ignore none))
   (let* ((mp (get-frame-pane canvas 'host-pane))
    (mpbg (medium-background (get-frame-pane canvas 'host-pane))))
     (loop 
       for (xo yo ) fixnum in old
       for (x y ) fixnum  in new
       do 
       (with-drawing-options (mp :ink mpbg)
        (medium-draw-point* mp xo yo))
       (medium-draw-point* mp x y))
       )
     )
;;;
(defun xor-draw-boxes (canvas points &key size erase? 
                        &allow-other-keys)
   "Draws or erases  filled squares with varying size.~
    Results with color background are undefined."
   #-:sbcl(declare 
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0)))
     (declare (ignore erase? ))
   (let* ((mp (get-frame-pane canvas 'host-pane)))
            (loop 
          for (x y ) fixnum in points for s fixnum in size
          do 
          (medium-draw-rectangle* mp x y (+ x s) (+ y s) T)
          )
      ))
;;; 
(defun xor-move-boxes (canvas old new &key size (old-size size)  
                        &allow-other-keys)
   "Moves filled squares with varying size.~
    Results with color background are undefined."
   #-:sbcl(declare 
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0)))
   (let* ((mp (get-frame-pane canvas 'host-pane))
    (mpbg (medium-background (get-frame-pane canvas 'host-pane))))
        (loop for (xo yo ) fixnum in old
          for (x y ) fixnum in new 
          for so fixnum in old-size 
          for s fixnum in size
          do 
          (with-drawing-options (mp :ink mpbg)
            (medium-draw-rectangle* mp xo yo (+ xo so 1) (+ yo so 1) T)
              )
          (medium-draw-rectangle* mp x y  (+ x s ) (+ y s) T)
      )))
;;;
(defun xor-draw-circles (canvas points &key size erase?
                          &allow-other-keys)
   "Draws or erases  filled circles with varying size.~
     Results with color background are undefined."
   #-:sbcl(declare 
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0)))
     (declare (ignore erase? ))
   ;(let ((mp (get-frame-pane canvas 'host-pane)))
        (loop 
          for (x y) fixnum in points for s fixnum in size
          do 
          (fast-draw-circle canvas x y s T)
          )
     ; )
   )
;;;
(defun xor-move-circles (canvas old new  &key size (old-size size)
                          &allow-other-keys)
   "Moves  filled circles with varying size.~
    Results with color background are undefined."
   #-:sbcl(declare 
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0))
     )
   (let* ((mp (get-frame-pane canvas 'host-pane))
    (mpbg (medium-background (get-frame-pane canvas 'host-pane))))
      (loop for (xo yo ) fixnum in old
        for (x y ) fixnum in new 
        for so fixnum in old-size 
        for s fixnum in size
        do  
          (with-drawing-options (mp :ink mpbg)
            (fast-erase-circle canvas xo yo so T)
              )
          (fast-draw-circle canvas x y s T)
        ))
   )
;;; DONE