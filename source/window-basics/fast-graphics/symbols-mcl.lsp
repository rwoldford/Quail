;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              symbols-mcl.lisp
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
;;;     
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute)
(export  '(canvas-draw-symbol canvas-draw-symbols
           canvas-erase-symbols  canvas-move-symbols)))




(defun rgb-colors (colors &optional default)
  (if default (setq default (get-rgb-color default)))
  (if *color-available*
    (loop for c in colors collect (or (and c (get-rgb-color c))
                                      default))))

(defun choose-mode (canvas &key (erase? nil))
  (declare (ignore canvas)
           (special  *host-or-mode*  *host-bic-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (if erase? (#_PenMode *host-bic-mode*)
      (#_PenMode *host-or-mode*)))


(defun draw-single-color-symbols 
       (canvas points  
               &key size symbol fill? color (erase? nil) invisible?
               &allow-other-keys)
  "Draws or erases colored symbols with varying size, symbol fill? invisible?."
  
  
  (let* ((h (canvas-height canvas)))
    (declare (optimize (speed 3) (safety 0)
                       (space 0) (compilation-speed 0))
             (type fixnum h)
             )
    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (choose-mode canvas :erase? erase?)
        (set-draw-color canvas color)
        (loop 
          for (x y ) fixnum in points 
          for sim  in symbol 
          for fill   in fill? 
          for s fixnum in size
          for i  in invisible?
          unless i
          do  
          (fast-draw-symbol sim r x (- h y) s fill))))))


(defun xor-move-single-color-symbols
       (canvas old new 
               &key size (old-size size) fill? (old-fill? fill?)
               symbol (old-symbol symbol) color invisible?
               &allow-other-keys)
  
  "Moves colored (shaded) symbols with varying size, symbol fill? invisible?.~
   Results with color background are undefined."
  
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (set-pen-color canvas color)
  
  (let* ((h (canvas-height canvas)))
    (declare (type fixnum h))
    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (#_PenMode *host-xor-mode*)
        (loop for (xo yo ) fixnum in old
              for (x y ) fixnum in new 
              for sim in symbol for simo in old-symbol
              for fill  in fill? for fillo  in old-fill?
              for s fixnum in size for so fixnum in old-size
              for i  in invisible?
              unless i
              do  
              (fast-draw-symbol simo r xo (- h yo) so fillo)
              (fast-draw-symbol sim r x (- h y) s fill))))))



(defun move-single-color-symbols
       (canvas old new 
               &key size (old-size size) fill? (old-fill? fill?)
               symbol (old-symbol symbol)  color invisible?
               &allow-other-keys)
  "Moves colored symbols with varying size, symbol fill? invisible?."
  
  (declare (special  *host-or-mode* *host-bic-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  
  (let* ((h (canvas-height canvas)))
    (declare (type fixnum h))
    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (set-draw-color canvas color)
        (loop for (xo yo ) fixnum in old
              for (x y ) fixnum in new 
              for sim in symbol for simo in old-symbol
              for fill  in fill? for fillo  in old-fill?
              for s fixnum in size for so fixnum in old-size
              for i  in invisible?
              unless i
              do  (mode-draw-symbol simo r xo (- h yo) so fillo *host-bic-mode*)
              (mode-draw-symbol sim r x (- h y) s fill *host-or-mode*))))))



(defun draw-multi-color-symbols 
       (canvas points  
               &key size symbol fill? color invisible?
               &allow-other-keys)
  "Draws or erases symbols with varying size, symbol fill? color invisible?."
  (declare (special *host-or-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (let* ((h (canvas-height canvas)))
    (declare (type fixnum h))
    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (#_PenMode *host-or-mode*)
        (loop 
          for (x y ) fixnum in points 
          for sim in symbol 
          for fill  in fill? 
          for s fixnum in size
          for c in color
          for i  in invisible?
          unless i
          do  
          (set-draw-color canvas c)
          (fast-draw-symbol sim r x (- h y) s fill))))))


(defun xor-move-multi-color-symbols
       (canvas old new 
               &key size (old-size size) fill? (old-fill? fill?)
               invisible?
               symbol (old-symbol symbol)  color  &allow-other-keys)
  "Moves symbols with varying size, symbol fill? color invisible?.~
   Results with color background are undefined."
  
  ;; move  symbols with varying 
  ;; size, symbol fill?, color (shade),  invisible? 
  ;; results with color background are undefined
  (declare (special  *host-xor-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  
  (let* ((h (canvas-height canvas)))
    (declare (type fixnum h))
    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (#_PenMode *host-xor-mode*)
        (loop for (xo yo ) fixnum in old
              for (x y ) fixnum in new 
              for sim in symbol for simo in old-symbol
              for fill  in fill? for fillo  in old-fill?
              for s fixnum in size for so fixnum in old-size 
              for c in color
              for i  in invisible?
              unless i do 
              (set-draw-color canvas c)
              (fast-draw-symbol simo r xo (- h yo) so fillo )
              (fast-draw-symbol sim r x (- h y) s fill )) ))))



(defun move-multi-color-symbols
       (canvas old new 
               &key size (old-size size) fill? (old-fill? fill?) invisible?
               symbol (old-symbol symbol) color rgb-color?
               &allow-other-keys)
  "Moves symbols with varying size, symbol fill? color invisible?."
  
  (declare (special  *host-or-mode* *host-bic-mode*)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  
  (let* ((h (canvas-height canvas)))
    (declare (type fixnum h))
    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (loop for (xo yo ) fixnum in old
              for (x y ) fixnum in new 
              for sim in symbol for simo in old-symbol
              for fill  in fill? for fillo  in old-fill?
              for s fixnum in size for so fixnum in old-size
              for c in color
              for i  in invisible?
              unless i do  
              (if rgb-color?
                (cc-set-rgb-color c)
                (set-draw-color canvas c))
              (mode-draw-symbol simo r xo (- h yo) so fillo *host-bic-mode* )
              (mode-draw-symbol sim r x (- h y) s fill *host-or-mode* )
              
              )))))



(defun canvas-draw-symbols (canvas points 
                                   &key size symbol color fill? invisible?
                                   (erase? nil) single-color?
                                   &allow-other-keys)
  
  "Draws or erases symbols with varying size, symbol fill? color invisible?."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           )
  (if (or erase? single-color?)
    (draw-single-color-symbols canvas points 
                               :size size :symbol symbol
                               :color (car color)
                               :fill? fill?
                               :invisible? invisible? 
                               :erase? erase?)
    (draw-multi-color-symbols canvas points 
                              :size size :symbol symbol
                              :color color
                              :fill? fill?
                              :invisible? invisible? 
                              :erase? erase?)))


(defun canvas-erase-symbols (canvas points 
                                    &key size symbol color fill? invisible? single-color?
                                    &allow-other-keys)
  "Erases symbols with varying size symbol fill? color invisible? and flag ~
   single-color? identifying whether they are all of one colour or not."
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (canvas-draw-symbols canvas points :size size :symbol symbol
                       :color color :fill?  fill? :invisible? invisible?
                       :single-color? single-color?
                       :erase? t))

(defun canvas-move-symbols (canvas old-points new-points
                                   &key size symbol color fill? invisible? 
                                   (old-size size)
                                   (old-symbol symbol)
                                   old-color 
                                   rgb-color? single-color?
                                   (old-fill? fill?)
                                   &allow-other-keys)
  "Moves symbols with varying size, symbol fill? color invisible?."
  
  (declare (ignore old-color)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  
  
  (if single-color?
    (move-single-color-symbols canvas old-points new-points 
                               :size size :symbol symbol :fill? fill?
                               :old-size old-size :old-symbol old-symbol 
                               :invisible? invisible? 
                               :old-fill? old-fill? :color (car color ))
    (move-multi-color-symbols canvas old-points new-points
                              :size size :symbol symbol :fill? fill?
                              :old-size old-size :old-symbol old-symbol 
                              :old-fill? old-fill?
                              :invisible? invisible? 
                              :rgb-color? t
                              :color (if (not rgb-color? )
                                       (rgb-colors color (pen-color-of canvas)) color)
                              )))



(defun canvas-draw-symbol 
       (canvas x y  
               &key size symbol fill? color erase?
               &allow-other-keys)
  "Draw  symbol using size symbol fill?  color and erase?"
  
  (declare (special  *host-or-mode* )
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  
  (let* ((h (canvas-height canvas)))
    (declare (type fixnum h))
    (h-draw::with-rectangle-arg (r 0 0 1 1)
      (with-focused-canvas canvas
        (if (colored-canvas-p canvas)
          (setq color (get-rgb-color color)))
        (choose-mode canvas :erase? erase?)
        
        (set-draw-color canvas color)
        (fast-draw-symbol symbol r x (- h y) size fill?)))))



