;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              symbols-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1991 George Washington University
;;;     N.G. Bennett 1993
;;;     G.W. Bennett 1996
;;;     
;;;     
;;;----------------------------------------------------------------------------------
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-draw-symbol canvas-draw-symbols
           canvas-erase-symbols  canvas-move-symbols)))

(defun rgb-colors (colors &optional default)
;;(if default (setq default (get-rgb-color default))) it's an rgb anyway in ACL
  (if *color-available*
    (loop for c in colors collect (or (and c c)
                                      default))))

#|
(defun rgb-colors (colors)
   colors
    (if *color-available*
   (loop for c in colors
     collect (if c
                (get-rgb-color c))))


(defun choose-mode (canvas &key (erase? nil))
   (declare 
     (special  *host-or-mode*  *host-bic-mode*
       cg::po-erase cg::po-invert cg::po-replace cg::po-paint)
     (optimize (speed 3) (safety 0)
       (space 0) (compilation-speed 0)))
   (if erase? (h-draw::set-pen-mode canvas *host-bic-mode*)
      (h-draw::set-pen-mode canvas *host-or-mode*)))
|#

(defun draw-single-color-symbols 
      (canvas points  
       &key size symbol fill? color (erase? nil) invisible?
       &allow-other-keys)
     "Draws or erases colored symbols with varying size, symbol fill? invisible?."
     #-:sbcl(declare (optimize (speed 3) (safety 0)
                      (space 0) (compilation-speed 0)))
  (let ((mp (get-frame-pane canvas 'host-pane)))
        (loop 
          for (x y) fixnum in points 
          for sim  in symbol 
          for fill   in fill? 
          for s fixnum in size
          for i  in invisible?
          unless i
          do
            (with-drawing-options (mp :ink color)
          (if erase?
             (fast-erase-symbol canvas sim x y s fill)
               (fast-draw-symbol canvas sim x y s fill)
               )
              )
          )
             ))

(defun xor-move-single-color-symbols
      (canvas old new 
       &key size (old-size size) fill? (old-fill? fill?)
       symbol (old-symbol symbol) color invisible?
       &allow-other-keys)
     "Moves colored (shaded) symbols with varying size, symbol fill? invisible?.~
      Results with color background are undefined."
     #-:sbcl(declare 
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0)))
     (set-pen-color canvas color)
    ;(let ((mp (get-frame-pane canvas 'host-pane)))
      (loop 
        for (xo yo ) fixnum in old
        for (x y ) fixnum in new 
        for sim in symbol for simo in old-symbol
        for fill  in fill? for fillo  in old-fill?
        for s fixnum in size for so fixnum in old-size
        for i  in invisible?
        unless i
        do 
        (fast-erase-symbol canvas simo xo yo so fillo)
         (fast-draw-symbol canvas sim x y s fill)
         )
        ;)
     )
;;; since clim has no dra3wing modes, move-single-color-symbols is just a call to xor-move-single-color-symbols
(defun move-single-color-symbols
      (canvas old new 
       &key size (old-size size) fill? (old-fill? fill?)
       symbol (old-symbol symbol)  color invisible?
       &allow-other-keys)
     "Moves colored symbols with varying size, symbol fill? invisible?."
(xor-move-single-color-symbols canvas old new :size size :old-size old-size
  :fill? fill? :old-fill? old-fill? :symbol symbol :old-symbol old-symbol
  :color color :invisible? invisible?)
)
;;;
(defun draw-multi-color-symbols 
      (canvas points  
       &key size symbol erase? fill? color invisible?
       &allow-other-keys)
     "Draws or erases symbols with varying size, symbol fill? color invisible?."
     #-:sbcl(declare 
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0)))
   (let ((mp (get-frame-pane canvas 'host-pane)))
      (loop 
        for (x y ) fixnum in points 
        for sim in symbol 
        for erase in erase?
        for fill  in fill? 
        for s fixnum in size
        for c in color
        for i  in invisible?
        unless i
        do  
        (if erase
           (fast-erase-symbol canvas sim x y s fill)
           (with-drawing-options (mp :ink c)
            (fast-draw-symbol canvas sim x y s fill)))
        )
        ))
;;;
(defun xor-move-multi-color-symbols
      (canvas old new 
       &key size (old-size size) fill? (old-fill? fill?)
       invisible?
       symbol (old-symbol symbol)  color  &allow-other-keys)
     "Moves symbols with varying size, symbol fill? color invisible?.~
Results with color background are undefined."
     #-:sbcl(declare 
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0)))
   (let ((mp (get-frame-pane canvas 'host-pane)))
      (loop 
        for (xo yo ) fixnum in old
        for (x y ) fixnum in new 
        for sim in symbol for simo in old-symbol
        for fill  in fill? for fillo  in old-fill?
        for s fixnum in size for so fixnum in old-size 
        for c in color
        for i  in invisible?
        unless i do 
        (fast-erase-symbol canvas simo xo yo so fillo)
        (with-drawing-options (mp :ink c)
         (fast-draw-symbol canvas sim x  y s fill ))
        )
     ))
;;;
;;; since clim does not have drawing modes, the following could be  just a call to (test-)xor-move-multi-color-symbols
;;; were it not for :rgb-color?
;;; I cannot find cc-set-rgb-color or set-draw-color in source/**/*.lsp !!
(defun move-multi-color-symbols
      (canvas old new 
       &key size (old-size size) fill? (old-fill? fill?) invisible?
       symbol (old-symbol symbol) color rgb-color?
       &allow-other-keys)
     "Moves symbols with varying size, symbol fill? color invisible?."
     #-:sbcl(declare 
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0)))
     (declare (ignorable rgb-color?)) ;09MAY2024
   (let ((mp (get-frame-pane canvas 'host-pane)))
      (loop  
        for (xo yo ) fixnum in old
        for (x y ) fixnum in new 
        for sim in symbol for simo in old-symbol
        for fill  in fill? for fillo  in old-fill?
        for s fixnum in size for so fixnum in old-size
        for c in color
        for i  in invisible?
        unless i do  
        ;(if rgb-color?
           ;(cc-set-rgb-color canvas c)
           ;(set-draw-color canvas c)) ;;; is all we are doing is setting (medium-foreground pane-of-canvas) to be c ?
        ;   )
        (fast-erase-symbol canvas simo xo yo so fillo)
        (with-drawing-options (mp :ink c) ;;; if so, that's what happens here .. so I could (declare (ignore rgb-color?)) at the outset
         (fast-draw-symbol canvas sim x  y s fill ))
        )
     ))
;;;
(defun canvas-draw-symbols (canvas points  &key size symbol color fill? 
                             invisible? (erase? nil) single-color?
                                                &allow-other-keys)
     "Draws or erases symbols with varying size, symbol fill? color invisible?."
     #-:sbcl(declare (optimize (speed 3) (safety 0)
                      (space 0) (compilation-speed 0))
        )
     ;(let ((mp (get-frame-pane canvas 'host-pane)))
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
        :erase? (coerce (make-array (length points) :initial-element erase?) 'list))
        )
      ;)
     )
;;;
(defun canvas-erase-symbols (canvas points 
  &key size symbol color fill? invisible? single-color?
                                                 &allow-other-keys)
     "Erases symbols with varying size symbol fill? color invisible? and flag ~
       single-color? identifying whether they are all of one colour or not."
     #-:sbcl(declare (optimize (speed 3) (safety 0)
                      (space 0) (compilation-speed 0))
        )
     (canvas-draw-symbols canvas points :size size :symbol symbol
      :color color :fill?  fill? :invisible? invisible?
      :single-color? single-color?
      :erase? t))
;;;
(defun canvas-move-symbols (canvas old-points new-points
                                                 &key size symbol color fill? invisible? 
                                                 (old-size size)
                                                 (old-symbol symbol)
                                                 old-color 
                                                 rgb-color? single-color?
                                                 (old-fill? fill?)
                                                 &allow-other-keys)
     "Moves symbols with varying size, symbol fill? color invisible?."
     #-:sbcl(declare 
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0))
        )
     (declare (ignore old-color))
     (if single-color?
        (move-single-color-symbols canvas old-points new-points 
         :size size :symbol symbol :fill? fill?
         :old-size old-size :old-symbol old-symbol 
         :invisible? invisible? 
         :old-fill? old-fill? :color  (first color))
        (move-multi-color-symbols canvas old-points new-points
         :size size :symbol symbol :fill? fill?
         :old-size old-size :old-symbol old-symbol 
         :old-fill? old-fill?
         :invisible? invisible? 
         :rgb-color? t
         :color color
         )
        )
     )
;;;
(defun canvas-draw-symbol 
      (canvas x y  
       &key size symbol fill? color erase?
       &allow-other-keys)
     "Draw/erase  symbol using size symbol fill? color and erase?"
     #-:sbcl(declare  
        (optimize (speed 3) (safety 0)
           (space 0) (compilation-speed 0)))
  (let* ((mp (get-frame-pane canvas 'host-pane))
    (mpbg (medium-background (get-frame-pane canvas 'host-pane))))
      ;(if (colored-canvas-p canvas)
      ;   (setq color color))
      ;(choose-mode canvas :erase? erase?)
      ;(set-draw-color canvas color)
      (if erase?
        (with-drawing-options (mp :ink mpbg)
        (fast-erase-symbol canvas symbol x y size fill?))
        (with-drawing-options (mp :ink color)
           (fast-draw-symbol canvas symbol x y size fill?))
      )
    ))
