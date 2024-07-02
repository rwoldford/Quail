;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              surface-fill.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;      P. Poirier 1992
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :window-basics)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(surface-fill fast-surface-fill)))

(defun fast-surface-fill (canvas
                          x-origin y-origin
                          x y cz szv c ncol aa
                          fast-color-table)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline aref + - / * sqrt truncate round))
   #-:sbcl(declare (inline fast-move-to fast-line-to
                   ))
  (with-pen-values-restored canvas
    (with-focused-canvas canvas
      (let*
        ((first-dim (the fixnum (aref szv 0)))
         (second-dim (the fixnum (aref szv 1)))
         (step-in-first (the fixnum (aref szv 2)))
         (step-in-second  (the fixnum (aref szv 3)))
         (origin (the fixnum (aref szv 4)))
         (dmx (the fixnum (- first-dim 1)))
         (dmy (the fixnum (- second-dim 1)))
         cd dc col1 dd
         )
        
        (setf cd (+ (truncate (/ ncol 2))  (aref c 0)))
        (setf dc (- (* first-dim second-dim) 1 origin))
        ; (loop for j from dmy downto 1 by 1 do
        (do ((j dmy (decf j)))
            ((< j 1))
          (setf col1 (truncate (- cd (/ (* (- j 0.5) (aref c 2)) dmy))))
          (setf dd dc)
          ; (loop for i from dmx downto 1 by 1 do
          (do ((i dmx (decf i)))
              ((< i 1))
            (set-fast-color 
             canvas
             (aref fast-color-table
                   (- col1
                      (truncate (/ (* (- i 0.5) (aref c 1)) dmx))
                      (truncate 
                       (* aa 
                          (/ (+ (aref cz dd)
                                (aref cz (- dd step-in-first))
                                (aref cz (- dd step-in-first step-in-second))
                                (aref cz (- dd step-in-second))
                                )
                             4)))
                      )))
            (wb:canvas-draw-filled-polygon 
             canvas
             (list 
              (cons (+ x-origin (aref x dd)) 
                    (+ y-origin (aref y dd)))
              (cons (+ x-origin (aref x (- dd step-in-first)))
                    (+ y-origin (aref y (- dd step-in-first))))
              (cons (+ x-origin (aref x (- dd step-in-first step-in-second))) 
                    (+ y-origin (aref y (- dd step-in-first step-in-second))))
              (cons (+ x-origin (aref x (- dd step-in-second)))
                    (+ y-origin (aref y (- dd step-in-second))))))
            (decf dd step-in-first)
            )
          (decf dc step-in-second)
          )
        )
      )))

(defun surface-fill (canvas
                     x-origin y-origin
                     x y cz szv c ncol aa
                     color-table)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline + - * / aref))
  #-:sbcl(declare (inline canvas-move-to canvas-draw-to))
  (with-pen-values-restored canvas
    (let*
      ((first-dim (the fixnum (aref szv 0)))
       (second-dim (the fixnum (aref szv 1)))
       (step-in-first (the fixnum (aref szv 2)))
       (step-in-second  (the fixnum (aref szv 3)))
       (origin (the fixnum (aref szv 4)))
       (dmx (the fixnum (- first-dim 1)))
       (dmy (the fixnum (- second-dim 1)))
       cd dc col1 dd
       )
      
      (setf cd (+ (truncate (/ ncol 2))  (aref c 0)))
      (setf dc (- (* first-dim second-dim) 1 origin))
      ; (loop for j from dmy downto 1 by 1 do
      (do ((j dmy (decf j)))
          ((< j 1))
        (setf col1 (truncate (- cd (/ (* (- j 0.5) (aref c 2)) dmy))))
        (setf dd dc)
        ; (loop for i from dmx downto 1 by 1 do
        (do ((i dmx (decf i)))
            ((< i 1))
          (wb:canvas-draw-filled-polygon 
           canvas
           (list 
            (cons (+ x-origin (aref x dd)) 
                  (+ y-origin (aref y dd)))
            (cons (+ x-origin (aref x (- dd step-in-first)))
                  (+ y-origin (aref y (- dd step-in-first))))
            (cons (+ x-origin (aref x (- dd step-in-first step-in-second))) 
                  (+ y-origin (aref y (- dd step-in-first step-in-second))))
            (cons (+ x-origin (aref x (- dd step-in-second)))
                  (+ y-origin (aref y (- dd step-in-second)))))
           :color
           (aref color-table
                 (- col1
                    (truncate (/ (* (- i 0.5) (aref c 1)) dmx))
                    (truncate 
                     (* aa 
                        (/ (+ (aref cz dd)
                              (aref cz (- dd step-in-first))
                              (aref cz (- dd step-in-first step-in-second))
                              (aref cz (- dd step-in-second))
                              )
                           4)))
                    )))
          (decf dd step-in-first)
          )
        (decf dc step-in-second)
        )
      )
    ))
