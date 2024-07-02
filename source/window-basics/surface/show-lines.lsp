;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              show-lines.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(show-lines fast-show-lines)))

(defun fast-show-lines (canvas
                        x-origin y-origin
                        x y cz szv c ncol aa
                        depth-cue? erase? fast-color-table)
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
         cd dc col1 dd dimx dimy
         )
        
        (cond
         (depth-cue?
          (setf cd (+ (truncate (/ ncol 2))  (aref c 0)))
          (setf dc (- (* first-dim second-dim) 1 origin))
          ; (loop for j from dmy downto 1 by 1 do
          (do ((j dmy (decf j)))
              ((< j 1))
            (setf col1 (truncate (- cd (/ (* j (aref c 2)) dmy))))
            (setf dd dc)
            (fast-move-to
	     canvas
             (+ x-origin
                (the fixnum (aref x dd)))
             (host-to-canvas-y
              canvas
              (+ y-origin
                 (the fixnum (aref y dd)))))
            ; (loop for i from dmx downto 1 by 1 do
            (do ((i dmx (decf i)))
                ((< i 1))
              (set-draw-color 
               canvas
               (aref 
                fast-color-table
                (- col1
                   (truncate (/ (* (- i 0.5) (aref c 1)) dmx))
                   (truncate (* aa 
                                (/ (+ (aref cz dd)
                                      (aref cz (- dd step-in-first))
                                      )
                                   2)))
                   )))
              (decf dd step-in-first)
              (fast-line-to
	       canvas
               (+ x-origin
                  (the fixnum (aref x dd)))
               (host-to-canvas-y
                canvas
                (+ y-origin
                   (the fixnum (aref y dd)))))
              )
            (setf col1 (truncate (- cd (/ (* (- j 0.5) (aref c 2)) dmy))))
            (setf dd dc)
            ; (loop for i from dmx downto 0 by 1 do
            (do ((i dmx (decf i)))
                ((< i 0))
              (set-draw-color 
               canvas
               (aref 
                fast-color-table
                (- col1
                   (truncate (/ (* i (aref c 1)) dmx))
                   (truncate (* aa 
                                (/ (+ (aref cz dd)
                                      (aref cz (- dd step-in-second))
                                      )
                                   2))))))
              (fast-move-to
	       canvas
               (+ x-origin
                  (the fixnum (aref x dd)))
               (host-to-canvas-y
                canvas
                (+ y-origin
                   (the fixnum (aref y dd)))))
              (fast-line-to
	       canvas
               (+ x-origin
                  (the fixnum (aref x (- dd step-in-second))))
               (host-to-canvas-y
                canvas
                (+ y-origin
                   (the fixnum (aref y (- dd step-in-second))))))
              (decf dd step-in-first)
              )
            (decf dc step-in-second)
            )
          (setf dd dc)
          (fast-move-to
	   canvas
           (+ x-origin
              (the fixnum (aref x dd)))
           (host-to-canvas-y
            canvas
            (+ y-origin
               (the fixnum (aref y dd)))))
          ; (loop for i from dmx downto 1 by 1 do
          (do ((i dmx (decf i)))
              ((< i 1))
            (set-draw-color 
             canvas
             (aref 
              fast-color-table
              (- (truncate cd)
                 (truncate (/ (* (- i 0.5) (aref c 1)) dmx))
                 (truncate (* aa 
                              (/ (+ (aref cz dd)
                                    (aref cz (- dd step-in-first))
                                    )
                                 2)))
                 )))
            (decf dd step-in-first)
            (fast-line-to
	     canvas
             (+ x-origin
                (the fixnum (aref x dd)))
             (host-to-canvas-y
              canvas
              (+ y-origin
                 (the fixnum (aref y dd)))))
            )
          )
         (T
          (if (= (abs step-in-first) 1) 
            (progn (setf dimx dmy)
                   (setf dimy dmx))
            (progn (setf dimy dmy)
                   (setf dimx dmx)))
          (setf dc 0)
          (if erase?
            (set-draw-color canvas
                            (canvas-background-color canvas)
                            ;;(aref fast-color-table 0)
                            )
            (set-draw-color canvas
                            (aref fast-color-table (truncate (/ ncol 2))))
            )
          ; (loop for j from 0 to dimy do
          (do ((j 0 (incf j)))
              ((> j dimy))
            (setf dd dc)
            (fast-move-to 
             canvas
             (+ (the fixnum (aref x dd))
                x-origin)
             (host-to-canvas-y
              canvas
              (+ y-origin
                 (the fixnum (aref y dd)))))
            ; (loop for i from 0 to (- dimx 1) do
            (do ((i 0 (incf i)))
                ((= i dimx))
              (incf dd (1+ dimy))
              (fast-line-to
               canvas
               (+ x-origin
                  (the fixnum (aref x dd)))
               (host-to-canvas-y
                canvas
                (+ y-origin
                   (the fixnum (aref y dd)))))
              )
            (incf dc)
            )
          (setf dd 0)
          ; (loop for i from 0 to dimx do
          (do ((i 0 (incf i)))
              ((> i dimx))
            (fast-move-to 
             canvas
             (+ x-origin
                (the fixnum (aref x dd)))
             (host-to-canvas-y
              canvas
              (+ y-origin
                 (the fixnum (aref y dd)))))
            ; (loop for j from 0 to (- dimy 1) do
            (do ((j 0 (incf j)))
                ((= j dimy))
              (incf dd)
              (fast-line-to
               canvas
               (+ x-origin
                  (the fixnum (aref x dd)))
               (host-to-canvas-y
                canvas
                (+ y-origin
                   (the fixnum (aref y dd)))))
              )
            (incf dd)
            )
          
          )
         )
        ))))


(defun show-lines (canvas
                   x-origin y-origin
                   x y cz szv c ncol aa
                   depth-cue? color-table)
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
      
      (cond
       (depth-cue?
        ; (loop for j from dmy downto 1 by 1 do
        (do ((j dmy (decf j)))
            ((< j 1))
          (setf col1 (truncate (- cd (/ (* j (aref c 2)) dmy))))
          (setf dd dc)
          (canvas-move-to
           canvas 
           (+ x-origin
              (the fixnum (aref x dd)))
           (+ y-origin
              (the fixnum (aref y dd))))
          ; (loop for i from dmx downto 1 by 1 do
          (do ((i dmx (decf i)))
              ((< i 1))
            (decf dd step-in-first)
            (canvas-draw-to
             canvas
             (+ x-origin
                (the fixnum (aref x dd)))
             (+ y-origin
                (the fixnum (aref y dd)))
             :color
             (aref color-table
                   (- col1
                      (truncate (/ (* (- i 0.5) (aref c 1)) dmx))
                      (truncate (* aa 
                                   (/ (+ (aref cz dd)
                                         (aref cz (+ dd step-in-first))
                                         )
                                      2)))
                      ))
             )
            )
          (setf col1 (truncate (- cd (/ (* (- j 0.5) (aref c 2)) dmy))))
          (setf dd dc)
          ; (loop for i from dmx downto 0 by 1 do
          (do ((i dmx (decf i)))
              ((< i 0))
            (canvas-move-to
             canvas      
             (+ x-origin
                (the fixnum (aref x dd)))
             (+ y-origin
                (the fixnum (aref y dd))))
            (canvas-draw-to
             canvas
             (+ x-origin
                (the fixnum (aref x (- dd step-in-second))))
             (+ y-origin
                (the fixnum (aref y (- dd step-in-second))))
             :color
             (aref color-table
                   (- col1
                      (truncate (/ (* i (aref c 1)) dmx))
                      (truncate (* aa 
                                   (/ (+ (aref cz dd)
                                         (aref cz (- dd step-in-second))
                                         )
                                      2))))))
            (decf dd step-in-first)
            )
          (decf dc step-in-second)
          )
        (setf dd dc)
        (canvas-move-to
             canvas      
             (+ x-origin
                (the fixnum (aref x dd)))
             (+ y-origin
                (the fixnum (aref y dd))))
        ; (loop for i from dmx downto 1 by 1 do
        (do ((i dmx (decf i)))
            ((< i 1))
          (decf dd step-in-first)
          (canvas-draw-to
           canvas
           (+ x-origin
              (the fixnum (aref x dd)))
           (+ y-origin
              (the fixnum (aref y dd)))
           :color
           (aref color-table
                 (- (truncate cd)
                    (truncate (/ (* (- i 0.5) (aref c 1)) dmx))
                    (truncate (* aa 
                                 (/ (+ (aref cz dd)
                                       (aref cz (+ dd step-in-first))
                                       )
                                    2)))
                    ))
           )
          )
        )
       (T
        ; (loop for j from dmy downto 0 by 1 do
        (do ((j dmy (decf j)))
            ((< j 0))
          (setf col1 (truncate (- cd (/ (* j (aref c 2)) dmy))))
          (setf dd dc)
          (canvas-move-to
           canvas       
           (+ x-origin
              (the fixnum (aref x dd)))
           (+ y-origin
              (the fixnum (aref y dd))))
          ; (loop for i from (- dmx 1) downto 0 by 1 do
          (do ((i (1- dmx) (decf i)))
              ((< i 0))
            (decf dd step-in-first)
            (canvas-draw-to
             canvas
             (+ x-origin
                (the fixnum (aref x dd)))
             (+ y-origin
                (the fixnum (aref y dd)))
             :color (aref color-table (truncate (/ ncol 2))))
            )
          (decf dc step-in-second)
          )
        
        (setf dc (- (* first-dim second-dim) 1 origin))
        ; (loop for i from dmx downto 0 by 1 do
        (do ((i dmx (decf i)))
            ((< i 0))
          (setf col1 (truncate (- cd (/ (* i (aref c 1)) dmx))))
          (setf dd dc)
          (canvas-move-to canvas 
                          (+ x-origin
                             (the fixnum (aref x dd)))
                          (+ y-origin
                             (the fixnum (aref y dd))))
          ; (loop for j from (- dmy 1) downto 0 by 1 do
          (do ((j (1- dmy) (decf j)))
              ((< j 0))
            (decf dd step-in-second)
            (canvas-draw-to
             canvas
             (+ x-origin
                (the fixnum (aref x dd)))
             (+ y-origin
                (the fixnum (aref y dd)))
             :color (aref color-table (truncate (/ ncol 2))))
            )
          (decf dc step-in-first)
          )
        )
       )
      )))
