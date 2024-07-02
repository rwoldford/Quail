;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              hide-lines.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(hide-lines fast-hide-lines)))

(defun hide-lines (canvas x y cz lower-y upper-y
                          dl nl
                          szv x-origin y-origin
                          mx my
                          a c ncol aa depth-cue? color-table)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline aref + - / * sqrt truncate))
  #-:sbcl-linux(declare (inline show-line))
  (with-pen-values-restored
    canvas
    (let*
      ((first-dim (aref szv 0))
       (second-dim (aref szv 1))
       (step-in-first (aref szv 2))
       (step-in-second (aref szv 3))
       (origin (aref szv 4))
       (dmx (- first-dim 1))
       (dmy (- second-dim 1))
       cd dc col1 dd col2
       a2 a5 base st ct
       )
      ; (loop for i from 0 to (- mx 1) do
      (do ((i 0 (incf i)))
          ((= i mx))
        (setf (aref lower-y i) my)
        (setf (aref upper-y i) 0))
       (setf a2 (aref a 2))
       (cond ((< (abs a2) 1e-5)
              (setf st 0.0)
              (setf ct 1.0))
             (T
              (setf a5 (aref a 5))
              (setf base (sqrt (+ (* a2 a2)
                                  (* a5 a5))))
              (setf st (/ a2 base))
              (setf ct (/ a5 base))))
      (setf dc origin)
      (cond 
       (depth-cue?
       (setf cd (+ (truncate (/ ncol 2)) (aref c 0)))
       ; (loop for j from 0 to (- dmy 1) do
       (do ((j 0 (incf j)))
           ((= j dmy))
         (setf col1 (truncate (- cd (/ (* j (aref c 2)) dmy))))
         (setf dd dc)
         ; (loop for i from 0 to (- dmx 1) do
         (do ((i 0 (incf i)))
             ((= i dmx))
           (setf col2 (- col1
                         (truncate (/ (* (+ i 0.5) (aref c 1)) dmx))
                         (truncate (* aa 
                                      (/ (+ (aref cz dd)
                                            (aref cz (+ dd step-in-first))
                                            )
                                         2)))))
           (show-line canvas
                      (aref x dd) (aref y dd)
                      (aref x (+ dd step-in-first)) (aref y (+ dd step-in-first))
                      x-origin y-origin
                      lower-y upper-y
                      dl nl
                      ct st mx my col2
                      color-table)
           (incf dd step-in-first))
         (setf col1 (truncate (- cd (/ (* (+ j 0.5) (aref c 2)) dmy))))
         (setf dd dc)
         ; (loop for i from 0 to dmx do
         (do ((i 0 (incf i)))
             ((> i dmx))
           (setf col2 (- col1
                         (truncate (/ (* i (aref c 1)) dmx))
                         (truncate (* aa 
                                      (/ (+ (aref cz dd)
                                            (aref cz (+ dd step-in-second))
                                            )
                                         2)))))
           (show-line canvas
                      (aref x dd) (aref y dd)
                      (aref x (+ dd step-in-second)) (aref y (+ dd step-in-second))
                      x-origin y-origin
                      lower-y upper-y
                      dl nl
                      ct st mx my col2
                      color-table)
           (incf dd step-in-first))
         (incf dc step-in-second))
       (setf dd dc)
       (setf cd (truncate (- cd (aref c 2))))
       ; (loop for i from 0 to (- dmx 1) do
       (do ((i 0 (incf i)))
           ((= i dmx))
         (setf col2 (- cd
                       (truncate (/ (* (+ i 0.5) (aref c 1)) dmx))
                       (truncate (* aa 
                                    (/ (+ (aref cz dd)
                                          (aref cz (+ dd step-in-first))
                                          )
                                       2)))))
         (show-line canvas 
                    (aref x dd) (aref y dd)
                    (aref x (+ dd step-in-first)) (aref y (+ dd step-in-first))
                    x-origin y-origin
                    lower-y upper-y
                    dl nl
                    ct st mx my col2
                    color-table)
         (incf dd step-in-first))
       )
      (T
       (setf col2 (truncate (/ ncol 2)))
       ; (loop for j from 0 to (- dmy 1) do
       (do ((j 0 (incf j)))
           ((= j dmy))
         (setf dd dc)
         ; (loop for i from 0 to (- dmx 1) do
         (do ((i 0 (incf i)))
             ((= i dmx))
           (show-line canvas
                      (aref x dd) (aref y dd)
                      (aref x (+ dd step-in-first)) (aref y (+ dd step-in-first))
                      x-origin y-origin
                      lower-y upper-y
                      dl nl
                      ct st mx my col2
                      color-table)
           (incf dd step-in-first))
         (setf dd dc)
         ; (loop for i from 0 to dmx do
         (do ((i 0 (incf i)))
             ((> i dmx))
           (show-line canvas
                      (aref x dd) (aref y dd)
                      (aref x (+ dd step-in-second)) (aref y (+ dd step-in-second))
                      x-origin y-origin
                      lower-y upper-y
                      dl nl
                      ct st mx my col2
                      color-table)
           (incf dd step-in-first))
         (incf dc step-in-second))
       (setf dd dc)
       ; (loop for i from 0 to (- dmx 1) do
       (do ((i 0 (incf i)))
           ((= i dmx))
         (show-line canvas 
                    (aref x dd) (aref y dd)
                    (aref x (+ dd step-in-first)) (aref y (+ dd step-in-first))
                    x-origin y-origin
                    lower-y upper-y
                    dl nl
                    ct st mx my col2
                    color-table)
         (incf dd step-in-first))
       )
      ))))


(defun show-line (canvas x1 y1 x2 y2 x-origin y-origin
                         lower-y upper-y dl nl 
                         ct st mx my col2 color-table)
  
  (declare (fixnum x1 y1 x2 y2 mx my col2)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline aref + - / * sqrt truncate round))
   #-:sbcl-linux(declare (inline canvas-move-to canvas-draw-to
                   update-border))
  (let (nx1 ny1 nx2 ny2 s)
    (setf mx (truncate mx 2))
    (setf my (truncate my 2))
    (decf x1 mx)
    (decf y1 my)
    (decf x2 mx)
    (decf y2 my)
    
    (setf nx1 (round (+ (* x1 ct) (* y1 (- st)) mx)))
    (setf ny1 (round (+ (* y1 ct) (* x1 st) my )))
    (setf nx2 (round (+ (* x2 ct) (* y2 (- st)) mx)))
    (setf ny2 (round (+ (* y2 ct) (* x2 st) my)))
    
    (setf s 1)
    (setf (aref nl 0) 0)
    (update-border upper-y nx1 ny1 nx2 ny2 s dl nl)
    (when (> (aref nl 0) 0)
      (setf (aref dl 0) (- (aref dl 0) mx))
      (setf (aref dl 1) (- (aref dl 1) my))
      (setf (aref dl 2) (- (aref dl 2) mx))
      (setf (aref dl 3) (- (aref dl 3) my))
      (canvas-move-to canvas 
                          (round (+ (* (aref dl 0) ct)
                                    (* (aref dl 1) st)
                                    mx
                                    x-origin))
                          (round (+ (* (aref dl 1) ct)
                                    (* (aref dl 0) (- st))
                                    my
                                    y-origin)))
      (canvas-draw-to canvas 
                          (round (+ (* (aref dl 2) ct)
                                    (* (aref dl 3) st)
                                    mx
                                    x-origin))
                          (round (+ (* (aref dl 3) ct)
                                    (* (aref dl 2) (- st))
                                    my
                                    y-origin))
                          :color
                          (aref color-table col2))
      )
    (when (> (aref nl 0) 1)
      (setf (aref dl 4) (- (aref dl 4) mx))
      (setf (aref dl 5) (- (aref dl 5) my))
      (setf (aref dl 6) (- (aref dl 6) mx))
      (setf (aref dl 7) (- (aref dl 7) my))
      (canvas-move-to canvas 
                          (round (+ (* (aref dl 4) ct)
                                    (* (aref dl 5) st)
                                    mx
                                    x-origin))
                          (round (+ (* (aref dl 5) ct)
                                    (* (aref dl 4) (- st))
                                    my
                                    y-origin)))
      (canvas-draw-to canvas 
                          (round (+ (* (aref dl 6) ct)
                                    (* (aref dl 7) st)
                                    mx
                                    x-origin))
                          (round (+ (* (aref dl 7) ct)
                                    (* (aref dl 6) (- st))
                                    my
                                    y-origin))
                          :color
                          (aref color-table col2))
      )
    
    (setf s -1)
    (setf (aref nl 0) 0)
    (update-border lower-y nx1 ny1 nx2 ny2 s dl nl)
    (when (> (aref nl 0) 0)
      (setf (aref dl 0) (- (aref dl 0) mx))
      (setf (aref dl 1) (- (aref dl 1) my))
      (setf (aref dl 2) (- (aref dl 2) mx))
      (setf (aref dl 3) (- (aref dl 3) my))
      (canvas-move-to canvas 
                          (round (+ (* (aref dl 0) ct)
                                    (* (aref dl 1) st)
                                    mx
                                    x-origin))
                          (round (+ (* (aref dl 1) ct)
                                    (* (aref dl 0) (- st))
                                    my
                                    y-origin)))
      (canvas-draw-to canvas 
                          (round (+ (* (aref dl 2) ct)
                                    (* (aref dl 3) st)
                                    mx
                                    x-origin))
                          (round (+ (* (aref dl 3) ct)
                                    (* (aref dl 2) (- st))
                                    my
                                    y-origin))
                          :color
                          (aref color-table
                                 col2))
      )
    (when (> (aref nl 0) 1)
      (setf (aref dl 4) (- (aref dl 4) mx))
      (setf (aref dl 5) (- (aref dl 5) my))
      (setf (aref dl 6) (- (aref dl 6) mx))
      (setf (aref dl 7) (- (aref dl 7) my))
      (canvas-move-to canvas 
                          (round (+ (* (aref dl 4) ct)
                                    (* (aref dl 5) st)
                                    mx
                                    x-origin))
                          (round (+ (* (aref dl 5) ct)
                                    (* (aref dl 4) (- st))
                                    my
                                    y-origin)))
      (canvas-draw-to canvas 
                          (round (+ (* (aref dl 6) ct)
                                    (* (aref dl 7) st)
                                    mx
                                    x-origin))
                          (round (+ (* (aref dl 7) ct)
                                    (* (aref dl 6) (- st))
                                    my
                                    y-origin))
                          :color
                          (aref color-table
                                 col2))
      )
    )
  )


(defun update-border (border x1 y1 x2 y2 s dl nl)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline aref + - / * ))
  #-:sbcl-linux(declare (inline update-line))
  
  (let ((dx 0)
        (dy 0)
        (up T)
        (tx1 0) (ty1 0) (tx2 0)
        (sx 0) (sy 0) (dxa 0) (dya 0)
        (d 0) (inc1 0) (inc2 0)
        (x 0) (y 0)
        )
    
      (declare (fixnum dxa dya dx dy tx1 tx2 ty1 sx sy d inc1 inc2 x y s x1 y1 x2 y2))
   (block outer     
      (setf tx2 0)
      
      (setf dx (- x2 x1))
      (setf dy (- y2 y1))
      
      (if (and (= 0 dx) (= 0 dy))
        (return-from outer 0))
      
      (if (> dx 0)
        (setf sx 1)
        (setf sx -1))
      (setf dxa (* dx sx))
      
      (if (> dy 0)
        (setf sy 1)
        (setf sy -1))
      (setf dya (* dy sy))

      (setf up nil)
      (if (> dya dxa)
        (progn 
          (setf inc1 (+ dxa dxa))
          (setf d (- inc1 dya))
          (setf inc2 (- d dya))
          (if (> (* s dy) 0)
            (progn
              (setf x x1)
              (do ((y y1 (+ y sy)))
                  ((if (minusp sy)
                     (< y y2)
                     (> y y2)))
                (if (> (* s (- y (aref border x))) 0)
                  (progn
                    (if (not up)
                      (progn
                        (setf up t)
                        (setf tx1 x)
                        (setf ty1 y)
                        ))
                    (setf (aref border x) y)
                    )
                  (if up
                    (progn
                      (setf up nil)
                      (update-line tx1 ty1 tx2 (- y sy) dl nl)
                      )
                    )
                  )
                (setf tx2 x)
                (if (< d 0)
                  (incf d inc1)
                  (progn
                    (incf d inc2)
                    (incf x sx)
                    )
                  )
                )
              (if up
                (update-line tx1 ty1 x2 y2 dl nl)
                )
              (return-from outer 0)
              )
            )
          (setf x x2)
          (do ((y y2 (- y sy)))
              ((if (minusp sy)
                 (> y y1)
                 (< y y1)))
            (if (> (* s (- y (aref border x))) 0)
              (progn
                (if (not up)
                  (progn
                    (setf up t)
                    (setf tx1 x)
                    (setf ty1 y)
                    ))
                (setf (aref border x) y)
                )
              (if up
                (progn
                  (setf up nil)
                  (update-line tx2 (+ y sy) tx1 ty1 dl nl)
                  )
                )
              )
            (setf tx2 x)
            (if (<= d 0)
              (incf d inc1)
              (progn
                (incf d inc2)
                (incf x (- sx))
                )
              )
            )
          (if up
            (update-line x1 y1 tx1 ty1 dl nl)
            )
          (return-from outer 0)
          )
        )
      (setf inc1 (+ dya dya))
      (setf d (- inc1 dxa))
      (setf inc2 (- d dxa))
      (setf y y1)
      (do ((x x1 (+ x sx)))
          ((if (minusp sx)
             (< x x2)
             (> x x2)))
        (if (> (* s (- y (aref border x))) 0)
          (progn
            (if (not up)
              (progn
                (setf up t)
                (setf tx1 x)
                (setf ty1 y)
                ))
            (setf (aref border x) y)
            )
          (if up
            (progn
              (setf up nil)
              (update-line tx1 ty1 (- x sx) (aref border (- x sx)) dl nl)
              )
            )
          )
        (if (<= d 0)
          (incf d inc1)
          (progn
            (incf d inc2)
            (incf y sy)
            )
          )
        )
      (if up
        (update-line tx1 ty1 x2 y2 dl nl)
        )
      (return-from outer  0)
      )
    )
  )


(defun update-line (x1 y1 x2 y2 dl nl)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline aref + - / * > =))
  
  (let ((n 0) (size1 0)  (size2 0)  (size3 0))
    (declare (fixnum x1 y1 x2 y2 n size1 size2 size3))
    (block outer
      (setf n (aref nl 0))
      (if (> n 1)
        (progn
          (setf size1 (+ (* (- x2 x1)
                            (- x2 x1))
                         (* (- y2 y1)
                            (- y2 y1))))
          (setf size2 (+ (* (- (aref dl 3) (aref dl 1))
                            (- (aref dl 3) (aref dl 1)))
                         (* (- (aref dl 2) (aref dl 0))
                            (- (aref dl 2) (aref dl 0)))))
          (setf size3 (+ (* (- (aref dl 7) (aref dl 5))
                            (- (aref dl 7) (aref dl 5)))
                         (* (- (aref dl 6) (aref dl 4))
                            (- (aref dl 6) (aref dl 4)))))
          (if (> size1 size2)
            (if (> size2 size3)
              (setf n 1)
              (setf n 0)
              )
            (if (> size1 size3)
              (setf n 1)
              (return-from outer 0)
              )
            )
          )
        (incf (aref nl 0) 1)
        )
      (if (= n 0)
        (progn
          (setf (aref dl 0) x1)
          (setf (aref dl 1) y1)
          (setf (aref dl 2) x2)
          (setf (aref dl 3) y2)    
          )
        (progn
          (setf (aref dl 4) x1)
          (setf (aref dl 5) y1)
          (setf (aref dl 6) x2)
          (setf (aref dl 7) y2)    
          )
        )
      (return-from outer 1)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;          Faster versions of same
;;;

(defun fast-hide-lines (canvas x y cz lower-y upper-y
                               dl nl
                               szv x-origin y-origin
                               mx my
                               a c ncol aa depth-cue? color-table)
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline aref + - / * sqrt truncate ))
  #-:sbcl-linux(declare (inline fast-show-line))
  (with-pen-values-restored canvas
    (with-focused-canvas canvas
      (let*
        ((first-dim (aref szv 0))
         (second-dim (aref szv 1))
         (step-in-first (aref szv 2))
         (step-in-second (aref szv 3))
         (origin (aref szv 4))
         (dmx (- first-dim 1))
         (dmy (- second-dim 1))
         cd dc col1 dd col2
         a2 a5 base st ct
         )
        ; (loop for i from 0 to (- mx 1) do 
        (do ((i 0 (incf i)))
            ((= i mx))
          (setf (aref lower-y i) my)
          (setf (aref upper-y i) 0))
        (setf dc origin)
        (setf a2 (aref a 2))
        (cond ((< (abs a2) 1e-5)
               (setf st 0.0)
               (setf ct 1.0))
              (T
               (setf a5 (aref a 5))
               (setf base (sqrt (+ (* a2 a2)
                                   (* a5 a5))))
               (setf st (/ a2 base))
               (setf ct (/ a5 base))))
        (cond
         (depth-cue?
          (setf cd (+ (truncate (/ ncol 2)) (aref c 0)))
          ; (loop for j from 0 to (- dmy 1) do
          (do ((j 0 (incf j)))
              ((= j dmy))
            (setf col1 (truncate (- cd (/ (* j (aref c 2)) dmy))))
            (setf dd dc)
            ; (loop for i from 0 to (- dmx 1) do
            (do ((i 0 (incf i)))
                ((= i dmx))
              (setf col2 (- col1
                            (truncate (/ (* (+ i 0.5) (aref c 1)) dmx))
                            (truncate (* aa 
                                         (/ (+ (aref cz dd)
                                               (aref cz (+ dd step-in-first))
                                               )
                                            2)))))
              (fast-show-line canvas
                              (aref x dd) (aref y dd)
                              (aref x (+ dd step-in-first))
                              (aref y (+ dd step-in-first))
                              x-origin y-origin
                              lower-y upper-y
                              dl nl
                              ct st mx my col2
                              color-table)
              (incf dd step-in-first))
            (setf col1 (truncate (- cd (/ (* (+ j 0.5) (aref c 2)) dmy))))
            (setf dd dc)
            ; (loop for i from 0 to dmx do
            (do ((i 0 (incf i)))
                ((> i dmx))
              (setf col2 (- col1
                            (truncate (/ (* i (aref c 1)) dmx))
                            (truncate (* aa 
                                         (/ (+ (aref cz dd)
                                               (aref cz (+ dd step-in-second))
                                               )
                                            2)))))
              (fast-show-line canvas
                              (aref x dd) (aref y dd)
                              (aref x (+ dd step-in-second))
                              (aref y (+ dd step-in-second))
                              x-origin y-origin
                              lower-y upper-y
                              dl nl
                              ct st mx my col2
                              color-table)
              (incf dd step-in-first))
            (incf dc step-in-second))
          (setf dd dc)
          (setf cd (truncate (- cd (aref c 2))))
          ; (loop for i from 0 to (- dmx 1) do
          (do ((i 0 (incf i)))
              ((= i dmx))
            (setf col2 (- col1
                          (truncate (/ (* (+ i 0.5) (aref c 1)) dmx))
                          (truncate (* aa 
                                       (/ (+ (aref cz dd)
                                             (aref cz (+ dd step-in-first))
                                             )
                                          2)))))
            (fast-show-line canvas 
                            (aref x dd) (aref y dd)
                            (aref x (+ dd step-in-first))
                            (aref y (+ dd step-in-first))
                            x-origin y-origin
                            lower-y upper-y
                            dl nl
                            ct st mx my col2
                            color-table)
            (incf dd step-in-first))
          )
         (T
          (setf col2 (truncate (/ ncol 2)))
          ; (loop for j from 0 to (- dmy 1) do
          (do ((j 0 (incf j)))
              ((= j dmy))
            (setf dd dc)
            ; (loop for i from 0 to (- dmx 1) do
            (do ((i 0 (incf i)))
                ((= i dmx))
              (fast-show-line canvas
                              (aref x dd) (aref y dd)
                              (aref x (+ dd step-in-first))
                              (aref y (+ dd step-in-first))
                              x-origin y-origin
                              lower-y upper-y
                              dl nl
                              ct st mx my col2
                              color-table)
              (incf dd step-in-first))
            (setf dd dc)
            ; (loop for i from 0 to dmx do
            (do ((i 0 (incf i)))
                ((> i dmx))
              (fast-show-line canvas
                              (aref x dd) (aref y dd)
                              (aref x (+ dd step-in-second))
                              (aref y (+ dd step-in-second))
                              x-origin y-origin
                              lower-y upper-y
                              dl nl
                              ct st mx my col2
                              color-table)
              (incf dd step-in-first))
            (incf dc step-in-second))
          (setf dd dc)
          ; (loop for i from 0 to (- dmx 1) do
          (do ((i 0 (incf i)))
              ((= i dmx))
            (fast-show-line canvas 
                            (aref x dd) (aref y dd)
                            (aref x (+ dd step-in-first))
                            (aref y (+ dd step-in-first))
                            x-origin y-origin
                            lower-y upper-y
                            dl nl
                            ct st mx my col2
                            color-table)
            (incf dd step-in-first))
          ))))))


(defun fast-show-line (canvas x1 y1 x2 y2 x-origin y-origin
                         lower-y upper-y dl nl 
                         ct st mx my col2 color-table)
  
  (declare (fixnum x1 y1 x2 y2 mx my col2)
           (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
           (inline aref + - / * sqrt truncate round ))
  #-:sbcl-linux(declare (inline fast-move-to fast-line-to
                   update-border))
  (let (nx1 ny1 nx2 ny2 s)
    (setf mx (truncate mx 2))
    (setf my (truncate my 2))
    (decf x1 mx)
    (decf y1 my)
    (decf x2 mx)
    (decf y2 my)
    
    (setf nx1 (round (+ (* x1 ct) (* y1 (- st)) mx)))
    (setf ny1 (round (+ (* y1 ct) (* x1 st) my )))
    (setf nx2 (round (+ (* x2 ct) (* y2 (- st)) mx)))
    (setf ny2 (round (+ (* y2 ct) (* x2 st) my)))
    
    (setf s 1)
    (setf (aref nl 0) 0)
    (update-border upper-y nx1 ny1 nx2 ny2 s dl nl)
    (set-fast-color canvas (aref color-table col2))
    (when (> (aref nl 0) 0)
      (setf (aref dl 0) (- (aref dl 0) mx))
      (setf (aref dl 1) (- (aref dl 1) my))
      (setf (aref dl 2) (- (aref dl 2) mx))
      (setf (aref dl 3) (- (aref dl 3) my))
      (fast-move-to
       canvas
       (round (+ (* (aref dl 0) ct)
                 (* (aref dl 1) st)
                 mx
                 x-origin))
       (host-to-canvas-y
        canvas 
        (round (+ (* (aref dl 1) ct)
                  (* (aref dl 0) (- st))
                  my
                  y-origin))))
      
      (fast-line-to
       canvas
       (round (+ (* (aref dl 2) ct)
                 (* (aref dl 3) st)
                 mx
                 x-origin))
       (host-to-canvas-y
        canvas 
        (round (+ (* (aref dl 3) ct)
                  (* (aref dl 2) (- st))
                  my
                  y-origin))))
      )
    (when (> (aref nl 0) 1)
      (setf (aref dl 4) (- (aref dl 4) mx))
      (setf (aref dl 5) (- (aref dl 5) my))
      (setf (aref dl 6) (- (aref dl 6) mx))
      (setf (aref dl 7) (- (aref dl 7) my))
      (fast-move-to
       canvas
       (round (+ (* (aref dl 4) ct)
                 (* (aref dl 5) st)
                 mx
                 x-origin))
       (host-to-canvas-y
        canvas 
        (round (+ (* (aref dl 5) ct)
                  (* (aref dl 4) (- st))
                  my
                  y-origin))))
      
      (fast-line-to 
       canvas
       (round (+ (* (aref dl 6) ct)
                 (* (aref dl 7) st)
                 mx
                 x-origin))
       (host-to-canvas-y
        canvas 
        (round (+ (* (aref dl 7) ct)
                  (* (aref dl 6) (- st))
                  my
                  y-origin))))
      )
    
    (setf s -1)
    (setf (aref nl 0) 0)
    (update-border lower-y nx1 ny1 nx2 ny2 s dl nl)
    (when (> (aref nl 0) 0)
      (setf (aref dl 0) (- (aref dl 0) mx))
      (setf (aref dl 1) (- (aref dl 1) my))
      (setf (aref dl 2) (- (aref dl 2) mx))
      (setf (aref dl 3) (- (aref dl 3) my))
      (fast-move-to
       canvas
       (round (+ (* (aref dl 0) ct)
                 (* (aref dl 1) st)
                 mx
                 x-origin))
       (host-to-canvas-y
        canvas 
        (round (+ (* (aref dl 1) ct)
                  (* (aref dl 0) (- st))
                  my
                  y-origin))))
      
      (fast-line-to 
       canvas
       (round (+ (* (aref dl 2) ct)
                 (* (aref dl 3) st)
                 mx
                 x-origin))
       (host-to-canvas-y
        canvas 
        (round (+ (* (aref dl 3) ct)
                  (* (aref dl 2) (- st))
                  my
                  y-origin))))
      )
    (when (> (aref nl 0) 1)
      (setf (aref dl 4) (- (aref dl 4) mx))
      (setf (aref dl 5) (- (aref dl 5) my))
      (setf (aref dl 6) (- (aref dl 6) mx))
      (setf (aref dl 7) (- (aref dl 7) my))
      (fast-move-to
       canvas
       (round (+ (* (aref dl 4) ct)
                 (* (aref dl 5) st)
                 mx
                 x-origin))
       (host-to-canvas-y
        canvas 
        (round (+ (* (aref dl 5) ct)
                  (* (aref dl 4) (- st))
                  my
                  y-origin))))
      
      (fast-line-to 
       canvas
       (round (+ (* (aref dl 6) ct)
                 (* (aref dl 7) st)
                 mx
                 x-origin))
       (host-to-canvas-y
        canvas 
        (round (+ (* (aref dl 7) ct)
                  (* (aref dl 6) (- st))
                  my
                  y-origin))))
      )
    )
  )
