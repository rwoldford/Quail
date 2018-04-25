;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              point-defs-mcl.lisp
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
;;;     R.W. Oldford 1992.
;;;      
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :wb) 


(defconstant *host-xor-mode* (h-draw::mode-arg :patxor))
(defconstant *host-or-mode* (h-draw::mode-arg :pator))
(defconstant *host-bic-mode* (h-draw::mode-arg :patbic))
(defconstant *host-copy-mode* (h-draw::mode-arg :patcopy))


(defvar *rgb-colors* )
(setq *rgb-colors* nil)

(defun get-rgb-colors ()
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0)))
  (if *color-available*
    (setq *rgb-colors* 
          (or *rgb-colors*
              (loop for c in *colors* collect
                    (cons c (cond ( (eq-colors c *black-color*)
                                    *black-rgb*)
                                  ( (eq-colors c *white-color*)
                                    *white-rgb*)
                                  (t (color-to-rgb c)))))))))

(defmacro get-rgb-color (c)
  (let ((pair (gensym)) (rgb-c (gensym)))
    
    `(let () 
       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0)))
       (if ,*color-available*
         (let ((,pair (assoc ,c (get-rgb-colors) :test #'=)))
           (if ,pair
             (cdr ,pair)
             (let ((,rgb-c (color-to-rgb ,c)))
               (push (cons ,c ,rgb-c) *rgb-colors*)
               ,rgb-c)))))))



(defmacro set-point-rect (r x y s )
  (let ((s2 (gensym))
        (l (gensym))
        (b (gensym)))
    `(let* ((,s2 (truncate ,s 2))
            (,l (- (the fixnum ,x) ,s2))
            (,b (+ (the fixnum ,y) ,s2)))
       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0))
                (type fixnum ,l ,b ,s2))
       (rset ,r rect.topleft (h-draw:make-point ,l (- ,b ,s)))
       (rset ,r rect.bottomright 
             (h-draw:make-point (+ ,l ,s) ,b )))))

(defmacro fast-draw-box (rect x y  size fill? )
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (set-point-rect ,rect
                     (the fixnum ,x)
                     (the fixnum ,y)
                     (the fixnum ,size))
     (if ,fill? (#_PaintRect ,rect) (#_FrameRect ,rect) )))



(defmacro fast-draw-circle (rect x y  size fill? )
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (set-point-rect ,rect
                     (the fixnum ,x)
                     (the fixnum ,y)
                     (if (oddp (the fixnum ,size))
                       (1- (the fixnum ,size))
                       (the fixnum ,size)))
      (if ,fill? (#_PaintOval ,rect) (#_FrameOval ,rect) )))

(defmacro fast-draw-cross (rect x y  s fill? )
  (declare (ignore rect fill?))
  (let ((s2 (gensym))
        (l (gensym))
        (b (gensym)))
    `(let* ((,s2 (truncate ,s 2))
            (,l (- (the fixnum ,x) ,s2))
            (,b (+ (the fixnum ,y) ,s2)))
       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0))
                (type fixnum ,l ,b ,s2))
       (#_MoveTo ,l (the fixnum ,y)) (#_Line (* 2 ,s2) 0)
       (#_MoveTo (the fixnum ,x) ,b) (#_Line 0 (- (* 2 ,s2))))))



(defmacro fast-draw-diamond (rect x y  s fill? )
  (declare (ignore rect))
  (let ((s2 (gensym))
        (l (gensym)) (r (gensym))
        (tp (gensym)) (b (gensym))
        (poly (gensym)))  
    
    `(let* ((,s2 (truncate ,s 2))
            (,l (- (the fixnum ,x) ,s2))
            (,b (+ (the fixnum ,y) ,s2))
            (,r (+ (the fixnum ,x) ,s2))
            (,tp (- (the fixnum ,y) ,s2)))
       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0))
                (type fixnum ,l ,r ,tp ,b ,s2)
                (inline #_LineTo #_MoveTo #_ClosePoly #_PaintPoly #_KillPoly))
       (#_MoveTo ,l (the fixnum ,y))
       (if ,fill?
         (let ((,poly (#_OpenPoly)))
           (#_LineTo (the fixnum ,x) ,tp) (#_LineTo ,r (the fixnum ,y))
           (#_LineTo (the fixnum ,x) ,b) (#_LineTo ,l (the fixnum ,y))
           (#_ClosePoly )
           (#_PaintPoly ,poly)
           (#_KillPoly ,poly))
         
         (progn
           (#_LineTo (the fixnum ,x) ,tp) (#_LineTo ,r (the fixnum ,y))
           (#_LineTo (the fixnum ,x) ,b) (#_LineTo ,l (the fixnum ,y)))))))



(defmacro fast-draw-triangle (rect x y  s fill? )
  (declare (ignore rect))
  (let ((s2 (gensym))
        (l (gensym)) (r (gensym))
        (tp (gensym)) (b (gensym))
        (poly (gensym)))
    `(let* ((,s2 (truncate ,s 2))
            (,l (- (the fixnum ,x) ,s2))
            (,b (+ (the fixnum ,y) ,s2))
            (,r (+ (the fixnum ,x) ,s2))
            (,tp (- (the fixnum ,y) ,s2)))
       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0))
                (type fixnum ,l ,r ,tp ,b ,s2)
                (inline #_LineTo #_MoveTo #_ClosePoly #_PaintPoly #_KillPoly) )
       (#_MoveTo ,l ,b)
       (if ,fill?
         (let ((,poly (#_OpenPoly)))
           (#_LineTo (the fixnum ,x) ,tp) (#_LineTo ,r ,b) (#_LineTo ,l ,b)
           (#_ClosePoly )
           (#_PaintPoly ,poly)
           (#_KillPoly ,poly))
         (progn
           (#_LineTo (the fixnum ,x) ,tp) (#_LineTo ,r ,b)
           (#_LineTo ,l ,b))))))
      
(defconstant *cos-18* (cos (* pi 0.1)) )

(defconstant *cos-54* (cos (* pi 0.3)))

(defconstant *sin-18* (sin (* pi 0.1)))

(defconstant *sin-54* (sin (* pi 0.3)))

(defmacro fast-draw-star (rect x y  s fill? )
  (declare (ignore rect fill?))
  (let ((r (gensym)) (r*c18 (gensym))
        (r*s18 (gensym)) (r*c54 (gensym)) (r*s54 (gensym))
        (x1  (gensym)) (y1 (gensym))
        (x2 (gensym)) (y2 (gensym))
        (x3 (gensym)) (x4 (gensym))
        (x5 (gensym)) (y5 (gensym)))
    
    `(let* ((,r (truncate ,s 2))
            (,r*c18 (round (* ,r *cos-18*)))
            (,r*s18 (round (* ,r *sin-18*)))
            (,r*c54 (round (* ,r *cos-54*)))
            (,r*s54 (round (* ,r *sin-54*)))
            (,x1  (+ (the fixnum ,x) (the fixnum ,r*c18)))
            (,y1 (- (the fixnum ,y) (the fixnum ,r*s18)))
            (,x2 (the fixnum ,x))
            (,y2 (- (the fixnum ,y) (the fixnum ,r)))
            (,x3 (- (the fixnum ,x) (the fixnum ,r*c18)))
            (,x4 (- (the fixnum ,x) (the fixnum ,r*c54)))
            (,x5 (+ (the fixnum ,x) (the fixnum ,r*c54)))
            (,y5 (+ (the fixnum ,y) (the fixnum ,r*s54))))
       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0))
                (type fixnum ,x1 ,x2 ,x3 ,x4 ,x5 ,y1 ,y2 ,y5))
       (#_MoveTo (the fixnum ,x) (the fixnum ,y)) (#_LineTo ,x1 ,y1)
       (#_MoveTo (the fixnum ,x) (the fixnum ,y)) (#_LineTo ,x2 ,y2)
       (#_MoveTo (the fixnum ,x) (the fixnum ,y)) (#_LineTo ,x3 ,y1)
       (#_MoveTo (the fixnum ,x) (the fixnum ,y)) (#_LineTo ,x4 ,y5)
       (#_MoveTo (the fixnum ,x) (the fixnum ,y)) (#_LineTo ,x5 ,y5))))

 
(defmacro fast-draw-poly-star (rect xc yc  s fill? )
  (declare (ignore rect ))
  (let ((r (gensym))  (r2  (gensym)) (r*c18 (gensym))
        (r*s18 (gensym)) (r*c54 (gensym)) (r*s54 (gensym))
        (x1  (gensym))
        (y1 (gensym))
        (x2 (gensym))
        (y2 (gensym))
        (x3 (gensym)) 
        (y3 (gensym))
        (x4 (gensym))
        (y4 (gensym))
        (x5 (gensym))
        (y5 (gensym))
        (u1 (gensym))
        (v1 (gensym))
        (u2 (gensym))
        (v2 (gensym))
        (u3 (gensym))
        (v3 (gensym))
        (u4 (gensym))
        (v4 (gensym))
        (u5 (gensym))
        (v5 (gensym))
        (poly (gensym)))
    
    
    `(let* ((,r (truncate ,s 2))
            (,r*c18 (round (* ,r *cos-18*)))
            (,r*s18 (round (* ,r *sin-18*)))
            (,r*c54 (round (* ,r *cos-54*)))
            (,r*s54 (round (* ,r *sin-54*)))
            (,r2 (truncate ,r 2))
            (,x1  (+ (the fixnum ,xc) (the fixnum ,r*c18))) 
            (,y1 (- (the fixnum ,yc) (the fixnum ,r*s18))) 
            (,x2 ,xc) 
            (,y2 (- (the fixnum ,yc) ,r))
            (,x3 (- (the fixnum ,xc)  (the fixnum ,r*c18))) (,y3 ,y1)
            (,x4 (- (the fixnum ,xc)  (the fixnum ,r*c54)))
            (,y4 (+ (the fixnum ,yc)  (the fixnum ,r*s54)))
            (,x5 (+ (the fixnum ,xc)  (the fixnum ,r*c54)))
            (,y5 ,y4)
            (,u1 (+ (the fixnum ,xc) (truncate  (the fixnum ,r*c54) 2)))
            (,v1 (- (the fixnum ,yc) (truncate  (the fixnum ,r*s54) 2)))
            (,u2 (- (the fixnum ,xc) (truncate  (the fixnum ,r*c54) 2)))
            (,v2 ,v1)
            (,u3 (- (the fixnum ,xc) (truncate  (the fixnum ,r*c18) 2)))
            (,v3 (+ (the fixnum ,yc) (truncate  (the fixnum ,r*s18) 2)))
            (,u4 ,xc)
            (,v4 (+ (the fixnum ,yc) (the fixnum ,r2)))
            (,u5 (+ (the fixnum ,xc) (truncate  (the fixnum ,r*c18) 2)))
            (,v5 (+ (the fixnum ,yc) (truncate  (the fixnum ,r*s18) 2)))
            (,poly (if ,fill? (#_OpenPoly)))
            )
       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0))
                (type fixnum ,x1 ,x2 ,x3 ,x4 ,x5 ,y1 ,y2 ,y3 ,y4 ,y5
                      ,u1 ,u2 ,u3 ,u4 ,u5 ,v1 ,v2 ,v3 ,v4 ,v5))
       
       
       (#_MoveTo (the fixnum ,x1) (the fixnum ,y1))
       (#_LineTo ,u1 ,v1)
       (#_LineTo  ,x2 ,y2) (#_LineTo  ,u2 ,v2)
       (#_LineTo  ,x3 ,y3) (#_LineTo  ,u3 ,v3)
       (#_LineTo  ,x4 ,y4) (#_LineTo  ,u4 ,v4)
       (#_LineTo  ,x5 ,y5) (#_LineTo  ,u5 ,v5)
       (#_LineTo  ,x1 ,y1)
       (when ,fill?
         (#_ClosePoly )
         (#_PaintPoly ,poly)
         (#_KillPoly ,poly)))))





(defmacro fast-draw-symbol (sim rect x y  size fill )
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (case ,sim
       (:box      (fast-draw-box ,rect ,x ,y ,size ,fill))
       (:circle   (fast-draw-circle ,rect ,x ,y ,size ,fill ))
       (:cross    (fast-draw-cross ,rect ,x ,y ,size ,fill ))
       (:star     (fast-draw-star ,rect ,x ,y ,size ,fill ))
       (:poly-star     (fast-draw-poly-star ,rect ,x ,y ,size ,fill ))
       (:diamond  (fast-draw-diamond ,rect ,x ,y ,size ,fill ))
       (:triangle (fast-draw-triangle ,rect ,x ,y ,size ,fill ))
       (t         nil))))

(defmacro mode-draw-symbol (sim rect x y  size fill mode)
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (#_PenMode ,mode)
     (fast-draw-symbol ,sim ,rect ,x ,y  ,size ,fill)))
  
  
(defmacro cc-set-rgb-color ( color)
  `(let () 
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (when ,color
       (#_rgbforecolor :ptr ,color))))

(defmacro bw-set-color ( color)
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (when ,color
       (#_PenPat (if (colorp ,color) 
                   (color-to-shade ,color)
                   ,color)))))

(defmacro set-draw-color (canvas color)
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (when ,color
       (if (colored-canvas-p ,canvas)
         (if (colorp ,color)
           (set-fore-color ,canvas ,color)
           (#_rgbforecolor :ptr ,color))
         (#_PenPat (if (colorp ,color) 
                     (color-to-shade ,color)
                     ,color))))))

(defmacro set-fast-color (canvas color)
  `(let () 
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (if (colored-canvas-p ,canvas)
       (#_rgbforecolor :ptr ,color)
       (#_PenPat ,color))))   

(defmacro fast-move-to (canvas x y)
  (declare (ignore canvas))
  `(let ()
     (declare (inline  #_MoveTo))
     (#_MoveTo (the fixnum ,x) (the fixnum ,y))))
          

(defmacro fast-line-to (canvas x y)
  (declare (ignore canvas))
  `(let ()
     (declare (inline  #_LineTo))
     (#_LineTo (the fixnum ,x) (the fixnum ,y))))
          

(defmacro fast-draw-line (canvas x1 y1 x2 y2 )
  (declare (ignore canvas))
  `(let ()
     (declare (inline #_MoveTo #_LineTo))
     (#_MoveTo (the fixnum ,x1) (the fixnum ,y1))
     (#_LineTo (the fixnum ,x2) (the fixnum ,y2))))

(defmacro mode-draw-line (canvas x1 y1 x2 y2 mode)
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (#_PenMode ,mode)
     (fast-draw-line ,canvas
                     (the fixnum ,x1)
                     (the fixnum ,y1)
                     (the fixnum ,x2)
                     (the fixnum ,y2))))

  
