;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              point-defs-clx.lisp
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
;;;     N.G. Bennett 1993
;;;      
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(in-package :wb) 

(defconstant *host-xor-mode* (boole-to-op :boole-xor))
(defconstant *host-or-mode* (boole-to-op :boole-ior))
(defconstant *host-bic-mode* (boole-to-op :boole-andc1))
(defconstant *host-copy-mode* (boole-to-op :boole-1))

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

(defmacro fast-draw-box (canvas x y  size fill? )
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
       (xlib:draw-rectangle (host-window ,canvas) (gcontext ,canvas)
	 ,x ,y ,size ,size ,fill?)))

(defmacro fast-draw-circle (canvas x y  size fill? )
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
       (xlib:draw-arc (host-window ,canvas) (gcontext ,canvas)
         ,x ,y ,size ,size 0 (* 2 pi) ,fill?)))

(defmacro fast-draw-cross (canvas x y  s fill? )
  (declare (ignore fill?))
  (let ((s2 (gensym))
        (l (gensym)))
    `(let* ((,s2 (truncate ,s 2))
	    (,l (list (- ,x ,s2) ,y 
		      (+ ,x ,s2) ,y 
		      ,x (- ,y ,s2) 
		      ,x (+ ,y ,s2))))
       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0)))
       (xlib:draw-lines (host-window ,canvas) (gcontext ,canvas) ,l))))

(defmacro fast-draw-diamond (canvas x y  s fill? )
  (let ((s2 (gensym))
        (l (gensym)))
    
    `(let* ((,s2 (truncate ,s 2))
	    (,l (list (- ,x ,s2) ,y 
		      ,x (+ ,y ,s2)
		      ,x (+ ,y ,s2)
		      (+ ,x ,s2) ,y
		      (+ ,x ,s2) ,y
		      ,x (- ,y ,s2)
		      ,x (- ,y ,s2)
		      (- ,x ,s2) ,y)))
       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0)))
       (xlib:draw-lines (host-window ,canvas) 
		        (gcontext ,canvas) ,l :fill-p ,fill?))))

(defmacro fast-draw-triangle (canvas x y  s fill? )
  (let ((s2 (gensym))
        (l (gensym)) 
        )
    `(let* ((,s2 (truncate ,s 2))
	    (,l (list (- ,x ,s2) (+ ,y ,s2) 
		      ,x (- ,y ,s2)
		      ,x (- ,y ,s2)
		      (+ ,x ,s2) (+ ,y ,s2)
		      (+ ,x ,s2) (+ ,y ,s2) 
		      (- ,x ,s2) (+ ,y ,s2)))) 
       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0)))
       (xlib:draw-lines (host-window ,canvas) 
		        (gcontext ,canvas) ,l :fill-p ,fill?)))) 

(defconstant *cos-18* (cos (* pi 0.1)))

(defconstant *cos-54* (cos (* pi 0.3)))

(defconstant *sin-18* (sin (* pi 0.1)))

(defconstant *sin-54* (sin (* pi 0.3)))

(defmacro fast-draw-star (canvas x y  s fill? )
  (declare (ignore fill?))
  (let ((r (gensym)) (r*c18 (gensym))
        (r*s18 (gensym)) (r*c54 (gensym)) (r*s54 (gensym))
        (x1  (gensym)) (y1 (gensym))
        (x2 (gensym)) (y2 (gensym))
        (x3 (gensym)) 
	(x4 (gensym)) 
        (x5 (gensym)) (y5 (gensym))
	(h (gensym)) (g (gensym))
	(pt-list (gensym)))
    
    `(let* ((,h (host-window ,canvas))
	    (,g (gcontext ,canvas))
	    (,r (truncate ,s 2))
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
            (,y5 (+ (the fixnum ,y) (the fixnum ,r*s54)))
	    (,pt-list (list ,x ,y ,x1 ,y1 ,x ,y ,x2 ,y2 ,x ,y ,x3 ,y1 ,x ,y
			    ,x4 ,y5 ,x ,y ,x5 ,y5)))
       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0)))
       (xlib:draw-lines ,h ,g ,pt-list))))

(defmacro fast-draw-symbol (canvas sim x y  size fill )
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (case ,sim
       (:box      (fast-draw-box ,canvas ,x ,y ,size ,fill))
       (:circle   (fast-draw-circle ,canvas ,x ,y ,size ,fill ))
       (:cross    (fast-draw-cross ,canvas ,x ,y ,size ,fill ))
       (:star     (fast-draw-star ,canvas ,x ,y ,size ,fill ))
       (:poly-star     (fast-draw-poly-star ,canvas ,x ,y ,size ,fill ))
       (:diamond  (fast-draw-diamond ,canvas ,x ,y ,size ,fill ))
       (:triangle (fast-draw-triangle ,canvas ,x ,y ,size ,fill ))
       (t         nil))))

(defmacro mode-draw-symbol (canvas sim x y  size fill mode)
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (xlib::set-gcontext-function (gcontext ,canvas) ,mode)
     (fast-draw-symbol ,canvas ,sim ,x ,y  ,size ,fill)))
  
(defmacro cc-set-rgb-color (canvas color)
  `(let () 
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (when ,color
       (xlib::set-gcontext-foreground (gcontext ,canvas) ,color))))

(defmacro bw-set-color (canvas color)
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (when ,color
       (set-draw-color canvas color))))

(defmacro set-draw-color (canvas color)
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (when ,color
       (if (colored-canvas-p ,canvas)
         (if (colorp ,color)
           (set-fore-color ,canvas ,color)
           (xlib::set-gcontext-foreground (gcontext ,canvas) ,color))))))

(defmacro set-fast-color (canvas color)
  `(let () 
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (if (colored-canvas-p ,canvas)
       (xlib::set-gcontext-foregound (gcontext ,canvas) ,color)
     )))   

(defmacro fast-move-to (canvas x y)
  `(let ()
     (setf (current-position ,canvas) (list ,x ,y))))

(defmacro fast-line-to (canvas x y)
  `(let ()
     (xlib:draw-line (host-window ,canvas) (gcontext ,canvas)
	(point-x (current-position ,canvas)) 
	(point-y (current-position ,canvas)) ,x ,y)))

(defmacro fast-draw-line (canvas  x1 y1 x2 y2 )
  `(let ()
     (xlib:draw-line (host-window ,canvas) (gcontext ,canvas) ,x1 ,y1 ,x2 ,y2)))

(defmacro mode-draw-line (canvas x1 y1 x2 y2 mode)
  `(let ()
     (declare (optimize (speed 3) (safety 0)
                        (space 0) (compilation-speed 0)))
     (xlib::set-gcontext-function (gcontext ,canvas) ,mode)
     (fast-draw-line ,canvas
		     (the fixnum ,x1)
                     (the fixnum ,y1)
                     (the fixnum ,x2)
                     (the fixnum ,y2))))



(defmacro fast-draw-poly-star (canvas xc yc  s fill? )
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
        (l (gensym)))
    
    
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
            (,l (list ,x1 ,y1 ,u1 ,v1
                      ,u1 ,v1 ,x2 ,y2
                      ,x2 ,y2 ,u2 ,v2
                      ,u2 ,v2 ,x3 ,y3
                      ,x3 ,y3 ,u3 ,v3
                      ,u3 ,v3  ,x4 ,y4
                      ,x4 ,y4 ,u4 ,v4
                      ,u4 ,v4 ,x5 ,y5
                      ,x5 ,y5 ,u5 ,v5
                      ,u5 ,v5  ,x1 ,y1)))

       (declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0)))
       
       (xlib:draw-lines (host-window ,canvas) 
		        (gcontext ,canvas) ,l :fill-p ,fill?))))