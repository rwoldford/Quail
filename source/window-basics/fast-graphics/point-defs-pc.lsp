;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              point-defs-pc.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is an addition to Window-basics for moving pointclouds
;;;  
;;;  Authors:
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1991 George Washington University
;;;     R.W. Oldford 1992.
;;;     N.G. Bennett 1993
;;;     G.W. Bennett 1996
;;;      
;;;     
;;;-------------------------------------------------------------------

(in-package :wb)
 
(defconstant *host-xor-mode* (boole-to-op :boole-xor))

(defconstant *host-or-mode* (boole-to-op :boole-1))

(defconstant *host-bic-mode* (boole-to-op :boole-1))

(defconstant *host-copy-mode* (boole-to-op :boole-1))

#|
(defvar *rgb-colors* )

(setq *rgb-colors* nil)

(defun get-rgb-colors ()
     (declare (optimize (speed 3) (safety 0)
                      (space 0) (compilation-speed 0))
       (special cg::po-replace cg::po-invert cg::po-paint cg::po-erase))
     (if *color-available*
        (setq *rgb-colors* 
           (or *rgb-colors*
                (loop for c in *colors* collect
                  (cons c (cond ( (eq-colors c *black-color*)
                                         *black-rgb*)
                                        ( (eq-colors c *white-color*)
                                         *white-rgb*)
                                        (t (color-to-rgb c)))))
                *colors*))))

(defmacro get-rgb-color (c)
   (let ((pair (gensym))
         (rgb-c (gensym)))
      `(let ()
          (declare (optimize (speed 3) (safety 0) (space 0)
                     (compilation-speed 0)))
          (if ,*color-available*
             (let ((,pair (assoc ,c (get-rgb-colors) :test #'=)))
                (if ,pair 
                   (cdr ,pair)
                   (let ((,rgb-c (color-to-rgb ,c)))
                      (push (cons ,c ,rgb-c) *rgb-colors*)
                      ,rgb-c)))))))
|#
;; Takes x and y to be left and top respectively
;; Returns a SQUARE
;; Uses cg:: inside. Checked in gwbtests\expts-pointdefs-pc.lsp
;; March 25, 1996

;; Revised version of 15 Feb 1998 follows
;; it uses x, as CENTRE 
;; old-parts-point-defs-pc.lsp
;; And now it is compatible with V/V-M/draw-macros.lsp form
;; with-point-symbol-bounds 17 Feb 1998 and with
;; h-draw::draw-inside-rectangle and ::erase-rect.

(defmacro fast-draw-box (canvas x y  size fill? )
   (declare (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
   (let ((s2 (gensym)) (lx (gensym)) (ly (gensym)) (mp (gensym)))
      `(let* ((,s2 (truncate ,size 2))
             (,lx  (- ,x ,s2))
               (,ly (- ,y ,s2))
             (,mp (cg::frame-child ,canvas)))
          (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
            (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
          (cond ((eq ,size 1)
                 ;(cg::set-pixel-x-y ,mp ,x ,y 18oct05
                 (setf (cg::pixel-x-y ,mp) ,x ,y ;18oct05
                  (cg::foreground-color ,mp)))
                (T
                  (if (= ,size 1)
                     (cg::fill-box ,mp
                      (cg::make-box ,x ,y (+ ,x ,size) (+ ,y ,size)))
                  (if ,fill?
                     (cg::fill-box ,mp
                      (cg::make-box ,lx ,ly
                        (+ ,lx ,size) (+ ,ly ,size )))
                        (cg::draw-box ,mp 
                         (cg::make-box ,lx ,ly
                          (+ ,lx ,size)
                              (+ ,ly ,size)))
                        ))
                  )
                ))))

;; Revised version of 15 Feb 1998 follows 
;; which uses x,y as CENTRE
(defmacro fast-erase-box (canvas x y  size fill? )
   (declare (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
   (let ((s2 (gensym)) (lx (gensym)) (ly (gensym)) (mp (gensym)))
      `(let* ((,s2 (truncate ,size 2))
              (,lx (- ,x ,s2))
                (,ly (- ,y ,s2))
              (,mp (cg::frame-child ,canvas)))
          (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
            (special cg::po-erase cg:po-invert cg::po-replace cg::po-paint))
          (cg::with-paint-operation (,canvas cg::po-erase)
           (if (= ,size 1)
              (cg::erase-box ,mp
               (cg::make-box ,x ,y (+ ,x ,size) (+ ,y ,size)))
           (if ,fill?
              (cg::erase-contents-box ,mp
               (cg::make-box ,lx ,ly
                  (+ ,lx ,size) (+ ,ly ,size)))
                 (cg::erase-box ,mp 
                  (cg::make-box ,lx ,ly
;                   (if (oddp ,size) (- ,ly 1) ,ly)
                   (+ ,lx ,size)  
;                   (if (oddp ,size ) (+ ,ly ,size -1) 
                     (+ ,ly ,size)))
                 ))
           ))
      ))

;; Takes x and y as co-ordinates of the centre
;; size as the radius but it SHOULD use 1/2 size
;; Uses cg:: inside. Checked in gwbtests\expte-pointdefs-pc.lsp
;; March 25, 1996
(defun cg-draw-filled-circle (stream centre radius)
   (declare (inline cg::fill-circle cg::draw-circle))
  (let ((mp (cg::frame-child stream)))
   (cg::fill-circle mp centre radius)
   (cg::draw-circle mp centre radius)))

(defun cg-erase-filled-circle (stream centre radius)
   (declare (inline cg::erase-contents-circle cg::erase-circle))
   (let ((mp (cg::frame-child stream)))
   (cg::erase-contents-circle mp centre radius)
   (cg::erase-circle mp centre radius)))


(defmacro fast-draw-circle (canvas x y  size fill? )
   (declare (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert)
     (inline cg-draw-filled-circle cg::draw-circle))
   (let ((s2 (gensym))
        (mp (gensym)))
      ;;; Check for size = 1 as special 05jan00
  `(let ((,mp (cg::frame-child ,canvas)))
      (cond ((eq 1 ,size)
              (setf (cg:pixel-x-y ,mp ,x ,y)
                    (cg:foreground-color ,mp)))
             (t 
               (let ((,s2 (if (oddp ,size) (1- (truncate (+ 1 ,size) 2))
                             (truncate (+ 1 ,size) 2)))) ;;; added + 1 24JNE99 oddp 11NOV99
                  (declare (optimize (speed 3) (safety 0)
                             (space 0) (compilation-speed 0))
                    (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
                  (if ,fill?
                     (cg-draw-filled-circle canvas 
                       (cg::make-position ,x ,y) ,s2)
                     (cg::draw-circle ,mp 
                       (cg::make-position ,x ,y) ,s2 )) 
                  ))
             ))))

(defmacro fast-erase-circle (canvas x y  size fill? )
   (declare (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert)
     (inline cg-erase-filled-circle cg::erase-circle))
   (let ((s2 (gensym)) (mp (gensym)))
      ;;; Check for size = 1 as special 05jan00
  `(let ((,mp (cg::frame-child ,canvas)))
      (cond ((eq 1 ,size)
              (setf (cg:pixel-x-y ,mp ,x ,y)
                    (cg:background-color ,mp)))
             (t
              (let ((,s2 (if (oddp ,size) (1- (truncate (+ 1 ,size) 2))
                            (truncate (+ 1 ,size) 2)))) ;;; added + 1 24JNE99 oddp 11NOV99
                 (declare (optimize (speed 3) (safety 0)
                            (space 0) (compilation-speed 0))
                   (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
                 (cg::with-paint-operation (,canvas cg::po-erase)
                  (if ,fill?
                     (cg-erase-filled-circle canvas 
                       (cg::make-position ,x ,y) ,s2)
                     (cg::erase-circle ,mp 
                       (cg::make-position ,x ,y) ,s2 ))) 
                 ))
             ))))

;; May 28, 1996
;; Takes x and y as the centre
;; Uses cg:: inside it - checked in gwbtests\expts-poindefs-pc.lsp
;; March 25, 1996
(defmacro fast-draw-cross (canvas x y  s fill? )
   (declare (ignore fill?)
     (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
   (let ((s2 (gensym))
         (l (gensym))
         (mp (gensym)))
      `(let* ((,s2 (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                      (truncate (+ 1 ,s) 2)))
              (,l (list (cg::make-position (- ,x ,s2) ,y )
                    (cg::make-position (+ ,x ,s2 1) ,y )
                    (cg::make-position ,x (- ,y ,s2) )
                    (cg::make-position ,x (+ ,y ,s2 1))
                    ))
              (,mp (cg::frame-child ,canvas)))
          (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
            (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
          (cg::move-to ,mp (first ,l))
          (cg::draw-to ,mp (second ,l))
          (cg::move-to ,mp (third ,l))
          (cg::draw-to ,mp (fourth ,l))
          )
      )
   )

(defmacro fast-erase-cross (canvas x y  s fill? )
   (declare (ignore fill?)
     (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
   (let ((s2 (gensym))
         (l (gensym))
         (mp (gensym)))
      `(let* ((,s2 (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                      (truncate (+ 1 ,s) 2)))
              (,l (list (cg::make-position (- ,x ,s2) ,y )
                    (cg::make-position (+ ,x ,s2 1) ,y )
                    (cg::make-position ,x (- ,y ,s2) )
                    (cg::make-position ,x (+ ,y ,s2 1))
                    ))
              (,mp (cg::frame-child ,canvas)))
          (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
            (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
          (cg::with-paint-operation (,canvas cg::po-erase)
           (cg::move-to ,mp (first ,l))
           (cg::erase-to ,mp (second ,l))
           (cg::move-to ,mp (third ,l))
           (cg::erase-to ,mp (fourth ,l))
           ))
      )
   )

;; Takes x and y as the centre
;; Does use cg:: inside it - checked in gwbtests\expts-poindefs-pc.lsp
;; March 25, 1996
(defmacro fast-draw-diamond (canvas x y  s fill? )
   (declare (special cg::po-erase cg::po-paint cg::po-replace cg::po-paint))
   (let ((s2 (gensym))
         (l (gensym))
         (mp (gensym)))
      ;; Check for size off the top 07jan00
  `(let ((,mp (cg::frame-child ,canvas)))
      (cond ((= 1 ,s)
              (setf (cg:pixel-x-y ,mp ,x ,y)
                    (cg:foreground-color ,mp)))
             (T
              (let* ((,s2 (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                             (truncate (+ 1 ,s) 2))) ;; added +1 27Jne99 oddp 11nov99
                     (,l (list (cg::make-position (- ,x ,s2) ,y)
                           (cg::make-position ,x (+ ,y ,s2))
                           (cg::make-position (+ ,x ,s2) ,y)
                           (cg::make-position ,x (- ,y ,s2))
                           (cg::make-position (- ,x ,s2) ,y)
                           )
                      )
                     )
                 (declare (optimize (speed 3) (safety 0)
                            (space 0) (compilation-speed 0))
                   (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
                 (if ,fill? (and (cg::fill-polygon ,mp ,l)
                                 (cg::draw-polyline ,mp ,l))
                    (cg::draw-polyline ,mp ,l))
                 ))))))

(defmacro fast-erase-diamond (canvas x y  s fill? )
     (declare (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
     (let ((s2 (gensym))
            (l (gensym))
           (mp (gensym)))
        ;;; Check for size off the top 07jan00
  `(let ((,mp (cg::frame-child ,canvas)))
         (cond ((= 1 ,s)
                 (setf (cg:pixel-x-y ,mp ,x ,y)
                       (cg:background-color ,mp)))
                (T
        (let* ((,s2 (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                  (truncate (+ 1 ,s) 2))) ;; added +1 27Jne99 oddp 11nov99
                    (,l (list (cg::make-position (- ,x ,s2) ,y)
                          (cg::make-position ,x (+ ,y ,s2))
                          (cg::make-position (+ ,x ,s2) ,y)
                          (cg::make-position ,x (- ,y ,s2))
                          (cg::make-position (- ,x ,s2) ,y)
                          )
                     )
                    )
              (declare (optimize (speed 3) (safety 0)
                               (space 0) (compilation-speed 0))
                 (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
              ;;       (xlib:draw-lines (host-window ,canvas) 
              ;;        (gcontext ,canvas) ,l :fill-p ,fill?)
              (cg::with-paint-operation (,canvas cg::po-erase)
               (if ,fill? (and (cg::erase-contents-polygon ,mp ,l)
                               (cg::erase-polyline ,mp ,l))
                  (cg::erase-polyline ,mp ,l))
               ))))
        )))

;; Takes x and y as the centre
;; Does use cg:: inside it - checked in gwbtests\expts-poindefs-pc.lsp
;; March 25, 1996
(defmacro fast-draw-triangle (canvas x y  s fill? )
   (declare (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
   (let ((s2 (gensym))
         (l (gensym)) (r (gensym))
         (tp (gensym)) (b (gensym))
         (poly (gensym))
         (mp (gensym)))
      `(let* ((,s2 (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                      (truncate (+ 1 ,s)  2))) ;; added +1 27Jne99
              (,l (list (cg::make-position (- ,x ,s2) (+ ,y ,s2))
                    (cg::make-position ,x (- ,y ,s2))
                    (cg::make-position (+ ,x ,s2) (+ ,y ,s2))
                    (cg::make-position (- ,x ,s2) (+ ,y ,s2))
                    ))
              (,mp (cg::frame-child ,canvas))
              ) 
          (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
            (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
          ;;       (xlib:draw-lines (host-window ,canvas) 
          ;;        (gcontext ,canvas) ,l :fill-p ,fill?)
          (if ,fill?
             (and (cg::draw-polyline ,mp ,l)
             (cg::fill-polygon ,mp ,l)) ;; 07dec99
             (cg::draw-polyline ,mp ,l))
          )
      )
   )

(defmacro fast-erase-triangle (canvas x y  s fill? )
   (declare (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
   (let ((s2 (gensym))
         (l (gensym)) (r (gensym))
         (tp (gensym)) (b (gensym))
         (poly (gensym))
         (mp (gensym)))
      `(let* ((,s2 (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                      (truncate (+ 1 ,s)  2))) ;; added +1 27Jne99
              (,l (list (cg::make-position (- ,x ,s2) (+ ,y ,s2))
                    (cg::make-position ,x (- ,y ,s2))
                    (cg::make-position (+ ,x ,s2) (+ ,y ,s2))
                    (cg::make-position (- ,x ,s2) (+ ,y ,s2))
                    ))
              (,mp (cg::frame-child ,canvas))
              ) 
          (declare (optimize (speed 3) (safety 0)
                     (space 0) (compilation-speed 0))
            (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
          ;;       (xlib:draw-lines (host-window ,canvas) 
          ;;        (gcontext ,canvas) ,l :fill-p ,fill?)
          (cg::with-paint-operation (,canvas cg::po-erase)
           (if ,fill?
              (and (cg::erase-polyline ,mp ,l)
              (cg::erase-contents-polygon ,mp ,l))
              (cg::erase-polyline ,mp ,l))
           ))
      )
   )

(defconstant *cos-18* (cos (* pi 0.1)))
(defconstant *cos-54* (cos (* pi 0.3)))
(defconstant *sin-18* (sin (* pi 0.1)))
(defconstant *sin-54* (sin (* pi 0.3)))

;; Takes x and y as the centre
;; Does use cg:: inside it - checked in gwbtests\expts-poindefs-pc.lsp
;; March 25, 1996
(defmacro fast-draw-star (canvas x y  s fill? )
   (declare (ignore fill?)
     (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
   (let ((r (gensym)) (r*c18 (gensym))
         (r*s18 (gensym)) (r*c54 (gensym)) (r*s54 (gensym))
         (x1  (gensym)) (y1 (gensym))
         (x2 (gensym)) (y2 (gensym))
         (x3 (gensym)) 
         (x4 (gensym)) 
         (x5 (gensym)) (y5 (gensym))
         (h (gensym)) (g (gensym))
         (pt-list (gensym))
         (mp (gensym)))
      ;;; Check for size = 1 off the top
  `(let ((,mp (cg::frame-child ,canvas)))
      (cond ((= 1 ,s)
              (setf (cg:pixel-x-y ,mp ,x ,y)
                    (cg:foreground-color ,mp))) ;; 3 lines 07jan00
             (T
              (let* ((,r (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                            (truncate (+ 1 ,s) 2))) ;; added +1 27Jne99
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
                     (,pt-list (list 
                                 (cg::make-position ,x ,y)
                                 (cg::make-position ,x1 ,y1)
                                 (cg::make-position ,x ,y)
                                 (cg::make-position ,x2 ,y2)
                                 (cg::make-position ,x ,y)
                                 (cg::make-position ,x3 ,y1)
                                 (cg::make-position ,x ,y)
                                 (cg::make-position ,x4 ,y5)
                                 (cg::make-position ,x ,y)
                                 (cg::make-position ,x5 ,y5)
                                 (cg::make-position ,x ,y)
                                 )
                      ))
                 (declare (optimize (speed 3) (safety 0)
                            (space 0) (compilation-speed 0))
                   (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
                 (cg::draw-polyline ,mp ,pt-list)
                 ))))))

(defmacro fast-erase-star (canvas x y  s fill? )
   (declare (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
   (declare (ignore fill?)
     (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
   (let ((r (gensym)) (r*c18 (gensym))
         (r*s18 (gensym)) (r*c54 (gensym)) (r*s54 (gensym))
         (x1  (gensym)) (y1 (gensym))
         (x2 (gensym)) (y2 (gensym))
         (x3 (gensym)) 
         (x4 (gensym)) 
         (x5 (gensym)) (y5 (gensym))
         (h (gensym)) (g (gensym))
         (pt-list (gensym))
         (mp (gensym)))
      ;; Check for size = 1 off the top
  `(let ((,mp (cg::frame-child ,canvas)))
      (cond ((= 1 ,s)
              (setf (cg:pixel-x-y ,mp ,x ,y)
                    (cg:background-color ,mp))) ;; 3 lines 07jan00
             (T
              (let* ((,r (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                            (truncate (+ 1 ,s) 2))) ;; added +1 27Jne99
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
                     (,pt-list (list 
                                 (cg::make-position ,x ,y)
                                 (cg::make-position ,x1 ,y1)
                                 (cg::make-position ,x ,y)
                                 (cg::make-position ,x2 ,y2)
                                 (cg::make-position ,x ,y)
                                 (cg::make-position ,x3 ,y1)
                                 (cg::make-position ,x ,y)
                                 (cg::make-position ,x4 ,y5)
                                 (cg::make-position ,x ,y)
                                 (cg::make-position ,x5 ,y5)
                                 (cg::make-position ,x ,y))
                      ))
                 (declare (optimize (speed 3) (safety 0)
                            (space 0) (compilation-speed 0))
                   (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
                 ;;       (xlib:draw-lines ,h ,g ,pt-list)
                 (cg::with-paint-operation (,canvas cg::po-erase)
                  (cg::erase-polyline ,mp ,pt-list)
                  )))))))

(defmacro fast-draw-symbol (canvas sim x y  size fill )
   (declare (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
   `(let ()
       (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0))
         (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
       (case ,sim
         (:box      (fast-draw-box ,canvas ,x ,y ,size ,fill))
         (:circle   (fast-draw-circle ,canvas ,x ,y ,size ,fill ))
         (:cross    (fast-draw-cross ,canvas ,x ,y ,size ,fill ))
         (:star     (fast-draw-star ,canvas ,x ,y ,size ,fill ))
         (:diamond  (fast-draw-diamond ,canvas ,x ,y ,size ,fill ))
         (:triangle (fast-draw-triangle ,canvas ,x ,y ,size ,fill ))
         (t         nil))))

(defmacro fast-erase-symbol (canvas sim x y  size fill )
   (declare (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
   `(let ()
       (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0))
         (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
       (case ,sim
         (:box      (fast-erase-box ,canvas ,x ,y ,size ,fill))
         (:circle   (fast-erase-circle ,canvas ,x ,y ,size ,fill ))
         (:cross    (fast-erase-cross ,canvas ,x ,y ,size ,fill ))
         (:star     (fast-erase-star ,canvas ,x ,y ,size ,fill ))
         (:diamond  (fast-erase-diamond ,canvas ,x ,y ,size ,fill ))
         (:triangle (fast-erase-triangle ,canvas ,x ,y ,size ,fill ))
         (t         nil))))

(defmacro mode-draw-symbol (canvas sim x y  size fill mode)
   (declare (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
  (let ((mp (gensym)))
   `(let ((,mp (cg::frame-child ,canvas)))
       (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0))
         (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
       (cg::with-paint-operation (,canvas ,mode)
        (fast-draw-symbol ,canvas ,sim ,x ,y  ,size ,fill)
        )
       )))

(defmacro mode-erase-symbol (canvas sim x y  size fill mode)
   (declare (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
   `(let ()
       (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0))
         (special cg::po-erase cg::po-paint cg::po-replace cg::po-invert))
       (cg::with-paint-operation (,canvas ,mode)
        (fast-erase-symbol ,canvas ,sim ,x ,y  ,size ,fill)
        )
       ))

(defmacro cc-set-rgb-color (canvas color)
   (declare (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
  (let ((mp (gensym)))
   `(let ((,mp (cg::frame-child ,canvas))) 
       (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0))
         (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
       (when ,color
         ;(cg::set-foreground-color ,mp ,color) 18oct05
         (setf (cg::foreground-color ,mp) ,color) ;18oct05
          ))))

(defmacro bw-set-color (canvas color)
   (declare (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
   `(let ()
       (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0))
         (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
       (when ,color
          (set-draw-color ,canvas ,color))))

(defmacro set-draw-color (canvas color)
   (declare (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
  (let ((mp (gensym)))
   `(let ((,mp (cg::frame-child ,canvas)))
       (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0))
         (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
       (when ,color
          (if (colored-canvas-p ,canvas)
             (if (colorp ,color)
                 ;(cg::set-foreground-color ,mp ,color) 18oct05
                 (setf (cg::foreground-color ,mp) ,color) ;18oct05
                ))))))

(defmacro set-fast-color (canvas color)
   (declare (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
  (let ((mp (gensym)))
   `(let ((,mp (cg::frame-child ,canvas))) 
       (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0))
         (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
       (if (colored-canvas-p ,canvas)
           ;(cg::set-foreground-color ,mp ,color) 18oct05
           (setf (cg::foreground-color ,mp) ,color) ;18oct05
          ))))

(defmacro fast-move-to (canvas x y)
  (let ((mp (gensym)))
   `(let ((,mp (cg::frame-child ,canvas)))
       (setf (cg::current-position ,mp) (cg::make-position ,x ,y)))))

(defmacro fast-line-to (canvas x y)
   (declare (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
  (let ((mp (gensym)))
   `(let ((,mp (cg::frame-child ,canvas)))
       (cg::draw-line ,mp (cg::current-position ,mp)
        (cg::make-position ,x ,y))
       )))

(defmacro fast-erase-to (canvas x y)
   (declare (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
  (let ((mp (gensym)))
   `(let ((,mp (cg::frame-child ,canvas)))
       (cg::with-paint-operation (,canvas cg::po-erase)
        (cg::erase-line ,mp (cg::current-position ,mp)
         (cg::make-position ,x ,y))
        ))))

(defmacro fast-draw-line (canvas  x1 y1 x2 y2 )
   (declare (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
  (let ((mp (gensym)))
   `(let ((,mp (cg::frame-child ,canvas)))
       (cg::draw-line ,mp (cg::make-position ,x1 ,y1)
        (cg::make-position ,x2 ,y2))
       )))

;;; More mode clarification needed
;; Original version follows

;; We need the following form for lines-pc.lsp
(defmacro fast-erase-line (canvas x1 y1 x2 y2)
   (declare (special cg::po-paint cg::po-replace cg::po-invert cg::po-erase))
  (let ((mp (gensym)))
   `(let ((,mp (cg::frame-child ,canvas)))
       (cg::with-paint-operation (,canvas cg::po-erase)
        (cg::erase-line ,mp (cg::make-position ,x1 ,y1)
         (cg::make-position ,x2 ,y2))
        ))))

;; New version from f-g\gwbtests\expts-mode-draw.lsp
(defmacro mode-draw-line (canvas x1 y1 x2 y2 mode)
   (declare (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
  (let ((mp (gensym)))
   `(let ((,mp (cg::frame-child ,canvas)))
       (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0))
         (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
       (cg::with-paint-operation (,canvas ,mode)
        (fast-draw-line ,canvas
          (the fixnum ,x1)
          (the fixnum ,y1)
          (the fixnum ,x2)
          (the fixnum ,y2))
        )
       )))

(defmacro mode-erase-line (canvas x1 y1 x2 y2 mode)
   (declare (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
   `(let ()
       (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0))
         (special cg::po-erase cg::po-invert cg::po-replace cg::po-paint))
       (cg::with-paint-operation (,canvas ,mode)
        (fast-erase-line ,canvas
          (the fixnum ,x1)
          (the fixnum ,y1)
          (the fixnum ,x2)
          (the fixnum ,y2))
        )
       ))
;;; End of file point-defs-pc.lsp
