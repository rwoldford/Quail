;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              working-point-defs-sblx.lsp
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
 
(defconstant +host-xor-mode+ (boole-to-op :boole-xor))

(defconstant +host-or-mode+ (boole-to-op :boole-1))

(defconstant +host-bic-mode+ (boole-to-op :boole-1))

(defconstant +host-copy-mode+ (boole-to-op :boole-1))

 (defmacro fast-draw-box (canvas x y size fill?)
       ;(let ((s2 (gensym)) (lx (gensym)) (ly (gensym)) (mp (gensym)))
         `(let* (;(s2 (truncate ,size 2))
          ;(lx (- ,x s2))
          ;(ly (- ,y s2))
          (mp (get-frame-pane ,canvas 'host-pane)))
         #-:sbcl(declare (optimize (speed 3) (safety 0)
          (space 0) (compilation-speed 0)))
         (cond ((eq ,size 1)
          (medium-draw-point* mp ,x ,y))
         (T
          (if ,fill?
        (medium-draw-rectangle* mp ,x ,y (+ ,x ,size) (+ ,y ,size) T)
        (medium-draw-rectangle* mp ,x ,y (+ ,x ,size) (+ ,y ,size) NIL)))
         ))
         ;)
       )

   ;;; (test-fast-draw-box *test-frame* 100 100 20 NIL) -> an outlined box of the right size and place.
   ;;; on to fast-erase-box
 (defmacro fast-erase-box (canvas x y size fill?)
       ;(let ((mpbg (gensym)) (mp (gensym)) (s2 (gensym)) (lx (gensym))
        ;(ly (gensym)) (mpfg (gensym)))
         `(let* ((mp (get-frame-pane ,canvas 'host-pane))
           (mpbg (medium-background (get-frame-pane ,canvas 'host-pane)))
           ;(mpfg (medium-foreground (get-frame-pane ,canvas 'host-pane)))
           ;(s2 (truncate ,size 2))
           ;(lx (- ,x s2))
           ;(ly (- ,y s2))
           )
          #-:sbcl(declare (optimize (speed 3) (safety 0)
                    (space 0) (compilation-speed 0)))
          (with-drawing-options (mp :ink mpbg)
          (cond ((eq ,size 1)
          (medium-draw-point* mp ,x ,y))
         (T
          (if ,fill?
        (draw-rectangle* mp ,x ,y (+ ,x ,size) (+ ,y ,size) :filled T)
        (draw-rectangle* mp ,x ,y (+ ,x ,size) (+ ,y ,size) :filled NIL))
          )
         )) 
      )
         ;)
       )

 ;;; on to fast-draw/erase-circle
 ;;; draw
 (defmacro fast-draw-circle (canvas x y  size fill? )
 "Uses medium-draw-ellipse* centred at x y"
   ;(let ((s2 (gensym))
   ;     (mp (gensym)))
  `(let ((mp (get-frame-pane ,canvas 'host-pane)))
      (cond ((eq 1 ,size)
          (medium-draw-point* mp ,x ,y))
             (t 
               (let ((s2 (if (oddp ,size) (1- (truncate (+ 1 ,size) 2))
                             (truncate (+ 1 ,size) 2)))) ;;; added + 1 24JNE99 oddp 11NOV99
                  #-:sbcl(declare (optimize (speed 3) (safety 0)
                             (space 0) (compilation-speed 0)))
                  (if ,fill?
                    (medium-draw-ellipse* mp ,x ,y s2 0 0 s2 0 (* 2 pi) T)
                     (medium-draw-ellipse* mp ,x ,y s2 0 0 s2 0 (* 2 pi) NIL)
                     ) 
                  ))
             ))
  ;)
   )

 ;;; now erase

 (defmacro fast-erase-circle (canvas x y size fill?)
       ;(let ((mpbg (gensym)) (mp (gensym)) (s2 (gensym)) (lx (gensym))
        ;(ly (gensym)) (mpfg (gensym)))
         `(let* ((mp (get-frame-pane ,canvas 'host-pane))
           (mpbg (medium-background (get-frame-pane ,canvas 'host-pane)))
           ;(mpfg (medium-foreground (get-frame-pane ,canvas 'host-pane)))
           (s2 (truncate ,size 2))
           ;(lx (- ,x s2))
           ;(ly (- ,y s2))
           )
          #-:sbcl(declare (optimize (speed 3) (safety 0)
                    (space 0) (compilation-speed 0)))
          (with-drawing-options (mp :ink mpbg)
          (cond ((eq ,size 1)
          (medium-draw-point* mp ,x ,y))
         (T
          (if ,fill?
                (medium-draw-ellipse* mp ,x ,y s2 0 0 s2 0 (* 2 pi) T)
                (medium-draw-ellipse* mp ,x ,y s2 0 0 s2 0 (* 2 pi) NIL)
          ))
         )) 
      )
         ;)
       )
;;; draw/erase cross
;;; draw
(defmacro fast-draw-cross (canvas x y  s fill? )
   #-:sbcl(declare (optimize (speed 3) (safety 0)
                 (space 0) (compilation-speed 0)))
   (declare (ignore fill?))
   ;(let ((s2 (gensym))
   ;      (l (gensym))
   ;      (mp (gensym)))
      `(let* ((s2 (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                      (truncate (+ 1 ,s) 2)))
              (l (list (h-draw::make-point (- ,x s2) ,y )
                    (h-draw::make-point (+ ,x s2 1) ,y )
                    (h-draw::make-point ,x (- ,y s2) )
                    (h-draw::make-point ,x (+ ,y s2 1))
                    ))
              (mp (get-frame-pane ,canvas 'host-pane)))
          (medium-draw-line* mp (h-draw::point-x (first l)) (h-draw::point-y (first l)) (h-draw::point-x (second l)) (h-draw::point-y (second l)))
          (medium-draw-line* mp (h-draw::point-x (third l)) (h-draw::point-y (third l)) (h-draw::point-x (fourth l)) (h-draw::point-y (fourth l)))
          )
      ;)
   )
;;; erase
(defmacro fast-erase-cross (canvas x y  s fill? )
   #-:sbcl(declare (optimize (speed 3) (safety 0)
                 (space 0) (compilation-speed 0)))
   (declare (ignore fill?))
   ;(let ((s2 (gensym))
   ;      (l (gensym))
   ;      (mp (gensym))
   ;      (mpbg (gensym))
   ;      (mpfg (gensym)))
      `(let* ((s2 (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                      (truncate (+ 1 ,s) 2)))
              (l (list (h-draw::make-point (- ,x s2) ,y )
                    (h-draw::make-point (+ ,x s2 1) ,y )
                    (h-draw::make-point ,x (- ,y s2) )
                    (h-draw::make-point ,x (+ ,y s2 1))
                    ))
              (mp (get-frame-pane ,canvas 'host-pane))
              (mpbg (medium-background (get-frame-pane ,canvas 'host-pane)))
              ;(mpfg (medium-foreground (get-frame-pane ,canvas 'host-pane)))
              )
      (with-drawing-options (mp :ink mpbg)
          (medium-draw-line* mp (h-draw::point-x (first l)) (h-draw::point-y (first l)) (h-draw::point-x (second l)) (h-draw::point-y (second l)))
          (medium-draw-line* mp (h-draw::point-x (third l)) (h-draw::point-y (third l)) (h-draw::point-x (fourth l)) (h-draw::point-y (fourth l)))
          )
          )
      ;)
   )
;;; draw/erase diamond
;;; draw

;;; Here is a forn which works:
; (draw-polygon* *pane* '(140 150 150 140 160 150 150 160 140 150) :filled nil :closed t)
;10200
(defmacro fast-draw-diamond (canvas x y  s fill? )
#-:sbcl(declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0)))
  ;(let ((s2 (gensym))
  ;      (l (gensym)) (r (gensym))
  ;      (tp (gensym)) (b (gensym)) (mp (gensym))
  ;      )  
    `(let* ((s2 (truncate ,s 2))
            (l (- (the fixnum ,x) s2))
            (b (+ (the fixnum ,y) s2))
            (r (+ (the fixnum ,x) s2))
            (tp (- (the fixnum ,y) s2))
            (mp (get-frame-pane ,canvas 'host-pane)))
    (declare (type fixnum l r tp b s2))
       (if ,fill?
        (medium-draw-polygon* mp (list ,x tp r ,y r ,y ,x b ,x b l ,y l ,y ,x tp) NIL T)
        (medium-draw-polygon* mp (list ,x tp r ,y r ,y ,x b ,x b l ,y l ,y ,x tp) T NIL))
         )
    ;)
  )
;;; erase [there does not seem to be one in other point-defs files]
(defmacro fast-erase-diamond (canvas x y  s fill? )
  #-:sbcl(declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0)))
  ;(let ((s2 (gensym))
  ;      (l (gensym)) (r (gensym))
  ;      (tp (gensym)) (b (gensym))(mp (gensym)) (mpbg (gensym))
  ;      )  
    `(let* ((s2 (truncate ,s 2))
            (l (- (the fixnum ,x) s2))
            (b (+ (the fixnum ,y) s2))
            (r (+ (the fixnum ,x) s2))
            (tp (- (the fixnum ,y) s2))
            (mp (get-frame-pane ,canvas 'host-pane))
            (mpbg (medium-background (get-frame-pane ,canvas 'host-pane))))
    (declare (type fixnum l r tp b s2))
    (with-drawing-options (mp :ink mpbg)
       (if ,fill?
        (medium-draw-polygon* mp (list ,x tp r ,y r ,y ,x b ,x b l ,y l ,y ,x tp) NIL T)
        (medium-draw-polygon* mp (list ,x tp r ,y r ,y ,x b ,x b l ,y l ,y ,x tp) T NIL))
       )
         )
    ;)
  )
;;; draw/erase triangle
;;; draw
(defmacro fast-draw-triangle (canvas x y  s fill? )
  ;(let ((s2 (gensym))
  ;      (l (gensym)) (r (gensym))
  ;      (tp (gensym)) (b (gensym))
  ;      (mp(gensym)))
    `(let* ((s2 (truncate ,s 2))
            (l (- (the fixnum ,x) s2))
            (b (+ (the fixnum ,y) s2))
            (r (+ (the fixnum ,x) s2))
            (tp (- (the fixnum ,y) s2))
            (mp (get-frame-pane ,canvas 'host-pane)))
       #-:sbcl(declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0))
                (type fixnum l r tp b s2))
       (if ,fill?
        (medium-draw-polygon* mp (list l b ,x tp ,x tp r b r b l b) NIL T)
        (medium-draw-polygon* mp (list l b ,x tp ,x tp r b r b l b) T NIL)
         ))
    ;)
  )
;;;erase
(defmacro fast-erase-triangle (canvas x y  s fill? )
  ;(let ((s2 (gensym))
  ;      (l (gensym)) (r (gensym))
  ;      (tp (gensym)) (b (gensym))
  ;      (mp(gensym))
  ;      (mpbg (gensym)))
    `(let* ((s2 (truncate ,s 2))
            (l (- (the fixnum ,x) s2))
            (b (+ (the fixnum ,y) s2))
            (r (+ (the fixnum ,x) s2))
            (tp (- (the fixnum ,y) s2))
            (mp (get-frame-pane ,canvas 'host-pane))
            (mpbg (medium-background (get-frame-pane ,canvas 'host-pane))))
       #-:sbcl(declare (optimize (speed 3) (safety 0)
                          (space 0) (compilation-speed 0))
                (type fixnum l r tp b s2))
       (with-drawing-options (mp :ink mpbg)
       (if ,fill?
        (medium-draw-polygon* mp (list l b ,x tp ,x tp r b r b l b) NIL T)
        (medium-draw-polygon* mp (list l b ,x tp ,x tp r b r b l b) T NIL)
         ))
       )
    ;)
  )
;;; draw/erase star
;;; draw , but first
(defconstant +cos-18+ (cos (* pi 0.1))) ;10MAY2024 to comply with convention about naming constants with + not *
(defconstant +cos-54+ (cos (* pi 0.3)))
(defconstant +sin-18+ (sin (* pi 0.1)))
(defconstant +sin-54+ (sin (* pi 0.3)))
#|
(defmacro fast-draw-star (canvas x y  s fill? )
   (declare (ignore fill?) (optimize (speed 3) (safety 0)
                            (space 0) (compilation-speed 0)))
   ;(let ((r (gensym)) (r*c18 (gensym))
   ;      (r*s18 (gensym)) (r*c54 (gensym)) (r*s54 (gensym))
   ;      (x1  (gensym)) (y1 (gensym))
   ;      (x2 (gensym)) (y2 (gensym))
   ;      (x3 (gensym)) 
   ;      (x4 (gensym)) 
   ;      (x5 (gensym)) (y5 (gensym))
   ;      (mp (gensym)))
      ;;; Check for size = 1 off the top
  `(let ((mp (get-frame-pane ,canvas 'host-pane)))
      (cond ((= 1 ,s)
          (medium-draw-point* mp ,x ,y))
             (T
              (let* ((r (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                            (truncate (+ 1 ,s) 2))) ;; added +1 27Jne99
                     (r*c18 (round (* r *cos-18*)))
                     (r*s18 (round (* r *sin-18*)))
                     (r*c54 (round (* r *cos-54*)))
                     (r*s54 (round (* r *sin-54*)))
                     (,x1  (+ (the fixnum ,x) (the fixnum r*c18)))
                     (,y1 (- (the fixnum ,y) (the fixnum r*s18)))
                     (,x2 (the fixnum ,x))
                     (,y2 (- (the fixnum ,y) (the fixnum r)))
                     (,x3 (- (the fixnum ,x) (the fixnum r*c18)))
                     (,x4 (- (the fixnum ,x) (the fixnum r*c54)))
                     (,x5 (+ (the fixnum ,x) (the fixnum r*c54)))
                     (,y5 (+ (the fixnum ,y) (the fixnum r*s54)))
                     )
                 (medium-draw-lines* mp (list ,x ,y ,x1 ,y1 ,x ,y ,x2 ,y2 ,x ,y ,x3 ,y1 ,x ,y ,x4 ,y5 ,x ,y ,x5 ,y5))
                 ))))
  ;)
   )
|#
;;;New version because of the *s
(defmacro fast-draw-star (canvas x y  s fill? )
   #-:sbcl(declare (optimize (speed 3) (safety 0)
                            (space 0) (compilation-speed 0)))
   (declare (ignore fill?))
   ;(let ((r (gensym)) (r*c18 (gensym))
   ;      (r*s18 (gensym)) (r*c54 (gensym)) (r*s54 (gensym))
   ;      (x1  (gensym)) (y1 (gensym))
   ;      (x2 (gensym)) (y2 (gensym))
   ;      (x3 (gensym)) 
   ;      (x4 (gensym)) 
   ;      (x5 (gensym)) (y5 (gensym))
   ;      (mp (gensym)))
      ;;; Check for size = 1 off the top
  `(let ((mp (get-frame-pane ,canvas 'host-pane)))
      (cond ((= 1 ,s)
          (medium-draw-point* mp ,x ,y))
             (T
              (let* ((r (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                            (truncate (+ 1 ,s) 2))) ;; added +1 27Jne99
                     (rc18 (round (* r +cos-18+)))
                     (rs18 (round (* r +sin-18+)))
                     (rc54 (round (* r +cos-54+)))
                     (rs54 (round (* r +sin-54+)))
                     (x1  (+ (the fixnum ,x) (the fixnum rc18)))
                     (y1 (- (the fixnum ,y) (the fixnum rs18)))
                     (x2 (the fixnum ,x))
                     (y2 (- (the fixnum ,y) (the fixnum r)))
                     (x3 (- (the fixnum ,x) (the fixnum rc18)))
                     (x4 (- (the fixnum ,x) (the fixnum rc54)))
                     (x5 (+ (the fixnum ,x) (the fixnum rc54)))
                     (y5 (+ (the fixnum ,y) (the fixnum rs54)))
                     )
                 (medium-draw-lines* mp (list ,x ,y x1 y1 ,x ,y x2 y2 ,x ,y x3 y1 ,x ,y x4 y5 ,x ,y x5 y5))
                 ))))
  ;)
   )



;;; erase
#|
(defmacro fast-erase-star (canvas x y  s fill? )
   (declare (ignore fill?) (optimize (speed 3) (safety 0)
                            (space 0) (compilation-speed 0)))
   ;(let ((r (gensym)) (r*c18 (gensym))
   ;      (r*s18 (gensym)) (r*c54 (gensym)) (r*s54 (gensym))
   ;      (x1  (gensym)) (y1 (gensym))
   ;      (x2 (gensym)) (y2 (gensym))
   ;      (x3 (gensym)) 
   ;      (x4 (gensym)) 
   ;      (x5 (gensym)) (y5 (gensym))
   ;      (mp (gensym))
   ;      (mpbg (gensym)))
      ;;; Check for size = 1 off the top
  `(let ((mp (get-frame-pane ,canvas 'host-pane)))
      (cond ((= 1 ,s)
          (medium-draw-point* mp ,x ,y))
             (T
              (let* ((r (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                            (truncate (+ 1 ,s) 2))) ;; added +1 27Jne99
                     (r*c18 (round (* r *cos-18*)))
                     (r*s18 (round (* r *sin-18*)))
                     (r*c54 (round (* r *cos-54*)))
                     (r*s54 (round (* r *sin-54*)))
                     (,x1  (+ (the fixnum ,x) (the fixnum r*c18)))
                     (,y1 (- (the fixnum ,y) (the fixnum r*s18)))
                     (,x2 (the fixnum ,x))
                     (,y2 (- (the fixnum ,y) (the fixnum r)))
                     (,x3 (- (the fixnum ,x) (the fixnum r*c18)))
                     (,x4 (- (the fixnum ,x) (the fixnum r*c54)))
                     (,x5 (+ (the fixnum ,x) (the fixnum r*c54)))
                     (,y5 (+ (the fixnum ,y) (the fixnum r*s54)))
                     (mpbg (medium-background (get-frame-pane ,canvas 'host-pane)))
                     )
              (with-drawing-options (mp :ink mpbg)
                 (medium-draw-lines* mp (list ,x ,y ,x1 ,y1 ,x ,y ,x2 ,y2 ,x ,y ,x3 ,y1 ,x ,y ,x4 ,y5 ,x ,y ,x5 ,y5))
                 )
                 ))))
  ;)
   )
|#
;;; new version because of the *s
(defmacro fast-erase-star (canvas x y  s fill? )
   #-:sbcl(declare (optimize (speed 3) (safety 0)
                            (space 0) (compilation-speed 0)))
   (declare (ignore fill?))
   ;(let ((r (gensym)) (r*c18 (gensym))
   ;      (r*s18 (gensym)) (r*c54 (gensym)) (r*s54 (gensym))
   ;      (x1  (gensym)) (y1 (gensym))
   ;      (x2 (gensym)) (y2 (gensym))
   ;      (x3 (gensym)) 
   ;      (x4 (gensym)) 
   ;      (x5 (gensym)) (y5 (gensym))
   ;      (mp (gensym))
   ;      (mpbg (gensym)))
      ;;; Check for size = 1 off the top
  `(let ((mp (get-frame-pane ,canvas 'host-pane)))
      (cond ((= 1 ,s)
          (medium-draw-point* mp ,x ,y))
             (T
              (let* ((r (if (oddp ,s) (1- (truncate (+ 1 ,s) 2))
                            (truncate (+ 1 ,s) 2))) ;; added +1 27Jne99
                     (rc18 (round (* r +cos-18+)))
                     (rs18 (round (* r +sin-18+)))
                     (rc54 (round (* r +cos-54+)))
                     (rs54 (round (* r +sin-54+)))
                     (x1  (+ (the fixnum ,x) (the fixnum rc18)))
                     (y1 (- (the fixnum ,y) (the fixnum rs18)))
                     (x2 (the fixnum ,x))
                     (y2 (- (the fixnum ,y) (the fixnum r)))
                     (x3 (- (the fixnum ,x) (the fixnum rc18)))
                     (x4 (- (the fixnum ,x) (the fixnum rc54)))
                     (x5 (+ (the fixnum ,x) (the fixnum rc54)))
                     (y5 (+ (the fixnum ,y) (the fixnum rs54)))
                     (mpbg (medium-background (get-frame-pane ,canvas 'host-pane)))
                     )
              (with-drawing-options (mp :ink mpbg)
                 (medium-draw-lines* mp (list ,x ,y x1 y1 ,x ,y x2 y2 ,x ,y x3 y1 ,x ,y x4 y5 ,x ,y x5 y5))
                 )
                 ))))
  ;)
   )



;;; draw/erase symbol
;;; draw
;;;;NOTE the test-fast-draw
(defmacro fast-draw-symbol (canvas sim x y  size fill )
   `(let ()
       #-:sbcl(declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0)))
       (case ,sim
         (:box      (fast-draw-box ,canvas ,x ,y ,size ,fill))
         (:circle   (fast-draw-circle ,canvas ,x ,y ,size ,fill ))
         (:cross    (fast-draw-cross ,canvas ,x ,y ,size ,fill ))
         (:star     (fast-draw-star ,canvas ,x ,y ,size ,fill ))
         (:diamond  (fast-draw-diamond ,canvas ,x ,y ,size ,fill ))
         (:triangle (fast-draw-triangle ,canvas ,x ,y ,size ,fill ))
         (t  NIL))))
;;; erase
(defmacro fast-erase-symbol (canvas sim x y  size fill )
   `(let ()
       #-:sbcl(declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0)))
       (case ,sim
         (:box      (fast-erase-box ,canvas ,x ,y ,size ,fill))
         (:circle   (fast-erase-circle ,canvas ,x ,y ,size ,fill ))
         (:cross    (fast-erase-cross ,canvas ,x ,y ,size ,fill ))
         (:star     (fast-erase-star ,canvas ,x ,y ,size ,fill ))
         (:diamond  (fast-erase-diamond ,canvas ,x ,y ,size ,fill ))
         (:triangle (fast-erase-triangle ,canvas ,x ,y ,size ,fill ))
         (t  NIL))))
;;; mode-draw/erase  
;;; since there does not seem to be a mode, just call fast/draw-erase
;;; NOTE the use of test- here
;;; draw
(defmacro mode-draw-symbol (canvas sim x y  size fill mode)
   (declare (ignore mode))
  ;(let ((mp (gensym)))
   `(let ((mp (get-frame-pane ,canvas 'host-pane)))
       #:-sbcl(declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0)))
       ;(fast-draw-symbol ,canvas ,sim ,x ,y ,size ,fill)
        (fast-draw-symbol ,canvas ,sim ,x ,y  ,size ,fill)
       )
   ;)
  )
;;; erase
(defmacro mode-erase-symbol (canvas sim x y  size fill mode)
   (declare (ignore mode))
  ;(let ((mp (gensym)))
   `(let ((mp (get-frame-pane ,canvas 'host-pane)))
       #-:sbcl(declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0)))
       ;(fast-erase-symbol ,canvas ,sim ,x ,y ,size ,fill)
        (fast-erase-symbol ,canvas ,sim ,x ,y  ,size ,fill)
       )
   ;)
  )
;;; cc-set-rgb-color
(defmacro cc-set-rgb-color (canvas color)
  ;(let ((mp (gensym)))
   `(let ((mp (get-frame-pane ,canvas 'host-pane))) 
       #-:sbcl(declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0)))
       (when ,color
         ;(cg::set-foreground-color ,mp ,color) 18oct05
         (setf (medium-ink mp) ,color) ;18dec2024
          ))
   ;)
  )
;;; set-draw-color
;;; UNTESTED BECAUSE I DO NOT HAVE (COLORED-CANVAS-P)
(defmacro set-draw-color (canvas color)
  ;(let ((mp (gensym)))
   `(let ((mp (get-frame-pane ,canvas 'host-pane)))
       #-:sbcl(declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0)))
       (when ,color
          (if (colored-canvas-p ,canvas)
             (if (colorp ,color)
                 ;(cg::set-foreground-color ,mp ,color) 18oct05
                 (setf (medium-ink mp) ,color) ;18dec2024
                ))))
   ;)
  )
;;; bw-set-color
;;; UNTESTED BECUASE I DO NOT HAVE (COLORED-CANVAS-P) NEEDED BY TEST-SET-DRAW-COLOR
;;; Note there is (make-gray-color luminence) where luminence 0 = black, 1 = white
(defmacro bw-set-color (canvas color)
   `(let ()
       #-:sbcl(declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0)))
       (when ,color
          (set-draw-color ,canvas ,color))))

;;; set-fast-color
;;; UNTESTED AS ABOVE
(defmacro set-fast-color (canvas color)
  ;(let ((mp (gensym)))
   `(let ((mp (get-frame-pane ,canvas 'host-pane))) 
       (if (colored-canvas-p ,canvas)
           ;(cg::set-foreground-color ,mp ,color) 18oct05
           (setf (medium-ink mp)  ,color) ;18dec2024
          ))
   ;)
  )
;;; fast-move-to
(defmacro fast-move-to (canvas x y)
  ;(let ((mp (gensym)))
   `(let ((mp (get-frame-pane ,canvas 'host-pane)))
    (stream-set-cursor-position mp ,x ,y))
   ;)
  )
;;; fast-line-to
(defmacro fast-line-to (canvas x y)
  ;(let ((mp (gensym))
  ;  (cur-pos (gensym))
  ;  (cur-pos-x (gensym))
  ;  (cur-pos-y (gensym)))
   `(let* ((mp (get-frame-pane ,canvas 'host-pane))
    (cur-pos (multiple-value-list (stream-cursor-position (get-frame-pane ,canvas 'host-pane))))
    (cur-pos-x (first cur-pos))
    (cur-pos-y (second cur-pos)))

    (medium-draw-line* mp cur-pos-x cur-pos-y ,x ,y)
       )
   ;)
  )
;;; fast-erase-to
(defmacro erase-line-to (canvas x y)
  ;(let ((mp (gensym))
  ;  (mpbg (gensym))
  ;  (cur-pos (gensym))
  ;  (cur-pos-x (gensym))
  ;  (cur-pos-y (gensym)))
   `(let* ((mp (get-frame-pane ,canvas 'host-pane))
    (cur-pos (multiple-value-list (stream-cursor-position (get-frame-pane ,canvas 'host-pane))))
    (cur-pos-x (first cur-pos))
    (cur-pos-y (second cur-pos))
    (mpbg (medium-background (get-frame-pane ,canvas 'host-pane))))
    (with-drawing-options (mp :ink mpbg)
    (medium-draw-line* mp cur-pos-x cur-pos-y ,x ,y)
    )
       )
   ;)
  )
;;; fast-draw-line
(defmacro fast-draw-line (canvas  x1 y1 x2 y2 )
  ;(let ((mp (gensym)))
   `(let ((mp (get-frame-pane ,canvas 'host-pane)))
    (medium-draw-line* mp ,x1 ,y1 ,x2 ,y2)
       )
   ;)
  )
;;; fast-erase-line
(defmacro fast-erase-line (canvas  x1 y1 x2 y2 )
  ;(let ((mp (gensym))
  ;  (mpbg (gensym)))
   `(let* ((mp (get-frame-pane ,canvas 'host-pane))
    (mpbg (medium-background mp) ))
   (with-drawing-options (mp :ink mpbg)
    (medium-draw-line* mp ,x1 ,y1 ,x2 ,y2)
    )
       )
   ;)
  )
;;; mode-draw-line
;;; there is no mode that I can see, so imitate test-fast-draw-line
(defmacro mode-draw-line (canvas x1 y1 x2 y2 mode)
       #+:sbcl(declare (ignore mode))
        (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0)))
  ;(let ((mp (gensym)))
   ;`(let ((mp (get-frame-pane ,canvas 'host-pane)))
        `(fast-draw-line ,canvas
          (the fixnum ,x1)
          (the fixnum ,y1)
          (the fixnum ,x2)
          (the fixnum ,y2))
        ;)
       ;)
  )
;;; mode-erase-line
(defmacro mode-erase-line (canvas x1 y1 x2 y2 mode)
       #+:sbcl(declare (ignore mode))
       (declare (optimize (speed 3) (safety 0)
                  (space 0) (compilation-speed 0)))
  ;(let ((mp (gensym)))
   ;`(let ((mp (get-frame-pane ,canvas 'host-pane)))
        `(fast-erase-line ,canvas
          (the fixnum ,x1)
          (the fixnum ,y1)
          (the fixnum ,x2)
          (the fixnum ,y2))
        ;)
       ;)
  )
;;; DONE