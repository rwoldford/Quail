;(defun make-point (x &optional y)  
;     "Returns a point having x and y as its coordinates."
;     (if y
;        (cg::make-position x y)
;        (if (cg::positionp x)
;           x
;           (error "X (~s) is not a position or y missing" x)
;           )))

;(defun point-x (point)
;     "Returns the x-coordinate of point."
;     (cg::position-x point))

;(defun point-y (point)
;     "Returns the y-coordinate of point."
;     (cg::position-y point))

;(defun pen-position (canvas)
; (let ((mp (cg::frame-child canvas)))
;   (cg::current-position mp)))

;(defun pen-size (canvas)
;  (let ((mp (cg::frame-child canvas )))
;    (cg::line-width mp)))

#| ;;original version 18oct05
(defun set-pen-size (canvas h &optional v)
  (declare (inline point-x point-y))
  (let ((mp (cg::frame-child canvas)))
    (if v
        (cg::set-line-width mp (floor (+ h v) 2))
      (cg::set-line-width mp (floor (+ (point-x h)
                                       (point-y h))
                                    2)))))
|#


#| old version 18oct05
(defun set-pen-color (canvas new-color)
  (let ((mp (cg::frame-child canvas)))
    (cg::set-foreground-color mp new-color)))
|# old version 18oct05

;; start new version 18oct05
;  (declare (inline point-x point-y))
;  (let ((mp (cg::frame-child canvas)))
;    (if v
;        (setf (cg::line-width mp) (floor (+h v) 2))
;      (setf (cg::line-width mp) (floor (+ (point-x h)
;                                         (point-y h))
;                                      2)))))
;; end new version 18oct05

;(defun pen-mode (canvas)
;  (let ((mp (cg::frame-child canvas)))
;     (cg::paint-operation mp)))

;(defun set-pen-mode (canvas new-mode)
;  (let ((mp (cg::frame-child canvas)))
;    ;(cg::set-paint-operation mp new-mode) 18oct05
;    (setf (cg::paint-operation mp) new-mode) ;18oct05
;    ))

;(defun set-pen-pattern (canvas new-mode)
 ; (let ((mp (cg::frame-child canvas)))
  ;  (setf (cg::line-texture mp) new-pattern)))

;;; start new version 18oct05
;(defun set-pen-color (canvas new-color)
;  (let ((mp (cg::frame-child canvas)))
;    (setf (cg::foreground-color mp) new-color)))
;;; end new version 18oct05

;(defun move-to (canvas h &optional v)
;  (let ((mp (cg::frame-child canvas)))
;     (if v
;        (cg::move-to-x-y mp h v)
;        (cg::move-to mp h))))


;(defun move (canvas h &optional v)
;  (let ((mp (cg::frame-child canvas)))
;     (if v
;        (cg::move-by-x-y mp h v)
;        (cg::move-by mp h))))

;(defun line-to (canvas h &optional v)
;  (let ((mp (cg::frame-child canvas)))
;     (cg::draw-to mp (make-point h v))))

;; WHat does the following form do then ? What is draw-by about ?
;; It draws from cur-pos to cur-pos+offset
;(defun line (canvas h &optional v)
;  (let ((mp (cg::frame-child canvas)))
;     (cg::draw-by mp (make-point h v))))


#|original version
 (defun draw-line (canvas x1 y1 x2 y2)
   (move-to canvas x1 y1)
   (line-to canvas x2 y2))
|#
;; Add a point if we are drawing horizontally or vertically
;; to fix the cross symbol in static form (vs dynamic form).
;(defun draw-line (canvas x1 y1 x2 y2)
;  (let ((mp (cg::frame-child canvas)))
;   (move-to canvas x1 y1)
;   (line-to canvas x2 y2)
;   (cond ((= x1 x2)
;          (cg::draw-to mp (cg::make-position x2 (+ 1 y2))))
;         ((= y1 y2)
;          (cg::draw-to mp (cg::make-position (+ 1 x2) y2)))
;         )
;   (cg::move-to mp (cg::make-position x2 y2) ;; to expected end for next draw
;     )))

   
;(defun draw-rectangle (canvas left right bot top)  ;;x1 x2 y1 y2)
;  (let ((mp (cg::frame-child canvas)))
;     (cg::draw-box mp (cg::make-box left top right bot))) ;; x1 y1 x2 y2)))
;  )

;; Version compatible with fast-macros using centre and radius
;; in drawing boxes - 15 Feb 98
;; and compatible with V/V-M/draw-macros.lsp/with-point-symbol-bounds
;; 17 Feb 98. and fixed overall Feb 25 1998
;(defun draw-inside-rectangle (canvas left &optional top right bot)
;   "Draws a specified box no longer worrying about ~
;Inside but compatible with fast-macros."
;  (let ((mp (cg::frame-child canvas)))
;   (cond ((numberp left)
;          (let* ((mx (truncate (+ left right) 2))
;                  (my (truncate (+ top bot ) 2))
;                  (sh (truncate (- right left) 2))
;                  (sv (truncate (- bot top) 2))
;                  (lhs (- mx sh))
;                  (rhs (+ lhs (- right left)))
;                  (ths (if (oddp (- bot top)) (- my sv -1) (- my sv))) ;<**
;                  (bhs (+ ths (- bot top))))
;              (if (= 1 (- rhs lhs)) ;;02nov99
;                 (setf (cg::pixel-x-y mp lhs ths) cg::black) ;;02nov99
;                  (cg::draw-box mp (cg::make-box lhs ths rhs bhs)))
;              ))
;         ((cg::boxp left)
;          ;;; left is then a box which must be picked apart
;          (let* ((lx (cg::box-left left)) (rx (cg::box-right left)) 
;                 (ty (cg::box-top left)) (by (cg::box-bottom left))
;                 (mx (truncate (+ lx rx) 2))
;                 (my (truncate (+ ty by ) 2))
;                 (sh (truncate (- rx lx) 2))
;                 (sv (truncate (- by ty) 2))
;                 (lhs (- mx sh))
;                 (rhs (+ lhs (- rx lx)))
;                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
;                 (bhs (+ ths (- by ty))))
;             (if (= 1 (- rhs lhs))   ;;02nov99
;                (setf (cg::pixel-x-y mp lhs ths) cg::black) ;;02nov99
;             (cg::draw-box mp (cg::make-box lhs ths rhs bhs)))
;             ))
;         ((cg::positionp top)
;          ;;; top is a position and thus so is left. left is top-left  top is bottom-right
;          (let* ((lx (cg::position-x left)) (rx (cg::position-x top))
;                 (ty (cg::position-y left)) (by (cg::position-y top))
;                (mx (truncate (+ lx rx) 2))
;                 (my (truncate (+ ty by ) 2))
;                 (sh (truncate (- rx lx) 2))
;                 (sv (truncate (- by ty) 2))
;                 (lhs (- mx sh))
;                 (rhs (+ lhs (- rx lx)))
;                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
;                 (bhs (+ ths (- by ty))))
;             (if (= 1 (- rhs lhs))  ;;02nov99
;                (setf (cg::pixel-x-y mp lhs ths) cg::black) ;;02nov99
;             (cg::draw-box mp (cg::make-box lhs ths rhs bhs)))
;                ))           
;         ((cg::positionp right)
;          ;;; left *is* left, top *is* top, right *is* position(bottom-right)
;          (let* ((lx left) (rx (cg::position-x right)) (ty top)
;                 (by (cg::position-y right))
;                 (mx (truncate (+ lx rx) 2))
;                 (my (truncate (+ ty by ) 2))
;                 (sh (truncate (- rx lx) 2))
;                 (sv (truncate (- by ty) 2))
;                 (lhs (- mx sh))
;                 (rhs (+ lhs (- rx lx)))
;                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
;                 (bhs (+ ths (- by ty))))
;             (if (= 1 (- rhs lhs)) ;;02nov99
;                (setf (cg::pixel-x-y mp lhs ths) cg::black) ;;02nov99
;             (cg::draw-box mp (cg::make-box lhs ths rhs bhs)))
;                )) 
;         #|
;         (t 
;           ;;; all four arguments are what they say they are
;           (let* ((mx (truncate (+ left right) 2))
;                  (my (truncate (+ top bot ) 2))
;                  (sh (truncate (- right left) 2))
;                  (sv (truncate (- bot top) 2))
;                  (lhs (- mx sh))
;                  (rhs (+ lhs (- right left)))
;                  (ths (if (oddp (- bot top)) (- my sv -1) (- my sv))) ;<**
;                  (bhs (+ ths (- bot top))))
;              (if (= 1 (- rhs lhs)) ;;02nov99
;                 (setf (cg::pixel-x-y mp lhs ths) cg::black) ;;02nov99
;                  (cg::draw-box mp (cg::make-box lhs ths rhs bhs)))
;              ))|#
;         )))

#|
> .. what (draw-inside-rectangle canvas left top right bottom) is supposed
> to draw.
>
> Is it supposed to really draw (+ 1 left)  (+ 1 top) (- 1 right) (- 1
> bot) ,for example, thus
>
> drawing 1-inside l t r b ?

I think so.  Can¹t tell from the mac version but from host-draw-clx.lsp
it seems that that is true.

(defun draw-inside-rectangle (canvas left &optional top right bot)
  (let* ((window (wb::host-window canvas))
  (gcontext (wb::gcontext canvas))
  (x1 (+ left 1))
  (y1 (+ top 1))
  (width (- right x1 1))
  (height (- bot y1 1)))
    (xlib:draw-rectangle window gcontext x1 y1 width height)
    (move-to canvas x1 y1)))


|#

#|  carry over from -pc and not correct even then
(defun draw-inside-rectangle (canvas left &optional top right bot)
   "Draws a specified box no longer worrying about ~
Inside but compatible with fast-macros."
  (let ((mp (clim-user::get-frame-pane canvas 'display)))
   (cond ((numberp bot)
          (let* ((mx (truncate (+ left right) 2))
                  (my (truncate (+ top bot ) 2))
                  (sh (truncate (- right left) 2))
                  (sv (truncate (- bot top) 2))
                  (lhs (- mx sh))
                  (rhs (+ lhs (- right left)))
                  (ths (if (oddp (- bot top)) (- my sv -1) (- my sv)))
                  (bhs (+ ths (- bot top))))
              (if (= 1 (- rhs lhs))
                  (clim-user::draw-point* mp lhs ths :ink clim-user::+black+)
                  (clim-user::draw-rectangle* mp lhs ths rhs bhs :filled NIL))
              ))
         ((equal (type-of left) 'STANDARD-RECTANGLE)
          ;;; left is then a box which must be picked apart
          (let* ((lx (clim-user::rectangle-min-x left)) (rx (clim-user::rectangle-max-x left))
                 (ty (clim-user::rectangle-min-y left)) (by (clim-user::rectangle-max-y left))
                 (mx (truncate (+ lx rx) 2))
                 (my (truncate (+ ty by ) 2))
                 (sh (truncate (- rx lx) 2))
                 (sv (truncate (- by ty) 2))
                 (lhs (- mx sh))
                 (rhs (+ lhs (- rx lx)))
                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
                 (bhs (+ ths (- by ty))))
             (if (= 1 (- rhs lhs))
                (clim-user::draw-point* mp lhs ths :ink clim-user::+black+)
                (clim-user::draw-rectangle* mp lhs ths rhs bhs :filled NIL))
             ))
         ((equal (type-of top) 'STANDARD-POINT)
          ;;; top is a position and thus so is left. left is top-left  top is bottom-right
          (let* ((lx (clim-user::point-x left)) (rx (clim-user::point-x top))
                 (ty (clim-user::point-y left)) (by (clim-user::point-y top))
                 (mx (truncate (+ lx rx) 2))
                 (my (truncate (+ ty by ) 2))
                 (sh (truncate (- rx lx) 2))
                 (sv (truncate (- by ty) 2))
                 (lhs (- mx sh))
                 (rhs (+ lhs (- rx lx)))
                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
                 (bhs (+ ths (- by ty))))
             (if (= 1 (- rhs lhs))  ;;02nov99
                (clim-user::draw-point* mp lhs ths :ink clim-user::+black+)
                (clim-user::draw-rectangle* mp lhs ths rhs bhs :filled NIL))
                ))           
         ((equal (type-of right) 'STANDARD-POINT)
          ;;; left *is* left, top *is* top, right *is* position(bottom-right)
          (let* ((lx left) (rx (clim-user::point-x right)) (ty top)
                 (by (clim-user::point-y right))
                 (mx (truncate (+ lx rx) 2))
                 (my (truncate (+ ty by ) 2))
                 (sh (truncate (- rx lx) 2))
                 (sv (truncate (- by ty) 2))
                 (lhs (- mx sh))
                 (rhs (+ lhs (- rx lx)))
                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
                 (bhs (+ ths (- by ty))))
             (if (= 1 (- rhs lhs)) ;;02nov99
                (clim-user::draw-point* mp lhs ths rhs bhs :ink clim-user::+black+)
                (clim-user::draw-rectangle* mp lhs ths rhs bhs :filled NIL))
                )) 
         #|
         (t 
           ;;; all four arguments are what they say they are
           (let* ((mx (truncate (+ left right) 2))
                  (my (truncate (+ top bot ) 2))
                  (sh (truncate (- right left) 2))
                  (sv (truncate (- bot top) 2))
                  (lhs (- mx sh))
                  (rhs (+ lhs (- right left)))
                  (ths (if (oddp (- bot top)) (- my sv -1) (- my sv))) ;<**
                  (bhs (+ ths (- bot top))))
              (if (= 1 (- rhs lhs)) ;;02nov99
                 (setf (cg::pixel-x-y mp lhs ths) cg::black) ;;02nov99
                  (cg::draw-box mp (cg::make-box lhs ths rhs bhs)))
              ))|#
         )))
     |#


;(defun draw-filled-rectangle (canvas left &optional top right bot)
;     "Greg's try at draw-filled-rectangle"
;     ;;; fill-box demands a box as well as a stream 
;     ;;; So I have to invert the order of tests as I've done elsewhere
;     ;;; in draw-ellipse
;  (let ((mp (cg::frame-child canvas)))
;     (if bot (if (oddp (- bot top))
;        (cg::fill-box mp (cg::make-box left (+ 1 top) right (+ 1 bot)))
;                (cg::fill-box mp (cg::make-box left top right bot)))
;        (if right (cg::fill-box mp (cg::make-box-from-corners (cg::make-position left top) right))
;           (if top (cg::fill-box mp (cg::make-box-from-corners left top))
;              (cg::fill-box mp left))))))


     #| outmoded
(defun draw-filled-rectangle (canvas left &optional top right bot)
     "Greg's try at draw-filled-rectangle"
     ;;; fill-box demands a box as well as a stream 
     ;;; So I have to invert the order of tests as I've done elsewhere
     ;;; in draw-ellipse
  (let ((mp (clim-user::get-frame-pane canvas 'display)))
     (if bot (if (oddp (- bot top))
        (clim-user::draw-rectangle* mp left (+ 1 top) right (+ 1 bot))
                (clim-user::draw-rectangle* mp left top right bot))
        (if right (clim-user::draw-rectangle mp (clim-user::make-point left top) right)
           (if top (clim-user::draw-rectangle mp left top)
              (clim-user::draw-rectangle mp (clim-user::rectangle-min-point left)
                (clim-user::rectangle-max-point left)))))))
|#


;;; New version to make compatible with draw-inside-rectangle
;;; Feb 25 1998
;(defun erase-rect (canvas left &optional top right bot)
;   "Erases a box 1 pixel inside specified box"
;   (cond ((cg::boxp left)
;          ;;; left is then a box which must be picked apart;
;          (let* ((lx (cg::box-left left)) (rx (cg::box-right left)) 
;                 (ty (cg::box-top left)) (by (cg::box-bottom left))
;                 (mx (truncate (+ lx rx) 2))
;                 (my (truncate (+ ty by ) 2))
;                 (sh (truncate (- rx lx) 2))
;                 (sv (truncate (- by ty) 2))
;                 (lhs (- mx sh))
;                 (rhs (+ lhs (- rx lx)))
;                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
;                 (bhs (+ ths (- by ty)))
;                 (the-box (cg::make-box lhs ths rhs bhs)))
;      (let ((mp (cg::frame-child canvas)))
;             (cg::erase-box mp the-box)
;             (cg::erase-contents-box mp the-box))
;          ))
;         ((cg::positionp top)
;          ;;; top is a position and thus so is left. left is top-left  top is bottom-right
;          (let* ((lx (cg::position-x left)) (rx (cg::position-x top))
;                 (ty (cg::position-y left)) (by (cg::position-y top))
;                 (mx (truncate (+ lx rx) 2))
;                 (my (truncate (+ ty by ) 2))
;                 (sh (truncate (- rx lx) 2))
;                 (sv (truncate (- by ty) 2))
;                 (lhs (- mx sh))
;                 (rhs (+ lhs (- rx lx)))
;                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
;                 (bhs (+ ths (- by ty)))
;                 (the-box (cg::make-box lhs ths rhs bhs)))
;      (let ((mp (cg::frame-child canvas)))
;             (cg::erase-box mp the-box)
;             (cg::erase-contents-box mp the-box)))
;        )
;         ((cg::positionp right)
;          ;;; left *is* left, top *is* top, right *is* position(bottom-right)
;          (let* ((lx left) (rx (cg::position-x right)) (ty top)
;                 (by (cg::position-y right))
;                 (mx (truncate (+ lx rx) 2))
;                 (my (truncate (+ ty by ) 2))
;                 (sh (truncate (- rx lx) 2))
;                 (sv (truncate (- by ty) 2))
;                 (lhs (- mx sh))
;                 (rhs (+ lhs (- rx lx)));
;                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
;                 (bhs (+ ths (- by ty)))
;                 (the-box (cg::make-box lhs ths rhs bhs)))
;     (let ((mp (cg::frame-child canvas)))
;             (cg::erase-box mp the-box)
;             (cg::erase-contents-box mp the-box)))  
;           )
;         (t 
;           ;;; all four arguments are what they say they are
;           (let* ((mx (truncate (+ left right) 2))
;                  (my (truncate (+ top bot ) 2))
;                  (sh (truncate (- right left) 2))
;                  (sv (truncate (- bot top) 2))
;                  (lhs (- mx sh))
;                  (rhs (+ lhs (- right left)))
;                  (ths (if (oddp (- bot top)) (- my sv -1) (- my sv))) ;<**
;                  (bhs (+ ths (- bot top)))
;                  (the-box (cg::make-box lhs ths rhs bhs)))
;      (let ((mp (cg::frame-child canvas)))
;              (cg::erase-box mp the-box)
;              (cg::erase-contents-box mp the-box)))
;         )
;   ))
;;; New version ends

;; there seems to be a need to separately erase r anf b sides as lines
#|
This mimics the old draw-inside-rectangle and is thus outmoded
(defun erase-rect (canvas left &optional top right bot)
   "Draws a specified box no longer worrying about ~
Inside but compatible with fast-macros."
  (let ((mp (clim-user::get-frame-pane canvas 'display)))
   (cond ((numberp bot)
          (let* ((mx (truncate (+ left right) 2))
                  (my (truncate (+ top bot ) 2))
                  (sh (truncate (- right left) 2))
                  (sv (truncate (- bot top) 2))
                  (lhs (- mx sh))
                  (rhs (+ lhs (- right left)))
                  (ths (if (oddp (- bot top)) (- my sv -1) (- my sv)))
                  (bhs (+ ths (- bot top))))
              (if (= 1 (- rhs lhs))
                  (clim-user::draw-point* mp lhs ths :ink (clim-user::medium-background mp))
                  (clim-user::draw-rectangle* mp lhs ths rhs bhs :ink (clim-user::medium-background mp)))
                (clim-user::draw-line* mp rhs ths rhs bhs :ink (clim-user::medium-background mp))
                (clim-user::draw-line* mp lhs bhs rhs bhs :ink (clim-user::medium-background mp))
             )
          )
         ((equal (type-of left) 'STANDARD-RECTANGLE)
          ;; left is then a box which must be picked apart
          (let* ((lx (clim-user::rectangle-min-x left)) (rx (clim-user::rectangle-max-x left))
                 (ty (clim-user::rectangle-min-y left)) (by (clim-user::rectangle-max-y left))
                 (mx (truncate (+ lx rx) 2))
                 (my (truncate (+ ty by ) 2))
                 (sh (truncate (- rx lx) 2))
                 (sv (truncate (- by ty) 2))
                 (lhs (- mx sh))
                 (rhs (+ lhs (- rx lx)))
                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
                 (bhs (+ ths (- by ty))))
             (if (= 1 (- rhs lhs))
                (clim-user::draw-point* mp lhs ths :ink (clim-user::medium-background mp))
                (clim-user::draw-rectangle* mp lhs ths rhs bhs :ink (clim-user::medium-background mp)))
                (clim-user::draw-line* mp rhs ths rhs bhs :ink (clim-user::medium-background mp))
                (clim-user::draw-line* mp lhs bhs rhs bhs :ink (clim-user::medium-background mp))
             )
          )
         ((equal (type-of top) 'STANDARD-POINT)
          ;;; top is a position and thus so is left. left is top-left  top is bottom-right
          (let* ((lx (clim-user::point-x left)) (rx (clim-user::point-x top))
                 (ty (clim-user::point-y left)) (by (clim-user::point-y top))
                 (mx (truncate (+ lx rx) 2))
                 (my (truncate (+ ty by ) 2))
                 (sh (truncate (- rx lx) 2))
                 (sv (truncate (- by ty) 2))
                 (lhs (- mx sh))
                 (rhs (+ lhs (- rx lx)))
                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
                 (bhs (+ ths (- by ty))))
             (if (= 1 (- rhs lhs))  ;;02nov99
                (clim-user::draw-point* mp lhs ths :ink (clim-user::medium-background mp))
                (clim-user::draw-rectangle* mp lhs ths rhs bhs :ink (clim-user::medium-background mp)))
                (clim-user::draw-line* mp rhs ths rhs bhs :ink (clim-user::medium-background mp))
                (clim-user::draw-line* mp lhs bhs rhs bhs :ink (clim-user::medium-background mp))
               )
         )           
         ((equal (type-of right) 'STANDARD-POINT)
          ;;; left *is* left, top *is* top, right *is* position(bottom-right)
          (let* ((lx left) (rx (clim-user::point-x right)) (ty top)
                 (by (clim-user::point-y right))
                 (mx (truncate (+ lx rx) 2))
                 (my (truncate (+ ty by ) 2))
                 (sh (truncate (- rx lx) 2))
                 (sv (truncate (- by ty) 2))
                 (lhs (- mx sh))
                 (rhs (+ lhs (- rx lx)))
                 (ths (if (oddp (- by ty)) (- my sv -1) (- my sv)))
                 (bhs (+ ths (- by ty))))
             (if (= 1 (- rhs lhs))
                (clim-user::draw-point* mp lhs ths rhs bhs :ink (clim-user::medium-background mp))
                (clim-user::draw-rectangle* mp lhs ths rhs bhs :ink (clim-user::medium-background mp)))
                (clim-user::draw-line* mp rhs ths rhs bhs :ink (clim-user::medium-background mp))
                (clim-user::draw-line* mp lhs bhs rhs bhs :ink (clim-user::medium-background mp))
                ))
)))
|#

#| outmoded
(defun erase-filled-rectangle (canvas left &optional top right bot)
     "Greg's try at draw-filled-rectangle"
     ;;; fill-box demands a box as well as a stream 
     ;;; So I have to invert the order of tests as I've done elsewhere
     ;;; in draw-ellipse
  (let ((mp (clim-user::get-frame-pane canvas 'display)))
     (if bot (if (oddp (- bot top))
        (clim-user::draw-rectangle* mp left (+ 1 top) right (+ 1 bot) :ink (clim-user::medium-background mp))
                (clim-user::draw-rectangle* mp left top right bot :ink (clim-user::medium-background mp)))
        (if right (clim-user::draw-rectangle mp (clim-user::make-point left top) right :ink (clim-user::medium-background mp))
           (if top (clim-user::draw-rectangle mp left top :ink (clim-user::medium-background mp))
              (clim-user::draw-rectangle mp (clim-user::rectangle-min-point left)
                (clim-user::rectangle-max-point left) :ink (clim-user::medium-background mp)))))))
|#

;(defun comp-color (canvas)
;     "Takes the complement of the current rgb-color triple of stream - returns this new triple"
;     (let*     ((mp (cg::frame-child canvas))
;               (current (cg::foreground-color mp)) 
;               ( new_red (logxor 255 (cg::rgb-red current)))
;               ( new_green (logxor 255 (cg::rgb-green current)))
;               ( new_blue (logxor 255  (cg::rgb-blue current)))
;               (newfgcol (cg::make-rgb :red new_red :green new_green
;                                :blue new_blue)))
;         newfgcol))   
;;;

;(defun invert-rectangle (canvas left &optional top right bot)
;     "A new version using a supplied CG macro"
;  (let ((mp (cg::frame-child canvas)))
;    (cg::with-paint-operation 
;        ;(mp cg::invert) 19oct05
;        (mp cg::po-invert) ;19oct05
;      ;;   (cg::with-foreground-color (canvas (comp-color  canvas))
;      (if bot (cg::fill-box mp 
;                 (cg::make-box left top right bot))
;         (if right
;            (cg::fill-box mp 
;             (cg::make-box-from-corners
;              (cg::make-position left top) (cg::make-position
;                                                            (cg::position-y right)
;                                                            (cg::position-x right)
;                                                            )))
;            (if top
;               (cg::fill-box mp
;                (cg::make-box-from-corners 
;                 (cg:make-position (cg::position-y left)
;                  (cg::position-x left)) (cg::make-position
;                                                    (cg::position-y top)
;                                                    (cg::position-x top))))
;               (cg::fill-box mp (cg::box-left left)
;                (cg::box-top left) (cg::box-right left)
;                (cg::box-bottom left)))
;            )
;         )
;      )
;     ))

#|
(defun draw-ellipse (canvas left &optional top right bot)
     ;;; aclpc uses quite a different set of parameters for ellipses
     ;;; centre, semi-major and minor axes, semi-major-axis-angle
     (cond ((cg::boxp left)
                ;;; left is then a box which must be picked apart
                (let* ((lx (cg::box-left left)) (rx (cg::box-right left))
                         (ty (cg::box-top left)) (by (cg::box-bottom left))
                         (centre (cg::make-position (/ (+ lx rx) 2) (/ (+ ty by) 2)))
                      ; (majorhalf (- rx lx)) (minorhalf (- by ty)))
                         (majorhalf (/ (- rx lx) 2)) (minorhalf (/ (- by ty) 2))                           ;; took out halves 10Nov99 
                      (mp (cg::frame-child canvas))
                      )
                    (cg::draw-ellipse mp centre majorhalf minorhalf 0))
              )
               ((cg::positionp top)
                ;;; top is a position and thus so is left
                ;;; left is top-left while top is bottom-right
                (let* ((lx (cg::position-x left)) (rx (cg::position-x top))
                         (ty (cg::position-y left)) (by (cg::position-y top))
                         (centre (cg::make-position (/ (+ lx rx) 2) (/ (+ ty by) 2)))
                       ;(majorhalf (- rx lx)) (minorhalf (- by ty)))
                         (majorhalf (/ (- rx lx) 2)) (minorhalf (/ (- by ty) 2))                              ;; took out halves 10Nov99
                         (mp (cg::frame-child canvas))
                         )
                    (cg::draw-ellipse mp centre majorhalf minorhalf 0))
                )
               ((cg::positionp right)
                ;;; left *is* left, top *is* top, right is pos(bottom-right)
                (let* ((lx left) (rx (cg::position-x right)) (ty top)
                         (by (cg::position-y right))
                         (centre (cg::make-position (/ (+ lx rx) 2) (/ (+ ty by) 2)))
                      ; (majorhalf (- rx lx)) (minorhalf (- by ty)))
                         (majorhalf (/ (- rx lx) 2)) (minorhalf (/ (- by ty) 2))                               ;; took out halves 10Nov99
                         (mp (cg::frame-child canvas))
                        )
                    (cg::draw-ellipse mp centre majorhalf minorhalf 0))
                 ) 
               (t
                 ;;; all four arguments are what they say they are
                ;;; Check for single point 05jan00
                (cond ((or (eq 1 (- right left)) (eq right left))
                      (setf (cg:pixel-x-y canvas left top)
                              (cg:foreground-color canvas)))
                      (t
                 (let* ((lx left) (rx right) (ty top) (by bot)
                          (centre (cg::make-position (/ (+ lx rx) 2) (/ (+ ty by) 2)))
                       ; (majorhalf (- rx lx)) (minorhalf (- by ty)))
                          (majorhalf (/ (- rx lx) 2)) (minorhalf (/ (- by ty) 2))
                                   ;; took out halves 10Nov99
                          (mp (cg::frame-child canvas))
                        )
                     (cg::draw-ellipse mp centre majorhalf minorhalf 0))
                   )
                  ))
 ))
|#

#|
;;; Changed 10Nov99 to add the circumference to the interior.
(defun draw-filled-ellipse (canvas left &optional top right bot)
     ;;; aclpc uses quite a different set of parameters for ellipses
     ;;; centre, semi-major and minor axes, semi-major-axis-angle
     (cond ((cg::boxp left)
                ;;; left is then a box which must be picked apart
                (let* ((lx (cg::box-left left)) (rx (cg::box-right left))
                         (ty (cg::box-top left)) (by (cg::box-bottom left))
                         (centre (cg::make-position (/ (+ lx rx) 2) (/ (+ ty by) 2)))
                      ; (majorhalf (- rx lx)) (minorhalf (- by ty)))
                         (majorhalf (/ (- rx lx) 2)) (minorhalf (/ (- by ty) 2)) ;; took out halves 10Nov99
                         (mp (cg::frame-child canvas)))
                    (cg::fill-ellipse mp centre majorhalf minorhalf 0)
                   (cg::draw-ellipse mp centre majorhalf minorhalf 0)))
               ((cg::positionp top)
                ;;; top is a position and thus so is left
                ;;; left is top-left while top is bottom-right
                (let* ((lx (cg::position-x left)) (rx (cg::position-x top))
                         (ty (cg::position-y left)) (by (cg::position-y top))
                         (centre (cg::make-position (/ (+ lx rx) 2) (/ (+ ty by) 2)))
                       ;(majorhalf (- rx lx)) (minorhalf (- by ty)))
                         (majorhalf (/ (- rx lx) 2)) (minorhalf (/ (- by ty) 2)) ;; took out halves 10Nov99
                       (mp (cg::frame-child canvas)))
                    (cg::fill-ellipse mp centre majorhalf minorhalf 0)
                   (cg::draw-ellipse mp centre majorhalf minorhalf 0)))
               ((cg::positionp right)
                ;;; left *is* left, top *is* top, right is pos(bottom-right)
                (let* ((lx left) (rx (cg::position-x right)) (ty top)
                         (by (cg::position-y right))
                         (centre (cg::make-position (/ (+ lx rx) 2) (/ (+ ty by) 2)))
                      ; (majorhalf (- rx lx)) (minorhalf (- by ty)))
                         (majorhalf (/ (- rx lx) 2)) (minorhalf (/ (- by ty) 2)) ;; took out halves 10Nov99
                        (mp (cg::frame-child canvas)))
                    (cg::fill-ellipse mp centre majorhalf minorhalf 0)
                   (cg::draw-ellipse mp centre majorhalf minorhalf 0)))
               (t
                 ;;; all four arguments are what they say they are
                ;;; Check for single point 05jan00
                (cond ((or (eq 1 (- right left)) (eq right left))
                      (let ((mp (cg::frame-child canvas)))
                      (setf (cg:pixel-x-y mp left top)
                              (cg:foreground-color mp))))
                      (t
                 (let* ((lx left) (rx right) (ty top) (by bot)
                          (centre (cg::make-position (/ (+ lx rx) 2) (/ (+ ty by) 2)))
                       ; (majorhalf (- rx lx)) (minorhalf (- by ty)))
                          (majorhalf (/ (- rx lx) 2)) (minorhalf (/ (- by ty) 2)) ;; took out halves 10Nov99
                          (mp (cg::frame-child canvas)))
                     (cg::fill-ellipse mp centre majorhalf minorhalf 0)
                    (cg::draw-ellipse mp centre majorhalf minorhalf 0)))
           ))))
|#


#|
(defun draw-arc (canvas start-angle arc-angle x-centre y-centre x-radius y-radius)
     ;;; Draws a SECTOR of an ellipse which includes drawing the radii
     (let ( (lengthangle (+ start-angle arc-angle)) 
            (centre (cg::make-position x-centre y-centre))
            (mp (cg::frame-child canvas)))
         (cg::draw-ellipse-sector mp centre x-radius y-radius 0 start-angle lengthangle)))
|#

#|
(defun fill-arc (canvas start-angle arc-angle 
                       x-centre y-centre x-radius y-radius)
     ;;; Draws a filled SECTOR of an ellipse including drawing the radii
     (let ( (lengthangle (+ start-angle arc-angle)) 
            (centre (cg::make-position x-centre y-centre))
            (mp (cg::frame-child canvas)))
         (cg::fill-ellipse-sector mp centre x-radius y-radius 0 start-angle lengthangle)))
|#

#|
(defun erase-arc (canvas start-angle arc-angle 
                            x-centre y-centre x-radius y-radius)
     ;;; Erases the SECTOR drawn by draw-arc 
     ;;; and then erases the contents, if any
     (let ( (lengthangle (+ start-angle arc-angle)) 
            (centre (cg::make-position x-centre y-centre))
            (mp (cg::frame-child canvas)))
         (cg::erase-ellipse-sector mp centre x-radius y-radius 0 start-angle lengthangle)
         (cg::erase-contents-ellipse-sector mp centre x-radius y-radius 0 start-angle lengthangle))
  )
|#

#|
(defun invert-arc (canvas start-angle arc-angle
                             x-centre y-centre x-radius y-radius)
     "A new version using supplied CG:: macro"
      (let ((lengthangle (+ start-angle arc-angle))
             (centre (cg::make-position x-centre y-centre))
            (mp (cg::frame-child canvas)))
     (cg::with-foreground-color (mp (comp-color canvas))
          (cg::fill-ellipse-sector mp centre x-radius y-radius 0 start-angle lengthangle)))   )
|#

#|
(defun draw-polygon (canvas list-of-points)
   (let*  ((mp (cg::frame-child canvas))
          (pts list-of-points)
          (start (first pts))
          (first-x (car start))
          (first-y (cdr start)))
      ;; Check for size = 1 right away for draw-polygon doe NOT default.
      (cond ((equal (first pts) (second pts))
             (setf (cg:pixel-x-y mp first-x first-y)
                   (cg:foreground-color mp))) ;; these 3 lines 07jan00
            ((and (= 4 (length pts)) (equal (first pts) (second pts)))
            (cg::draw-polygon mp 
              (list 
              (cg::make-position (- first-x 1) first-y)
              (cg::make-position first-x (+ 1 first-y))
              (cg::make-position (+ 1 first-x) first-y)
              (cg::make-position first-x (- first-y 1))
              (cg::make-position (- first-x 1) first-y))))
           ((and (= 3 (length pts))(equal (first pts) (second pts)))
            (cg::draw-polygonmp 
              (list
               (cg::make-position (- first-x 1) (+ first-y 1))
               (cg::make-position first-x (- first-y 1))
               (cg::make-position (+ first-x 1) (+ first-y 1))
               (cg::make-position (- first-x 1) (+ first-y 1)))))
      (T
       (cg::draw-polygon mp 
         (mapcar #'(lambda (x)
                     (cg::make-position 
                       (car x) (cdr x)))
           pts)))
      )))
|#

(defun draw-filled-polygon (canvas list-of-points)
   (let ((mp (clim-user::get-frame-pane canvas 'display)))
    (clim-user::draw-polygon mp list-of-points)
    ))
          (pts list-of-points)
          (start (first pts))
          (first-x (car start))
          (first-y (cdr start)))
      ;;; Check for size = 1 off the top - see draw-polygon itself.
      (cond ((equal (first pts) (second pts))
             (setf (cg:pixel-x-y mp first-x first-y)
                   (cg:foreground-color mp))) ;; these 3 lines 07jan00
            ((equal (first pts) (second pts))
             (let ((vert 
            ;;(cg::fill-polygon canvas
              (list 
              (cg::make-position (- first-x 1) first-y)
              (cg::make-position first-x (+ 1 first-y))
              (cg::make-position (+ 1 first-x) first-y)
              (cg::make-position first-x (- first-y 1))
              (cg::make-position (- first-x 1) first-y))))
                (cg::fill-polygon mp vert)
                (cg::draw-polygon mp vert)))
            ((and (= 3 (length pts))(equal (first pts) (second pts)))
             (let ((vert 
           ; (cg::fill-polygonmp 
              (list
               (cg::make-position (- first-x 1) (+ first-y 1))
               (cg::make-position first-x (- first-y 1))
               (cg::make-position (+ first-x 1) (+ first-y 1))
               (cg::make-position (- first-x 1) (+ first-y 1)))))
                (cg::fill-polygon mp vert)
                (cg::draw-polygon mp vert)))
      (T
       (let ((vert
       ;(cg::fill-polygon canvas 
         (mapcar #'(lambda (x)
                     (cg::make-position 
                       (car x) (cdr x)))
           pts)))
       (cg::draw-polygon mp vert)
          (cg::fill-polygon mp vert)))
       )
      ))


#|
(defun draw-filled-polygon (canvas list-of-points)
   (cg::fill-polygon canvas
    (mapcar #'(lambda (x)
                (cg::make-position
                 (car x) (cdr x)))
      list-of-points)))
|#

#|
(defun draw-string (canvas string)
   "Since acl draws DOWN, we have to move up first and then 
     draw a string on the  canvas at the NEW current position with
     the current font and colour."
   (let* ((mp (cg::frame-child canvas))
         (y-shift (cg::make-position 0 (- (cg::font-height
                                           (cg::fontmetrics mp)))))
         (current (cg::transparent-character-background mp))) ;N1
   (cg::move-to mp (cg::position+ (cg::current-position mp)
                        y-shift))
      (setf (cg::transparent-character-background mp) :transparent) ;N2
     (princ string mp)
      (setf (cg::transparent-character-background mp) current) ;N3
         (cg::move-to mp (cg::position- (cg::current-position mp)
                        y-shift))
      ))
|#