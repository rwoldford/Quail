;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        host-draw-sblx.lsp
;;;  for mcclim                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;     G.W. Bennett 1995 2017.
;;;--------------------------------------------------------------------------------
;;; This contains checked pieces of host-draw and accumulates routines as they are done
 (in-package "HOST-DRAW")
;;; MCCLIM note 25JULY2020
;;; Most of these functions refer to canvas, which is an application-frame,
;;; whereas the stream needed is its pane of type 'wb::host-pane 
;;; which is defined in the wb package in host-window-sblx!!

;;; Moved the (shadw.) contents to window-basics-package.lsp 01SEP2021
 ;(shadow '(make-point point-x point-y draw-line draw-rectangle draw-ellipse draw-polygon))

(eval-when (:compile-toplevel :load-toplevel :execute)  (export '(make-point)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)  (export '(point-x point-y *hardcopy-ptr* pen-show pen-hide pen-shown-p 
  pen-position pen-size set-pen-size pen-mode set-pen-mode pen-pattern 
  set-pen-pattern set-pen-color move-to move line-to line draw-line
          draw-rectangle draw-filled-rectangle draw-inside-rectangle
          erase-rect invert-rectangle draw-ellipse draw-filled-ellipse draw-arc 
          fill-arc erase-arc invert-arc fill-arc draw-polygon 
          make-bitmap copy-bits
  draw-filled-polygon make-bitmap copy-bits origin set-origin 
  draw-string draw-char)
        ))

 (defun is-position (arg)
   (eql (type-of arg) 'CLIM:STANDARD-POINT))       
        
(defun make-point (x &optional y)
  "Returns a point having x and y as its coordinates."
  (if y
    (clim-user::make-point x y)
    (if (is-position x)  ;; There seems to be no positionp
      x
      (error "X (~s) is not a position or y missing" x)
      )))

;; point-x point-y will have to be shadowed
(defun point-x (point)
  "Returns the x-coordinate of point."
  (clim-user::point-x point))


(defun point-y (point)
  "Returns the y-coordinate of point."
  (clim-user::point-y point))


(defvar *hardcopy-ptr*
   NIL
   "A system dependent pointer to an open printer device.")

;; The effect pen-show and pen-hide and pen-shown-p would have
;; to depend on things like +transparent-ink+. 
(defun pen-show (canvas)
  (declare (ignore canvas))
     nil)

(defun pen-hide (canvas)
  (declare (ignore canvas))
     nil)

(defun pen-shown-p (canvas)
  (declare (ignore canvas))
    nil)


;; There is clim-user::pointer-position which takes pointer as an argument.
;; but its stream-cursor-position I need.

(defun pen-position (canvas)
 (let ((mp (clim-user::get-frame-pane canvas 'wb::host-pane)))
  (multiple-value-bind (x y)
    (clim-user::stream-cursor-position mp)
    (make-point x y))))
    
  ;(declare (ignore canvas))
  ;NIL
  ;)

;; width for lines comes from a :thickness argument when they are drawn
;; there is a generic fn  line-style-thickness with arg  line-style.
;; There is make-line-style, but I don't yet see how to get the current
;; value of thickness associated with a pane.

;; Aha! I do now. Look under medium- things!!

;; There is, for example, (medium-line-style *test-pane*) which returns
;; a standard-line-style [P 62] and then I can ask for
;; (line-style-thickness (m-l-s *t-p*)) -> 1

;; Then there is (medium-foreground/background *t-p*) and so on.


(defun pen-size (canvas)
  (let* ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
    (its-styles (clim-user::medium-line-style mp)))
    (clim-user::line-style-thickness its-styles)
    ))

;; pen-size <-> line-thickness is part of (medium-line-style some-pane)
;; which is immutable (!). Thus we cannot write to it directly.
;; We have to copy the elements of the current one, replacing the
;; thickness we want.

;; When there is no v, h must be a point.

(defun set-pen-size (canvas h &optional v)
  #-:sbcl(declare (inline point-x point-y))
  (format t "~%Just inside set-pen-size")
  (format t "~%s-p-s h is ~s " h)
  (format t "~%s-p-s v is ~s " v)
  (format t "~%s-p-s canvas is ~s " canvas)
  (format t "~%s-p-s is it a frame ? ~s " (clim:application-frame-p canvas))
  (format t "~%s-p-s its host-pane pane is ~s " (clim:get-frame-pane canvas 'host-pane))
  (let* ((its-pane (clim::get-frame-pane canvas 'host-pane)) ;,_ from 'wb::host-pane 09FE21
      (its-line-style (clim:medium-line-style its-pane))
    (its-unit (clim:line-style-unit its-line-style))
    (its-dashes (clim:line-style-dashes its-line-style))
    (its-joint-shape (clim:line-style-joint-shape its-line-style))
    (its-cap-shape (clim:line-style-cap-shape its-line-style))
    (new-thickness (if v (floor (+ h v) 2)
      (floor (+ (point-x h) (point-y h)) 2)))
    (new-style (clim:make-line-style :unit its-unit :thickness new-thickness :dashes its-dashes
      :joint-shape its-joint-shape :cap-shape its-cap-shape))
    )
    (format t "~%s-p-s In set-pen-size after defvar new-thickness check current values BEFORE make-line-style")
    (format t "~%s-p-s input canvas is ~s " canvas)
    (format t "~%s-p-s is canvas a frame ? ~s " (clim:application-frame-p canvas))

    (format t "~%s-p-s input h is ~s " h)
    (format t "~%s-p-s input v is ~s " v)
    (format t "~%s-p-s its-pane is ~s " its-pane)
    (format t "~%s-p-s its-line-style is ~s " its-line-style)
    (format t "~%s-p-s its-unit is ~s " its-unit)
    (format t "~%is-p-s ts-dashes is ~s " its-dashes)
    (format t "~%s-p-s its-joint-shape is ~s " its-joint-shape)
    (format t "~%s-p-s its-cap-shape is ~s " its-cap-shape)
    (format t "~%s-p-s new-thickness is ~s " new-thickness)
    (format t "~%s-p-s new-style is ~s " new-style)
  (setf (clim:medium-line-style its-pane) new-style)
  )
  )

;; Need to find out what paint-operation is and 
;; therefore what  clim calls it
(defun pen-mode (canvas)
  (declare (ignore canvas))
  )


(defun set-pen-mode (canvas new-mode)
  "There does not seem to be any analogue of the mode/operation in CLIM"
  (declare (ignore canvas new-mode))
  )

(defun set-pen-pattern (canvas new-pattern)
  "Sets the drawing color of canvas to (Q)new-color"
  (let ((mp (clim::get-frame-pane canvas 'wb::host-pane)))
    (setf (clim:medium-foreground mp) new-pattern))
    )


;; drawing in color under clim is done via he :ink option
;; to whichever drawing function is being invoked, so it's tied to
;; the drawing function rather than to the pane. But foreground and
;; background colors can be changed at any time (says P83). How
;; to do this ? 
;; Apparently by using (setf (medium-foreground something)) P58

(defun set-pen-color (canvas new-color)
  "Sets the drawing color of canvas to (Q)new-color"
  (let ((mp (clim::get-frame-pane canvas 'wb::host-pane)))
    (format t "~% Just inside h-draw:set-pen-color")
    (format t "~%s-p-c new-color is ~s " new-color)
    (format t "~%s-p-c canvas is ~s " canvas)
    (format t "~%s-p-c is canvas a frame ? ~s " (clim:application-frame-p canvas))
    (format t "~%s-p-c does canvas have a host-pane ~s " (clim:get-frame-pane canvas 'host-pane))
    (setf (clim:medium-foreground mp)  new-color)))

;; move-to will depend on setting the position of the mouse.
;; Said mouse is an instance of a clim pointer (p283). 
;; Pointers are associated with clim ports (p283).
;; I find this via (clim::user port *some-pane*) (p353-4). Thus the
;; current position is 
;;; (clim-user::pointer-position (clim-user::port-point
;;;  (clim-user::port *some-pane*))).
;;; This is setfable (p283). The functions pointer-set-position [and friends]
;; are not available!
;; But really it is the position of the cursor which I need to set (P329+).


(defun move-to (canvas h &optional v)
  (let ((mp (clim-user::get-frame-pane canvas 'wb::host-pane)))
     (if v
      (clim-user::stream-set-cursor-position mp h v)
      (clim-user::stream-set-cursor-position mp 
        (clim-user::point-x h) (clim-user::point-y h))
        )))

;; and there is stream-increment-cursor-position which does just that!

(defun move (canvas h &optional v)
  (let ((mp (clim-user::get-frame-pane canvas 'wb::host-pane)))
     (if v
      (clim-user::stream-increment-cursor-position mp h v)
      (clim-user::stream-increment-cursor-position mp (clim-user::point-x h)
        (clim-user::point-y h))
        )))

;; Presumably line-to draws from the current cursor position
(defun line-to (canvas h &optional v)
  (let* ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
    (its-curs-pos (multiple-value-bind (p q) (clim-user::stream-cursor-position mp)
      (make-point p q)
    )))
  (clim-user::draw-line mp its-curs-pos (make-point h v))
     ))


(defun line (canvas h &optional v)
  (let* ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
    (its-curs-pos (multiple-value-bind (p q) (clim-user::stream-cursor-position mp)
      (make-point p q)))
    (cp-x (point-x its-curs-pos))
    (cp-y (point-y its-curs-pos)))
  (if v 
    (clim-user::draw-line* mp cp-x cp-y (+ cp-x h) (+ cp-y v))
    (clim-user::draw-line* mp cp-x cp-y (+ cp-x (point-x h)) (+ cp-y (point-y h)))
    )))


;; draw-line will have to be shadowed too.
(defun draw-line (canvas x1 y1 x2 y2)
   (move-to canvas x1 y1)
   (line-to canvas x2 y2)
   (move-to canvas (make-point x2 y2)) ;; to expected end for next draw
     )

;; draw-rectangle will have to be shadowed
;; (p33.) By default draw-rectangle(*) has :filled t

(defun draw-rectangle (canvas left top right bot)
  (let ((mp (clim-user::get-frame-pane canvas 'wb::host-pane)))
    (clim-user::draw-rectangle* mp left top right bot :filled NIL)
     ))

;; There seems to be no rectangle-p in clim-user.
;; (setf rect (make-rectangle p1 p2)) - where p1 and p2 are points - works
;; (type-of rect) -> the symbol STANDARD-RECTANGLE
;; so (equal (type-of zot) 'STANDARD-RECTANGLE) shuld do
;; likewise for cg::position-p -> type 'STANDARD-POINT

;; An auxiliary form to unpack various rectangle inputs
(defun unpack-rectangle-args (canvas left &optional top right bottom)
  (declare (ignore canvas))
  (cond ((numberp bottom)
           (list left top right bottom))
         ((equal (type-of right) 'CLIM:STANDARD-POINT)
           (list left top (clim-user::point-x right) (clim-user::point-y right)))
         ((equal (type-of top) 'CLIM:STANDARD-POINT)
           (list (clim-user::point-x left) (clim-user::point-y left) (clim-user::point-x top) (clim-user::point-y top)))
         ((equal (type-of left) 'CLIM:STANDARD-RECTANGLE)
           (list (clim-user::rectangle-min-x left) (clim-user::rectangle-min-y left)
                 (clim-user::rectangle-max-x left) (clim-user::rectangle-max-y left)))
         ))

;; Now we can check the type of rectangle and draw appropriately
(defun draw-inside-rectangle (canvas left &optional top right bot)
  "Draws the (unfilled) rectangle left+1 top+1 right-1 bot-1 if that is not degenerate,
  ~a horizontal/vertical line if appropriate, or nothing."
  (let* ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
         (ltrb (unpack-rectangle-args canvas left top right bot))
         (lx (first ltrb))
         (tx (second ltrb))
         (rx (third ltrb))
         (bx (fourth ltrb))
         )
         (if (or (> (1+ lx) (1- rx)) (> (1+ tx) (1- bx))) 
              NIL
              (if (eq (1+ lx) (1- rx))
                (clim-user::draw-line* mp (1+ lx) tx (1+ lx) bx)
              (if (eq (1+ tx) (1- bx))
                (clim-user::draw-line* mp lx (1+ tx) rx (1+ tx))
                (clim-user::draw-rectangle* mp (1+ lx) (1+ tx) (1- rx) (1- bx) :filled NIL))
              ))
    ))

;; clim-user::draw-rectangle* uses :filled T as its default
;; the following seems to work

(defun draw-filled-rectangle (canvas left &optional top right bot)
  "Draws the filled rectangle left+1 top+1 right-1 bot-1 if that is not degenerate,
  ~a horizontal/vertical line if appropriate, or nothing."
  (let* ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
         (ltrb (unpack-rectangle-args canvas left top right bot))
         (lx (first ltrb))
         (tx (second ltrb))
         (rx (third ltrb))
         (bx (fourth ltrb))
         )
  ;(format t "~% mp is ~s " mp)
  ;(format t "~% lx is ~d " lx)
  ;(format t "~% tx is ~d " tx)
  ;(format t "~% rx is ~d " rx)
  ;(format t "~% bx is ~d " bx)
         (if (or (> (1+ lx) (1- rx)) (> (1+ tx) (1- bx))) 
              NIL
              (if (eq (1+ lx) (1- rx))
                (clim-user::draw-line* mp (1+ lx) tx (1+ lx) bx)
              (if (eq (1+ tx) (1- bx))
                (clim-user::draw-line* mp lx (1+ tx) rx (1+ tx))
                (clim-user::draw-rectangle* mp (1+ lx) (1+ tx) (1- rx) (1- bx)))
              ))
    ))



;; mcclim erases by drawing with the background color explicitly, it seems.

(defun erase-rect (canvas left &optional top right bot)
  (let* ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
         (ltrb (unpack-rectangle-args canvas left top right bot))
         (lx (first ltrb))
         (tx (second ltrb))
         (rx (third ltrb))
         (bx (fourth ltrb))
         )
         (if (or (> (1+ lx) (1- rx)) (> (1+ tx) (1- bx))) 
              NIL
              (if (eq (1+ lx) (1- rx))
                (clim-user::draw-line* mp lx tx lx bx :ink (clim-user::medium-background mp))
              (if (eq (1+ tx) (1- bx))
                (clim-user::draw-line* mp lx tx rx tx :ink (clim-user::medium-background mp))
                (clim-user::draw-rectangle* mp lx tx rx bx :filled NIL :ink (clim-user::medium-background mp)))
                ;(clim-user::draw-rectangle* mp (1+ lx) (1+ tx) (1- rx) (1- bx) :filled NIL :ink (clim-user::medium-background mp)))
              ))
    ))


(defun erase-filled-rectangle (canvas left &optional top right bot)
  "Draws the filled rectangle left top right bot if that is not degenerate,
  ~a horizontal/vertical line if appropriate, or nothing."
  (let* ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
         (ltrb (unpack-rectangle-args canvas left top right bot))
         (lx (first ltrb))
         (tx (second ltrb))
         (rx (third ltrb))
         (bx (fourth ltrb))
         )
         (if (or (> (1+ lx) (1- rx)) (> (1+ tx) (1- bx))) 
              NIL
              (if (eq (1+ lx) (1- rx))
                (clim-user::draw-line* mp (1+ lx) tx (1+ lx) bx :ink (clim-user::medium-background mp))
              (if (eq (1+ tx) (1- bx))
                (clim-user::draw-line* mp lx (1+ tx) rx (1+ tx) :ink (clim-user::medium-background mp))
                (clim-user::draw-rectangle* mp lx tx rx bx :ink (clim-user::medium-background mp)))
                ;(clim-user::draw-rectangle* mp (1+ lx) (1+ tx) (1- rx) (1- bx) :ink (clim-user::medium-background mp)))
              ))
    ))



;; Things to be sorted out here .. perhaps the return values from color-rgb need to the
;; *255 and rounded to integer ? Truncated more likely.
;;  (mapcar #'(lambda (x) (truncate x)) (mapcar #'(lambda (y) (* 255 y)) (multiple-value-list (color-rgb +yellow+))))
;; -> (255 255 0) seems to do the job.

(defun comp-color (canvas)
     "Takes the complement of the current rgb-color triple of stream - returns this new triple"
     (let*     ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
               (current (clim-user::medium-foreground mp))
               (its-rgb  (mapcar #'(lambda (x) (truncate x)) 
                (mapcar #'(lambda (y) (* 255 y)) (multiple-value-list (clim-user::color-rgb current)))))
               (new_red (/ (- 255 (first its-rgb)) 255))
               ( new_green (/ (- 255.0 (second its-rgb)) 255))
               ( new_blue (/ (- 255.0 (third its-rgb)) 255))
               (newfgcol (clim-user::make-rgb-color  new_red  new_green
                                 new_blue)))
         newfgcol))   
;;(comp-color *test-frame*) {whose foreground color is +black+} ->
;; #<NAMED-COLOR "white">

;; The following seems to work
;; using :ink rather than wrapping with cg::po-invert as the Allegro version does
;; NOTE:: This requires that the rectangle be drawn after something like
;; (setf (medium-foreground *some-color*)) since invert-rectangle takes the inverse
;; or complement of that foreground. If the drawing is not done this way, there is no way
;; to get the color with which the rectangle was drawn.
(defun invert-rectangle (canvas left &optional top right bot)
       "A mcclim version"
       (let* ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
         (ltrb (unpack-rectangle-args canvas left top right bot))
         (lx (first ltrb))
         (tx (second ltrb))
         (rx (third ltrb))
         (bx (fourth ltrb)))
         (clim-user::draw-rectangle* mp lx tx rx bx :ink (comp-color canvas))
        ))
#|         
         
         (if bot (clim-user::draw-rectangle* mp left top right bot :ink (comp-color canvas))
       (if right
           (clim-user::draw-rectangle mp (clim-user::make-point left top) right :ink (comp-color canvas))
           (if top
         (clim-user::draw-rectangle mp left top :ink (comp-color canvas))
         (clim-user::draw-rectangle* mp (clim-user::rectangle-min-x left)
                   (clim-user::rectangle-min-y left)
                   (clim-user::rectangle-max-x left)
                   (clim-user::rectangle-max-y left) :ink (comp-color canvas))
         )
           )
       )
         ))
|#

(defun draw-ellipse (canvas left &optional top right bot)
"Draws an unfillled ellipse, parallel to the axes, for which LTRB is the surrounding box."
 (let* ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
        (ltrb (unpack-rectangle-args canvas left top right bot))
        (cx (truncate (/ (+ (first ltrb) (third ltrb)) 2)))
        (cy (truncate (/ (+ (second ltrb) (fourth ltrb)) 2)))
        (semix (- (third ltrb) cx))
        (semiy (- (fourth ltrb) cy))
        )
  (if (and (> semix 0) (> semiy 0))
        (clim-user::draw-ellipse* mp cx cy semix 0 0 semiy :filled NIL)
        NIL)
    ))


(defun draw-filled-ellipse (canvas left &optional top right bot)
"Draws a fillled ellipse, parallel to the axes, for which LTRB is the surrounding box."
 (let* ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
        (ltrb (unpack-rectangle-args canvas left top right bot))
        (cx (truncate (/ (+ (first ltrb) (third ltrb)) 2)))
        (cy (truncate (/ (+ (second ltrb) (fourth ltrb)) 2)))
        (semix (- (third ltrb) cx))
        (semiy (- (fourth ltrb) cy))
        )
  (if (and (> semix 0) (> semiy 0))
        (clim-user::draw-ellipse* mp cx cy semix 0 0 semiy )
        NIL)
    ))


(defun draw-arc (canvas start-angle arc-angle x-centre y-centre x-radius y-radius)
     "Draws a SECTOR of an ellipse which includes drawing the radii"
     (let ((mp (clim-user::get-frame-pane canvas 'wb::host-pane)))
        (clim-user::draw-ellipse* mp x-centre y-centre x-radius 0 0 y-radius :start-angle start-angle :end-angle arc-angle :filled NIL)
        ))

(defun fill-arc (canvas start-angle arc-angle x-centre y-centre x-radius y-radius)
     "Draws a SECTOR of an ellipse which includes drawing the radii"
     (let ((mp (clim-user::get-frame-pane canvas 'wb::host-pane)))
        (clim-user::draw-ellipse* mp x-centre y-centre x-radius 0 0 y-radius :start-angle start-angle :end-angle arc-angle )
        ))


(defun erase-arc (canvas start-angle arc-angle x-centre y-centre x-radius y-radius)
     "Erases a SECTOR of an ellipse which includes the radii and the contents, if any"
     (let ((mp (clim-user::get-frame-pane canvas 'wb::host-pane)))
        (clim-user::draw-ellipse* mp x-centre y-centre x-radius 0 0 y-radius :start-angle start-angle :end-angle arc-angle 
          :filled NIL :ink (clim-user::medium-background mp))
        (clim-user::draw-ellipse* mp x-centre y-centre x-radius 0 0 y-radius :start-angle start-angle :end-angle arc-angle 
          :ink (clim-user::medium-background mp))
        ))


(defun invert-arc (canvas start-angle arc-angle
                             x-centre y-centre x-radius y-radius)
     "Inverts a filled arc"
      (let ((mp (clim-user::get-frame-pane canvas 'wb::host-pane)))
        (clim-user::draw-ellipse* mp x-centre y-centre x-radius 0 0 y-radius :start-angle start-angle :end-angle arc-angle
          :ink (comp-color canvas))
        ))
      
;; This needs to be shadowed
(defun draw-polygon (canvas list-of-points)
  "Draws an unfilled polygon defined by a list of x-y coordinates or a list of points"
   (let*  ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
           (head (car list-of-points)))
   (if (numberp head)
    (clim-user::draw-polygon* mp list-of-points :filled NIL)
    (if (equal (type-of head) 'CLIM:STANDARD-POINT)
      (clim-user::draw-polygon mp list-of-points :filled NIL))
    )))

(defun draw-filled-polygon (canvas list-of-points)
  "Draws a filled polygon defined by a list of x-y coordinates or a list of points"
   (let*  ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
           (head (car list-of-points)))
   (if (numberp head)
    (clim-user::draw-polygon* mp list-of-points)
    (if (equal (type-of head) 'CLIM:STANDARD-POINT)
      (clim-user::draw-polygon mp list-of-points)
      NIL))
    ))


(defun make-bitmap (left &optional top right bottom &aux rowbytes bm)
  (declare (ignore left top right bottom rowbytes bm))
   "This will make a bitmap one day. April 30, 1997  - gwb"
   NIL
   )

(defun copy-bits (source-bitmap dest-bitmap source-rect dest-rect 
                  &optional (mode 0) mask-region)
  (declare (ignore source-bitmap dest-bitmap source-rect dest-rect
     mode mask-region))
   "ACL surely contains the analogue of this. April 30, 1997 - gwb"
   NIL
   )


(defun kill-polygon (canvas polygon)
  "Erases a polygon+contents - input list of xi y1 coords, list of (xi,yi) points, CLIM polygon"
  (let ((mp (clim-user::get-frame-pane canvas 'wb::host-pane)))
    (cond ((and (listp polygon) (numberp (first polygon)))
        ;;; we have a long list of coordinates x1 y1 x2 y2 ...
        (clim-user::map-over-polygon-segments 
          #'(lambda (x1 y1 x2 y2) (clim-user::draw-line* mp x1 y1 x2 y2
                                                       :ink (clim-user::medium-background mp)))
          (clim-user::make-polygon* polygon))
        (clim-user::draw-polygon* mp polygon :ink (clim-user::medium-background mp)))
          ((equal (type-of polygon) 'CLIM:STANDARD-POLYGON)
           ;;; we have a clim polygon
           (clim-user::map-over-polygon-segments 
          #'(lambda (x1 y1 x2 y2) (clim-user::draw-line* mp x1 y1 x2 y2
                                                       :ink (clim-user::medium-background mp)))
           polygon)
           (clim-user::draw-polygon* mp (poly2crds polygon)
                                     :ink (clim-user::medium-background mp)))
          ((and (listp polygon) (equal (type-of (car polygon)) 'CLIM:STANDARD-POINT))
           ;; we have a list of vertices
           (clim-user::map-over-polygon-segments 
          #'(lambda (x1 y1 x2 y2) (clim-user::draw-line* mp x1 y1 x2 y2
                                                       :ink (clim-user::medium-background mp)))
          (clim-user::make-polygon polygon))
           (clim-user::draw-polygon mp polygon 
                                    :ink (clim-user::medium-background mp)))
           )
        )
    )

    

(defun poly2crds (polygon)
  "Argument is a CLIM polygon .. result is list of coordinates xi yi"
  (let* ((polypts (clim-user::polygon-points polygon))
         (zoo (list ())))
    (mapcar #'(lambda (pt)
               (setf zoo (push (clim-user::point-x pt) zoo))
               (setf zoo (push (clim-user::point-y pt) zoo)))
               polypts)
            (rest (reverse zoo))
               ))

#| Not neede - for the record!
(defun pts2crds (polygon)
  "Argument is list of vertices .. result is list of coordinates xi yi"
  (let ((zoo (list ())))
    (mapcar #'(lambda (pt)
               (setf zoo (push (clim-user::point-x pt) zoo))
               (setf zoo (push (clim-user::point-y pt) zoo)))
               polypts)
            (rest (reverse zoo))
    ))
|#

;;; At some point we should consider whether there is to be a *default-quail-text-style*
;;; and where it should be defined - here or down in fonts somewhere.


(defun draw-string (canvas string)
   "Draws a string at the current cursor position - a tex-style needs to be supplied. Cannot be 0 0"
   (let* ((mp (clim-user::get-frame-pane canvas 'wb::host-pane))
          (cur-pos (multiple-value-list (clim-user::stream-cursor-position mp)))
          (position (clim-user::make-point (first cur-pos) (second cur-pos))))
   ;(format t "~%mp is ~s " mp)
   ;(format t "~%current-position is ~s " current-position)
     (clim-user::draw-text mp string position :text-style (clim:make-text-style :fix :bold :large))
      ))

(defun draw-char (canvas char)
  "Uses h-draw:draw-string and thus draws at the current cursor position"
   (let ((mp (clim-user::get-frame-pane canvas 'wb::host-pane)))
     (draw-string mp  char)
         ))
