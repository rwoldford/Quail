;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               canvas-ops-sblx.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     G.W. Bennett 1996
;;;     R.W. Oldford 1996
;;;     
;;;----------------------------------------------------------------------------------
;;; Changed defsetf on canvas-title to use 
;;; cg::set-stream-title
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(;;size-changed-p
           canvas-visible-p
           at-top-p close-canvas hide-canvas show-canvas
           canvas-width canvas-height
           canvas-title canvas-set-title which-canvas move-canvas
           shape-canvas
           canvas-to-top canvas-at-top canvases )))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*current-canvas* *quail-canvases* revise-ccqc update-ccqc)))


 (defvar *current-canvas* NIL)

  (defvar *quail-canvases*  '())

  (defun update-ccqc (canvas)
  "set *current-canvas* to be canvas, remove the canvas entry from *q-c,*~
  (cons (cons canvas (frame-state canvas)) *q-c*"
  (setf *current-canvas* canvas)
  (setf *quail-canvases* (cons (cons canvas :ENABLED) ;(frame-state canvas))
    (remove-if #'(lambda (x) (eql (car x) canvas)) *quail-canvases*)))
  )

  (defun revise-ccqc (canvas)
  "set *current-canvas* to be the first :ENABLED car on *quail-canvases*~
  minus the entry for canvas itself [NIL if find-if fails - OK]~
  if there is a *current-canvas*,  set *quail-canvases* to be (update-ccqc *current-canvas*)~
  otherwise exit"
  (setf *current-canvas* (car (find-if #'(lambda (x) (eql (cdr x) :ENABLED)) 
    (remove-if #'(lambda (y) (eql (car y) canvas)) *quail-canvases*))))
  (if *current-canvas*
  (update-ccqc *current-canvas*) NIL)
  )


#| now in color/bw-canvas-sblx.lsp
 (define-presentation-action do-change-canvas
    (blank-area nil quail-color-canvas :gesture :select)
    (object)
  (with-application-frame (frame)
    (update-ccqc frame)
    ))
 |#

#| the next 9 methods need the canvas specialiser which is not available
   until canvas/canvas.lsp has run which is AFTER this file
   They have been moved to a new file canvas  canvas/color-canvas-ops-sblx.lsp
   on 28AUG2021. This file will appear AFTER color-canvas-sblx.lsp in the
   canvas module of wb's .asd file

 (defmethod enable-frame :after ((self canvas))
  "The :before method will have set the frame-state of self to :DISABLED~
  so now this part of the call to enable-frame will pop it back up. 
  Then update-ccqc."
  ;(format t "~%:after on enable-frame called.")
  (update-ccqc self)
  )

(defmethod enable-frame :before ((self canvas))
  "If self is not :ENABLED, disable-it so that the :after method can pop it back up."
  ;(format t "~%:before on enable-frame called.")
  (unless (eql (frame-state self) :ENABLED) (disable-frame self))
  )

(defmethod destroy-frame :after ((self canvas))
  "First reset the state of canvas on *quail-canvases* to be :DISOWNED - then revise-ccqc"
  (setf (cdr (find-if #'(lambda (x) (eql (car x) self)) *quail-canvases*)) :DISOWNED)
  (revise-ccqc self))


(defmethod raise-frame :after ((self canvas))
  (cond ((eql self *current-canvas*)
    NIL)
  ((not (eql :ENABLED (frame-state self)))
    NIL)
  (T (update-ccqc self))
  ))

(defmethod shrink-frame :after ((self canvas))
  "First reset the state of canvas on *quail-canvases* to be :SHRUNK - then revise-ccqc"
  (setf (cdr (find-if #'(lambda (x) (eql (car x) self)) *quail-canvases*)) :SHRUNK)
  (revise-ccqc self))


(defmethod move-sheet ((self canvas) (x fixnum) (y fixnum))
  )

(defmethod move-sheet ((self canvas) (x fixnum) (y fixnum))
  (move-sheet (frame-top-level-sheet self) x y)
  (update-ccqc self))


(defmethod resize-sheet ((self canvas) (x fixnum) (y fixnum))
  ;(format t "~%Entering do-nothing primary on resize-sheet")
  )


(defmethod resize-sheet ((self canvas) (w fixnum) (h fixnum))
  (resize-sheet (frame-top-level-sheet self) w h)
  (update-ccqc self))
|#


#|
(defun size-changed-p (display)                           ; cbh
  ;; for a particular kind of display, redefine this
  (declare (ignore display)) T)
|#

;;; see canvases below
(defun at-top-p (canvas)
  "Test whether the canvas is the top-most in the display."
   (and (canvas-p canvas) (eql canvas (caar *quail-canvases*))
    )
   )

; destroy-frame
(defun close-canvas (canvas)
  "Close the canvas if it's open."
  (clim::destroy-frame canvas))

;; need to setf state to be :shrunk NOPE just shrink-frame
(defun hide-canvas (canvas)
  "Hides the canvas from display."
  (clim::shrink-frame canvas))

;; raise-frame brings it to the top of the list but does not actually show it   
(defun show-canvas (canvas)
  "Displays the canvas."
  (clim::enable-frame canvas)
   )
 
 #|
 ; defined already in monitor/screen-sblx.lsp 09MAR2022 gwb 
;; the outside of canvas  
(defun exterior-frame-rect (canvas)
  "Returns the exterior rectangle of canvas ~
  in screen coordinates x L -> R, y T -> B."
       (transform-region (sheet-delta-transformation (frame-top-level-sheet canvas) (graft canvas))
                          (sheet-region (frame-top-level-sheet canvas))))
|#                          

(defun canvas-width  (canvas)
  "Returns the display width of the canvas (in pixels)."
  (clim::rectangle-width (exterior-frame-rect canvas)))
  ;(- (cg::visible-box-width canvas) (cg::vertical-scrollbar-thickness))) ;;05may06

(defun canvas-height (canvas)
  "Returns the display height of the canvas (in pixels)."
  (clim::rectangle-height (exterior-frame-rect  canvas)))
  ;(- (cg::visible-box-height canvas) (cg::horizontal-scrollbar-thickness))) ;;05may06

;;;============================================================
;;; Canvas properties
;;;============================================================
;;; assuming  to the front  means to the top of some list
;;; raise-frame should do
(defun canvas-to-top (canvas)
  "Brings the canvas to the front of all other windows."
  (clim::raise-frame canvas))

;;; the value of *application-frame* shoud do - can't use this, only available inside a frame
(defun canvas-at-top ()
  "Returns the canvas at the front of all other canvases."
  (first (canvases)))

;;; don't yet know how to get this
;;; Aha! see clim-spec under frammanager
;;; (frame-manager-frames (frame-manager *quail-menubar-window*))
;;; returns a list of frames
;;; (eql (car *this*) *application-frame*)
;;; returns T
(defun canvases ()
  "Returns a list of all existing canvases."*quail-canvases*)

(defun which-canvas (screen-position)
  "Returns the canvas at the screen-position, if it exists."
  ;;loop over all windows, find the frontmost one containing position
  ;; return it if it is a canvas
  (find-if #'(lambda (w)
    (clim::region-contains-position-p 
      (clim::sheet-region (clim::frame-top-level-sheet w))
      (h-draw::point-x screen-position) (h-draw::point-y screen-position)))
  (canvases)))


(defun canvas-title (canvas)
  (clim::frame-pretty-name canvas))
   
;;; Changed 18 Feb 1998 from (cg::set-stream-title to (setf (cg::stream-title
(defsetf canvas-title (canvas) (new-value)
  `(let ((my-new-value ,new-value))
      (setf (clim::frame-pretty-name ,canvas) my-new-value)
     )
   )


(defun canvas-set-title (c title)
   "Sets the canvas title to be the value of the second argument ~
    title must be a string."
   (setf (clim::frame-pretty-name c) title))

(defun move-canvas (canvas x &optional y)
     "Reposition the canvas to the position (x y) in screen coordinates.  ~
   If only one argument is given then it is assumed to be a position."
   (let ((ftls (frame-top-level-sheet canvas)))
     (cond
               ((integerp y)
                (move-sheet ftls x y)
                )
               (T
                (move-sheet ftls (h-draw::point-x x) (h-draw::point-y x))
               ))
     ))

(defun shape-canvas (canvas width &optional height)
     "Resize a canvas to have width and height as given.  If the optional ~
argument height is not supplied, then it is assumed that the first argument  ~
is either a position specifying the width and height as its x and y coordinates, ~
or a region whose width and height specify the corresponding attributes of the canvas."
(let ((ftls (frame-top-level-sheet canvas)))
     (cond
               (height
                (clim::resize-sheet ftls width height)
                )
               ((position-p width)
                (clim::resize-sheet ftls (h-draw::point-x width) (h-draw::point-y width)))
               ((region-p width)
               (clim::resize-sheet ftls (region-width width) (region-height width)))
               (T nil)
               )))

;===================================================
; canvas-visible-p things
; mcclim does not seem to have facilities for accessing the z-order of its frames
; and all its region-X functions are with region coords rather than screen coords
; hence much of what follows
; gwb august 2021
;===================================================

(defun canvas-in-screen-coords (canvas &optional (v-kludge 30))
  "Returns the clim bounding rectangle of the exterior of canvas ~
  in *screen* units. ~
  mcclim does not know about the 'title-bar' and so the bounding rectangle ~
  returned has v-min too big by its height .. hence v-kludge."
  (declare (optimize (speed 3)))
  (let ((basic-region
  (transform-region (clim::sheet-delta-transformation (clim::frame-top-level-sheet canvas)
    (graft (clim::frame-top-level-sheet canvas)))
    (sheet-region (clim::frame-top-level-sheet canvas)))))
    (list (clim::bounding-rectangle-min-x basic-region)
          (clim::bounding-rectangle-max-x basic-region)
          (- (clim::bounding-rectangle-min-y basic-region) v-kludge)
          (clim::bounding-rectangle-max-y basic-region))))

(defun screen-point-on-canvas (canvas h v)
  "Determines whether the screen point pt h v is in or on the boundary of canvas"
  (declare (fixnum h v) (optimize (speed 3)))
  (let* ((canvas-exterior (canvas-in-screen-coords canvas ))
    (h-min (first canvas-exterior))
    (h-max (second canvas-exterior))
    (v-min (third canvas-exterior))
    (v-max (fourth canvas-exterior)))
  (if (and (>= h h-min) (<= h h-max) (>= v v-min) (<= v v-max)) T nil)
  )
  )

(defun range-covered-p (list-of-canvases h-min h-max v-min v-max)
  (declare (optimize (speed 3)))
  (let ((h-range (make-list-from-ends h-min h-max))
        (v-range (make-list-from-ends v-min v-max)))
  (if (every #'(lambda (v)
    (every #'(lambda (h)
    (some #'(lambda (c)
    (screen-point-on-canvas c h v)) list-of-canvases))
      h-range))
      v-range)
  T
  NIL
  )
  ))

(defun revise-canvas-list (canvas list-of-cons-canvas-and-state)
  "Returns a new list of just the canvas components which are (1) not [the arg] ~
  nor (2) have cdr not equal to :ENABLED"
  (let ((new-list '()))
    (setf new-list (mapcar #'(lambda (y) (car y))
      (remove-if #'(lambda (x)
      (or  (eql (car x) canvas)  (not (eql (cdr x) :ENABLED))))
    list-of-cons-canvas-and-state)))
    new-list))
;;;
;;; I need to make an integer list including from l-min l0max

(defun make-list-from-ends (l-min l-max)
  "Returns an integer list including from l-max to l-min"
  (declare (fixnum l-min l-max) (optimize (speed 3)))
  (let ((result '()))
    (do ((i l-min (1+ i)))
      ((> i l-max))
      (setf result (cons i result)))
    result
  ))

(defun canvas-visible-p (canvas)
  "Determines whether some part of canvas is not covered by any other canvas"
  (cond ((eql canvas *current-canvas*) T)
        ((not (eql (frame-state canvas) :ENABLED)) NIL)
        (T
          (let* ((screen-canvas (canvas-in-screen-coords canvas))
                (ch-min (first screen-canvas))
                (ch-max (second screen-canvas))
                (cv-min (third screen-canvas))
                (cv-max (fourth screen-canvas))
                (updated-canvas-list (revise-canvas-list canvas *quail-canvases*)))
          (not (range-covered-p updated-canvas-list ch-min ch-max cv-min cv-max))))))
;;;


