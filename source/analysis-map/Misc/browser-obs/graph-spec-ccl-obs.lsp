;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains the functions and data structures which
;;; originally defined in the file having the same name without the
;;; "obs" (for obsolete) extension.  All code contained here has been
;;; superseded by other code.
;;;
;;; RWO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; This file contains the definitions of functions specific for Mac needed
;;; by GRAPH.
;;;

(eval-when (:execute :compile-toplevel)
  (require 'records)              ; system files needed
  (require 'traps))               ; graph-mac is not compatible with quickdraw

;;;
;;; package definitions
;;;

(in-package 'pcl)
(in-package 'specific)

(import 'pcl::object)      ; make specific::object be pcl::object to avoid
(shadow 'object)           ; a conflict with ccl:object when object is exported
                           ; from pcl in browser-pcl

(use-package 'ccl)         ; in package ccl are defined the records and traps
(use-package 'ccl 'graph 'window-basics)



(use-package 'specific 'graph)

(proclaim '(object-variable wptr))

;;; Not used. Depends now on window-basics ********* rwo
;;;
;;; Structure definition not defined in graph-var (specific to Mac)
;;;

(defstruct mouse-state
             position
             button)

;;;
;;;
;;; function definitions
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BITMAP FUNCTIONS  <<<<<<<<<<<<< Should be replaced by window-basics bitmap
;;;                                 funs **************** rwo
;;;
;;; ************
(defun make-bitmap (width height)

;;;
;;; make a new empty bitmap with given dimensions in pixels
;;;

 (let ((zero 0)
       (bm-object (make-bit-map))
        row-bytes bm)
    (with-rectangle-arg (r zero zero width height) ; the Y axis of the screen
                                                   ; is oriented from top to bottom
       (setf row-bytes (logand #xfffe
                               (+ 2 (ash (- (rref r rect.right) 1) -3))))
       (setf bm (_NewPtr :errchk
                  :d0 (+ 14 (* row-bytes (rref r rect.bottom)))
                  :a0))
       (rset bm bitmap.bounds r)
       (rset bm bitmap.rowbytes row-bytes)
       (rset bm bitmap.baseaddr (%inc-ptr bm 14)))
       (setf (bit-map-height bm-object) height)
       (setf (bit-map-width bm-object) width)
       (setf (bit-map-bitmap bm-object) bm)
    bm-object))


(defun bitmap-p (bitmap)

;;;
;;; returns T if bitmap is a bitmap
;;;

 (typep bitmap 'bit-map))


(defun bitmap-height (bitmap)

  (bit-map-height bitmap))


(defun bitmap-width (bitmap)

  (bit-map-width bitmap))

 
;;; moved to window-basics as penmode-arg
(defun mode-arg (thing)
   
  (or (and (fixnump thing)
           (<= 0 thing 15)
           thing)
      (position  thing *pen-modes*)
      (quail-error "Unknown pen mode : ~a" thing)))


(defun copy-bitmap-to-window-region (source-bitmap
                                     dest-window
                                     dest-left
                                     dest-top
                                     width
                                     height)

;;; sometimes, I don't know why, the slot bitmap of a bit-map becomes equal to
;;; nil. When this is the case, copybits doesn't work and we unexpectedly quit
;;; allegro

(with-port (ask dest-window wptr)
  (rlet ((r-source :rect :top 0 :left 0 :bottom height :right width)
         (r-dest   :rect :top (- (+ dest-top height)) :left dest-left
                         :bottom (- dest-top) :right (+ dest-left width)))
     (_CopyBits :ptr (bit-map-bitmap source-bitmap)
                :ptr (rref (rref (ask dest-window wptr) window.port) 
                           grafport.portbits)
                :ptr r-source
                :ptr r-dest
                :word (mode-arg :patCopy)
                :ptr nil))))


(defun copy-window-region-to-bitmap (source-window
                                     source-left
                                     source-top
                                     dest-bitmap
                                     width
                                     height)

;;;

(with-port (ask  source-window wptr)
  (rlet ((r-source :rect :top (- (+ source-top height)) :left source-left
                         :bottom (- source-top) :right (+ source-left width))
         (r-dest   :rect :top 0 :left 0 :bottom height :right width))
     
     (_CopyBits :ptr (rref (rref (ask source-window wptr) window.port)
                           grafport.portbits)
                :ptr (bit-map-bitmap dest-bitmap)
                :ptr r-source
                :ptr r-dest
                :word (mode-arg :patCopy)
                :ptr nil))))
;;; ***********

;;; Replaced by window-basics functions  ********** rwo
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;
 ;;; MOUSE READING FUNCTIONS
 ;;;

(defun get-mouse-state ()

 ;;;
 ;;; this function saves the state of the mouse in global variable *last-mouse-state*
 ;;; *last-mouse-state* is a variable of type mouse-state which has 2 slots :
 ;;;     - position which is the position of the mouse
 ;;;     - button which is the state of the button of the mouse. This state can be
 ;;;       left, middle or up (but not right)
 ;;;       as the mouse has only one button, the convention is that we return left
 ;;;       if both the button and option key are pressed, and we return middle if
 ;;;       only middle key is pressed without option
 ;;;

 (declare (special *last-mouse-state*))
 (or (boundp '*last-mouse-state*)
     (setf *last-mouse-state* (make-mouse-state)))
 (rlet((event :Event))
   (if (eq 0 (_GetNextEvent :word 6 :ptr event :word))
       (with-port (ask (front-window) wptr)   ; if no event just update position of mouse
          (rlet ((pos :point))
             (_GetMouse :ptr pos)
             (_LocalToGlobal :ptr pos)
             (setf (mouse-state-position *last-mouse-state*)
                   (make-position (rref pos Point.h)
                                  (rref pos Point.v)))))
       (progn                ; read mouse event
          (setf (mouse-state-position *last-mouse-state*)
                (make-position (point-h (rref event event.where))
                               (point-v (rref event event.where))))
          (setf (mouse-state-button *last-mouse-state*)
                (if (eq (rref event event.what) 1)                ; if mouse button down
                    (if (logbitp 11 (rref event event.modifiers)) ; if option key down
                        'left
                        'middle)
                    'up))))
   *last-mouse-state*))


(defun mouse-position ()

 ;;; 
 ;;; return last mouse position last time get-mouse-state was called
 ;;;

 (declare (special *last-mouse-state*))
 (mouse-state-position *last-mouse-state*))


(defun last-mouse-state (state)

 ;;;
 ;;; state is left, middle, right or up
 ;;;
 ;;; returns T if the button *last-mouse-state* is state
 ;;;

 (declare (special *last-mouse-state*))
 (when (eq state (mouse-state-button *last-mouse-state*))
       t))



(defun get-cursor-position (window)

 ;;;
 ;;; returns the position of the cursor in the local coordinates of the window
 ;;;

 (with-port (ask window wptr)
    (rlet ((pos :Point))
      (_GetMouse :ptr pos)
      (make-position (rref pos Point.h)
                     (- (rref pos Point.v))))))
;;; **********

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FONT FUNCTIONS
;;;

;;;
;;; String-Width is already defined in Allegro
;;;
;;; ******** renamed with prefix canvas- and moved to window-basics **** rwo
(defun font-p (font)
  "Returns t if font is a window-basics font, NIL otherwise."
  (listp font))


(defun make-font (font-description &optional stream)
  ;;
  ;; for mac, the font and font-description are equivalent
  ;;
  (declare (ignore stream))
  font-description)

;;;

;;; These are replaced by canvas-*  **** rwo
(defun font-ascent (font)
  (font-info font))


(defun font-descent (font)
  (second (multiple-value-list (font-info font))))


(defun font-height (font)
  (multiple-value-bind (ascent descent widmax leading) (font-info font)
   (declare (ignore widmax))
   (+ ascent descent leading)))
;;; 

;;;replaced by canvas-draw-line ********** rwo     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GRAPHIC OPERATIONS
;;;

(defun draw-line (X1 Y1 X2 Y2 width operation window)

;;;
;;; draw a line in window from point (X1 Y1) to (X2 Y2) of width width
;;;

  (with-port (ask window wptr)
     (rlet ((Pen-State :PenState)) 
       (rset Pen-State PenState.pnPat *black-pattern*)
       (_SetPenState :ptr Pen-State)
       (_PenSize :long (make-point width width))
       (_PenMode :word  (case operation
                                ((replace) (mode-arg :patCopy))
                                ((paint) (mode-arg :patOr))
                                ((invert) (mode-arg :patXor))
                                ((erase) (mode-arg :patBic))))
       (_MoveTo :long (make-point X1 (- Y1)))
       (_LineTo :long (make-point X2 (- Y2))))))

;;; 

;;; replaced by canvas-clear  *** rwo
(defun clear-window (window)

;;;
;;; clears the window ; we must create a new wptr if the window has been closed
;;;

  (ask window (window-show))
  (with-port (ask window wptr)
     (_EraseRect :ptr (graph-rect window))))
;;;

;;; replaced by canvas-font  **** rwo
(defun get-window-font (window)
  (ask window (window-font)))
;;; 

;;; replaced by setf on canvas-font **** rwo
(defun put-window-font (font window)
  (ask window (set-window-font font)))
;;;


(defun fill-region (window left bottom width height &optional texture)

;;;
;;; fill the region defined by left, bottom, width, height of window
;;; with texture
;;;

   (or texture (setf texture *black-pattern*))
   (with-port (ask window wptr)
      (_FillRect :ptr (make-rect (max 0 (- (+ bottom height)))
                                 (max 0 left)
                                 (min (get-window-height window)     
                                      (- top))
                                 (min (get-window-width window)
                                      (+ left width)))
                 :ptr texture)))


;;; Now canvas-p

(defun window-p (window) (typep window graph-window))

;;; Superseded by canvas-invert

(defun invert-region (window left top width height shade)

;;;
;;; invert the region in window
;;;

  (declare (ignore shade))
  (with-port (ask window wptr)
     (_InverRect :ptr 
         (make-rect (max 0 (- (+ top height)))
                    (max 0 left)
                    (min (get-window-height window)     
                          (- top))
                    (min (get-window-width window)
                         (+ left width))))))


;;; Superseded by canvas-move-to
(defun move-window-cursor-to (x y window)
  "Set the pen to the given position."
  (with-port (ask window wptr)
    (_MoveTo :long (make-point x (- y)))))

;;; Turned into window-basics function canvas-to-top
(defun window-to-top (window)
  "Brings the window to the front of all other windows."
  (ask window (window-select)))


;;; Turned into window-basics function position-in-region-p
(defun inside-region (region position)
  "Returns T if the position is inside the region."
  (let ((x (position-x position))
        (y (position-y position)))
    (and (<= (region-left region) x)
         (> (+ (region-left region) (region-width region)) x)
         (<= (region-bottom region) y)
         (> (+ (region-bottom region) (region-height region)) y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  I don't understand what the following was defined for.
;;;  make-point, point-h, and point-v will be as defined by
;;;  Mac CL anyway.  ***** rwo
;;;
;;; Pascal record definitions not specified in record.lisp
;;;

(defrecord Point
   (v integer 0)
   (h integer 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Following now in window-basics file wb-records-ccl
;;;

(defun make-rect (top left bottom right)

  (let (rect)
     (setf rect (make-record :rect))
     (set-record-field rect :rect ':topleft (make-point left top))
     (set-record-field rect :rect ':bottomright (make-point right bottom))
     rect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following is actually defined in quickdraw!  ********* rwo
;;;

(defmacro with-rectangle-arg ((var left top right bottom) &body body)
"Takes a rectangle, two points, or four coordinates and makes a rectangle.  ~
 Body is evaluated with VAR bound to that rectangle."

 `(rlet ((,var :rect))
    (cond (,bottom
           (rset ,var rect.topleft (make-point ,left ,top))
           (rset ,var rect.bottomright (make-point ,right ,bottom)))
          (,right
           (quail-error "illegal rectangle argument: ~s ~s ~s ~s"
                  ,left ,top ,right ,bottom))
          (,top 
           (rset ,var rect.topleft (make-point ,left nil))
           (rset ,var rect.bottomright (make-point ,top nil)))
          (t (setf ,var (pointer-arg ,left))))
    ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following is actually defined in quickdraw!  ********* rwo
;;;
;;;  used by with-rectangle-arg
;;;

(defun pointer-arg (thing)
  (if (handlep thing)
       thing
       (quail-error "Argument: ~a is not a Macintosh pointer" thing)))
