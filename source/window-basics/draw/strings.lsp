;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               strings.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)


(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-string-region  canvas-character-region
           canvas-character-width canvas-draw-string
           canvas-draw-character canvas-princ-string
           canvas-princ-character
           )))


(defun canvas-draw-horizontal-string
       (canvas string
               &key
               left bottom width height
               clip?
               justification)
  "Draw the string horizontally in the given region of the canvas."
  (unless width (setf width (canvas-string-width canvas string)))
  (unless height (setf height (canvas-font-height canvas)))
  (unless left (setf left (canvas-x canvas)))
  (unless bottom (setf bottom (canvas-y canvas)))
  (with-focused-canvas canvas
    (let*
      ((right (+ left width))
       (top (+ bottom height))
       (font (canvas-font canvas))
       (string-wid (canvas-string-width canvas string :font font)) 
       (vert-just (if (listp justification)
                    (or (find :top justification)
                        (find :bottom justification))
                    justification))
       (horiz-just (if (listp justification)
                     (or (find :left justification)
                         (find :right justification))
                     justification))
       (string-x 
        (case horiz-just
          (:left left)
          (:right (max left (- (+ left width ) string-wid)))
          (t  (max left (truncate (- (+ left right ) string-wid) 2)))))
       
       (string-y 
        (case vert-just
          (:bottom (+ bottom (canvas-font-descent font)))
          (:top (max (+ bottom (canvas-font-descent font))
                     (- (+ bottom height) (canvas-font-ascent font))))
          (t  (max (+ bottom (canvas-font-descent font))
                   (truncate (- (+ bottom top)
                                (+ (canvas-font-ascent font)
                                   (canvas-font-descent font))
                                )
                             2))))))
      
      (canvas-move-to canvas string-x string-y)
      (if clip?
        (clipped-draw-string canvas string (make-region left bottom width height))
        (canvas-princ-string canvas string))
      
      (canvas-move-to canvas (canvas-x canvas) bottom))))

(defun canvas-draw-vertical-string             ;; added by C. Hurley
       (canvas string 
               &key 
               left bottom width height
               justification clip?)
  (unless width (setf width (canvas-string-width canvas "W")))
  (unless height (setf height (* (length string) (canvas-font-height canvas))))
  (unless left (setf left (canvas-x canvas)))
  (unless bottom (setf bottom (- (canvas-y canvas) height)))
  (with-focused-canvas canvas
    (let* ((font (canvas-font canvas))
           (char-wid (canvas-string-width canvas "A"))
           (char-asc (canvas-font-ascent font))
           (char-ht (+ char-asc (canvas-font-descent font)))
           (top (+ bottom height -1))
           (vert-just (if (listp justification)
                        (or (find :top justification)
                            (find :bottom justification))
                        justification))
           (horiz-just (if (listp justification)
                         (or (find :left justification)
                             (find :right justification))
                         justification))
           (x-pos 
            (case horiz-just
              (:left (+ left (truncate char-wid 2)))
              (:right (+ left (max 1  (- width (truncate char-wid 2)))))
              (t (+ left (max 1 (truncate (- width char-wid) 2))))))
           (y-pos 
            (case vert-just
              (:top top)
              (:bottom (- top  (max 1 (round  (- height (* char-ht (length string)))))))
              (t   (- top (max 1 (round  (- height (* char-ht (length string))) 2)))))))
      (decf y-pos  char-asc)
      (canvas-move-to canvas x-pos y-pos)
      (with-display-mode canvas (display-mode-of canvas)
                         (canvas-draw-vertical-str canvas x-pos y-pos string
                                                   :bottom bottom
                                                   :clip? clip?)
        (canvas-draw-vertical-str canvas x-pos y-pos string
                                  :bottom bottom
                                  :clip? clip?)
        ))))

(defun canvas-draw-vertical-str (canvas x-pos y-pos string
                                        &key bottom clip?)
  (let ((char-ht (+ (canvas-font-ascent canvas)
                    (canvas-font-descent canvas))))
    (if clip?
      ; (loop for i from 0 below (length string)
      ;       for char = (elt  string i)
      ;       while (> y-pos bottom)
      ;       do
      (let ((slen (length string)))
        (do ((i 0 (incf i)))
            ((or (<= y-pos bottom) (= i slen)))
          (let ((char (elt string i)))
            (canvas-move-to
             canvas 
             (- x-pos (truncate (canvas-string-width canvas (string char))
                                2))
             y-pos)
            (canvas-princ-character canvas char)
            (decf y-pos char-ht)
            (canvas-move-to canvas x-pos y-pos))))
      ; (loop for i from 0 below (length string)
      ;       for char = (elt  string i)
      ;       do
      (let ((slen (length string)))
        (do ((i 0 (incf i)))
            ((= i slen))
          (let ((char (elt string i)))
            (canvas-move-to
             canvas 
             (- x-pos
                (truncate (canvas-string-width canvas (string char))
                          2))
             y-pos)
            (canvas-princ-character canvas char)
            (decf y-pos char-ht)
            (canvas-move-to canvas x-pos y-pos)))))))

(defun canvas-draw-character (canvas char
                                   &key
                                   (font NIL)
                                   (color NIL))
  "Draws the character char on the canvas at the current pen-position ~
   using the font font and color color. ~
   Default values for font and color are those of the canvas."
  
  (with-canvas-font canvas font
    (with-pen-color canvas color
      (canvas-princ-character canvas char))))


(defun canvas-string-region (canvas string
			     &key
			     (vertical-p nil)
			     (font (canvas-font canvas)))
  "Returns a region at the current pen location in the ~
   canvas that will contain the given string."
  (if vertical-p (quail-error "Can't handle vertical text!"))
  (let ((ascent (canvas-font-ascent font))
        (descent  (canvas-font-descent font))
        (width (canvas-string-width canvas string :font font))
        (left (canvas-x canvas))
        (bottom  (canvas-y canvas)))
    (make-region left 
                 (- bottom descent)
                 width (+ ascent descent)
                 )))



(defun canvas-character-region (canvas char
				&key
				(font (canvas-font canvas)))
  "Returns a region at the current pen location in the ~
   canvas that will contain the given character char."
  (canvas-string-region canvas (string char) :font font))

(defun canvas-character-width (canvas char
			       &key
			       (font (if canvas
                                           (canvas-font canvas)
                                           *normal-graphics-font*)))
  "Returns width (in pixels) of the given character char.  ~
   If canvas is NIL, it determines the ~
   width according to the font alone."
  (declare (special *normal-graphics-font*))
  (canvas-string-width canvas (string char) :font font))


(defun canvas-draw-string
       (canvas string
               &key
               (font nil)
               region
               left bottom width height
               (orientation NIL)
               (justification NIL)
               (clip? NIL)
               color)
  "Draw the string in the canvas.  ~
   Display the string in the given region (or that determined by left bottom ~
   width and height) of canvas.  If no region specs are given, the string is ~
   drawn at the current pen position.~
   Justification can be :left or :right to justify horizontal position, ~
   :top or :bottom to justify vertical position, or a list of a vertical and/or ~
   horizontal justification to specify both directions.  Default justification is ~
   :left horizontally and :bottom vertically.  ~
   If clip? is non-NIL, the string is clipped to the specified region."
  
  (unless orientation (setf orientation :horizontal))
  (unless justification
    (setf justification
          (case orientation
            (:horizontal '(:bottom :left))
            (:vertical '(:top :left)))))
  (when (not (stringp string))
    (if (symbolp string)
      (setf string (string-capitalize (format NIL "~a" string)))
      (setq string (princ-to-string string))))
  (when region
    (setf left (region-left region)
          width (region-width region)
          bottom (region-bottom region)
          height (region-height region)))
  (with-canvas-font canvas font
    (with-pen-color canvas color
      
      (if (eq orientation :horizontal)
        (canvas-draw-horizontal-string
         canvas string
         :left left :bottom bottom :width width :height height
         :justification justification :clip? clip?)
        (canvas-draw-vertical-string
         canvas string
         :left left :bottom bottom
         :width width :height height
         :justification justification :clip? clip?)
        ))))
   

(defun canvas-draw-center-string (canvas string left bottom width height &key font)
  "Prints a string in canvas centred in a box of given width and height ~
   and bottom left corner using the given font or the default canvas font."
  
  (unless (stringp string)
    (if (symbolp string)
      (setq string (symbol-name string))
      (setq string (format nil "~s" string))))
  (prog* ((font-used (or font (canvas-font canvas)))
          (str-width (canvas-string-width canvas string :font font-used)))
    (canvas-move-to canvas             ; 
     (+ left (floor
              (+ (- width str-width) 1)
              2))
     (+ bottom (floor
                (+ (- height (canvas-font-ascent font-used))
                   (canvas-font-descent font-used))
                2))
     ))
  (canvas-draw-string canvas string :font font))



(defun canvas-princ-character (canvas char)
  "Draws the character char on the canvas at the current pen-position ~
   using the current values of the canvas for font and color."
  (with-focused-canvas canvas
    (with-display-mode canvas (display-mode-of canvas)
                     (canvas-princ-character canvas char)
      (h-draw::draw-char canvas char))))

(defun canvas-princ-string (canvas string)
  "Draws the string on the canvas at the current pen-position ~
   using the current values of the canvas for font and color."
  (with-focused-canvas canvas
    (with-display-mode canvas (display-mode-of canvas)
                       (canvas-princ-string canvas string)
      (h-draw::draw-string canvas string))))
