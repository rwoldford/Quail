;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               font.lisp
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
;;;     R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(canvas-font-transfer-modes canvas-font-styles canvas-font-names
           canvas-make-font copy-canvas-font canvas-font-p canvas-font-name
           set-canvas-font-name canvas-font-style set-canvas-font-style
           canvas-font-size set-canvas-font-size
           canvas-font-transfer-mode set-canvas-font-transfer-mode
           canvas-font-ascent canvas-font-descent canvas-font-leading
           canvas-font-height with-canvas-font canvas-font-equal)))

(defun canvas-font-transfer-modes ()
  "Returns a list of legal transfer modes for canvas fonts"
  (h-draw:font-transfer-modes))

(defun canvas-font-styles ()
  "Returns the list of currently available canvas font styles."
  (h-draw:font-styles))

(defun canvas-font-names ()
  "Returns a list of font names currently available."
  (h-draw:font-names))

(defun canvas-make-font (&key
                         (name NIL)
                         (style :plain)
                         (size 10)
                         (transfer-mode NIL))
  "Makes a font of with the given font characteristics."
  (if (not (listp style))
    (setf style (list style)))
  (list 'canvas-font name style size transfer-mode))

(defun canvas-font-equal (font1 font2)
  "Tests whether font1 and font2 represent the same canvas-font."
  (flet ((sort-font-list (font)
           (list
            (canvas-font-name font)
            (canvas-font-size font)
            (canvas-font-transfer-mode font)
            (sort (canvas-font-style font)
                  #'(lambda (x y)
                      (< (position x (canvas-font-styles))
                         (position y (canvas-font-styles))))))))
    (equal (sort-font-list (copy-canvas-font font1))
           (sort-font-list (copy-canvas-font font2)))))
    

(defun copy-canvas-font (canvas-font)
  "Returns a new canvas-font that is a copy of the one given."
  (copy-tree canvas-font))


(defun canvas-font-p (font)
  "Returns t if font is a canvas font, NIL otherwise."
  (and (listp font)
       (eq (first font) 'canvas-font)))

(defun canvas-font-name (canvas-font)
  "Returns the name of this canvas font."
  (second canvas-font))

(defun set-canvas-font-name (canvas-font name)
  "Sets the name of this canvas font."
  (if (member name (canvas-font-names) 
              :test
              #'(lambda (a b)
                  (cond
                   ((and (stringp a) (stringp b))
                    (string-equal a b))
                   (T (equal a b)))))
    (setf (second canvas-font) name)
    (error "Illegal canvas font name ~s" name))
  (h-draw:clear-host-font-cache canvas-font)
  name)

(defun canvas-font-style (canvas-font)
  "Returns the style list of this canvas font."
  (third canvas-font))

(defun set-canvas-font-style (canvas-font style)
  "Sets the style list of this canvas font."
  (cond
   ((null style)
    (error "Illegal canvas font style ~s" style))
   ((listp style)
    (loop for s in style
          do
          (set-canvas-font-style canvas-font s)))
   ((member style (canvas-font-style canvas-font)) style)
   ((member style (canvas-font-styles))
    (cond
     ((eql :plain style) (setf (third canvas-font) (list style)))
     (T (setf (third canvas-font)
              (append (list style)
                      (remove :plain (third canvas-font)))))))
   (T (error "Illegal canvas font style ~s" style))
   )
  (h-draw:clear-host-font-cache canvas-font)
  style)

(defun canvas-font-size (canvas-font)
  "Returns the size of this canvas font."
  (fourth canvas-font))

(defun set-canvas-font-size (canvas-font size)
  "Sets the size of this canvas font."
  (if (and (integerp size)
           (<= 1 size)
           (<= size 127))
    (setf (fourth canvas-font) size)
    (error "Illegal canvas font size ~s" size))
  (h-draw:clear-host-font-cache canvas-font)
  size)
  
(defun canvas-font-transfer-mode (canvas-font)
  "Returns the transfer-mode of this canvas font."
  (fifth canvas-font))
  
(defun set-canvas-font-transfer-mode (canvas-font transfer-mode)
  "Sets the transfer-mode of this canvas font."
  (if (member transfer-mode (canvas-font-transfer-modes))
    (setf (fifth canvas-font) transfer-mode)
    (error "Illegal canvas font transfer-mode ~s" transfer-mode))
  (h-draw:clear-host-font-cache canvas-font)
  transfer-mode)


(defmacro with-canvas-font (canvas font &body forms)
  "Performs the forms with the font of canvas temporarily reset to ~
   the value given.  A NIL font is ignored."
  (let ((old-font (gensym "with-canvas-font")))
    `(let (,old-font)
       (cond
        ((and ,font (not (canvas-font-equal ,font (canvas-font ,canvas))))
         (setf ,old-font (canvas-font ,canvas))
         (setf (canvas-font ,canvas) ,font)
         ,@forms
         (setf (canvas-font ,canvas) ,old-font))
        (T ,@forms)))))

(defun canvas-font-description (canvas-font)
  "Returns four values that represent (in pixels) the ascent, ~
   descent, max-width, and leading (suggested spacing between ~
   lines) of the canvas-font."
  
  (h-draw:host-font-description
   (if (canvas-font-p canvas-font)
     (canvas-font-to-host-font canvas-font)
     canvas-font)))

(defun canvas-font-ascent (canvas-or-font &key font)
  "Returns the ascent of the font supplied or the ascent ~
   of the font associated with the canvas if a canvas is given ~
   instead."
  (setf font
        (or (if (canvas-font-p canvas-or-font)
              canvas-or-font
              NIL)
            font
            (if (canvas-p canvas-or-font)
              (canvas-font canvas-or-font)
              NIL)))
  (canvas-font-description font))

(defun canvas-font-descent (canvas-or-font &key font)
  "Returns the descent of the font supplied or the descent ~
   of the font associated with the canvas if a canvas is given ~
   instead."
  (setf font
        (or (if (canvas-font-p canvas-or-font)
              canvas-or-font
              NIL)
            font
            (if (canvas-p canvas-or-font)
              (canvas-font canvas-or-font)
              NIL)))
  (second (multiple-value-list (canvas-font-description font))))

(defun canvas-font-leading  (canvas-or-font &key font)
  "Returns the leading of the font supplied or the leading ~
   of the font associated with the canvas if a canvas is given ~
   instead."
  (setf font
        (or (if (canvas-font-p canvas-or-font)
              canvas-or-font
              NIL)
            font
            (if (canvas-p canvas-or-font)
              (canvas-font canvas-or-font)
              NIL)))
  (fourth (multiple-value-list (canvas-font-description font))))


(defun canvas-font-height (canvas-or-font &key font)
  "Returns the height of the font supplied or the height ~
   of the font associated with the canvas if a canvas is given ~
   instead."
  (setf font
        (or (if (canvas-font-p canvas-or-font)
              canvas-or-font
              NIL)
            font
            (if (canvas-p canvas-or-font)
              (canvas-font canvas-or-font)
              NIL)))
  (multiple-value-bind (ascent descent widmax leading)
                       (canvas-font-description font)
   (declare (ignore widmax))
   (+ ascent descent leading)))

(defun canvas-font-to-host-font (canvas-font)
  "Translates the canvas font representation to the representation ~
   of fonts in the host Common Lisp.  ~
   (:see-also host-font-to-canvas-font canvas-make-font)"
  (h-draw:cached-host-font canvas-font))

(defun host-font-to-canvas-font (host-font)
  "Translates the host Common Lisp font representation to the representation ~
   of a canvas font in window-basics.  ~
   (:see-also canvas-font-to-host-font canvas-make-font)"
  (let
    ((name (h-draw:get-canvas-font-name host-font))
     (size (h-draw:get-canvas-font-size host-font))
     (style (h-draw:get-canvas-font-style host-font))
     (mode (h-draw:get-canvas-font-transfer-mode host-font)))
    (canvas-make-font
     :name name
     :size size
     :style style
     :transfer-mode mode)
    )
  )
