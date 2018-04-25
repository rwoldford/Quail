;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        host-fonts-clx.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     N.G. Bennett 1993.
;;;     R.W. Oldford 1994.
;;;
;;;--------------------------------------------------------------------------------

(in-package :host-draw)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(font-transfer-modes font-styles font-transfer-modes font-names
          cached-host-font clear-host-font-cache
          get-canvas-font-name get-canvas-font-size get-canvas-font-style
          get-canvas-font-transfer-mode host-font-description)
        ))


(defvar
  *font-transfer-modes*
  (list :boole-1
	:boole-2
	:boole-andc1
	:boole-andc2
	:boole-and
	:boole-c1
	:boole-c2
	:boole-clr
	:boole-eqv
	:boole-ior
	:boole-nand
	:boole-nor
	:boole-orc1
	:boole-orc2
	:boole-set
	:boole-xor)
  "Font transfer-modes cache")

(defun font-transfer-modes ()
  *font-transfer-modes*)

(defvar
  *font-styles*
  (list :plain :bold :italic)
  "Font styles cache")

(defun font-styles ()
  *font-styles*)

(defun font-names ()
  (declare (special wb::*default-display* *font-names*))
  (let ((cached-font-names
         (assoc wb::*default-display* *font-names*))
        font-names)
    (cond
     (cached-font-names
      (setf font-names (cdr cached-font-names)))
     (T (setf font-names (xlib::list-font-names wb::*default-display* "*"))
        (push (cons wb::*default-display* font-names) *font-names*)
        ))
    font-names))

(defvar
  *font-names*
  NIL
  "Font name cache")
  
(defun match-name (name fonts)
  (remove-if-not
   #'(lambda (element)
       (search name element))
   fonts))

(defun match-style (style fonts)
  (remove-if-not
   #'(lambda (element)
       (cond ((equal style '(:plain))
              (or  (search "light-r-normal" element)
                   (search "medium-r-normal" element)
                   (search "regular-r-normal" element)))
             ((equal style '(:bold))
              (or  (search "bold-r-normal" element)
                   (search "demi-r-normal" element)
                   (search "demibold-r-normal" element)))
             ((equal style '(:italic))
              (or  (search "-o-normal" element)
                   (search "-i-normal" element)))
             ((or (equal style '(:italic :bold))
                  (equal style '(:bold :italic)))
              (or  (search "bold-o-normal" element)
                   (search "bold-i-normal" element)
                   (search "demi-o-normal" element)
                   (search "demi-i-normal" element)
                   (search "demibold-o-normal" element)
                   (search "demibold-i-normal" element)))
             ))
   fonts))

(defun match-psize (size fonts)
  (loop
    for font in fonts
    when
    (string-not-lessp
     (get-word-i font 7)
     (format nil "~a" size))
    collect font))

(defun match-font (canvas-font host-fonts)
  (let ((fonts NIL)
        font)
    (setq fonts (match-name (wb::canvas-font-name canvas-font) host-fonts))
    (setq fonts (match-style (wb::canvas-font-style canvas-font) fonts))
    (setf font (first (match-psize (wb::canvas-font-size canvas-font) fonts)))
    (cond
     (font font)
     (T
      (format *error-output* "~&Warning: Canvas font ~s not found.  Trying ~s"
              canvas-font "fixed")
      "fixed"))))

(defun cached-host-font (canvas-font)
  "Returns the host-font that has been cached for this canvas-font. ~
   If one is not cached, it is found, cached, and returned."
  (declare (special wb::*default-display*))
  (let ((host-fonts (sixth canvas-font))
        (host-font NIL))
    (when host-fonts
      (setf host-font
            (cdr (assoc wb::*default-display*  host-fonts))))
    (unless host-font
      (setf host-font
            (xlib::open-font
             wb::*default-display*
             (match-font
              canvas-font
              (font-names))))
      (If host-fonts
        (push (cons wb::*default-display* host-font) (sixth canvas-font))
        (nconc canvas-font (list (list (cons wb::*default-display* host-font))))))
    host-font))



(defun clear-host-font-cache (canvas-font)
  "Clears the host-font cache for this canvas-font."
  (if (sixth canvas-font)
    (rplacd (last canvas-font 2) NIL)))

(defun get-word-i (string &optional (i 1))
  (with-input-from-string (s string)
    (let ((num-dashes 0)
          (char (read-char s NIL NIL))
          (result NIL)
          )
      (loop
        while char
        until (> num-dashes i)
        do
        (if (eq char #\-) (incf num-dashes))
        (if (and (= num-dashes i)
                 (not (eq char #\-)))
          (push char result))
        (setf char (read-char s NIL NIL)))
      (if result
        (with-output-to-string (rs )
          (loop for c in (reverse result) do (write-char c rs))
          ))
      )))

(defun get-canvas-font-name (host-font)
  "Translates the host Common Lisp font representation to get the name ~
   of the corresponding canvas-font in window-basics."
  (get-word-i host-font 2))

(defun get-canvas-font-size (host-font)
  "Translates the host Common Lisp font representation to get the size ~
   of the corresponding canvas-font in window-basics."
  (get-word-i host-font 7))

(defun get-canvas-font-style (host-font)
  "Translates the host Common Lisp font representation to get the style ~
   of the corresponding canvas-font in window-basics."
  (cond
   ((or (search "medium-r-normal" host-font)
        (search "regular-r-normal" host-font)
        (search "light-r-normal" host-font))
    '(:plain))
   ((or (search "medium-o-normal" host-font)
        (search "medium-i-normal" host-font)
        (search "regular-o-normal" host-font)
        (search "regular-i-normal" host-font)
        (search "light-o-normal" host-font)
        (search "light-i-normal" host-font))
    '(:italic))
   ((or (search "bold-r-normal" host-font)
        (search "demi-r-normal" host-font)
        (search "demibold-r-normal" host-font))
    '(:bold))
   ((or (search "bold-o-normal" host-font)
        (search "bold-i-normal" host-font)
        (search "demi-o-normal" host-font)
        (search "demi-i-normal" host-font)
        (search "demibold-o-normal" host-font)
        (search "demibold-i-normal" host-font))
    '(:bold :italic))))


(defun get-canvas-font-transfer-mode (host-font)
  "Translates the host Common Lisp font representation to get the transfer-mode ~
   of the corresponding canvas-font in window-basics."
  NIL)


(defun host-font-description (host-font)
  "Returns four values that represent (in pixels) the ascent, ~
   descent, max-width, and leading (suggested spacing between ~
   lines) of the host-font."
  (let* ((ascent (xlib:font-ascent host-font))
	 (descent (xlib:font-descent host-font))
	 (leading 5)  ;; not given!!
	 (max-width (xlib:max-char-width host-font)))
    (values ascent descent leading max-width)))
