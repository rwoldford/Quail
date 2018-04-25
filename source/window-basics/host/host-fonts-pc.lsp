;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   host-fonts-pc.lsp                                                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 Statistical Computing Laboratory, University of Waterloo 
;;;
;;; Authors:                                                                   
;;;
;;;     R.W.Oldford 1994.                                                       
;;;     G.W.Bennett 1996.                                                      
;;;
;;;-----------------------------------------------------------------------------
;;;
;;; ACL fonts have 4 attributes:
;;; Family - which is a keyword from the set 
;;;          :decorative - novelty fonts
;;;          :modern - fixed pitch, constant stroke width
;;;          :roman  - proportional with serifs and variable stroke width
;;;          :script - like handwriting
;;;          :swiss  - proportional without serifs and variable stroke width
;;;                    usually helvetica.
;;;          nil     - user does not mind which is used
;;;
;;; Face   - which resembles Quail's name, with values
;;;          :system  :fixedsys :terminal :ms\ serif :ms\ sans\ serif
;;;          :courier :symbol :small\ fonts :modern :marlett :arial
;;;          :courier\ new :times\ new\ roman :wingdings 
;;;
;;; Size   - in PIXELS. This depends on Face at least as far as what
;;;          is built in. It seems that (almost) any size can be used
;;;          and that scaling is done in some way.
;;;          Various forms deal with the conversion of points <-> pixels.
;;;
;;; Style  - a LIST of zero or more of :bold, :italic, :underline, :condensed
;;;
;;; In this implementation a Quail font-name will be a pair (ACL_family. ACL_face)
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :HOST-DRAW)

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

(defvar 
   *points-per-inch*
   72
   "The number of font points per inch")

(defun font-transfer-modes ()
     *font-transfer-modes*)

(defvar
   *font-styles*
   (list :plain :bold :italic :underline :condensed)
   "Font styles cache")

(defun font-styles ()
     "A list of styles - the fourth attribute of a font"
     *font-styles*)

(defun font-names ()
     "Collects (family.face) list for (cg::screen cg::*system*)"
     ;(declare (special (cg::screen cg::*system*)))
     (let (fonts
             (root (cg::screen cg::*system*)))
      (cg::with-device-context (hdc (cg::screen cg::*system*))
         (loop for face in (cg::font-faces root)
           do
           (loop for family in (list :modern :roman :swiss NIL)
             do
             (push (cons family face) fonts))
           ))
         fonts))

(defvar
   *font-names*
   NIL
   "Font name cache")

(defun built-in-font-types (&optional (canvas (cg::screen cg::*system*)))
     "Creates a list of (list (cons family face) size style) for most of the ~
supplied fonts omitting ornamental ones."
     (let (whole-list
            (f-family (list :modern :roman :swiss NIL)))
         ;; :decorative and :script omitted from family
      (cg::with-device-context (hdc (cg::screen cg::*system*))
         (loop for a-family in f-family
           do 
           (let (f-faces
                  (all-faces (cg::font-faces canvas)))
               (loop for face in all-faces
                 do
                 (push face f-faces))
               (loop for a-face in f-faces
                 do 
                 (let ((f-sizes (cg::font-sizes canvas a-face)))
                     (loop for a-size in f-sizes
                       do
                       (let ((f-styles (list '() :bold :italic :underline :condensed)))
                           (loop for a-style in f-styles
                             do
                             (push (list (cons a-family a-face) a-size a-style) whole-list))))))))
)
         whole-list))


(defvar
   *built-in-font-types*
   (built-in-font-types)
   )

(defun cached-host-font (canvas-font &optional (fonts *built-in-font-types*))
     "Returns a real ACL font from the result of matching canvas-font ~
over a list representing real ACL fonts."
     (let ((host-font (sixth canvas-font)))
         (unless host-font
              (setf host-font
                      (cg::make-font
                       (car (second canvas-font))
                       (cdr (second canvas-font))
                       (font-point-to-pixel (fourth canvas-font))
                       (if (equal (list :plain) (third canvas-font))
                          NIL
                          (third canvas-font))))
              (nconc canvas-font (list host-font)))
         host-font)
     )


(defun clear-host-font-cache (canvas-font)
     "Clears the host-font cache for this canvas-font."
     (if (sixth canvas-font)
        (setf canvas-font (nbutlast canvas-font))))

(defun get-canvas-font-name (host-font)
     "Translates the host Common Lisp font representation to get the name ~
of the corresponding canvas-font in window-basics."
     (cons (cg::font-family host-font) (cg::font-face host-font)))

(defun get-canvas-font-size (host-font &optional (canvas (cg::screen cg::*system*)))
     "Translates the (pixel) size of the host Common Lisp font to the ~
(point) size of the corresponding canvas-font in window-basics."
  (cg::with-device-context (hdc (cg::screen cg::*system*))
(font-pixel-to-point (cg::font-size host-font) canvas)))

(defun get-canvas-font-style (host-font)
     "Translates the host Common Lisp font representation to get the style ~
of the corresponding canvas-font in window-basics."
   (cg::with-device-context (hdc (cg::screen cg::*system*))
     (or (cg::font-style host-font) :plain)))

(defun get-canvas-font-transfer-mode (host-font)
     "Translates the host Common Lisp font representation to get the transfer-mode ~
of the corresponding canvas-font in window-basics."
     (declare (ignore host-font))
     ;; don't know what we mean by this yet.
     :boole-1)

(defun host-font-description (host-font)
     "Returns four values that represent (in pixels) the ascent, ~
descent, max-width, and leading (suggested spacing between ~
lines) of the host-font."
  (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
     (if (cg::fontp host-font)
        (let ((old-fh (cg::font-handle scrn))
               font-metrics ascent descent width leading)
          ;(cg::set-font scrn host-font) 18oct05
          (setf (cg::font scrn) host-font) ;18oct05
            (setf font-metrics (cg::fontmetrics scrn))     
            (setq ascent
               (cg::font-ascent  font-metrics)
               descent
               (cg::font-descent  font-metrics)
               width
               (cg::font-max-char-width  font-metrics)
               leading
               (cg::font-leading  font-metrics))
          ;(cg::set-font scrn old-fh) 18oct05
          (setf (cg::font scrn) old-fh) ;18oct05
            (values ascent descent width leading)
            )
        (error "Not a host font! : ~s" host-font)
        )
      )
     )
   )

(defun font-pixel-to-point (pixel &optional (canvas (cg::screen cg::*system*)))
     "Returns the point size of a font for canvas given pixel size."
  (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
     (round (* *points-per-inch* (/ pixel (cg::stream-units-per-inch canvas)))))
    ))

(defun font-point-to-pixel (point &optional (canvas (cg::screen cg::*system*)))
     "Returns the pixel size of a font for canvas given point size."
 (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
     (round (* (cg::stream-units-per-inch canvas) (/ point *points-per-inch*))))
    ))
