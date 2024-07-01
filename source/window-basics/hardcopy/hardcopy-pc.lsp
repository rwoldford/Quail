;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         hardcopy-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :wb)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(canvas-hardcopy hardcopy-bitmap)))

(defgeneric hardcopy-bitmap (bitmap &key left top width height)
  (:documentation "Produces a hardcopy of the bitmap."))

(defgeneric canvas-hardcopy (canvas &key left top width height)
  (:documentation "Produces a hardcopy of the canvas,~
                   or a selected region in canvas coordinates"))

(defmethod canvas-hardcopy ((self canvas) &key left top width height)
  (case
    (pick-one (list :postscript :printer))
    (:postscript
     (canvas-to-ps self))
    (:printer
     (if (or left top width height)
       (hardcopy-bitmap self
                        :left left
                        :top top
                        :width width
                        :height height)
        ;; a mclism
       (print-canvas self)))))


;;; For printing a selected rectangle of the canvas as a bitmap.
(defmethod hardcopy-bitmap ((w canvas) &key left top width height)
  (setq left (or left 72))
  (setq top (or top 72))
  (setq width (or width (canvas-width w)))
  (setq height (or height (canvas-height w)))

   )

(defvar the-array nil) ; 07AUG2023
(defvar the-tex nil) ; 07AUG2923

(defun print-canvas (stuff)
     "stuff is assumed to be a bitmap-window~
 this merges output-to-window-or-printer and printer-test"
 (declare (special cg::*screen*)) ;26SEP2023
     (multiple-value-setq (the-array the-tex)
          (cg::get-pixels stuff (cg::visible-box stuff)))
     (setf (cg::texture-info-invert-p the-tex) T) ;; NOTE
     (setf the-array (cg::reflect-pixel-map-in-y the-array)) ;; Added 100598 -- SEEMS OK
     (let* ((printer-stream (cg::open-stream 'cg::printer 'cg::null-location :output))
                (left-margin 20)
                (top-margin 20)
                (width (cg::texture-info-width the-tex))
                (height (cg::texture-info-height the-tex))
                )
          (if printer-stream
              (with-open-stream (stream printer-stream)
                  (setf (cg::stream-units-per-inch stream)
                           (round (* 0.85 (cg::stream-units-per-inch cg::*screen*))))
                  (cg::draw-box stream (cg::make-box left-margin
                                                      top-margin
                                                        (+ left-margin width)
                                                        (+ top-margin height)))
                  (setf (cg::stream-origin stream)
                           (cg::make-position (cg::left-margin stream t) (cg::top-margin stream t)))
                  (cg::copy-pixels-to-stream stream the-array 
                     the-tex
                     (cg::make-box left-margin top-margin
                          (+ left-margin width)
                          (+ top-margin height))
                     (cg::make-box 0 0 width height)
                     cg::po-replace)
                  )
              (error "Could not open printer")))
     )
