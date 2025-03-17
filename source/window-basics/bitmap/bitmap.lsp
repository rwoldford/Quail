;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               bitmap.lisp
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
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

;#| stubbed 11MAR2022  gwb
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(bitmap-width bitmap-height bitmap-host-bitmap
           bitmap
           *circle-bitmap* *cross-bitmap* *square-bitmap* *star-bitmap*
           make-blank-bitmap make-circle-bitmap make-box-bitmap
           plot-glyph-at)))
;|#
;(eval-when (:compile-toplevel :load-toplevel :execute) (export '(bitmap)))

;;;============================================================
;;; Bitmaps
;;;============================================================


(defstruct bitmap
  width height host-bitmap)


; stubbed 11MAR2022  gwb
(defvar *circle-bitmap*   NIL)
(defvar *cross-bitmap*    NIL)
(defvar *square-bitmap*   NIL)
(defvar *star-bitmap*     NIL)

(defun setup-bitmaps ()
  (declare
   (special *circle-bitmap* *box-bitmap* *star-bitmap* *cross-bitmap*))
  (setf *circle-bitmap*
        (make-bitmap
         :width 12
         :height 12
         :host-bitmap
         (let ((bm (h-draw::make-bitmap 0 0 11 11)))
           (with-bitmap-as-window bm w
             (h-draw::draw-arc w 0 360 0 0 5 5)
             )
           bm)
         ))
  
  (setf *cross-bitmap*
        (make-bitmap
         :width 12
         :height 12
         :host-bitmap
         (let ((bm (h-draw::make-bitmap 0 0 11 11)))
           (with-bitmap-as-window bm w
             (h-draw::move-to w 0 0)
             (h-draw::line-to w 10 10)
             (h-draw::move-to w 10 0)
             (h-draw::line-to w 0 10)
             )
           bm)
         ))
  
  (setf *square-bitmap*
        (make-bitmap
         :width 12
         :height 12
         :host-bitmap
         (let ((bm (h-draw::make-bitmap 0 0 11 11)))
           (with-bitmap-as-window bm w
             (h-draw::move-to w 0 0)
             (h-draw::line-to w 10 0)
             (h-draw::line-to w 10 10)
             (h-draw::line-to w 0 10)
             (h-draw::line-to w 0 0)
             )
           bm)
         ))
  
  (setf *star-bitmap*
        (make-bitmap
         :width 12
         :height 12
         :host-bitmap
         (let ((bm (h-draw::make-bitmap 0 0 11 11)))
           (with-bitmap-as-window bm w
             (let ((ang (/ (* 2.0 pi) 8.0)))
               (dotimes (r 8)
                 (h-draw::move-to w 5 5)
                 (h-draw::line w (round (* 5 (sin (* r ang))))
                               (round (* 5 (cos (* r ang)))))
                 )
               )
             )
           bm)
         )))

(eval-when (:load-toplevel) (setup-bitmaps))
(add-restore-lisp-functions #'setup-bitmaps)

(defun make-blank-bitmap (width height)
  (make-bitmap
   :width width
   :height height
   :host-bitmap (h-draw::make-bitmap 0 0 width height)))

(defun make-circle-bitmap (diameter &key fill?  pattern )
  (declare (special *black-shade*))
  (unless pattern (setf pattern *black-shade*))
  (make-bitmap
   :height diameter
   :width diameter
   :host-bitmap (let ((bm (h-draw::make-bitmap 0 0 (1- diameter) (1- diameter)))
                      (radius (truncate (/ diameter 2.0))))
                  (with-bitmap-as-window bm w
                    (if (null fill?)
                      (h-draw::draw-arc w 0 360 radius radius radius radius)
                      (progn
                        (h-draw::set-pen-pattern w pattern)
                        (h-draw::fill-arc w 0 360 radius radius
                                          radius radius))
                      ))
                  bm)
   ))


(defun make-box-bitmap 
       (width height &key fill?  pattern )
  (declare (special *black-shade*))
  (unless pattern (setf pattern *black-shade*))
  (make-bitmap
   :height height
   :width width
   :host-bitmap (let ((bm (h-draw::make-bitmap 0 0 (1- width) (1- height))))
                  (with-bitmap-as-window bm w
                    (if (null fill?)
                      (h-draw::draw-rectangle w 0 (1- width) 0 (1- height))
                      (progn
                        (h-draw::set-pen-pattern w pattern)
                        (h-draw::draw-filled-rectangle
                         w 0 (1- width) 0 (1- height)))))
                  bm)
   ))


;;; stubbed 12MAR2022  gwb
(defun plot-glyph-at (canvas position-x position-y glyph
                             &key (operation :default))
  (declare (ignorable canvas position-x position-y glyph operation)) ;  10MAY2024 to avoid Warnings
;(format t "~%Not implements in mcclim yet")
  (let* ((glyph-width (bitmap-width glyph))
         (glyph-height (bitmap-height glyph))
         (newx (- position-x 
                  (truncate glyph-width 2)))
         (newy (- position-y 
                  (truncate glyph-height 2))))
    (canvas-bitblt canvas glyph :canvas-left newx 
                   :canvas-bottom newy :operation operation))
                   
  )