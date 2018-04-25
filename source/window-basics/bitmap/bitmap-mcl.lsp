;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               bitmap-mcl.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(bitmap-p make-bitmap canvas-bitblt copy-canvas-region-to-bitmap
          with-bitmap-as-window canvas-bitblt copy-canvas-region-to-bitmap)))

;;;============================================================
;;; Bitmaps
;;;============================================================


;;(defun bitmap-p (b)
;;  (eq (class-name b) 'bitmap))
;; (defun make-bitmap (left &optional right top bottom)
;;;  (ccl::make-bitmap left right top bottom))


(defun make-window-use-bitmap (w bm)
  "Make the window w use the host bitmap bm as its draw area."
  (let ((grafport (rref (wptr w) windowRecord.port)))
    (with-port grafport
     (_SetPBits :ptr bm)
     (_MovePortTo :word 0 :word 0)
     (_PortSize
      :word (+ 1 (- (rref bm bitmap.bounds.right)
                    (rref bm bitmap.bounds.left)))
      :word (+ 1 (- (rref bm bitmap.bounds.bottom)
                    (rref bm bitmap.bounds.top))))
     (_RectRgn
      :ptr (rref grafport grafport.visrgn)
      :ptr (rref bm bitmap.bounds)))))

(defmacro with-bitmap-as-window (bm w &body body)
  `(let ((,w (make-instance 'window :window-show nil)))
     (make-window-use-bitmap ,w ,bm)
     (h-draw::erase-rect ,w (rref ,bm bitmap.bounds))
     ,@body
     (ccl:window-close ,w)))

#|
(defun array-to-bitmap (the-array bitmap-width)
  
  ;;; make a bitmap with an array of integer and a width for the bitmap
  
  (let* ((the-bitmap (make-bitmap :width bitmap-width
                                  :height (array-dimension the-array 1)))
         (bm (ccl:rref (bitmap-host-bitmap the-bitmap) bitmap.baseaddr))
         (offset 0))
    
    (ccl:rset (bitmap-host-bitmap the-bitmap) bitmap.rowBytes
              (array-dimension the-array 0))
    (dotimes (y (array-dimension the-array 1))
      (dotimes (x (array-dimension the-array 0))
        (ccl:%put-byte bm (aref the-array x y) offset)
        (setq offset (+ 1 offset))))
    the-bitmap))

(defun bitmap-to-array (the-bitmap bitmap-width)
  
  ;;; make a bitmap with an array of integer and a width for the bitmap
  
  (let* ((the-bitmap (make-bitmap :width bitmap-width
                                  :height (array-dimension the-array 1)))
         (bm (ccl:rref (bitmap-host-bitmap the-bitmap) bitmap.baseaddr))
         (offset 0))
    
    (ccl:rset (bitmap-host-bitmap the-bitmap) bitmap.rowBytes
              (array-dimension the-array 0))
    (dotimes (y (array-dimension the-array 1))
      (dotimes (x (array-dimension the-array 0))
        (ccl:%put-byte bm (aref the-array x y) offset)
        (setq offset (+ 1 offset))))
    the-bitmap))
|#

(defun canvas-bitblt (canvas bitmap
		      &key
		      (canvas-left 0) (canvas-bottom 0)
		      (bitmap-left 0) (bitmap-bottom 0)
		      (width (bitmap-width bitmap))
		      (height (bitmap-height bitmap))
		      (operation :default))
  (let* ((raw-bitmap (bitmap-host-bitmap bitmap))
         (bitmap-y-adjust (rref raw-bitmap bitmap.bounds.bottom))
         (canvas-window (wptr canvas))
         (canvas-bitmap (rref canvas-window windowRecord.port.portbits))
         (canvas-visRgn (rref canvas-window windowRecord.port.visRgn))
         (bitblt-op (boole-to-op operation)))
    (rlet ((raw-rect :rect)
           (canvas-rect :rect))
      (rset raw-rect rect.left bitmap-left)
      (rset raw-rect rect.top (- bitmap-y-adjust (+ bitmap-bottom height -1)))
      (rset raw-rect rect.right (+ bitmap-left width -1))
      (rset raw-rect rect.bottom (- bitmap-y-adjust bitmap-bottom))
      (rset canvas-rect rect.left canvas-left)
      (rset canvas-rect rect.top (canvas-to-host-y canvas (+ canvas-bottom height -1)))
      (rset canvas-rect rect.right (+ canvas-left width -1))
      (rset canvas-rect rect.bottom (canvas-to-host-y canvas canvas-bottom))
      (h-draw::copy-bits raw-bitmap canvas-bitmap
                         raw-rect canvas-rect
                         bitblt-op canvas-visRgn)
      )))

(defun copy-canvas-region-to-bitmap (source-canvas
                                     source-left
                                     source-top
                                     dest-bitmap
                                     width
                                     height)
  "Copy a region of the canvas to a bitmap."
  (with-port (wptr source-canvas)
    (rlet ((r-source :rect :top (- (+ source-top height)) :left source-left
                         :bottom (- source-top) :right (+ source-left width))
           (r-dest   :rect :top 0 :left 0 :bottom height :right width))
      
      (_CopyBits :ptr
                 (rref (rref (wptr source-canvas) windowRecord.port)
                       grafport.portbits)
                 :ptr (bitmap-host-bitmap dest-bitmap)
                 :ptr r-source
                 :ptr r-dest
                 :word (h-draw::mode-arg :patCopy)
                 :ptr (%null-ptr)))))
