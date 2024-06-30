;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       bitmap-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     R.W. Oldford 1989-1992
;;;     G.W. Bennett 1996
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;----------------------------------------------------------------------------------
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(bitmap-p make-bitmap canvas-bitblt copy-canvas-region-to-bitmap
          with-bitmap-as-window canvas-bitblt copy-canvas-region-to-bitmap)))
;;;============================================================
;;; Bitmaps seem to be known as Pixel-Maps in ACLPC3
;;; See Ver2 manual 4-9 ff.
;;;============================================================
;; From bitmap-mcl

(defun bitmap-p (b) ;; cut in 11MAR2022  gwb
  (eq (class-name b) 'bitmap))

(defun make-window-use-bitmap (w bm)
  (declare (ignorable w bm)) ;10MAY2024
   #| "Make the window w use the host bitmap bm as its draw area."
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
|#
(format *terminal-io* "~&make-window-use-bitmap unimplemented for SBCL-LINUX~%")
)

(defmacro with-bitmap-as-window (bm w &body body)
  (declare (ignorable bm w body))
   #|
`(let ((,w (make-instance 'window :window-show nil)))
(make-window-use-bitmap ,w ,bm)
(h-draw::erase-rect ,w (rref ,bm bitmap.bounds))
,@body
(ccl:window-close ,w)))
|#
(format *terminal-io* "~&with-bitmap-as-window unimplemented for SBCL-LINUX~%")
)


(defun array-to-bitmap (the-array bitmap-width)
  (declare (ignorable the-array bitmap-width))
  (format *terminal-io* "~&array-to-bitmap unimplemented for SBCL-LINUX~%")
   ;;; make a bitmap with an array of integer and a width for the bitmap
   #|
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
   the-bitmap)
|#
)

(defun bitmap-to-array (the-bitmap bitmap-width)
  (declare (ignorable the-bitmap bitmap-width))
  (format *terminal-io* "~&bitmap-to-array unimplemented for SBCL-LINUX~%")
   ;;; make a bitmap with an array of integer and a width for the bitmap
   #|
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
   the-bitmap)
|#
)

  
(defun canvas-bitblt (canvas bitmap
      &key
      (canvas-left 0) (canvas-bottom 0)
      (bitmap-left 0) (bitmap-bottom 0)
      (width (bitmap-width bitmap))
      (height (bitmap-height bitmap))
      (operation :default))
  (declare (ignorable canvas bitmap canvas-left canvas-bottom bitmap-left bitmap-bottom
                              width height operation)) ;10MAY2024
   (format *terminal-io* "~&canvas-bitblt unimplemented for SBCL-LINUX~%")
#|
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
|#
   ) 

(defun copy-canvas-region-to-bitmap (source-canvas
                                     source-left
                                     source-top
                                     dest-bitmap
                                     width
                                     height)
  "Copy a region of the canvas to a bitmap."
  (declare (ignorable source-canvas source-left source-top dest-bitmap width height)) ;10MAY2024
   (format *terminal-io* "~&copy-canvas-region-to-bitmap unimplemented
for SBCL-LINUX~%")
   #|
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
                 :ptr (%null-ptr))))
   |#
   )  
