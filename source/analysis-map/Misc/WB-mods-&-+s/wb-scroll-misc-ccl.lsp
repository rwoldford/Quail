;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               wb-scroll-misc-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988,1989,1990,1991
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;

(in-package :window-basics)

(defun get-clipping-region (window)
  "Returns the content region of the window."
  (quail-print "In get-clipping-region (window).")
  (with-dereferenced-handles ((rgn (rref (ask window wptr) window.contRgn)))
   (rlet ((rect :rect))
    (copy-record (rref rgn region.rgnBBox :storage :pointer)
                 :rect
                 rect)
    (make-region (get-window-x-offset window)
                 (get-window-y-offset window)
                 (- (rref rect rect.right)
                    (rref rect rect.left))
                 (- (rref rect rect.bottom)
                    (rref rect rect.top)))))
  (quail-print "Exitting get-clipping-region (window)."))




(defun print-without-length-check (text window)
  "Prints the text on window without checking the length of data."
  (with-port (ask window wptr)
    (with-pstrs ((ptr text))
      (_DrawString :ptr ptr))))


(defun get-window-region (window)
  "Window content region in screen coordinates."
  (quail-print "In get-window-region (window).")
  (when (ask window (boundp 'wptr))
    (with-dereferenced-handles ((rgn (rref (ask window wptr) window.contRgn)))
      (rlet ((rect :rect))
        (copy-record (rref rgn region.rgnBBox :storage :pointer)
                     :rect
                     rect)
        (make-region (rref rect rect.left)
                     (rref rect rect.top)
                     (- (rref rect rect.right)
                        (rref rect rect.left))
                     (- (rref rect rect.bottom)
                        (rref rect rect.top)))))))




 #|
(defobfun (set-window-position *canvas*) (h &optional v)
  ;; moving a window

  ;; move the window
  (usual-set-window-position h v)
  #|
  ;; get new position coords
  (unless v
    (setq v (point-v h))
    (setq h (point-h h)))
  
  ;; save the new position in the browser object
  (let ((browser (get-window-browser (self))))
    (when browser
      (setf (slot-value browser 'left) h)
      (setf (slot-value browser 'bottom) v)))
  |#) |#

