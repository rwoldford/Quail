;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               host-window-mcl.lisp
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

(export '(host-window))


(defclass host-window (window)
  ((the-scroller :accessor the-scroller))
  (:documentation
   "Mixin to canvas that captures properties of the host window system."))


(defmethod initialize-instance ((self host-window) &key
                                (scroller-class 'ccl::scroller)
                                (scroll-bar-class 'scroll-bar-dialog-item)
                                h-scroll-class v-scroll-class
                                (track-thumb-p t)
                                field-size)
  (call-next-method)
  (setf (the-scroller self)
        (make-instance
         scroller-class
         :view-container self
         :view-size (subtract-points (view-size self) #@(15 15))
         :view-position #@(0 0)
         :draw-scroller-outline nil
         :scroll-bar-class scroll-bar-class
         :h-scroll-class h-scroll-class
         :v-scroll-class v-scroll-class
         :track-thumb-p track-thumb-p
         :field-size field-size)))


(defmethod ccl::set-view-size ((self host-window) h &optional v)
  (declare (ignore h v))
  (without-interrupts
   (call-next-method)
   (let* ((new-size (subtract-points (view-size self) #@(15 15))))    
       (set-view-size (the-scroller self) new-size))
   (redisplay self)
   ))

(defmethod ccl::window-zoom-event-handler ((self host-window) message)
  (declare (ignore message))
  (without-interrupts
   (call-next-method)
   (let* ((new-size (subtract-points (view-size self) #@(15 15))))
       (set-view-size (the-scroller self) new-size))
   ))

(defmethod ccl::window-close ((c host-window))
  "Destroys the canvas if the mouse button is down over the ~
   close box (Mac style to avoid garbage) ~
   hides the canvas if command is down with mouse button."
  (declare (special *last-menubar* *current-event*))
  (if (and (boundp '*current-event*)
           (eq (rref *current-event* eventRecord.modifiers) 256))
    (#_HideWindow :ptr (wptr c))
    (call-next-method))
  )

;;-*- Mode: Lisp; Package: CCL -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pict-Scrap.Lisp
;;
;; Copyright 1989-1994, Apple Computer, Inc.
;; Copyright 1995 Digitool, Inc.

;;
;;
;;  This file a scrap-handler for scraps of type PICT
;;
;;  Once this is installed, windows which copy and paste PICTs will
;;  be able to share their work with other applications
;;
;; Modified for 2.0 by Henry Lieberman

;;;;;;;;;;;;
;;
;; Modification History
;;
;; 04/24/92 bill Don't push multiple entries on *scrap-handler-alist*
;;               if loaded multiple times. Also, eliminate the memory
;;               leak in internalize-scrap (thanx to Bob Strong).
;; ------------- 2.0
;; 11/18/91 bill Don't need to require traps or records anymore.
;; 08/24/91 gb  Use new traps; don't use $applScratch

;;(in-package :ccl)

(defclass pict-scrap-handler (scrap-handler) ())

(defmethod set-internal-scrap ((self pict-scrap-handler) scrap)
  (let* ((old-pict (slot-value self 'ccl::internal-scrap)))
    (when (handlep old-pict)
      (#_KillPicture old-pict)))        ;dispose of the old pict before we
                                        ;put a new one in its place
                                        ;this will crash if your program has
                                        ;other pointers to the pict, so
                                        ;always make sure cut/copy really do
                                        ;-copy- the pict
  (call-next-method self scrap)
  (when scrap (pushnew :pict *scrap-state*)))

(defmethod externalize-scrap ((pict-scrap-handler pict-scrap-handler))
  (let* ((the-pict (slot-value pict-scrap-handler 'ccl::internal-scrap))
         (size (#_GetHandleSize the-pict)))
    (when the-pict
      (with-dereferenced-handles
        ((the-pict the-pict))
        (#_PutScrap size :pict the-pict)))))

(defmethod internalize-scrap ((self pict-scrap-handler))
  (let* ((the-pict (slot-value self 'ccl::internal-scrap)))
    (unless (handlep the-pict)
      (setq the-pict
            (setf (slot-value self 'ccl::internal-scrap)
                   (#_NewHandle 0))))
    (rlet ((junk :signed-long))
      (#_GetScrap the-pict :pict junk))
    the-pict))


(defmethod get-internal-scrap ((pict-scrap-handler pict-scrap-handler))
  (slot-value pict-scrap-handler 'ccl::internal-scrap))

(eval-when (:load-toplevel :execute)
  (let ((p (assq :pict *scrap-handler-alist*)))
    (if p 
      (setf (cdr p) (make-instance 'pict-scrap-handler))
      (push `(:pict . ,(make-instance 'pict-scrap-handler))
            *scrap-handler-alist*)))
  )

;;;;;;;;;;;;;;;;;;;;;
;;
;; supporting cut and paste with picts
;;
;; because it doesn't remember the picts which it pastes,
;; it can only cut a pseudo-pict, that is, a pict which
;; contains the window's current contents as a bitmap.


(defmethod ccl::paste ((pict-window host-window))
  (let* ((pict (get-scrap :pict)))
    (when pict
      (with-port (wptr pict-window)
        (rlet ((r :rect))
          (with-dereferenced-handles ((pict-point pict))
            (copy-record (rref pict-point :picture.picframe
                               :storage :pointer)
                         :rect
                         r))
        (#_DrawPicture pict r))))))

(defmethod ccl::copy ((pict-window host-window))
  (let* ((wptr (wptr pict-window)))
    (rlet ((rect :rect 
                 :left (rref wptr windowrecord.portrect.left)
                 :top (rref wptr windowrecord.portrect.top)
                 :right (rref wptr windowrecord.portrect.right)
                 :bottom (rref wptr windowrecord.portrect.bottom)))
      (with-port wptr
        (#_cliprect rect)
        (let* ((pict (#_OpenPicture rect))
         ;;      (bits (rref wptr :windowrecord.portbits))
               )
          (wb::redisplay pict-window)
         ;; (#_CopyBits 
         ;;  bits 
         ;;  bits 
         ;;  rect 
         ;;  rect 0        ;transfer mode
         ;;  (%null-ptr))
          (#_ClosePicture)
          (put-scrap :pict pict))))))

(defmethod ccl::clear ((pict-window host-window))
  (let ((wptr (wptr pict-window)))
    (with-port wptr
      (#_EraseRect (rref wptr :windowrecord.portrect)))))

(defmethod ccl::cut ((pict-window host-window))
  (ccl::copy pict-window)
  (ccl::clear pict-window))