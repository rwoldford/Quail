;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             quail-methods-mcl.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988 - 1989
;;;     R.W. Oldford 1985 - 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)


#|This should be unnecessary

(defmethod specific::make-menubar ((browser quail-browser))

;;; specialization from browser to build menus for left and middle items

  (declare (special specific::*class-menubar-cache*))
  (or (boundp 'specific::*class-menubar-cache*) 
      (setq specific::*class-menubar-cache* nil))
  (unless (assoc (class-of browser) specific::*class-menubar-cache*)

    ;; if menubar doesn't exist, create it and save it in association list

    (push (cons (class-of browser)
                (append (list (car ccl:*default-menubar*))
                     (list (specific::create-menu-with-list 
                             (slot-value browser 'browser::left-title-items)
                             "info"))
                     (list (specific::create-menu-with-list 
                             (slot-value browser 'browser::middle-title-items)
                             "Edit"))))
          specific::*class-menubar-cache*)))

|#



(defmethod set-anonymous-object ((self initable-object))
       
;;; mac version does nothing

  self)


(defmethod descriptive-label ((self network-view))

;;; use a bitmap as a descriptive label
;;; this method creates a bitmap representing the view and returns it
;;; this method is almost similar to object function window-draw-content
;;; defined for icon-window except that we have to create a grafport instead of
;;; a window to build the bitmap with a title

  (let* ((my-icon (eval (slot-value self 'sub-view-icon)))
         (title (icon-title self))
         (width (wb:bitmap-width my-icon))
         (height (wb:bitmap-height my-icon))
         (bitmap-result (wb:make-bitmap :width width :height height))
         (my-rect (ccl::make-record :rect :top 0 :left 0
                                    :bottom height
                                    :right width))
         (my-window (wb:make-canvas
                     :window-type :single-edge-box
                     :window-size (make-point width height))))
  
    ;; build a new grafport which portBits is bitmap-result

     (wb:canvas-bitblt my-icon my-window
                    :canvas-left 0 :canvas-bottom (- (wb:bitmap-height my-icon)))
     (set-view-font my-window '("helvetica" 10))
     (wb:canvas-move-to (max 2 (ash (- (wb:bitmap-width my-icon)
                                           (string-width title)) -1))
                     -9 my-window)
     (with-port (wptr my-window)
        (#_TextMode :word (wb::penmode-arg :patXor)))
     (wb:canvas-draw-string title my-window)
     (wb:copy-canvas-region-to-bitmap my-window 0 (- (wb:bitmap-height my-icon))
                                   bitmap-result
                                   (wb:bitmap-width my-icon) 
                                   (wb:bitmap-height my-icon))
     (wb:close-canvas  my-window)
     bitmap-result))

