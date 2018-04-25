;;; -*- Mode: LISP -*-


(in-package 'specific)


(defmethod make-menubar ((browser Z::Z-browser))

;;; specialization from browser to build menus for left and middle items

  (declare (special *class-menubar-cache*))
  (or (boundp '*class-menubar-cache*) 
      (setq *class-menubar-cache* nil))
  (unless (assoc (class-of browser) *class-menubar-cache*)

    ;; if menubar doesn't exist, create it and save it in association list

    (push (cons (class-of browser)
                (append (list (car *default-menubar*))
                     (create-menu-with-list 
                             (slot-value browser 'browser::left-title-items)
                             "info")
                     (create-menu-with-list 
                             (slot-value browser 'browser::middle-title-items)
                             "Edit")))
          *class-menubar-cache*)))


(in-package 'Z)


(defmethod set-anonymous-object ((self initable-object-mixin))
       
;;; mac version does nothing

  self)


(defmethod descriptive-label ((self network-view))

;;; use a bitmap as a descriptive label
;;; this method creates a bitmap representing the view and returns it
;;; this method is almost similar to object function window-draw-content
;;; defined for icon-window except that we have to create a grafport instead of
;;; a window to build the bitmap with a title

 (let* ((my-icon (eval (slot-value self 'sub-view-icon)))
        (title (Z::icon-title self))
        (width (bitmap-width my-icon))
        (height (bitmap-height my-icon))
        (bitmap-result (make-bitmap :width width :height height))
        (my-rect (specific::make-rect 0 0 height width))
        (my-window (oneof *window*
                            :window-type :single-edge-box
                            :window-size (make-point width height))))
  
    ;; build a new grafport which portBits is bitmap-result

     (canvas-bitblt my-icon my-window
                    :canvas-left 0 :canvas-bottom (- (bitmap-height my-icon)))
     (ask my-window (set-window-font '("helvetica" 10)))
     (canvas-move-to (max 2 (ash (- (bitmap-width my-icon)
                                           (string-width title)) -1))
                     -9 my-window)
     (with-port (ask my-window wptr)
        (_TextMode :word (wb::penmode-arg :patXor)))
     (Print-without-length-check title my-window)
     (copy-canvas-region-to-bitmap my-window 0 (- (bitmap-height my-icon))
                                   bitmap-result
                                   (bitmap-width my-icon) 
                                   (bitmap-height my-icon))
     (ask my-window (window-close))
     bitmap-result))

