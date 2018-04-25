;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;                         dialog-items-clx.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  Authors:
;;;     R.W. Oldford 1994
;;;
;;;
;;;


(in-package :wb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     dialog-items
;;;

(defclass dialog-item ()
  ((window               :initform NIL
  	                 :initarg :window
	                 :accessor window-of)
   (gcontext             :initform NIL
	                 :initarg :gcontext
	                 :accessor gcontext-of)
   (background-color     :initform *grey-color*
                         :initarg :background-color
                         :accessor dialog-background-color-of)
   (foreground-color     :initform *black-color*
                         :initarg :foreground-color
                         :accessor dialog-foreground-color-of)
   (border-width         :initform 0
                         :initarg :border-width
                         :accessor dialog-border-width-of)
   (owner                :initform NIL
                         :initarg :owner
                         :accessor owner-dialog)
   (item                 :initform NIL
                         :initarg :item
                         :accessor dialog-original-item)
   (window-border-width  :initform 0
                         :initarg :window-border-width)
   )
  )

(defun dialog-item-p (thing)
  (typep thing 'dialog-item))

(defmethod initialize-instance :after ((self dialog-item)
			               &key 
			               gcontext
				       window
                                       border-width
			               &allow-other-keys)
  
  (let* ((parent (if (xlib::window-p window)
                   (parent-of window)
                   (xlib::screen-root
                    (xlib::display-default-screen
                     *default-display*))))
         (colormap (xlib::window-colormap parent))
         (fore-color (xlib::alloc-color
                      colormap
                      (dialog-foreground-color-of self)))
         (back-color (xlib::alloc-color
                      colormap
                      (dialog-background-color-of self)))
         )
    
    ;; sort out the window
    ;; First get one.
    (cond
     ((xlib::window-p window)
      (xlib::with-state (window)
        (setf (xlib::window-background window) back-color)
        (setf (xlib::drawable-border-width window)
              (window-border-width-of self))))
     (T
      (setf window
            (xlib::create-window
             :parent parent
             :x 0 :y 0 :width 16 :height 16
             :background back-color
             :override-redirect :on
             :border-width (slot-value self 'window-border-width)
             :event-mask
             (xlib::make-event-mask 
              :button-press
              :button-release	
              :key-press
              :key-release
              :exposure
              :enter-window
              :leave-window
              :owner-grab-button))))
     )
    ;; Hook back to self from window
    (if (xlib::window-plist window)
      (push (cons :wb-class-instance self)
            (xlib::window-plist window))
      (setf (xlib::window-plist window)
            (acons :wb-class-instance self NIL)))
    ;; Stuff the window away.
    (setf (window-of self) window)
    
    ;; sort out the gcontext
    (unless (xlib::gcontext-p gcontext)
      (setf gcontext
            (xlib::create-gcontext
             :drawable window)))
    (setf (gcontext-of self) gcontext)
    (setf (xlib::gcontext-background gcontext)
          back-color)
    (setf (xlib::gcontext-foreground gcontext)
          fore-color)
    
    )
  
  ;; compute the geometry
  (dialog-item-compute-geometry self)
  
  ;; return self
  self)

(defgeneric dialog-item-compute-geometry (dialog-item)
  (:documentation "Compute the geometry for this dialog-item."))

(defmethod dialog-item-compute-geometry ((self T))
  "Taken care of by dialog-item initialize-instance :after ~
   for simple dialog-items."
  NIL)

(defgeneric process-button-press-event (dialog-item &optional window)
  (:documentation "Function called when a button down event occurs ~
                   within a dialog-item's window."))

(defmethod process-button-press-event ((button dialog-item) &optional window)
  (declare (ignore button window)))

(defgeneric process-button-release-event (dialog-item &optional window)
  (:documentation "Function called when a button up event occurs ~
                   within a dialog-item's window."))

(defmethod process-button-release-event ((button dialog-item) &optional window)
  (declare (ignore button window)))
  
(defmethod wb-refresh ((thing dialog-item))
  (declare (ignore thing)))


(defgeneric process-key-press-event (dialog-item window code state)
  (:documentation "Function called when a key down event occurs ~
                   within a dialog-item's window."))

(defmethod process-key-press-event ((self dialog-item) window code state)
  (declare (ignore self window code state)))


(defgeneric highlight-dialog-item (dialog-item)
  (:documentation "Function which draws a highlighted version of the dialog-item."))


(defgeneric downlight-dialog-item (dialog-item)
  (:documentation "Function which draws a downlighted version of the dialog-item."))


(defgeneric dialog-enter-notify (dialog-item &optional window state)
  (:documentation "Function called when the pointer enters the dialog-item."))

(defmethod dialog-enter-notify ((self dialog-item) &optional window state)
  (declare (Ignore self window)))

(defgeneric dialog-leave-notify (dialog-item &optional window)
  (:documentation "Function called when the pointer leaves the dialog-item."
                  ))

(defmethod dialog-leave-notify ((self dialog-item) &optional window)
  (declare (Ignore self window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   Buttons
;;;

(defclass button (dialog-item)
  ((on?        :initform NIL
               :initarg :on?
               :accessor button-on-p)
   (on-action  :initform NIL
               :initarg :on-action
               :accessor button-on-action-of)
   (off-action :initform NIL
               :initarg :off-action
               :accessor button-off-action-of)
   (alive?     :initform NIL
               :accessor button-alive-p)
   
   )
  )

(defun make-button (&rest args)
  (apply #'make-instance 'button args))


(defmethod button-on ((button button))
  (highlight-dialog-item button)
  (setf (button-on-p button) T))


(defmethod button-off ((button button))
  (downlight-dialog-item button)
  (setf (button-on-p button) NIL)
  )

(defmethod process-button-press-event ((button button) &optional window)
  (declare (ignore window))
  (setf (button-alive-p button) T)
  (if (button-on-p button)
    (button-off button)
    (button-on button)))

(defmethod process-button-release-event ((button button) &optional window)
  (declare (ignore window))
  (setf (button-alive-p button) NIL)
  (if (button-on-p button)
    (if (functionp (button-on-action-of button))
      (funcall (button-on-action-of button))
      (eval (button-on-action-of button)))
    (if (functionp (button-off-action-of button))
      (funcall (button-off-action-of button))
      (eval (button-off-action-of button)))))

(defmethod dialog-enter-notify ((button button) &optional window state)
  (declare (Ignore window))
  (if (host-to-canvas-mouse-state state)
    (process-button-press-event button window)))

(defmethod dialog-leave-notify ((button button) &optional window)
  (declare (Ignore window))
  (if (button-alive-p button)
    (process-button-press-event button window))
  (setf (button-alive-p button) NIL))

(defmethod wb-refresh ((thing button))
  (if (button-on-p thing)
    (highlight-dialog-item thing)
    (downlight-dialog-item thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Check-boxes
;;;

(defclass check-box (button)
  ((box-size :initform 10
             :initarg :box-size
             :accessor check-box-size-of
             :documentation "The width (and length) in pixels of the check box.")
   )
  (:default-initargs :window-border-width 2)
  )

(defmethod box-checked-p ((self check-box))
  (button-on-p self))

(defmethod (setf box-checked-p) (new-value (self check-box))
  (setf (button-on-p self) new-value))

(defmethod initialize-instance :after ((self check-box)
                                       &rest initargs
                                       &key (check? NIL))
  (setf (button-on-p self) check?)
  self)
                                      
(defmethod downlight-dialog-item ((self check-box))
  (setf (box-checked-p self) NIL)
  (let* ((drawable (window-of self))
         (gcontext (gcontext-of self))
         (line-width (dialog-border-width-of self))
         (width (xlib::drawable-width drawable))
         (height (xlib::drawable-height drawable))
         )
    (xlib::with-state (drawable)
      (let ((erase-color (xlib::gcontext-background gcontext)))
            (xlib:with-gcontext (gcontext :line-width line-width
                                          :foreground erase-color)
              (xlib:draw-segments drawable gcontext
                                  (list 0 0 width height
                                        0 (- height line-width)
                                        (- width line-width) 0)))))
    ))


(defmethod highlight-dialog-item ((self check-box))
  (setf (box-checked-p self) T)
  (let* ((drawable (window-of self))
         (gcontext (gcontext-of self))
         (line-width (dialog-border-width-of self))
         (width (xlib::drawable-width drawable))
         (height (xlib::drawable-height drawable))
         )
    (xlib::with-state (drawable)
      (xlib:with-gcontext (gcontext :line-width line-width)
        (xlib:draw-segments drawable gcontext
                              (list 0 0 width height
                                    0 (- height line-width)
                                    (- width line-width) 0))
        ))))
       

(defmethod dialog-item-compute-geometry ((self check-box))
  (let* ((window (window-of self))
         (size (check-box-size-of self)))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) size
	    (xlib::drawable-height window) size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   Text-buttons
;;;

(defclass text-button (button)
  ((text        :initform "  "
                :initarg :text
                :accessor text-button-text-of)
   (text-margin :initform 3
                :initarg :margin
                :accessor text-button-margin-of)
   (font        :initform *default-menu-font*
                :initarg :font
                :accessor text-button-font-of)
   )
  )

(defun make-text-button (&rest args)
  (apply #'make-instance 'text-button args))

(defmethod downlight-dialog-item ((self text-button))
  (let* ((drawable (window-of self))
         (gcontext (gcontext-of self))
	 (font (xlib::gcontext-font gcontext))
         (text-margin (text-button-margin-of self))
         (line-width (dialog-border-width-of self))
	 (text-start-x (+ line-width text-margin))
	 (text-start-y (+ line-width text-margin (xlib::font-ascent font)))
         (width (xlib::drawable-width drawable))
         (height (xlib::drawable-height drawable))
         (colormap (xlib::window-colormap (window-of self)))
         (light-gray (xlib::alloc-color colormap *light-gray-color*))
         (dark-gray (xlib::alloc-color colormap *dark-gray-color*))
         (lw/2 (round line-width 2))
         (left-x lw/2)
         (top-y lw/2)
         (bottom-y (- height lw/2))
         (right-x (- width line-width))
         )
    (xlib::with-state (drawable)
      ;; Left and top of button
      (xlib:with-gcontext (gcontext :foreground light-gray :line-width line-width)
        (xlib:draw-lines drawable gcontext
                         (list left-x bottom-y
                               left-x top-y
                               right-x top-y )))
      ;; Bottom and right of button
      (xlib:with-gcontext (gcontext :foreground dark-gray :line-width line-width)
        (xlib:draw-lines drawable gcontext
                         (list right-x (+ top-y lw/2)
                               right-x bottom-y
                               (+ left-x lw/2) bottom-y)))
      ;; Draw the text
      (xlib::draw-image-glyphs drawable gcontext
                               text-start-x text-start-y
                               (text-button-text-of self))
      ))
  )


(defmethod highlight-dialog-item ((self text-button))
  (let* ((drawable (window-of self))
         (gcontext (gcontext-of self))
	 (font (xlib::gcontext-font gcontext))
         (text-margin (text-button-margin-of self))
         (line-width (dialog-border-width-of self))
	 (text-start-x (+ line-width text-margin))
	 (text-start-y (+ line-width text-margin (xlib::font-ascent font)))
         (width (xlib::drawable-width drawable))
         (height (xlib::drawable-height drawable))
         (colormap (xlib::window-colormap (window-of self)))
         (light-gray (xlib::alloc-color colormap *light-gray-color*))
         (dark-gray (xlib::alloc-color colormap *dark-gray-color*))
         (lw/2 (round line-width 2))
         (left-x lw/2)
         (top-y lw/2)
         (bottom-y (- height lw/2))
         (right-x (- width line-width))
         )
    (xlib::with-state (drawable)
      ;; Left and top of button
      (xlib:with-gcontext (gcontext :foreground dark-gray :line-width line-width)
        (xlib:draw-lines drawable gcontext
                         (list left-x bottom-y
                               left-x top-y
                               right-x top-y )))
      ;; Right and bottom of button
      (xlib:with-gcontext (gcontext :foreground light-gray :line-width line-width)
        (xlib:draw-lines drawable gcontext
                         (list right-x (+ top-y lw/2)
                               right-x bottom-y
                               (+ left-x lw/2) bottom-y)))
      ;; Draw the text
      (xlib::draw-image-glyphs drawable gcontext
                               text-start-x text-start-y
                               (text-button-text-of self))
      )))


(defmethod dialog-item-compute-geometry :before 
  ((self text-button))
  (let ((gcontext (gcontext-of self))
        (font (canvas-font-to-host-font
               (text-button-font-of self))))
    (setf (xlib::gcontext-font gcontext) font)))

(defmethod dialog-item-compute-geometry ((self text-button))
  (let* ((gcontext (gcontext-of self))
	 (font (xlib::gcontext-font gcontext))
	 (window (window-of self))
         (margin (text-button-margin-of self))
	 (text-width (xlib::text-extents
                      font
                      (text-button-text-of self)))
	 (text-height (+ (xlib::font-ascent font)
                         (xlib::font-descent font)))
         (line-width (dialog-border-width-of self))
	 (button-width (+ text-width (* 2 (+ line-width margin))))
	 (button-height (+ text-height (* 2 (+ line-width margin)))))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) button-width
	    (xlib::drawable-height window) button-height))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   Text-dialog-item
;;;

(defclass text-dialog-item (dialog-item)
  ((text        :initform "  "
                :initarg :text
                :accessor dialog-text-of)
   (text-margin :initform 2
                :initarg :margin
                :accessor text-margin-of)
   (font        :initform *default-menu-font*
                :initarg :font
                :accessor dialog-text-font-of)
   )
  
  )
  
(defmethod wb-refresh ((self text-dialog-item))
  (let* ((drawable (window-of self))
         (gcontext (gcontext-of self))
	 (font (xlib::gcontext-font gcontext))
         (line-width (dialog-border-width-of self))
         (text-margin (+ line-width (text-margin-of self)))
	 (text-start-x text-margin)
	 (text-start-y (+ text-margin (xlib::font-ascent font)))
         (width (xlib::drawable-width drawable))
         (height (xlib::drawable-height drawable))
         )
    ;; Draw the text (boxed if necessary).
    (xlib::with-state (drawable)
      (if (> line-width 0)
        (xlib::with-gcontext (gcontext :line-width line-width)
          (xlib::draw-rectangle drawable gcontext 0 0 width height)))
      (xlib::draw-image-glyphs drawable gcontext
                               text-start-x text-start-y
                               (dialog-text-of self))))
  )

(defmethod dialog-item-compute-geometry :before 
  ((self text-dialog-item))
  (let ((gcontext (gcontext-of self))
        (font (canvas-font-to-host-font
               (dialog-text-font-of self))))
    (setf (xlib::gcontext-font gcontext) font)))

(defmethod dialog-item-compute-geometry ((self text-dialog-item))
  (let* ((gcontext (gcontext-of self))
	 (font (xlib::gcontext-font gcontext))
	 (window (window-of self))
         (margin (text-margin-of self))
         (line-width (dialog-border-width-of self))
         (edges-width (* 2 (+ margin line-width)))
	 (text-width (xlib::text-extents
                      font
                      (dialog-text-of self)))
	 (text-height (+ (xlib::font-ascent font)
                         (xlib::font-descent font)))
	 (dialog-width (+ text-width edges-width))
	 (dialog-height (+ text-height edges-width)))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) dialog-width
	    (xlib::drawable-height window) dialog-height))))



(defmethod process-button-press-event ((self text-dialog-item) &optional window)
  (declare (ignore self window))
  )

(defmethod process-button-release-event ((self text-dialog-item) &optional window)
  (declare (ignore self window))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   Editable-text-dialog-item
;;;

(defclass editable-text-dialog-item (text-dialog-item)
  ((box-width   :initform 200
                :initarg :box-width
                :accessor text-box-width-of)
   (input-mode? :initform NIL
                :initarg :await-input?
                :accessor awaiting-input-p)
   )
  
  )
 
(defmethod wb-refresh ((self editable-text-dialog-item))
  (let* ((drawable (window-of self))
         (gcontext (gcontext-of self))
	 (font (xlib::gcontext-font gcontext))
         (line-width (dialog-border-width-of self))
         (text-margin (+ line-width (text-margin-of self)))
	 (text-start-x text-margin)
	 (text-start-y (+ text-margin (xlib::font-ascent font)))
         (width (xlib::drawable-width drawable))
         (height (xlib::drawable-height drawable))
         )
    ;; Draw the text (boxed if necessary).
    (xlib::with-state (drawable)
      (if (> line-width 0)
        (xlib::with-gcontext (gcontext :line-width line-width)
          (xlib::draw-rectangle drawable gcontext 0 0 width height)))
      (xlib::draw-image-glyphs drawable gcontext
                               text-start-x text-start-y
                               (dialog-text-of self))))
      
    )


(defmethod dialog-item-compute-geometry ((self editable-text-dialog-item))
  (let* ((gcontext (gcontext-of self))
	 (font (xlib::gcontext-font gcontext))
	 (window (window-of self))
         (margin (text-margin-of self))
         (line-width (dialog-border-width-of self))
         (edges-width (* 2 (+ margin line-width)))
	 (text-width (xlib::text-extents
                      font
                      (dialog-text-of self)))
	 (text-height (+ (xlib::font-ascent font)
                         (xlib::font-descent font)))
	 (dialog-width (max (text-box-width-of self)
                            (+ text-width edges-width)))
	 (dialog-height (+ text-height edges-width))
         )
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) dialog-width
	    (xlib::drawable-height window) dialog-height))))



(defmethod process-button-press-event ((self editable-text-dialog-item)
                                       &optional window)
  (unless window (setf window (window-of self)))
  (let* ((gcontext (gcontext-of self))
	 (width (xlib::drawable-width window))
	 (height (xlib::drawable-height window))
         )
    (setf (awaiting-input-p self) T)
    (set-dialog-input-gcontext gcontext window)
    (xlib::with-gcontext
      (gcontext :foreground (xlib::gcontext-background gcontext))
      (xlib::draw-rectangle window gcontext 0 0 width height t)
      )
    
    (wb-refresh self)
    (xlib::grab-keyboard
     window :owner-p t 
     :sync-pointer-p nil :sync-keyboard-p nil)
    )
  )

(defun set-dialog-input-gcontext (gcontext window
                                           &key
                                           (foreground *black-color*)
                                           (background *white-color*))
  (let* ((colormap (xlib::window-colormap window))
         (fore-color (xlib::alloc-color
                      colormap
                      foreground))
         (back-color (xlib::alloc-color
                      colormap
                      background))
         )
    (setf (xlib::gcontext-foreground gcontext) fore-color)
    (setf (xlib::gcontext-background gcontext) back-color)
    ))

(defun reset-dialog-gcontext (self gcontext window)
  (let* ((colormap (xlib::window-colormap window))
         (fore-color (xlib::alloc-color
                      colormap
                      (dialog-foreground-color-of self)))
         (back-color (xlib::alloc-color
                      colormap
                      (dialog-background-color-of self))))
    (setf (xlib::gcontext-foreground gcontext) fore-color)
    (setf (xlib::gcontext-background gcontext) back-color)
    ))

         
(defmethod process-button-release-event ((self editable-text-dialog-item)
                                         &optional window)
  (declare (ignore self window)))

(defmethod dialog-leave-notify ((self editable-text-dialog-item) &optional window)
  (when (awaiting-input-p self)
    (unless window (setf window (window-of self)))
    (let* ((gcontext (gcontext-of self))
	   (width (xlib::drawable-width window))
	   (height (xlib::drawable-height window))
           (line-width (dialog-border-width-of self))
           (text-margin (+ line-width (text-margin-of self)))
           (font (xlib::gcontext-font gcontext))
	   (text-start-x text-margin)
	   (text-start-y (+ text-margin (xlib::font-ascent font)))
           )
      ;; Set input mode off
      (setf (awaiting-input-p self) NIL)
      (reset-dialog-gcontext self gcontext window)
      (xlib::with-gcontext
        (gcontext :foreground (xlib::gcontext-background gcontext))
        (xlib::draw-rectangle window gcontext 0 0 width height t)
        )
      
      (wb-refresh self)
      )
    (xlib::ungrab-keyboard (xlib::drawable-display window))
    ))

(defmethod process-key-press-event ((self editable-text-dialog-item) window code state)
  (let* ((gcontext (gcontext-of self))
	 (display (xlib::drawable-display window))
	 (width (xlib::drawable-width window))
	 (height (xlib::drawable-height window))
	 (font (xlib::gcontext-font gcontext))
         (line-width (dialog-border-width-of self))
         (text-margin (+ line-width (text-margin-of self)))
	 (text-start-x text-margin)
	 (text-start-y (+ text-margin (xlib::font-ascent font)))
	 character)
    (setf character
          (xlib::keycode->character display code state))
    (when (awaiting-input-p self)
      (cond ((eq character #\return)
             (dialog-leave-notify self window)
             )
	    ((member
              (format nil "~a" character)
              '("CAPS-LOCK" "NIL" "LEFT-SHIFT"
                "RIGHT-SHIFT" "LEFT-CONTROL"
                "RIGHT-CONTROL" "LEFT-ALT"
                "RIGHT-ALT")
              :test #'string=)
             )
            (T 
             (if (dialog-text-of self)
               (setf (dialog-text-of self)
                     (format nil "~a~a" (dialog-text-of self) character))
               (setf (dialog-text-of self)
                     (format nil "~a" character)))
             (when (or (eq character #\backspace)
                       (eq character #\Rubout)
                       (eq character #\Delete))
               (let ((start 0)
                     (end (- (length (dialog-text-of self)) 2)))
                 (when (> start end) (setf end 0))
                 (setf (dialog-text-of self)
                       (subseq (dialog-text-of self) start end))))
             (xlib::with-gcontext
               (gcontext :foreground (xlib::gcontext-background gcontext)
                         :line-width 2)
               (xlib::draw-rectangle window gcontext 0 0 width height t))
             (xlib::draw-image-glyphs
              window gcontext
              text-start-x text-start-y
              (dialog-text-of self))
             (xlib::display-finish-output display)
             )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   list-dialog-item
;;;

(defclass list-dialog-item (text-dialog-item)
  ((item        :initform NIL
                :initarg :item
                :accessor dialog-item-of)
   (selected?   :initform NIL
                :initarg :selected?
                :accessor dialog-item-selected-p)
   )
  
  )
  
(defmethod wb-refresh ((self list-dialog-item))
  (if (dialog-item-selected-p self)
    (highlight-dialog-item self)
    (downlight-dialog-item self)))

(defmethod highlight-dialog-item ((self list-dialog-item))
  (let* ((drawable (window-of self))
         (gcontext (gcontext-of self))
	 (font (xlib::gcontext-font gcontext))
         (line-width (dialog-border-width-of self))
         (text-margin (+ line-width (text-margin-of self)))
	 (text-start-x text-margin)
	 (text-start-y (+ text-margin (xlib::font-ascent font)))
         (width (xlib::drawable-width drawable))
         (height (xlib::drawable-height drawable))
         (colormap (xlib::window-colormap (parent-of drawable)))
         (fore-color (xlib::alloc-color
                      colormap
                      (dialog-foreground-color-of self)))
         (back-color (xlib::alloc-color
                      colormap
                      (dialog-background-color-of self)))
         )
    ;; Draw the text (boxed if necessary).
    (xlib::with-state (drawable)   
      (if (> line-width 0)
        (xlib::with-gcontext (gcontext :line-width line-width)
          (xlib::draw-rectangle drawable gcontext 0 0 width height)))
      (xlib::with-gcontext (gcontext :background fore-color
                                       :foreground back-color)
          (xlib::draw-image-glyphs drawable gcontext
                                   text-start-x text-start-y
                                   (dialog-text-of self))))))

(defmethod downlight-dialog-item ((self list-dialog-item))
  (let* ((drawable (window-of self))
         (gcontext (gcontext-of self))
	 (font (xlib::gcontext-font gcontext))
         (line-width (dialog-border-width-of self))
         (text-margin (+ line-width (text-margin-of self)))
	 (text-start-x text-margin)
	 (text-start-y (+ text-margin (xlib::font-ascent font)))
         (width (xlib::drawable-width drawable))
         (height (xlib::drawable-height drawable))
         )
    ;; Draw the text (boxed if necessary).
    (xlib::with-state (drawable)   
      (if (> line-width 0)
        (xlib::with-gcontext (gcontext :line-width line-width)
          (xlib::draw-rectangle drawable gcontext 0 0 width height)))
      (xlib::draw-image-glyphs drawable gcontext
                               text-start-x text-start-y
                               (dialog-text-of self)))))

(defmethod process-button-press-event ((self list-dialog-item) &optional window)
  (declare (ignore self window))
  (let ((owner (owner-dialog self)))
    (cond
     ((dialog-item-selected-p self)
      (downlight-dialog-item self)
      (setf (dialog-item-selected-p self) NIL)
      (when owner
        (case (dialog-selection-type-of owner)
          ((:contiguous :disjoint)
           (setf (selected-dialog-items-of owner)
                 (remove self (selected-dialog-items-of owner))))
          (:single
           (setf (selected-dialog-items-of owner) NIL)))
        )
      )
     (T
      (highlight-dialog-item self)
      (setf (dialog-item-selected-p self) T)
      (when owner
        (case (dialog-selection-type-of owner)
          ((:contiguous :disjoint)
           (push self (selected-dialog-items-of owner)))
          (:single
           (loop for item in (selected-dialog-items-of owner)
                 do
                 (downlight-dialog-item item)
                 (setf (dialog-item-selected-p item) NIL)
                 finally
                 (setf (selected-dialog-items-of owner) (list self)))))
        )))))

(defmethod process-button-release-event ((self list-dialog-item) &optional window)
  (declare (ignore self window))
  )
        


