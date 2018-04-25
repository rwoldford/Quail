;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;                         dialog-clx.lisp
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
;;;     dialogs
;;;

(defclass dialog ()
  ((window           :initform NIL
  	             :initarg :window
	             :accessor window-of)
   (title            :initform "Dialog"
  	             :initarg :title
	             :accessor dialog-title-of)
   (gcontext         :initform NIL
	             :initarg :gcontext
	             :accessor gcontext-of)
   (dialog-x         :initform 5
	             :initarg :x
	             :accessor dialog-x-of)
   (dialog-y         :initform 5
	             :initarg :y
	             :accessor dialog-y-of)
   (background-color :initform *grey-color*
                     :initarg :background-color
                     :accessor dialog-background-color-of)
   (foreground-color :initform *black-color*
                     :initarg :foreground-color
                     :accessor dialog-foreground-color-of)
   (border-width     :initform 0
                     :initarg :border-width
                     :accessor dialog-border-width-of)
   (horizontal-space :initform 5 :initarg :horizontal-space
                     :accessor dialog-h-space)
   (vertical-space   :initform 5 :initarg :vertical-space
                     :accessor dialog-v-space)
   (text             :initform NIL
                     :initarg :text
                     :accessor dialog-text-of)
   (font             :initform *default-menu-font*
                     :initarg :font
                     :accessor dialog-font-of)
   (done?            :initform NIL
                     :accessor dialog-done-p)
   
   )
  )

(defun dialog-p (thing)
  (typep thing 'dialog))

(defun shutdown-dialog (dialog)
  (xlib::unmap-window (window-of dialog))
  (xlib::destroy-window (window-of dialog)))

(defun dialog-done (dialog)
  (setf (dialog-done-p dialog) T))

(defmethod initialize-instance :after ((self dialog)
			               &key 
			               gcontext
			               &allow-other-keys)
  
  (let* (window
         (parent (xlib::screen-root
                  (xlib::display-default-screen
                   *default-display*)))
         (colormap (xlib::window-colormap parent))
         (fore-color (xlib::alloc-color
                      colormap
                      (dialog-foreground-color-of self)))
         (back-color (xlib::alloc-color
                      colormap
                      (dialog-background-color-of self)))
         (width 16)
         (height 16)
         (title (dialog-title-of self))
         (font (canvas-font-to-host-font
                (dialog-font-of self)))
         )
    
    ;; sort out the window
    ;; First get one.
    (setf window
          (xlib::create-window
           :parent parent
           :x (dialog-x-of self)
           :y (dialog-y-of self)
           :width 16 :height 16
           :background back-color
           :override-redirect :on
           :border-width 2
           :event-mask
           (xlib::make-event-mask 
            :button-press
            :button-release	
            :key-press
            :key-release
            :exposure
            :owner-grab-button)))
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
             :drawable parent)))
    (setf (gcontext-of self) gcontext)
    (setf (xlib::gcontext-background gcontext)
          back-color)
    (setf (xlib::gcontext-foreground gcontext)
          fore-color)
    (setf (xlib::gcontext-font gcontext) font)
    
    ;; set up the title etc. ... mimics host-window-clx
    (xlib:set-wm-properties window
                            :name title
                            :icon-name title
                            :resource-name "Dialog"
                            :resource-class 'dialog
                            :x (dialog-x-of self) :y (dialog-y-of self)
                            :width width :height height
                            :min-width 10 :min-height 10
                            :input :off :initial-state :normal)

    ;; compute the interior
    (dialog-compute-interior self)
    
    ;; compute the geometry
    (dialog-compute-geometry self)
    
    ;; display the dialog
    (dialog-display self)
    
    ;; clear the event queue
    (xlib::display-finish-output *default-display*)
    
    ;; Return self
    self
    )
  )


(defgeneric dialog-compute-interior (dialog)
  (:documentation "Compute the interior for this dialog."))

(defmethod dialog-compute-interior ((self T))
  "Does nothing."
  (declare (ignore self))
  NIL)

(defgeneric dialog-compute-geometry (dialog)
  (:documentation "Compute the geometry for this dialog."))

(defmethod dialog-compute-geometry ((self T))
  "Does nothing."
  (declare (ignore self))
  NIL)

(defgeneric dialog-display (dialog)
  (:documentation "Display this dialog."))

(defmethod dialog-display ((self dialog))
  (let ((window (window-of self)))
      (xlib::map-window window)
      (xlib::map-subwindows window)))


(defgeneric dialog-execute (dialog)
  (:documentation "Run the dialog."))

(defmacro when-dialog-done ((dialog &key (timeout 180)) &body forms)
  "Let's the dialog appear and process events until it declares ~
   itself done or timeout seconds have elapsed.  It then processes ~
   the forms in the body in sequence and returns the value of the last form ~
   processed. ~
   (:required ~
   (:arg dialog The dialog that is processing events.) ~
   ) ~
   (:key ~
   (:arg timeout 180 The number of seconds to wait before processing forms ~
   anyway.  If non-NIL this should be an integer greater than zero. ~
   If NIL, then the loop exits only when dialog-done-p returns non-NIL for ~
   this dialog so there is a potential infinite loop here.))~
   (:body ~
   (:arg forms Forms to be evaluated when the dialog is done processing events.~
   ))"
  (let ((time-sym (gensym "timeout"))
        (dialog-sym (gensym "dialog")))
    `(let ((,time-sym ,timeout)
           (,dialog-sym ,dialog))
       (cond
        ((and ,time-sym (numberp ,time-sym) (> ,time-sym 0))
         (let ((start (get-internal-real-time))
               (elapsed-time))
           (loop
             (when (dialog-done-p ,dialog-sym)
               (return (progn ,@forms)))
             (setf elapsed-time
                   (/ (- (get-internal-real-time)
                         start)
                      internal-time-units-per-second))
             (when (> elapsed-time ,timeout)
               (warn "~&Dialog ~s timed out after ~s seconds. ~&~
                      Processing continued.~%"
                     ,dialog-sym elapsed-time)
               (return (progn ,@forms)))))
         )
        ((null ,time-sym)
         (loop 
           (when (dialog-done-p ,dialog-sym)
             (return (progn ,@forms)))))
        (T (error "~&Illegal timeout argument: ~s seconds.~%"
                  ,time-sym)))
       )
    ))

(defmethod process-button-press-event ((self dialog) &optional window)
  "Dialogs don't field button events."
  (declare (ignore self window))
  NIL)


(defmethod process-button-release-event ((self dialog) &optional window)
  "Dialogs don't field button events."
  (declare (ignore self window))
  NIL)

(defmethod process-key-press-event ((self dialog) window code state)
  "Dialogs don't field key events."
  (declare (ignore self window code state))
  NIL)

(defmethod wb-refresh ((self dialog))
  (let* ((gcontext (gcontext-of self))
         (border-width (dialog-border-width-of self))
         (window (window-of self))
         (h-space (dialog-h-space self))
         (v-space (dialog-v-space self))
         (font (xlib::gcontext-font gcontext))
         (font-ascent (xlib::font-ascent font))
         (text-height
          (+ font-ascent (xlib::font-descent font)))
         (title-width (xlib::text-extents font (dialog-title-of self))))
    (xlib::with-state (window)
      (xlib::draw-image-glyphs window
                               gcontext
                               (round 
                                (- (xlib::drawable-width window)
                                   title-width)
                                2)
                               (+ border-width 
                                  v-space
                                  font-ascent)
                               (dialog-title-of self))
      (xlib::draw-image-glyphs window
                               gcontext
                               (+ border-width h-space)
                               (+ border-width 
                                  v-space text-height
                                  v-space font-ascent)
                               (dialog-text-of self))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      y-or-n-dialog
;;;
;;;

(defclass y-or-n-dialog (dialog)
  ((yes-text      :initform "Yes"
                  :initarg :yes-text
                  :accessor yes-text-of)
   (no-text       :initform "No"
                  :initarg :no-text
                  :accessor no-text-of)
   (cancel-text   :initform "Cancel"
                  :initarg :cancel-text
                  :accessor cancel-text-of)
   (yes-button    :initform NIL
                  :accessor yes-button-of)
   (no-button     :initform NIL
                  :accessor no-button-of)
   (cancel-button :initform NIL
                  :accessor cancel-button-of)
   (selection     :initform T
                  :accessor y-or-n-selection-of)
   ))


(defun y-or-n-dialog (message 
		      &key
                      (yes-text "Yes")
                      (no-text "No")
                      (cancel-text "Cancel")
		      &allow-other-keys)
  (let ((d (make-instance 'y-or-n-dialog 
             :text message
             :yes-text yes-text
             :no-text no-text
             :cancel-text cancel-text)))
    (when-dialog-done (d :timeout NIL) (y-or-n-selection-of d))))

(defmethod dialog-compute-interior ((self y-or-n-dialog))
  (let ((font (dialog-font-of self))
        (text (dialog-text-of self))
        (yes-text (yes-text-of self))
        (no-text (no-text-of self))
        (cancel-text (cancel-text-of self))
        (gcontext (gcontext-of self)))
    (unless text
      (setf text
            (format NIL "~a or ~a?" yes-text no-text))
      )
    (setf (xlib::gcontext-font gcontext)
          (canvas-font-to-host-font font))
    (setf (dialog-text-of self) text
          (yes-button-of self)
          (make-text-button
           :text yes-text
           :gcontext gcontext
           :font font
           :border-width 2
           :on-action 
           #'(lambda ()
               (shutdown-dialog self)
               (setf (y-or-n-selection-of self) T)
               (dialog-done self))
           )
          (no-button-of self)
          (make-text-button
           :text no-text
           :gcontext gcontext
           :font font
           :border-width 2
           :on-action 
           #'(lambda ()
               (shutdown-dialog self)
               (setf (y-or-n-selection-of self) NIL)
               (dialog-done self)
               )
           )
          (cancel-button-of self)
          (make-text-button
           :text cancel-text
           :gcontext gcontext
           :border-width 2
           :font font
           :on-action 
           #'(lambda ()
               (shutdown-dialog self)
               (format *quail-terminal-io* "~&Cancelled.~%")
               (setf (y-or-n-selection-of self) :cancel)
               (dialog-done self)
               (throw :cancel T))
           )))
  (let* ((dw (window-of self))
         (yes-bw (window-of (yes-button-of self)))
         (no-bw (window-of (no-button-of self)))
         (cancel-bw (window-of (cancel-button-of self))))
    (xlib::reparent-window yes-bw dw 20 20)
    (xlib::reparent-window no-bw dw 20 50)
    (xlib::reparent-window cancel-bw dw 20 80))
  self)

(defmethod dialog-compute-geometry ((self y-or-n-dialog))
  (let* ((y-b (yes-button-of self))
         (n-b (no-button-of self))
         (c-b (cancel-button-of self))
         (y-bw (window-of y-b))
         (n-bw (window-of n-b))
         (c-bw (window-of c-b))
         (gcontext (gcontext-of self))
         (window (window-of self))
         (border-width (dialog-border-width-of self))
         (h-space (dialog-h-space self))
         (v-space (dialog-v-space self))
         (font (xlib::gcontext-font gcontext))
         (font-ascent (xlib::font-ascent font))
         (text-height
          (+ font-ascent (xlib::font-descent font)))
         (title-width (xlib::text-extents font (dialog-title-of self)))
         (text-width (xlib::text-extents font (dialog-text-of self)))
         (item-height (xlib::drawable-height y-bw))
         (y-width (xlib::drawable-width y-bw))
         (n-width (xlib::drawable-width n-bw))
         (c-width (xlib::drawable-width c-bw))
         (button-width (max y-width n-width c-width))
         self-width self-height button-y)
    
    (setf (xlib::drawable-width y-bw) button-width
          (xlib::drawable-width n-bw) button-width
          (xlib::drawable-width c-bw) button-width)
    (setf self-width
          (+ border-width
             h-space
             (max text-width title-width
                  (+ button-width h-space button-width h-space button-width))
             h-space
             border-width)
          self-height
          (+ border-width
             v-space
             text-height
             v-space
             text-height
             v-space
             item-height
             v-space
             border-width))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) self-width
            (xlib::drawable-height window) self-height))
    (setf button-y (- self-height
                      item-height v-space border-width))
    (xlib::with-state (y-bw)
      (setf (xlib::drawable-x y-bw) (+ h-space border-width)
            (xlib::drawable-y y-bw)
            button-y))
    (xlib::with-state (n-bw)
      (setf (xlib::drawable-x n-bw)
            (+ border-width (* 2 h-space) button-width)
            (xlib::drawable-y n-bw)
            button-y))
    (xlib::with-state (c-bw)
      (setf (xlib::drawable-x c-bw)
            (+ border-width (* 3 h-space) button-width button-width)
            (xlib::drawable-y c-bw)
            button-y))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      list-dialog
;;;
;;;

(defclass list-dialog (dialog)
  ((ok-text             :initform "OK"
                        :initarg :ok-text
                        :accessor ok-text-of)
   (cancel-text         :initform "Cancel"
                        :initarg :cancel-text
                        :accessor cancel-text-of)
   (ok-button           :initform NIL
                        :accessor ok-button-of)
   (cancel-button       :initform NIL
                        :accessor cancel-button-of)
   (selection-type      :initform :disjoint
                        :accessor dialog-selection-type-of
                        :initarg :selection-type)
   (selected-items      :initform NIL
                        :accessor selected-dialog-items-of)
   (dialog-items        :initform NIL
                        :accessor dialog-items-of)
   (list-of-items       :initform NIL
                        :initarg :items
                        :accessor list-dialog-items-of)
   (item-print-function :initform #'(lambda (x) (format nil "~a" x))
                        :initarg :item-print-function
                        :accessor dialog-item-print-function-of)
   (list-window         :initform NIL
                        :accessor dialog-list-window-of)
   ))

(defmethod dialog-display ((self list-dialog))
  (let ((window (window-of self)))
      (xlib::map-window window)
      (xlib::map-subwindows window)
      (xlib::map-subwindows (dialog-list-window-of self))
      ))

(defun select-item-from-list
       (item-list
        &key
        (title "List selection")
        (prompt-text "Please choose from the following list:")
        (select-text "OK")
        (cancel-text "Cancel")
        (item-print-function 
         #'(lambda (x) (format nil "~a" x)))
        (selection-type :single))
  (let ((d (make-instance 'list-dialog
             :title title
             :text prompt-text
             :item-print-function item-print-function
             :ok-text select-text
             :cancel-text cancel-text
             :selection-type selection-type
             :items item-list)))
    (when-dialog-done (d :timeout NIL)
      (loop for dialog-item in (selected-dialog-items-of d)
            collect
            (dialog-item-of dialog-item)))))
    



(defmethod dialog-compute-interior ((self list-dialog))
  (let* ((font (dialog-font-of self))
         (text (dialog-text-of self))
         (ok-text (ok-text-of self))
         (cancel-text (cancel-text-of self))
         (gcontext (gcontext-of self))
         (item-function (dialog-item-print-function-of self))
         (colormap (xlib::window-colormap
                    (xlib::screen-root
                     (xlib::display-default-screen
                      *default-display*))))
         (back-color (xlib::alloc-color
                      colormap
                      (dialog-background-color-of self)))
         dialog-window list-window ok-bw cancel-bw)
    (unless text
      (case (dialog-selection-type-of self)
        ((:disjoint :contiguous)
         (setf text "Please choose one or more:"))
        (:single (setf text "Please choose one:"))))
    (setf (xlib::gcontext-font gcontext)
          (canvas-font-to-host-font font))
    (setf (dialog-text-of self) text
          (ok-button-of self)
          (make-text-button
           :text ok-text
           :gcontext gcontext
           :font font
           :border-width 2
           :on-action 
           #'(lambda ()
               (shutdown-dialog self)
               (dialog-done self))
           )
          (cancel-button-of self)
          (make-text-button
           :text cancel-text
           :gcontext gcontext
           :font font
           :border-width 2
           :on-action 
           #'(lambda ()
               (shutdown-dialog self)
               (format *quail-terminal-io* "~&Cancelled.~%")
               (dialog-done self)
               (throw :cancel T))
           ))
    (setf (dialog-items-of self)
          (loop for item in (list-dialog-items-of self)
                collect
                (make-instance 'list-dialog-item
                  :font font :gcontext gcontext
                  :owner self
                  :item item
                  :text (funcall item-function item))))
    
    ;; The following sets up a default selection.
    (process-button-press-event (first (dialog-items-of self)))
    (process-button-release-event (first (dialog-items-of self)))
    
    
    (setf dialog-window (window-of self))
    (setf list-window
          (xlib::create-window
           :parent dialog-window
           :x 0 :y 0 :width 16 :height 16
           :background back-color
           :override-redirect :on
           :border-width 1 ;;(dialog-border-width-of self)
           :event-mask
           (xlib::make-event-mask 
            :button-press
            :button-release	
            :key-press
            :key-release
            :exposure
            :owner-grab-button)))
    (setf (dialog-list-window-of self) list-window)
    (loop for dialog-item in (dialog-items-of self)
          do (xlib::reparent-window 
              (window-of dialog-item)
              list-window 0 0))
    (setf ok-bw (window-of (ok-button-of self)))
    (setf cancel-bw (window-of (cancel-button-of self)))
    (xlib::reparent-window ok-bw dialog-window 0 0)
    (xlib::reparent-window cancel-bw dialog-window 0 0)
    self))

(defmethod dialog-compute-geometry ((self list-dialog))
  (let* ((y-b (ok-button-of self))
         (c-b (cancel-button-of self))
         (y-bw (window-of y-b))
         (c-bw (window-of c-b))
         (list-window (dialog-list-window-of self))
         (gcontext (gcontext-of self))
         (window (window-of self))
         (border-width (dialog-border-width-of self))
         (h-space (dialog-h-space self))
         (v-space (dialog-v-space self))
         (font (xlib::gcontext-font gcontext))
         (font-ascent (xlib::font-ascent font))
         (text-height
          (+ font-ascent (xlib::font-descent font)))
         (title-width (xlib::text-extents font (dialog-title-of self)))
         (text-width (xlib::text-extents font (dialog-text-of self)))
         (button-height (xlib::drawable-height y-bw))
         (y-width (xlib::drawable-width y-bw))
         (c-width (xlib::drawable-width c-bw))
         (button-width (max y-width c-width))
         self-width self-height button-y list-y
         list-width list-height)
    
    ;; First the interior of the list window
    (setf list-height
          (* (length (list-dialog-items-of self))
             (reduce #'(lambda (height dialog-item)
                         (max height 
                              (xlib::drawable-height
                               (window-of dialog-item))))
                     (dialog-items-of self)
                     :initial-value 0)))
    (setf list-width
          (reduce #'(lambda (width dialog-item)
                      (max width 
                           (xlib::drawable-width
                            (window-of dialog-item))))
                  (dialog-items-of self)
                  :initial-value 0))
    
    ;; sort out button-width
    (setf (xlib::drawable-width y-bw) button-width
          (xlib::drawable-width c-bw) button-width)
    ;; Now self width and height
    (setf self-width
          (+ border-width
             h-space
             (max title-width
                  text-width
                  (+ button-width h-space button-width)
                  list-width
                  )
             h-space
             border-width)
          self-height
          (+ border-width
             v-space
             text-height
             v-space
             text-height
             v-space
             list-height
             v-space
             button-height
             v-space
             border-width))
    
    (setf (xlib::drawable-width list-window)
          (- self-width border-width h-space h-space border-width)) 
    (setf (xlib::drawable-height list-window)
          list-height)
    (loop for item in (dialog-items-of self)
          with h = 0
          do
          (let* ((w (window-of item))
                 (delta (xlib::drawable-height w)))
            (setf (xlib::drawable-width w) list-width)
            (setf (xlib::drawable-y w) h)
            (incf h delta)))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) self-width
            (xlib::drawable-height window) self-height))
    
    ;; Now the locations
    (xlib::with-state (list-window)
      (setf (xlib::drawable-x list-window) (+ h-space border-width)
            (xlib::drawable-y list-window)
            (+ border-width
               v-space
               text-height
               v-space
               text-height
               v-space)))
    (setf button-y (- self-height
                      button-height v-space border-width))
    (xlib::with-state (y-bw)
      (setf (xlib::drawable-x y-bw) (+ h-space border-width)
            (xlib::drawable-y y-bw)
            button-y))
    (xlib::with-state (c-bw)
      (setf (xlib::drawable-x c-bw)
            (+ border-width h-space button-width h-space)
            (xlib::drawable-y c-bw)
            button-y))))




