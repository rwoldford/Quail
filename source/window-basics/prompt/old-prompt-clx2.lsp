;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               prompt-clx.lisp
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
;;;    N.G. Bennett 1993
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;  History:
;;;
;;;  - Construct a uniform prompting facility.
;;;

(in-package :wb)

(export  '(prompt-user save-value prompt-t-or-f prompt-true-or-false
           prompt-for-items))

(defun prompt-user (&key (type T)
                         (read-type :string)
                         (prompt-string NIL)
                         (initial-string "")
                         (ok-text "OK")
                         (cancel-text "Cancel")
                         left top
                         width
                         (height 100))
  "Prompts user for input. ~
   (:key ~
   (:arg Prompt-string NIL The string used to query the user.) ~
   (:arg initial-string \"\" Appears to the user as the default answer.) ~
   (:arg ok-text \"OK\" String used to prompt user for confirmation.) ~
   (:arg cancel-text \"Cancel\" String used to prompt user for cancellation.) ~
   (:arg type T  Data type of item to be returned by user.  Any Common Lisp ~
   data-type is acceptable.) ~
   (:arg read-type :string  How to read the typed input.  ~
   Legal values are :string :eval or :read.) ~
   (:arg left NIL  Location of the left of the prompt window.) ~
   (:arg top NIL  Location of the top of the prompt window.) ~
   (:arg width NIL  Width of prompt window.) ~
   (:arg height 100  Height of prompt window.) ~
   )"
  (declare (ignore left top width height))
  (setq prompt-string
        (if prompt-string
          (if (stringp prompt-string)
            (format NIL prompt-string)
            (format NIL "~a" prompt-string))
          (format nil "Please enter a ~S" type)))
  (let* ((result (get-string-from-user 
                  prompt-string
                  :initial-string initial-string
                  :ok-text ok-text
                  :cancel-text cancel-text)))
    (setf result
          (case read-type
            (:string result)
            (:eval (eval (read-from-string result :preserve-whitespace t)))
            (:read (read-from-string result :preserve-whitespace t))
            )
          )
    (if (typep result type)
      result
      (quail-error "~S is not of type ~S" result type))))

(defun save-value (arg)
  "Saves the value of arg as the value of a symbol retrieved from a prompt."
  (eval
   `(setf ,(prompt-user  :read-type :read
                         :type 'symbol
                         :prompt-string
                         "Give a symbol to store the value on:")
          ,arg)))

(defun prompt-t-or-f (message
                      &key
                      (true-text "Yes")
                      (false-text "No")
                      (cancel-text "Cancel"))
  "Prompts the user to make a decision between two alternatives (true-text and false-text) ~
   in response to the information given in message.  T is returned if the first letter ~
   of the true-text (default the string Yes) is typed by the user; NIL is returned if the first letter ~
   of the false-text (default the string No) is typed by the user.  ~
   If cancelled by the user, processing ends. ~
   (:see-also prompt-for-items pick-one prompt-true-or-false)"
  (y-or-n-dialog message
                 :yes-text true-text
                 :no-text false-text
                 :cancel-text cancel-text))

(defun prompt-true-or-false (message
                             &key
                             (true-text "Yes")
                             (false-text "No")
                             (cancel-text "Cancel"))
  "Prompts the user to make a decision between two alternatives (true-text and false-text) ~
   in response to the information given in message.  T is returned if the ~
   true-text (default the string Yes) is typed by the user; NIL is returned if the ~
   false-text (default the string No) is typed by the user.  ~
   If cancelled by the user, processing ends. ~
   (:see-also prompt-for-items pick-one prompt-t-or-f)"
  (prompt-t-or-f message
                 :yes-text true-text
                 :no-text false-text
                 :cancel-text cancel-text))

(defun prompt-for-items (list
                         &key (prompt-text "Choose")
                         (item-function NIL)
                         (selection-type :single))
  "Prompts user to select one or more items from the list.  ~
   Prompt-text is displayed to help the user choose.  ~
   Selection-type is one of :single (indicating only a single item ~
   can be selected), :contiguous  (indicating only contiguous items ~
   can be selected), and :disjoint (indicating that any subset of the items ~
   can be selected).  It returns a list of all items selected, NIL if no items ~
   were selected.  ~
   Item-print-function is the function called on each item to display it in ~
   selection table. ~
   (:see-also pick-one prompt-t-or-f)"
  (if (functionp item-function)
    (select-item-from-list list
                           :window-title prompt-text
                           :table-print-function 
                           #'(lambda(x s) 
			       (format nil "~a" (funcall item-function x)))
                           :selection-type selection-type)
    (select-item-from-list list
                           :window-title prompt-text
                           :selection-type selection-type)))

(defclass list-dialog ()
  ((window :initform NIL
  	   :initarg :window
	   :accessor list-dialog-window-of)
   (gcontext :initform NIL
	     :initarg :gcontext
	     :accessor list-dialog-gcontext-of)
   (title :initform NIL
	  :initarg :title
	  :accessor list-dialog-title-of)
   (selection-type :initform NIL
		   :initarg :selection-type
		   :accessor list-dialog-selection-type-of)
   (items :initform NIL
          :initarg :items
          :accessor list-dialog-items-of)
   (ok-button :initform NIL
	      :initarg :ok-button
	      :accessor list-dialog-ok-button-of)
   (cancel-button :initform NIL
		  :initarg :cancel-button
		  :accessor list-dialog-cancel-button-of)))

(defclass list-dialog-item ()
  (
   (window :initform NIL
	   :initarg :window
	   :accessor list-dialog-item-window-of)
   (gcontext :initform NIL
	     :initarg :gcontext
	     :accessor list-dialog-item-gcontext-of)
   (item :initform NIL
	 :initarg :item
	 :accessor list-dialog-item-item-of)
   (text  :initform NIL
	  :initarg :text
	  :accessor list-dialog-item-text-of)
   (selected-p :initform NIL
	       :initarg :selected-p
	       :accessor list-dialog-item-selected-p)))

(defclass y-or-n-dialog ()
  (
   (window :initform NIL
           :initarg :window
           :accessor y-or-n-dialog-window-of)
   (gcontext :initform NIL
             :initarg :gcontext
             :accessor y-or-n-dialog-gcontext-of)
   (text :initform NIL
	 :initarg :text
	 :accessor y-or-n-dialog-text-of)
   (yes-button :initform NIL
               :initarg :yes-button
               :accessor y-or-n-dialog-yes-button-of)
   (no-button :initform NIL
              :initarg :no-text
              :accessor y-or-n-dialog-no-button-of)
   (cancel-button :initform NIL
                  :initarg :cancel-text
                  :accessor y-or-n-dialog-cancel-button-of)))

(defclass text-dialog ()
  (
   (window :initform NIL
           :initarg :window
           :accessor text-dialog-window-of)
   (text   :initform NIL
           :initarg :text
           :accessor text-dialog-text-of)
   (title  :initform NIL
           :initarg :title
           :accessor text-dialog-title-of)
   (item   :initform NIL
	   :initarg :item
	   :accessor text-dialog-item-of)
   (title-window :initform NIL
                 :initarg :title-window
                 :accessor text-dialog-title-window-of)
   (gcontext :initform NIL
             :initarg :gcontext
             :accessor text-dialog-gcontext-of)
   (action :initform NIL
           :initarg :action
           :accessor text-dialog-action-of)))

(defclass check-dialog ()
  (
   (window :initform NIL
	   :initarg :window
	   :accessor check-dialog-window-of)
   (text   :initform NIL
	   :initarg :text
	   :accessor check-dialog-text-of)
   (item   :initform NIL
	   :initarg :item
	   :accessor check-dialog-item-of)
   (gcontext :initform NIL
	     :initarg :gcontext
	     :accessor check-dialog-gcontext-of)
   (t-or-nil :initform NIL
	     :initarg :t-or-nil
	     :accessor check-dialog-t-or-nil-of)
   (action :initform NIL
	   :initarg :action
	   :accessor check-dialog-action-of)
   (button :initform NIL
	   :initarg :button
	   :accessor check-dialog-button-of)))

(defclass dialog ()
  (
   (title :initform NIL
	  :initarg :title
	  :accessor dialog-title)
   (window :initform NIL
	   :initarg :window
	   :accessor dialog-window-of)
   (gcontext :initform NIL
	     :initarg :gcontext
	     :accessor dialog-gcontext-of)
   (ok-button :initform NIL 
	      :initarg :ok-button 
	      :accessor dialog-ok-button)
   (cancel-button :initform NIL 
	          :initarg :cancel-button 
	          :accessor dialog-cancel-button)
   (text-dialogs :initform NIL
	         :initarg :text-dialogs
		 :accessor dialog-text-dialogs)
   (check-dialogs :initform NIL
		  :initarg :check-dialogs
		  :accessor dialog-check-dialogs)))

(defmethod initialize-instance :after ((self dialog)
			               &key 
				       (font-name "fixed")
				       (title "A Dialog")
				       (check-dialogs NIL)
				       (text-dialogs NIL)
				       (ok-text "OK")
				       (cancel-text "CANCEL"))
  (declare (special *default-display*))
  (let* ((screen (xlib::display-default-screen *default-display*))
	 (black (xlib::screen-black-pixel screen))
	 (white (xlib::screen-white-pixel screen))
	 (parent (xlib::screen-root screen))
	 (font (xlib::open-font *default-display* font-name)))
    (setf (dialog-window-of self) 
          (xlib::create-window :parent parent 
		               :x 0 :y 0 :width 16 :height 16
		               :background white
		               :border-width 2
		               :override-redirect :on
		               :event-mask (xlib::make-event-mask 
                                            :exposure
                                            :button-press
                                            :button-release))
          (xlib::window-plist (dialog-window-of self))
	  (acons :wb-class-instance self NIL)
          (dialog-gcontext-of self)
          (xlib::create-gcontext :drawable parent
		                 :foreground black
		                 :background white
		                 :font font)
          (dialog-title self) title
          (dialog-ok-button self)
          (make-button :text ok-text
                       :gcontext (dialog-gcontext-of self)
                       :action :OK)
          (dialog-cancel-button self)
          (make-button :text cancel-text
                       :gcontext (dialog-gcontext-of self)
                       :action :CANCEL))
    (let ((dw (dialog-window-of self))
	  (ok-bw (button-window-of (dialog-ok-button self)))
	  (cancel-bw (button-window-of (dialog-cancel-button self))))
      (setf (dialog-check-dialogs self) NIL)
      (setf (dialog-text-dialogs self) NIL)
      (dolist (check-dialog check-dialogs)
        (setf (dialog-check-dialogs self)
              (append (dialog-check-dialogs self) 
                      (list (make-check-dialog ;;:display *default-display* 
                                               :text (car check-dialog)
                                               :t-or-nil (cadr check-dialog)
                                               :action NIL
                                               :gcontext (dialog-gcontext-of self)))))
	(let* ((curr-d (car (last (dialog-check-dialogs self)))))
	  (xlib::reparent-window (check-dialog-window-of curr-d)
                                 dw 0 0)
	  (xlib::reparent-window
           (button-window-of (check-dialog-button-of curr-d))
           dw 0 0)))
      (dolist (text-dialog text-dialogs)
	(setf (dialog-text-dialogs self)
              (append (dialog-text-dialogs self)
                      (list (make-text-dialog ;;:display *default-display*
                                              :title (car text-dialog)
                                              :text (cadr text-dialog)
                                              :action NIL
                                              :gcontext (dialog-gcontext-of self)))))
	(let* ((curr-d (car (last (dialog-text-dialogs self)))))
	  (xlib::reparent-window (text-dialog-window-of curr-d) dw 0 0)
	  (xlib::reparent-window (text-dialog-title-window-of curr-d) dw 0 0)))
      (xlib::reparent-window ok-bw dw 20 20)
      (xlib::reparent-window cancel-bw dw 20 50)
      (setf (xlib::window-plist ok-bw)
	    (acons :associated-with self (xlib::window-plist ok-bw))
	    (xlib::window-plist cancel-bw)
	    (acons :associated-with self (xlib::window-plist cancel-bw)))
      (dialog-compute-geometry self)
      (xlib::map-window dw)
      (xlib::with-state (dw)
        (setf (xlib::drawable-x dw) 300
              (xlib::drawable-y dw) 300))
      (xlib::map-subwindows dw)))
  self)

(defun max-check-dialog-width (check-dialogs)
  (let ((max-width 0))
    (dolist (check-dialog check-dialogs)
      (setf max-width 
	    (max max-width (when check-dialog
                             (+ (* 4 *button-margin*)
                                (xlib:drawable-width (check-dialog-window-of check-dialog))
                                (xlib:drawable-width (button-window-of (check-dialog-button-of check-dialog))))))))
    max-width))

(defun max-text-dialog-width (text-dialogs)
  (let ((max-width 0))
    (dolist (text-dialog text-dialogs)
      (setf max-width
	    (max max-width (when text-dialog
	                     (+ (* 4 *button-margin*)
	                        (xlib:drawable-width (text-dialog-window-of text-dialog))
	                        (xlib:drawable-width (text-dialog-title-window-of text-dialog)))))))
    max-width))

(defun max-text-dialog-title-width (text-dialogs)
  (let ((max-width 0))
    (dolist (text-dialog text-dialogs)
      (setf max-width
            (max max-width (when text-dialog
                             (+ (* 4 *button-margin*)
                                (xlib:drawable-width (text-dialog-title-window-of text-dialog)))))))
    max-width))

(defun dialog-compute-geometry (dialog)
  (let* ((gcontext (dialog-gcontext-of dialog))
	 (window (dialog-window-of dialog))
         (font (xlib::gcontext-font gcontext))
	 (title-width (xlib::text-extents font (dialog-title dialog)))
	 (check-dialogs (dialog-check-dialogs dialog))
	 (text-dialogs (dialog-text-dialogs dialog))
	 (max-t-d-width (max-text-dialog-width text-dialogs))
	 (max-t-t-width (max-text-dialog-title-width text-dialogs))
	 (max-c-d-width (max-check-dialog-width check-dialogs))
	 (item-height (xlib::drawable-height (button-window-of (dialog-ok-button dialog))))
	 (ok-width (xlib::drawable-width (button-window-of (dialog-ok-button dialog))))
	 (cancel-width (xlib::drawable-width (button-window-of (dialog-cancel-button dialog))))
	 dialog-width dialog-height)
    (setf dialog-width (+ (max title-width (+ (* 4 *button-margin*) ok-width cancel-width)
			       max-c-d-width max-t-d-width) (* 2 *button-margin*))
	  dialog-height (* (+ item-height (* 2 *button-margin*)) (+ 2 (length check-dialogs) (length text-dialogs))))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) dialog-width
	    (xlib::drawable-height window) dialog-height))
    (let* ((next-top (+ item-height (* 2 *button-margin*)))
	   (increment next-top))
      (dolist (check-dialog check-dialogs)
	(let ((dw (check-dialog-window-of check-dialog))
	      (bw (button-window-of (check-dialog-button-of check-dialog))))
	  (xlib::with-state (dw)
	    (setf (xlib::drawable-x dw) *button-margin*
		  (xlib::drawable-y dw) next-top))
          (xlib::with-state (bw)
	    (setf (xlib::drawable-x bw) (- max-c-d-width 
					   (xlib::drawable-width bw) 
					   *button-margin*)
		  (xlib::drawable-y bw) next-top))
	  (incf next-top increment)))
      (dolist (text-dialog text-dialogs)
	(let ((dw (text-dialog-window-of text-dialog))
	      (tw (text-dialog-title-window-of text-dialog)))
	  (xlib::with-state (tw)
	    (setf (xlib::drawable-x tw) *button-margin*
		  (xlib::drawable-y tw) next-top))
	  (xlib::with-state (dw)
	    (setf (xlib::drawable-x dw) (+ (* 3 *button-margin*) max-t-t-width)
		  (xlib::drawable-y dw) next-top))
	  (incf next-top increment))))
    (let ((ok-window (button-window-of (dialog-ok-button dialog)))
	  (cancel-window (button-window-of (dialog-cancel-button dialog))))
      (xlib::with-state (ok-window)
	(setf (xlib::drawable-x ok-window) *button-margin*
	      (xlib::drawable-y ok-window) (- (xlib::drawable-height window)
					      (xlib::drawable-height ok-window)
					      *button-margin*)))
      (xlib::with-state (cancel-window)
	(setf (xlib::drawable-x cancel-window) (+ (* 3 *button-margin*) (xlib::drawable-width ok-window))
              (xlib::drawable-y cancel-window) (- (xlib::drawable-height window)
						  (xlib::drawable-height cancel-window)
						  *button-margin*))))))

(defclass button ()
  ((window :initform NIL
	   :initarg :window
	   :accessor button-window-of)
   (text :initform NIL
	 :initarg :text
	 :accessor button-text-of)
   (gcontext :initform NIL
	     :initarg :gcontext
	     :accessor button-gcontext-of)
   (action :initform NIL
	   :initarg :action
	   :accessor button-action)))

(defun button-p (thing)
  (typep thing 'button))

(defun dialog-p (thing)
  (typep thing 'dialog))

(defun y-or-n-dialog-p (thing)
  (typep thing 'y-or-n-dialog))

(defmethod wb-refresh ((self button))
  (let* ((gcontext (button-gcontext-of self))
	 (window (button-window-of self))
	 (font (xlib::gcontext-font gcontext))
	 (text-start-x *button-margin*)
	 (text-start-y (+ *button-margin* (xlib::font-ascent font))))
    (xlib::draw-image-glyphs window gcontext text-start-x text-start-y (button-text-of self))))

(defmethod wb-refresh ((self dialog))
  (let* ((gcontext (dialog-gcontext-of self))
	 (window (dialog-window-of self))
	 (font (xlib::gcontext-font gcontext))
	 (window-width (xlib::drawable-width window))
	 (title (dialog-title self))
	 (title-width (xlib::text-extents font title))
	 (text-start-x (round (- (/ window-width 2) (/ title-width 2))))
	 (text-start-y (+ *button-margin* (xlib::font-ascent font))))
    (xlib::draw-image-glyphs window gcontext text-start-x text-start-y title)))

(defmethod wb-refresh ((self y-or-n-dialog))
  (let* ((gcontext (y-or-n-dialog-gcontext-of self))
         (window (y-or-n-dialog-window-of self))
         (font (xlib::gcontext-font gcontext))
         (window-width (xlib::drawable-width window))
         (title (y-or-n-dialog-text-of self))
         (title-width (xlib::text-extents font title))
         (text-start-x (round (- (/ window-width 2) (/ title-width 2))))
         (text-start-y (+ *button-margin* (xlib::font-ascent font))))
    (xlib::draw-image-glyphs window gcontext text-start-x text-start-y title)))

(defmethod wb-refresh ((self check-dialog))
  (let* ((gcontext (check-dialog-gcontext-of self))
	 (window (check-dialog-window-of self))
	 (font (xlib::gcontext-font gcontext))
	 (text (check-dialog-text-of self))
	 (text-start-x *button-margin*)
	 (text-start-y (+ *button-margin* (xlib::font-ascent font))))
    (xlib::draw-image-glyphs window gcontext text-start-x text-start-y text)))

(defmethod wb-refresh ((self text-dialog))
  (let* ((gcontext (text-dialog-gcontext-of self))
	 (window (text-dialog-window-of self))
	 (title-window (text-dialog-title-window-of self))
	 (font (xlib::gcontext-font gcontext))
	 (text (text-dialog-text-of self))
	 (title (text-dialog-title-of self))
	 (text-start-x *button-margin*)
	 (text-start-y (+ *button-margin* (xlib::font-ascent font))))
    (xlib::draw-image-glyphs window gcontext text-start-x text-start-y text)
    (xlib::draw-image-glyphs title-window gcontext text-start-x text-start-y title)))


(defmethod initialize-instance :after ((self button)
			               &key 
				       ;;(display NIL)
				       (text "OK")
			               gcontext
				       (action NIL)
			               &allow-other-keys)
  ;;(unless display (setf display (open-default-host-display)))
  (setf (button-window-of self) 
	(xlib::create-window
         :parent (xlib::screen-root
                  (xlib::display-default-screen
                   *default-display*))
         :x 0 :y 0 :width 16 :height 16
         :background (xlib::screen-white-pixel
                      (xlib::display-default-screen
                       *default-display*))
         :override-redirect :on
         :border-width 2
         :event-mask (xlib::make-event-mask 
                      :button-press
                      :button-release
                      :exposure
                      :owner-grab-button))
	(xlib::window-plist (button-window-of self))
        (acons :wb-class-instance self NIL)
	(button-gcontext-of self) gcontext
	(button-action self) action
	(button-text-of self) text)
  (button-compute-geometry self)
  self)

(defun button-compute-geometry (button)
  (let* ((gcontext (button-gcontext-of button))
	 (font (xlib::gcontext-font gcontext))
	 (window (button-window-of button))
	 (text-width (xlib::text-extents font
                                         (button-text-of button)))
	 (text-height (+ (xlib::font-ascent font)
                         (xlib::font-descent font)))
	 (button-width (+ text-width (* 2 *button-margin*)))
	 (button-height (+ text-height (* 2 *button-margin*))))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) button-width
	    (xlib::drawable-height window) button-height))))

(defvar *button-margin* 4)

(defun make-button (&rest args)
  (apply #'make-instance 'button args))

(defun make-dialog (&rest args)
  (apply #'make-instance 'dialog args))

(defun make-check-dialog (&rest args)
  (apply #'make-instance 'check-dialog args))

(defun make-y-or-n-dialog (&rest args)
  (apply #'make-instance 'y-or-n-dialog args))

(defmethod initialize-instance :after
           ((self check-dialog)
            &key
            ;;(display NIL)
            text
            gcontext
            (t-or-nil NIL)
            (action NIL)
            &allow-other-keys)
  (declare (special *default-display*))
  (let ((display *default-display*)) ;;(setf display (open-default-host-display)))
    (setf (check-dialog-window-of self)
	  (xlib::create-window
           :parent (xlib::screen-root
                    (xlib::display-default-screen display))
           :x 0 :y 0 :width 16 :height 16
           :background (xlib::screen-white-pixel
                        (xlib::display-default-screen display))
           :override-redirect :on
           :border-width 0
           :event-mask (xlib::make-event-mask 
                        :exposure))
	  (xlib::window-plist (check-dialog-window-of self))
          (acons :wb-class-instance self NIL)
	  (check-dialog-gcontext-of self) gcontext
	  (check-dialog-action-of self) action
	  (check-dialog-text-of self) (format nil "~a" text)
          (check-dialog-item-of self) text
	  (check-dialog-t-or-nil-of self) t-or-nil
	  (check-dialog-button-of self)
          (make-button :display display
                       :text (format nil "~3a" t-or-nil)
                       :gcontext gcontext
                       :action NIL)
	  (xlib::window-plist
           (button-window-of (check-dialog-button-of self)))
	  (acons :associated-with self
                 (xlib::window-plist 
                  (button-window-of
                   (check-dialog-button-of self)))))
    (check-dialog-compute-geometry self)
    self))

(defun get-association (window)
  (cdr (assoc :associated-with (xlib::window-plist window))))

(defun check-dialog-p (thing)
  (typep thing 'check-dialog)) 

(defun check-dialog-compute-geometry (check-dialog)
  (let* ((window (check-dialog-window-of check-dialog))
	 (gcontext (check-dialog-gcontext-of check-dialog))
	 (font (xlib::gcontext-font gcontext))
	 (width
          (+ (* 2 *button-margin*) 
             (xlib::text-extents
              font
              (check-dialog-text-of check-dialog))))
	 (height (+ (* 2 *button-margin*)
                    (xlib::font-ascent font)
                    (xlib::font-descent font))))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) width
	    (xlib::drawable-height window) height))))

(defmethod initialize-instance
           :after ((self text-dialog)
                   &key
                   ;;(display NIL)
                   (text "")
                   title
                   gcontext
                   (action NIL)
                   &allow-other-keys)
  (declare (special *default-display*))
  (let ((display *default-display*))
    ;;(unless display (setf display (open-default-host-display)))
    (setf (text-dialog-window-of self)
	  (xlib::create-window
           :parent (xlib::screen-root
                    (xlib::display-default-screen display))
           :x 0 :y 0 :width 16 :height 16
           :background 
           (xlib::screen-white-pixel
            (xlib::display-default-screen display))
           :override-redirect :on
           :border-width 2
           :event-mask
           (xlib::make-event-mask 
            :button-press
            :button-release	
            :key-press
            :key-release
            :exposure
            :owner-grab-button))
	  (xlib::window-plist (text-dialog-window-of self))
          (acons :wb-class-instance self NIL)
	  (text-dialog-title-window-of self)
	  (xlib::create-window 
           :parent (xlib::screen-root 
                    (xlib::display-default-screen display))
           :x 0 :y 0 :width 16 :height 16
           :background
           (xlib::screen-white-pixel
            (xlib::display-default-screen display))
           :override-redirect :on
           :border-width 0
           :event-mask (xlib::make-event-mask 
                        :exposure
                        :owner-grab-button))
	  (xlib::window-plist
           (text-dialog-title-window-of self))
          (acons :wb-class-instance self NIL)
	  (text-dialog-gcontext-of self) gcontext
	  (text-dialog-action-of self) action
	  (text-dialog-title-of self) (format NIL "~a" title)
	  (text-dialog-item-of self) title
	  (text-dialog-text-of self) (format NIL "~a" text))
    (text-dialog-compute-geometry self)
    self))

(defun make-text-dialog (&rest args)
  (apply #'make-instance 'text-dialog args))

(defun text-dialog-compute-geometry (text-dialog)
  (let* ((window (text-dialog-window-of text-dialog))
	 (title-window (text-dialog-title-window-of text-dialog))
	 (gcontext (text-dialog-gcontext-of text-dialog))
	 (text (text-dialog-text-of text-dialog))
	 (title (text-dialog-title-of text-dialog))
	 (font (xlib::gcontext-font gcontext))
	 (title-width (+ (* 2 *button-margin*) 
                         (xlib::text-extents font title)))
	 (height (+ (* 2 *button-margin*) 
                    (xlib::font-ascent font)
                    (xlib::font-descent font)))
	 (width (max (xlib::text-extents font text) 
                     (* 15 (xlib::text-extents font " ")))))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) width
	    (xlib::drawable-height window) height))
    (xlib::with-state (title-window)
      (setf (xlib::drawable-width title-window) title-width
	    (xlib::drawable-height title-window) height))))

(defmethod process-button-press-event ((c button) window root-x root-y)
  (let* ((width (xlib::drawable-width window))
         (height (xlib::drawable-height window))
         (gcontext (button-gcontext-of c))
         (bg (xlib::gcontext-foreground gcontext))
         (fg (xlib::gcontext-background gcontext))
	 (result NIL)
	 (action NIL)
         (association (get-association window)))
    (when (check-dialog-p association)
      (setf (button-text-of c)
            (if (string= (button-text-of c) "T  ")
              "NIL" "T  "))
      (setf (check-dialog-t-or-nil-of association)
            (not (check-dialog-t-or-nil-of association))))
    (xlib::with-gcontext (gcontext :function *host-bic-mode*
                                   :foreground fg :background bg)
      (xlib::draw-rectangle window gcontext 0 0 width height t)
      (wb-refresh c))
    (setf result (if (or 
                      (dialog-p association)
                      (y-or-n-dialog-p association)
                      (list-dialog-p association)) T NIL))
    (when (or 
           (dialog-p association)
           (y-or-n-dialog-p association)
           (list-dialog-p association))
      (setf action (button-action c)))
    (values result action)))

(defmethod process-button-press-event ((c text-dialog) window root-x root-y)
  (let ((gcontext (text-dialog-gcontext-of c))
	(width (xlib::drawable-width window))
	(height (xlib::drawable-height window)))
    (xlib::with-gcontext
      (gcontext :foreground (xlib::gcontext-background gcontext))
      (xlib::draw-rectangle window gcontext 0 0 width height t))
    (setf (text-dialog-text-of c) nil)
    (xlib::grab-keyboard
     window :owner-p t 
     :sync-pointer-p nil :sync-keyboard-p nil)
    NIL))

(defmethod process-button-press-event ((c dialog) w init-x init-y)
  (let ((offset-x (- init-x (xlib::drawable-x w)))
        (offset-y (- init-y (xlib::drawable-y w)))
        (display (xlib::drawable-display w)))
    (xlib::event-case
     (display :discard-p t :force-output-p t)
     (:button-release
      (window root-x root-y)
      (when (eq w window)
        (xlib::with-state (w)
          (setf (xlib::drawable-x w) (- root-x offset-x)
                (xlib::drawable-y w) (- root-y offset-y))))
      (if (eq w window) T NIL))
     (otherwise () NIL)))
  NIL)

(defmethod process-button-press-event ((c y-or-n-dialog) w init-x init-y)
  (let ((offset-x (- init-x (xlib::drawable-x w)))
        (offset-y (- init-y (xlib::drawable-y w)))
        (display (xlib::drawable-display w)))
    (xlib::event-case
     (display :discard-p t :force-output-p t)
     (:button-release
      (window root-x root-y)
      (when (eq w window)
        (xlib::with-state (w)
          (setf (xlib::drawable-x w) (- root-x offset-x)
                (xlib::drawable-y w) (- root-y offset-y))))
      (if (eq w window) T NIL))
     (otherwise () NIL)))
  NIL)

(defmethod process-button-press-event ((c list-dialog) w x y)
  (let ((offset-x (- x (xlib::drawable-x w)))
	(offset-y (- y (xlib::drawable-y w)))
	(display (xlib::drawable-display w)))
    (xlib::event-case
     (display :discard-p t :force-output-p t)
     (:button-release
      (window root-x root-y)
      (when (eq w window)
        (xlib::with-state (w)
          (setf (xlib::drawable-x w) (- root-x offset-x)
                (xlib::drawable-y w) (- root-y offset-y))))
      (if (eq w window) T NIL))
     (otherwise () NIL)))
  NIL)

(defun any-item-selected-p (list-dialog)
  (let ((items (list-dialog-items-of list-dialog)))
    (loop for item in items
          when (list-dialog-item-selected-p item) return item
          finally (return NIL))))

(defun previous-item-selected-p (list-dialog item)
  (let* ((items (list-dialog-items-of list-dialog))
	 (prev-pos (- (position item items) 1))
	 previous)
    (when (< prev-pos 0) (setf prev-pos (- (length items) 1)))
    (setf previous (nth prev-pos items))
    (list-dialog-item-selected-p previous)))

(defun next-item-selected-p (list-dialog item)
  (let* ((items (list-dialog-items-of list-dialog))
	 (next-pos (+ 1 (position item items)))
	 next)
    (when (> next-pos (- (length items) 1)) (setf next-pos 0))
    (setf next (nth next-pos items))
    (list-dialog-item-selected-p next)))

(defmethod process-button-press-event ((c list-dialog-item) w x y)
  (let* ((l-d (get-association w))
	 (gcontext (list-dialog-item-gcontext-of c))
	 (width (xlib::drawable-width w))
	 (height (xlib::drawable-height w))
	 (fg (xlib::gcontext-foreground gcontext))
	 (bg (xlib::gcontext-background gcontext))
	 (selected (list-dialog-item-selected-p c))
	 (select-type (list-dialog-selection-type-of l-d))
	 (items (list-dialog-items-of l-d))
	 (any-selected (any-item-selected-p l-d))
	 (prev-selected (previous-item-selected-p l-d c))
	 (next-selected (next-item-selected-p l-d c))
	 (select NIL))
    (if selected
      (progn
        (xlib::with-gcontext
          (gcontext :foreground bg :background fg)
          (xlib::draw-rectangle w gcontext 0 0 width height t))
        (wb-refresh c)
        (setf (list-dialog-item-selected-p c) NIL)
        NIL)
      (progn
        (cond
         ((eq select-type :single) 
          (if any-selected 
            (let*
              ((a-w (list-dialog-item-window-of any-selected))
               (a-width (xlib::drawable-width a-w))
               (a-height (xlib::drawable-height a-w))
               (agc (list-dialog-item-gcontext-of any-selected)))
              (setf (list-dialog-item-selected-p any-selected) NIL)
              (xlib::with-gcontext (agc :foreground bg :background fg)
                (xlib::draw-rectangle a-w agc 0 0 a-width a-height t))
              (wb-refresh any-selected)
              (setf select T))
            (setf select T)))
         ((eq select-type :conjoint)
          (if any-selected
            (when (or prev-selected next-selected)
              (setf select T))
            (setf select T)))
         ((eq select-type :disjoint) (setf select T)))
        (when select
          (setf (list-dialog-item-selected-p c) T)
          (wb-refresh c)))))
  NIL)

(defmethod process-button-release-event ((c list-dialog-item) window))

(defmethod process-button-release-event ((c button) event-window)
  (let* ((w (xlib::drawable-width event-window))
         (h (xlib::drawable-height event-window))
         (gcontext (button-gcontext-of c))
         (fg (xlib::gcontext-background gcontext))
         (bg (xlib::gcontext-foreground gcontext))
         (association (get-association event-window)))
    (xlib::with-gcontext (gcontext :foreground fg :background bg)
      (xlib::draw-rectangle event-window gcontext 0 0 w h t))
    (wb-refresh c)
    NIL))

(defmethod process-button-release-event ((c text-dialog) event-window) NIL)

(defmethod process-button-release-event ((c dialog) event-window) NIL)

(defmethod process-key-press-event ((c text-dialog) window code state)
  (let* ((gcontext (text-dialog-gcontext-of c))
	 (display (xlib::drawable-display window))
	 (width (xlib::drawable-width window))
	 (height (xlib::drawable-height window))
	 (font (xlib::gcontext-font gcontext))
	 (text-start-x *button-margin*)
	 (text-start-y (+ *button-margin* (xlib::font-ascent font)))
	 character)
    (setf character (xlib::keycode->character display code state))
    (cond ((eq character #\return) (progn (xlib::ungrab-keyboard display) NIL))
	  ((member
            (format nil "~a" character)
            '("CAPS-LOCK" "NIL" "LEFT-SHIFT"
              "RIGHT-SHIFT" "LEFT-CONTROL"
              "RIGHT-CONTROL" "LEFT-ALT"
              "RIGHT-ALT")
            :test #'string=) NIL)
          (T (progn
	       (if (text-dialog-text-of c)
                 (setf (text-dialog-text-of c)
                       (format nil "~a~a" (text-dialog-text-of c) character))
	         (setf (text-dialog-text-of c)
                       (format nil "~a" character)))
	       (when (eq character #\backspace)
		 (let ((start 0)
		       (end (- (length (text-dialog-text-of c)) 2)))
		   (when (> start end) (setf end 0))
		   (setf (text-dialog-text-of c)
                         (subseq (text-dialog-text-of c) start end))))
	       (xlib::with-gcontext
                 (gcontext :foreground (xlib::gcontext-background gcontext))
	         (xlib::draw-rectangle window gcontext 0 0 width height t))
	       (xlib::draw-image-glyphs
                window gcontext
                text-start-x text-start-y
                (text-dialog-text-of c))
	       (xlib::display-finish-output display)
               NIL)))))

(defmethod host-window ((self dialog))
  (dialog-window-of self))

(defmethod host-window ((self y-or-n-dialog))
  (y-or-n-dialog-window-of self))

(defmethod host-window ((self list-dialog))
  (list-dialog-window-of self))

(defmethod cancel-button ((self dialog))
  (dialog-cancel-button self))

(defmethod cancel-button ((self y-or-n-dialog))
  (y-or-n-dialog-cancel-button-of self))

(defmethod cancel-button ((self list-dialog))
  (list-dialog-cancel-button-of self))

(defun show-me-my-dialog (dialog)
  (let ((display (xlib::drawable-display (host-window dialog)))
	(action NIL))
    (xlib::display-finish-output display)
    (xlib:event-case
     (display :force-output-p t :discard-p t)
     (:exposure
      (window count)
      (when (zerop count)
        (xlib::with-event-queue (display)
          (wb-refresh (get-wb-class-instance window))))
      nil)
     (:button-press
      (window root-x root-y)
      (let (result)
        (multiple-value-setq (result action)
          (process-button-press-event
           (get-wb-class-instance window)
           window root-x root-y))
        result))
     (:button-release
      (event-window)
      (process-button-release-event
       (get-wb-class-instance event-window) event-window))
     (:key-press
      (window code state)
      (process-key-press-event (get-wb-class-instance window)
                               window code state))
     (otherwise () nil)
     )
    (xlib::close-display display)
    (cond ((eq action :CANCEL) (error "Dialog action cancelled"))
	  ((eq action :OK) (reconstruct-command-line dialog))
	  ((eq action :YES) T)
	  ((eq action :NO) NIL))))

(defmethod reconstruct-command-line ((self dialog))
  (let* ((text-dialogs (dialog-text-dialogs self))
	 (check-dialogs (dialog-check-dialogs self))
	 (texts NIL)
	 (checks NIL))
    (dolist (text-dialog text-dialogs)
      (push
       (list (text-dialog-item-of text-dialog) 
             (if (string= (text-dialog-text-of text-dialog)
                          "NIL")
               NIL
               T))
       texts))
    (dolist (check-dialog check-dialogs)
      (push (list (check-dialog-item-of check-dialog)
                  (check-dialog-t-or-nil-of check-dialog))
            checks))
    (values checks texts)))

(defmethod reconstruct-command-line ((self list-dialog))
  (let ((items (list-dialog-items-of self)))
    (loop for item in items 
          when (list-dialog-item-selected-p item)
          collect (list-dialog-item-item-of item))))

(defmethod initialize-instance :after
           ((self y-or-n-dialog)
            &key
            ;;(display NIL)
            (font-name "fixed")
            (yes-text "Yes")
            (no-text "No")
            (cancel-text "Cancel")
            (title "Do you wish to do this?")
            &allow-other-keys)
  (declare (special *default-display*))
  (let ((display *default-display*))
    ;;(unless display (setf display (open-default-host-display)))
    (let* ((screen (xlib::display-default-screen display))
	   (black (xlib::screen-black-pixel screen))
	   (white (xlib::screen-white-pixel screen))
	   (parent (xlib::screen-root screen))
	   (font (xlib::open-font display font-name)))
      (setf (y-or-n-dialog-window-of self)
	    (xlib::create-window :parent parent
			         :x 300 :y 300 :width 16 :height 16
			         :background white
			         :override-redirect :on
			         :event-mask
                                 (xlib::make-event-mask 
                                  :button-press
                                  :button-release
                                  :exposure))
	    (xlib::window-plist (y-or-n-dialog-window-of self))
	    (acons :wb-class-instance self NIL)
	    (y-or-n-dialog-gcontext-of self)
	    (xlib::create-gcontext :drawable parent
			           :foreground black
			           :background white
			           :font font)
	    (y-or-n-dialog-text-of self) title
	    (y-or-n-dialog-yes-button-of self)
            (make-button :display display
                         :text yes-text
                         :gcontext
                         (y-or-n-dialog-gcontext-of self)
                         :action :YES)
	    (y-or-n-dialog-no-button-of self)
            (make-button :display display
                         :text no-text
                         :gcontext
                         (y-or-n-dialog-gcontext-of self)
                         :action :NO)
	    (y-or-n-dialog-cancel-button-of self)
            (make-button :display display
                         :text cancel-text
                         :gcontext
                         (y-or-n-dialog-gcontext-of self)
                         :action :CANCEL))
      (let ((dw (y-or-n-dialog-window-of self))
	    (yes-bw (button-window-of
                     (y-or-n-dialog-yes-button-of self)))
	    (no-bw (button-window-of
                    (y-or-n-dialog-no-button-of self)))
	    (cancel-bw (button-window-of
                        (y-or-n-dialog-cancel-button-of self))))
        (xlib::reparent-window yes-bw dw 20 20)
        (xlib::reparent-window no-bw dw 20 50)
        (xlib::reparent-window cancel-bw dw 20 80)
        (setf (xlib::window-plist yes-bw)
	      (acons :associated-with self
                     (xlib::window-plist yes-bw))
              (xlib::window-plist no-bw)
	      (acons :associated-with self
                     (xlib::window-plist no-bw))
	      (xlib::window-plist cancel-bw)
	      (acons :associated-with self
                     (xlib::window-plist cancel-bw)))
        (y-or-n-dialog-compute-geometry self)
        (xlib::map-window dw)
        (xlib::map-subwindows dw)))
    self))

(defun y-or-n-dialog-compute-geometry (y-or-n-dialog)
  (let* ((d y-or-n-dialog)
	 (y-b (y-or-n-dialog-yes-button-of d))
	 (n-b (y-or-n-dialog-no-button-of d))
	 (c-b (y-or-n-dialog-cancel-button-of d))
	 (y-bw (button-window-of y-b))
	 (n-bw (button-window-of n-b))
	 (c-bw (button-window-of c-b))
	 (gcontext (y-or-n-dialog-gcontext-of d))
	 (window (y-or-n-dialog-window-of d))
	 (font (xlib::gcontext-font gcontext))
	 (title-width
          (xlib::text-extents font (y-or-n-dialog-text-of d)))
	 (item-height (xlib::drawable-height y-bw))
	 (y-width (xlib::drawable-width y-bw))
	 (n-width (xlib::drawable-width n-bw))
	 (c-width (xlib::drawable-width c-bw))
	 d-width d-height)
    (setf d-width
          (+ (max title-width 
                  (+ (* 4 *button-margin*)
                     y-width n-width c-width))
             (* 2 *button-margin*))
          d-height (* (+ item-height (* 2 *button-margin*)) 2))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) d-width
	    (xlib::drawable-height window) d-height))
    (xlib::with-state (y-bw)
      (setf (xlib::drawable-x y-bw) *button-margin*
	    (xlib::drawable-y y-bw)
            (- d-height item-height *button-margin*)))
    (xlib::with-state (n-bw)
      (setf (xlib::drawable-x n-bw)
            (+ (* 3 *button-margin*) y-width)
	    (xlib::drawable-y n-bw)
            (- d-height item-height *button-margin*)))
    (xlib::with-state (c-bw)
      (setf (xlib::drawable-x c-bw)
            (+ (* 5 *button-margin*) y-width n-width)
	    (xlib::drawable-y c-bw)
            (- d-height item-height *button-margin*)))))

(defun get-string-from-user (prompt-string
			     &key
                             (initial-string NIL)
                             size
                             (ok-text "OK")
                             (cancel-text "Cancel")
                             position
			     &allow-other-keys)
  (declare (ignore size position))
  (let ((d (make-dialog
            :text-dialogs (list (list prompt-string initial-string))
            :ok-text ok-text
            :cancel-text cancel-text))
	(string NIL))
    (multiple-value-bind (checks texts) (show-me-my-dialog d)
      (setf string (cadr (assoc prompt-string texts :test #'string=))))
    string))

(defun y-or-n-dialog (message 
		      &key
                      (yes-text "Yes")
                      (no-text "No")
                      (cancel-text "Cancel")
		      &allow-other-keys)
  (let ((d (make-y-or-n-dialog 
            :title message
            :yes-text yes-text
            :no-text no-text
            :cancel-text cancel-text)))
    (show-me-my-dialog d)))

(defmethod initialize-instance :after
           ((self list-dialog-item)
            &key
            ;;(display NIL)
            gcontext text
            &allow-other-keys)
  (declare (special *default-display*))
  (let ((display *default-display*))
    ;;(unless display (setf display (open-default-host-display)))
    (let* ((screen (xlib::display-default-screen display))
           (black (xlib::screen-black-pixel screen))
           (white (xlib::screen-white-pixel screen))
           (parent (xlib::screen-root screen)))
      (setf (list-dialog-item-window-of self)
	    (xlib::create-window
             :parent parent
             :background white
             :x 0 :y 0 :width 16 :height 16
             :override-redirect :on
             :border-width 2
             :event-mask (xlib::make-event-mask :exposure
                                                :button-press
                                                :button-release))
	    (xlib::window-plist (list-dialog-item-window-of self))
            (acons :wb-class-instance self NIL)
	    (list-dialog-item-text-of self) text)
      (list-dialog-item-compute-geometry self))))

(defun list-dialog-item-compute-geometry (item)
  (let* ((window (list-dialog-item-window-of item))
	 (gcontext (list-dialog-item-gcontext-of item))
	 (font (xlib::gcontext-font gcontext))
	 (text (list-dialog-item-text-of item))
	 (width (+ (* 2 *button-margin*) (xlib::text-extents font text)))
	 (height (+ (* 2 *button-margin*) (xlib::font-ascent font)
		    (xlib::font-descent font))))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) width
	    (xlib::drawable-height window) height))))

(defmethod initialize-instance :after
           ((self list-dialog)
            &key
            ;;(display NIL)
            (font-name "fixed")
            (title "Choose from the list")
            (table-print-function
             #'(lambda (x s) (format nil "~a" x)))
            (list-of-items NIL)
            (ok-text "OK")
            (cancel-text "CANCEL")
            &allow-other-keys)
  (declare (special *default-display*))
  ;;(unless display (setf display (open-default-host-display)))
  (let* ((display *default-display*)
         (screen (xlib::display-default-screen display))
	 (black (xlib::screen-black-pixel screen))
	 (white (xlib::screen-white-pixel screen))
	 (parent (xlib::screen-root screen))
	 (font (xlib::open-font display font-name)))
    (setf (list-dialog-window-of self)
          (xlib::create-window :parent parent
			       :x 300 :y 300 :width 16 :height 16
			       :background white
			       :border-width 2
			       :override-redirect :on
			       :event-mask (xlib::make-event-mask
                                            :exposure
                                            :button-press
                                            :button-release))
          (xlib::window-plist (list-dialog-window-of self))
	  (acons :wb-class-instance self NIL)
          (list-dialog-gcontext-of self)
          (xlib::create-gcontext :drawable parent
			         :foreground black
			         :background white
			         :font font)
          (list-dialog-title-of self) title
          (list-dialog-ok-button-of self)
          (make-button :display display
                       :text ok-text
                       :gcontext (list-dialog-gcontext-of self)
                       :action :OK)
          (list-dialog-cancel-button-of self)
          (make-button :display display
                       :text cancel-text
                       :gcontext (list-dialog-gcontext-of self)
                       :action :CANCEL))
    (let* ((l-dw (list-dialog-window-of self))
	   (ok-bw (button-window-of (list-dialog-ok-button-of self)))
	   (cancel-bw (button-window-of (list-dialog-cancel-button-of self))))
      (xlib::reparent-window ok-bw l-dw 0 0)
      (xlib::reparent-window cancel-bw l-dw 0 0)
      (setf (xlib::window-plist ok-bw)
	    (acons :associated-with self (xlib::window-plist ok-bw)))
      (setf (xlib::window-plist cancel-bw)
	    (acons :associated-with self (xlib::window-plist cancel-bw))))
    (dolist (item list-of-items)
      (setf (list-dialog-items-of self)
            (append (list-dialog-items-of self)
                    (list (make-list-dialog-item
                           :display display
                           :gcontext (list-dialog-gcontext-of self)
                           :item item
                           :text (funcall table-print-function item NIL)))))
      (let* ((curr-i (car (last (list-dialog-items-of self))))
	     (i-w (list-dialog-item-window-of curr-i)))
	(xlib::reparent-window (list-dialog-item-window-of curr-i)
	                       (list-dialog-window-of self) 0 0)
	(setf (xlib::window-plist i-w)
	      (acons :associated-with self (xlib::window-plist i-w)))))
    (list-dialog-compute-geometry self)
    (xlib::map-window (list-dialog-window-of self))
    (xlib::map-subwindows (list-dialog-window-of self))
    self)))

(defun make-list-dialog (&rest args)
  (apply #'make-instance 'list-dialog args))

(defun make-list-dialog-item (&rest args)
  (apply #'make-instance 'list-dialog-item args))

(defun list-dialog-p (thing)
  (typep thing 'list-dialog))

(defun list-dialog-item-p (thing)
  (typep thing 'list-dialog-item))

(defmethod wb-refresh ((self list-dialog))
  (let* ((window (list-dialog-window-of self))
	 (gcontext (list-dialog-gcontext-of self))
	 (font (xlib::gcontext-font gcontext))
	 (title (list-dialog-title-of self))
	 (title-width (xlib::text-extents font title))
	 (window-width (xlib::drawable-width window))
	 (text-start-x (- (round window-width 2)
                          (round title-width 2)))
	 (text-start-y (+ *button-margin* 
                          (xlib::font-ascent font))))
    (xlib::draw-image-glyphs window gcontext 
                             text-start-x text-start-y title)))

(defmethod wb-refresh ((self list-dialog-item))
  (let* ((window (list-dialog-item-window-of self))
	 (gcontext (list-dialog-item-gcontext-of self))
	 (font (xlib::gcontext-font gcontext))
	 (text (list-dialog-item-text-of self))
	 (fg (xlib::gcontext-foreground gcontext))
	 (bg (xlib::gcontext-background gcontext))
	 (fn *host-bic-mode*)
	 (width (xlib::drawable-width window))
	 (height (xlib::drawable-height window))
	 (selected-p (list-dialog-item-selected-p self))
	 (text-start-x *button-margin*)
	 (text-start-y (+ *button-margin* (xlib::font-ascent font))))
    (if selected-p
      (xlib::with-gcontext (gcontext :function fn
				     :foreground bg
				     :background fg)
	(xlib::draw-rectangle window gcontext 0 0 width height T)
        (xlib::draw-image-glyphs window gcontext 
	     		  	 text-start-x text-start-y text))
      (xlib::draw-image-glyphs window gcontext 
			       text-start-x text-start-y text))))

(defun max-list-dialog-item-width (items)
  (let ((result 0))
    (dolist (item items)
      (let ((w (list-dialog-item-window-of item)))
        (setf result 
	      (max result (+ 2 *button-margin* (xlib::drawable-width w))))))
    result))

(defun list-dialog-compute-geometry (dialog)
  (let* ((window (list-dialog-window-of dialog))
	 (gcontext (list-dialog-gcontext-of dialog))
	 (font (xlib::gcontext-font gcontext))
	 (title (list-dialog-title-of dialog))
	 (items (list-dialog-items-of dialog))
	 (title-width (+ (* 2 *button-margin*)
                         (xlib::text-extents font title)))
	 (item-width (max-list-dialog-item-width items))
	 (item-height (+ (* 2 *button-margin*)
                         (xlib::font-ascent font)
			 (xlib::font-descent font)))
	 (height (+ (* (+ (length items) 2) item-height)
                    (* 2 *button-margin*)))
	 (ok-button (list-dialog-ok-button-of dialog))
	 (cancel-button (list-dialog-cancel-button-of dialog))
	 (ok-width (xlib::drawable-width
                    (button-window-of ok-button)))
	 (cancel-width (xlib::drawable-width
                        (button-window-of cancel-button)))
	 (buttons-width (+ (* 4 *button-margin*)
                           ok-width cancel-width))
	 (width (+ (* 2 *button-margin*) 
	       	   (max title-width item-width buttons-width))))
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) width
	    (xlib::drawable-height window) height))
    (let ((next-top item-height))
      (dolist (item items)
	(let ((w (list-dialog-item-window-of item)))
	  (xlib::with-state (w)
	    (setf (xlib::drawable-width w)
                  (- item-width (* 2 *button-margin*))
		  (xlib::drawable-x w) *button-margin*
		  (xlib::drawable-y w) next-top)))
	(incf next-top item-height)))
    (let ((ok-window (button-window-of ok-button))
	  (cancel-window (button-window-of cancel-button))
	  (button-y (- (xlib::drawable-height window)
                       item-height))
	  (ok-x *button-margin*)
	  (cancel-x (+ (* 3 *button-margin*) ok-width)))
      (xlib::with-state (ok-window)
	(setf (xlib::drawable-x ok-window) ok-x
	      (xlib::drawable-y ok-window) button-y))
      (xlib::with-state (cancel-window)
	(setf (xlib::drawable-x cancel-window) cancel-x
	      (xlib::drawable-y cancel-window) button-y)))))

(defun select-item-from-list
       (list
        &key
        window-title
        (select-text "OK")
        (cancel-text "Cancel")
        (table-print-function 
         #'(lambda (x s) (format nil "~a" x)))
        (selection-type :single))
  (let ((d (make-list-dialog :title window-title
			     :table-print-function table-print-function
			     :ok-text select-text
			     :cancel-text cancel-text
			     :selection-type selection-type
			     :list-of-items list)))
    (show-me-my-dialog d)))
