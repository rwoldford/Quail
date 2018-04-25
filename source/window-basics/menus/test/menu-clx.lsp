;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               menu-clx.lisp
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
;;;     C.B. Hurley 1989-1992
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(export  '(make-menu wb-pop-up-menu destroy-menu set-menu-fns menu-p put-menu-prop
           get-menu-prop installed-p menu select-in-menu get-menu-item
           disable-menu-item enable-menu-item check-menu-item
           set-menu-item-string))


;;; --------------------------------------------------------------------------------------
;;;
;;;                           Hierarchical pop-up menus
;;;
;;; --------------------------------------------------------------------------------------


;;; First set up a slot so that each menu-item knows which menu it belongs to.
;;; 

(defclass wb-menu-mixin ()
  ((user-properties   :initarg :user-properites
                      :initform nil
                      :accessor user-properties-of)
   (sub-item-fn      :initarg :sub-item-fn
                      :initform #'default-sub-item-fn
                      :accessor sub-item-fn-of)
   (when-held-fn     :initarg :when-held-fn
                      :initform #'default-when-held-fn
                      :accessor when-held-fn-of)
   (when-unheld-fn   :initarg :when-unheld-fn
                      :initform #'default-when-unheld-fn
                      :accessor when-unheld-fn-of)
   (when-selected-fn :initarg :when-selected-fn
                      :initform #'default-when-selected-fn
                      :accessor when-selected-fn-of)))

(defclass wb-menu (wb-menu-mixin)
  ((title :initform "Choose and Item" :accessor title-of)
   (parent-menu :initform NIL :accessor parent-menu-of)
   (type :initform NIL :accessor menu-type)
   (items :initform NIL :accessor items-of)
   (function :initform NIL :accessor function-of)
   (message :initform NIL :accessor message-of)
   (window :initform NIL :accessor window-of)
   (gcontext :initform NIL :accessor gcontext-of)
   (width :initform 0 :accessor width-of)
   (enabled :initform T :accessor enabled-p)
   (event-handler :initform NIL :accessor event-handler-of)
   (title-width :initform 0 :accessor title-width-of)
   (item-window :initform NIL :accessor item-window-of)
   (items-alist :initform NIL :accessor items-alist-of)))

(defmethod initialize-instance :after ((self wb-menu)
				&key (display (xlib::open-display *default-host*))
				     sub-item-fn when-selected-fn when-held-fn when-unheld-fn
				     (title NIL)
				     (type :pop-up)
				     (canvas NIL)
				     (font *default-menu-font*)
				     (item-parent NIL)
				     (parent-menu NIL)
				     (function NIL)
				&allow-other-keys)
  (declare (special *default-menu-font*))
  (let* ((parent-window 
	    (if parent-menu (parent-of (window-of parent-menu))
	      (xlib::screen-root (xlib::display-default-screen display))))
	 (colormap (xlib::window-colormap parent-window))
	 (black (xlib::alloc-color colormap *black-color*))
	 (grey (xlib::alloc-color colormap *grey-color*)))
    (setf (xlib::window-event-mask parent-window)
      (logior (xlib::window-event-mask parent-window)
	      (xlib::make-event-mask :substructure-notify)))
    ;;(unless (xlib::display-after-function display)
    ;;  (setf (xlib::display-after-function display) #'xlib::display-finish-output))
    (setf 
      (function-of self) function
      (menu-type self) type
      (title-of self) (if (stringp title) title (format nil "~a" title))
      (parent-menu-of self) parent-menu
      (gcontext-of self) 
      (if parent-menu 
        (gcontext-of parent-menu)
        (xlib::create-gcontext :drawable parent-window
                       :foreground black
		       :background grey
                       :font (canvas-font-to-host-font font)))
      (window-of self)
      (xlib::create-window :parent parent-window
	             :x 0 :y 0 :width 16 :height 16
		     :background grey :colormap colormap
		     :override-redirect :on
		     :border-width 2
		     :event-mask (xlib::make-event-mask 
						  :button-press
						  :button-release
						  :visibility-change
						  :exposure
						  :owner-grab-button
						)))
    (unless item-parent (setf item-parent (window-of self)))
    (setf
      (item-window-of self)
      (xlib::create-window :parent item-parent
		     :x 0 :y 0 :width 16 :height 16
		     :background grey :colormap colormap
		     :event-mask (xlib::make-event-mask 
						  :enter-window 
						  :button-press 
						  :button-release
						  :owner-grab-button))
      (title-width-of self)
      (xlib::text-extents (xlib::gcontext-font (gcontext-of self)) (title-of self)))
      (set-menu-fns self
	 	    :selected when-selected-fn
		    :held when-held-fn
		    :unheld when-unheld-fn
		    :sub-item sub-item-fn)
    self))

(defun set-menu-fns (menu &key (selected nil) (held nil) (unheld nil) (sub-item nil))
  (if (and selected (functionp selected))  (setf (when-selected-fn-of menu) selected))
  (if (and held     (functionp held))      (setf (when-held-fn-of menu) held))
  (if (and unheld   (functionp unheld))    (setf (when-unheld-fn-of menu) unheld))
  (if (and sub-item (functionp sub-item))  (setf (sub-item-fn-of menu) sub-item)))

(defun menu-p (menu)
  (typep menu 'wb-menu))

(defun put-menu-prop (menu property value)
  "Stores value on property of menu"
  (setf (user-properties-of menu) (acons property value (user-properties-of menu))))


(defun get-menu-prop (menu property)
  "Returns the value associated with the property on menu"
  (cdr (assoc property (user-properties-of menu))))


;;; ---------------------------------------------------------------------------------
;;;
;;;                   Modifying the menu appearance
;;;
;;; ---------------------------------------------------------------------------------

(defun get-menu-item (menu item-string)
  (second (find item-string 
    (items-alist-of menu) 
    :test #'(lambda (x y) (string= x (title-of y)))
    :key #'cadr)))

(defun disable-menu-item (menu item-string)   
  "Disables the menu item identified by item-string."
    (let ((item (get-menu-item menu item-string)))
      (if item
        (menu-item-disable item))))

(defun menu-item-disable (item)
  (setf (enabled-p item) NIL))

(defun enable-menu-item (menu item-string)   
  "Enables the menu item identified by item-string."
  (let ((item (get-menu-item menu item-string)))
      (if item
        (menu-item-enable item))))

(defun menu-item-enable (item)
  (setf (enabled-p item) T))

(defun check-menu-item (menu item-string &optional (check-char nil) )
 "Places a check-char beside item identified by item-string ~
  if check-char is nil no check mark appears ~
  if check-char is t a default check mark is used."
  (let ((item (get-menu-item menu item-string)))
    (if item
        (set-menu-item-check-mark item check-char))))


(defun set-menu-item-string (menu item-string new-string)
  "Changes the string for the menu item currently ~
   identified by item-string to new-string."
  (let ((item (get-menu-item menu item-string)))
    (when item
      (setf (title-of item) new-string)
      (menu-compute-geometry (parent-menu-of item))
      (menu-compute-geometry item))))

(defun sub-items-of (item)
  (items-of item))

(defun item-action-p (item)
  (if (function-of item) T NIL))

(defun item-action (item)
  (function-of item))

(defun item-message (item)
  (if (message-of item)
    (message-of item)
    "This item will be selected when the button is released."))

(defun wb-inform-user (message)
  (print message))

(defun menu-set-items (menu &rest items)
  (let ((item-list NIL)
	(items-alist NIL))
    (dolist (item items) 
      (let* ((item-menu (make-instance 'wb-menu
				   :title (first item)
				   :parent-menu menu
				   :display (xlib::drawable-display (window-of menu))
				   :item-parent (window-of menu) :function (second item)
				   :when-selected-fn (when-selected-fn-of menu)
				   :when-held-fn (when-held-fn-of menu)
				   :when-unheld-fn (when-unheld-fn-of menu)
				   :sub-item-fn (sub-item-fn-of menu)
				   :function (second item)
				   :message (third item)))
             (item-window (item-window-of item-menu)))
        (push item-menu item-list)
        (push (list item-window item-menu) items-alist)
	(when (and (> (length item) 4) (eq (fourth item) :sub-items))
	  (nconc items-alist (apply #'menu-set-items item-menu (fifth item))))
	(setf (items-of menu) (reverse item-list))
	(menu-compute-geometry menu)))
	items-alist))

(defparameter *menu-item-margin* 4)

(defmacro max-item-width (menu)
  `(let ((items (items-of ,menu))
	 (max-width (title-width-of ,menu)))
    (dolist (item items)
      (setf max-width (max max-width (title-width-of item))))
    max-width))

(defun update-item-window (item height width next-left next-top)
  (let ((window (item-window-of item)))
    (xlib::with-state (window)
      (setf (xlib::drawable-height window) height
	    (xlib::drawable-width  window) width
            (xlib::drawable-x      window) next-left 
            (xlib::drawable-y      window) next-top))))

(defmacro menu-refresh (menu)
  `(let* ((window (window-of ,menu))
	 (gcontext (gcontext-of ,menu))
	 (title-x (round (- (width-of ,menu) (title-width-of ,menu)) 2))
	 (baseline-y (+ (xlib::font-ascent (xlib::gcontext-font gcontext)) *menu-item-margin*)))
    (unless (eq (menu-type ,menu) :title)
      (xlib::draw-image-glyphs window gcontext title-x baseline-y 
	(title-of ,menu)))
    (dolist (item (items-of ,menu))
      (xlib::draw-image-glyphs (item-window-of item) gcontext 2 baseline-y 
	(title-of item)))))

(defmacro find-item (window menu)
 `(second (assoc ,window (items-alist-of ,menu))))

(defmacro find-menu (window menu)
 `(second (find ,window 
    (items-alist-of ,menu)
    :test #'(lambda (x y) (eq x (window-of y)))
    :key #'cadr)))

(defmethod parent-of ((self xlib::window))
  (multiple-value-bind (children parent) (xlib::query-tree self)
    (declare (ignore children))
    parent))

(defmethod parent-of ((self wb-menu))
  (multiple-value-bind (children parent) (xlib::query-tree (item-window-of self))
    (declare (ignore children))
    parent))

(defmacro host-to-canvas-mouse-state (state)
  `(cond ((eq ,state 256) :left)
	 ((eq ,state 257) :left)
	 ((eq ,state 260) :left)
	 ((eq ,state 512) :middle)
	 ((eq ,state 513) :middle)
	 ((eq ,state 516) :middle)
	 ((eq ,state 1024) :right)
	 ((eq ,state 1025) :right)
	 ((eq ,state 1028) :right)))

(defun recurse-unmap-children (menu)
  (dolist (item (items-of menu))
    (when (items-of item) 
      (recurse-unmap-children item))
      ;;(format t "Unmapping Window of Menu with Title: ~a~%" (title-of item))
      (when (eq (xlib::window-map-state (window-of item)) :viewable)
	(xlib::unmap-window (window-of item)))))

(defun shutdown-menu (menu)
  (xlib::unmap-window (window-of menu)))

(defun destroy-menu (menu)
  (when (and (event-handler-of menu) (not (eq (menu-type menu) :title)))
    (mp:process-kill (event-handler-of menu))
    (xlib::close-display (xlib::drawable-display (window-of menu)))))

(defun mouse-in-item-p (item)
  (multiple-value-bind (x y) (xlib::pointer-position (item-window-of item))
    (and (<= x (xlib::drawable-width (item-window-of item)))
         (<= y (xlib::drawable-height (item-window-of item)))
         (>= x 0)
         (>= y 0))))

(defmacro place-item-sub-menu (item)
  `(let* ((window (item-window-of ,item))
	 (parent (parent-of window))
	 position)
    (if (eq (menu-type (parent-menu-of ,item)) :pop-up)
      (setf position (make-position 
        (+ (xlib::drawable-x parent) (xlib::drawable-width parent)
          *menu-item-margin*)
	(+ (xlib::drawable-y parent) (xlib::drawable-y window))))
      (setf position (make-position
        (+ (xlib::drawable-x parent) (xlib::drawable-x window))
        (+ (xlib::drawable-y parent) (xlib::drawable-height parent) 
          *menu-item-margin*))))
    (menu-present ,item position)))

(defun menu-event-handler (menu &aux (selected-item NIL) (item NIL) (last NIL))
  (xlib::unmap-window (item-window-of menu))
  (setf item (first (items-of menu)))
  (xlib::event-case ((xlib::drawable-display (window-of menu)) :force-output-p t)

    (:exposure
      (window count)
	(when (zerop count)
	  (if (eq window (window-of menu))
	    (xlib::with-event-queue ((xlib::drawable-display window))
	      (menu-refresh menu))
	    (if item
	      (xlib::with-event-queue ((xlib::drawable-display window))
	        (menu-refresh item)))))
      nil)

    (:button-release
      (event-window state)
        (setf selected-item (find-item event-window menu))
        (when (and selected-item (enabled-p selected-item)
		   (mouse-in-item-p selected-item))
	  (funcall (when-selected-fn-of selected-item)
	           selected-item
	           (parent-menu-of selected-item)
	           (host-to-canvas-mouse-state state)))
      (shutdown-menu menu)
      nil)

    (:enter-notify
      (window)
      (menu-downlight item)
      (setf last item item (find-item window menu))
      (unless (member item (items-of last))
        (dolist (thing (items-of (parent-menu-of item)))
          (when (eq (xlib::window-map-state (window-of thing)) :viewable)
	    (xlib::unmap-window (window-of thing)))))
      (when (enabled-p item)
	(when (funcall (sub-item-fn-of item) (parent-menu-of item) item)
	  (xlib::with-event-queue ((xlib::drawable-display window))
	    (place-item-sub-menu item)))
	(menu-highlight item))
      nil)

    (:unmap-notify
      (window)
      (when (setf last (find-menu window menu))
	(dolist (thing (items-of last))
	  (when (eq (xlib::window-map-state (window-of thing)) :viewable)
	    (xlib::unmap-window (window-of thing)))))
      nil)

    (:visibility-notify
      (window state)
      (when (member state '(:partially-obscured :fully-obscured))
	(setf (xlib::window-priority window) :above))
      nil)

    (otherwise () nil))
  (xlib::close-display (xlib::drawable-display (window-of menu)))
  selected-item)

(defmacro menu-highlight (item)
  `(let* ((window (item-window-of ,item))
	  (gcontext (gcontext-of ,item))
	  (width (- (xlib::drawable-width window) 2))
	  (height (- (xlib::drawable-height window) 2)))
   (xlib::with-gcontext (gcontext :line-width 3)
	(xlib::draw-rectangle window gcontext 0 0 width height))))

(defmacro menu-downlight (item)
  `(let* ((window (item-window-of ,item))
	  (gcontext (gcontext-of ,item))
	  (width (xlib::drawable-width window))
	  (height (xlib::drawable-height window))
	  (back (xlib::gcontext-background gcontext)))
   (xlib::with-gcontext (gcontext :line-width 3 :foreground back)
        (xlib::draw-rectangle window gcontext 0 0 (- width 2) (- height 2)))))


(defun menu (menu &optional position)
  (if (eq (menu-type menu) :title)
    (setf position (make-position 0 0))
    (progn
      (unless (event-handler-of menu)
      (setf (event-handler-of menu) (mp:process-run-function
        "Quail Menu Event Handler" #'menu-event-handler menu)))))
  (menu-present menu position)
  (xlib::display-finish-output (xlib::drawable-display (window-of menu))))

(defmacro menu-present (menu &optional position)
  `(let ((window (window-of ,menu))
	x y)
    (if ,position
      (setf x (aref ,position 0) y (aref ,position 1))
      (multiple-value-setq (x y) (xlib::global-pointer-position (xlib::drawable-display window))))
    (xlib::with-state (window)
      (setf (xlib::drawable-x window) x
            (xlib::drawable-y window) y))
    (xlib::map-window window)))

(defun menu-compute-geometry (menu)
  (if (eq (menu-type menu) :pop-up)
    (let* ((menu-font (xlib::gcontext-font (gcontext-of menu)))
           (items (items-of menu))
           menu-width)
      (setf menu-width (+ (max-item-width menu) *menu-item-margin* *menu-item-margin*))
      (let ((delta-y (+ (xlib::font-ascent menu-font) (xlib::font-descent menu-font) *menu-item-margin* *menu-item-margin*)))
        (xlib::with-state ((window-of menu))
          (setf (xlib::drawable-width (window-of menu)) menu-width
                (xlib::drawable-height (window-of menu)) (+ 2 (* (+ 1 (length items)) delta-y))))
        (let ((next-item-top delta-y))
          (dolist (item items)
            (update-item-window item delta-y menu-width 0 next-item-top)
            (incf next-item-top delta-y))))
      (xlib::map-subwindows (window-of menu))
      (setf (width-of menu) menu-width))
    (let* ((menu-font (xlib::gcontext-font (gcontext-of menu)))
           (item-height (+ (xlib::font-ascent menu-font) (xlib::font-descent menu-font) *menu-item-margin* *menu-item-margin*))
           (items-width NIL)
           (items       (items-of menu))
           menu-width)
      (setf item-width (+ (* *menu-item-margin* 2) (max-item-width menu))
            items-width (* item-width (length items))
            menu-width (+ items-width (* *menu-item-margin* 4)))
      (let ((delta-x (+ item-width *menu-item-margin*)))
        (xlib::with-state ((window-of menu))
          (setf (xlib::drawable-width  (window-of menu)) menu-width
                (xlib::drawable-height (window-of menu)) item-height))
        (let ((item-left *menu-item-margin*))
          (dolist (next-item items)
            (update-item-window next-item item-height item-width item-left 0)
            (incf item-left delta-x))))
      (xlib::map-subwindows (window-of menu))
      (setf (width-of menu) menu-width))))

(defun make-menu
       (&key items sub-item-fn when-selected-fn when-held-fn when-unheld-fn
	     canvas
             (title NIL)
	     (font *default-menu-font*)
	     (change-offset-fn NIL)
	     (menu-type :pop-up)
	     &aux menu display)
  (declare (ignore change-offset-fn)
	   (special *default-menu-font*))
  
  (if (and canvas (eq menu-type :title))
    (setf display (xlib::drawable-display (host-window canvas)))
    (setf display (xlib::open-display *default-host*)))
  (setf menu (make-instance 'wb-menu
	      		    :display display
			    :title title
			    :type menu-type
			    :font font
			    :canvas canvas))
  (set-menu-fns menu :selected when-selected-fn
		     :held when-held-fn
		     :unheld when-unheld-fn
		     :sub-item sub-item-fn)
  (setf (items-alist-of menu) (apply #'menu-set-items menu items))
  (push (list (item-window-of menu) menu) (items-alist-of menu))
  menu)
