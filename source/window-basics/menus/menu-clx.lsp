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
;;;     N.G. Bennett 1993
;;;     R.W. Oldford 1994  
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(make-menu wb-pop-up-menu destroy-menu set-menu-fns menu-p put-menu-prop
           get-menu-prop installed-p menu select-in-menu get-menu-item
           disable-menu-item enable-menu-item check-menu-item
           set-menu-item-string)))


;;; --------------------------------------------------------------------------------------
;;;
;;;                           Hierarchical pop-up menus
;;;
;;; --------------------------------------------------------------------------------------


;;; First set up a slot so that each menu-item knows which menu it belongs to.
;;; 

(defclass wb-menu ()
  ((title             :initform "Choose an Item" :accessor title-of)
   (title-width       :initform 0
                      :accessor title-width-of)
   (type              :initform NIL :accessor menu-type)
   (user-properties   :initarg :user-properites
                      :initform nil
                      :accessor user-properties-of)
   (sub-item-fn       :initarg :sub-item-fn
                      :initform #'default-sub-item-fn
                      :accessor sub-item-fn-of)
   (when-held-fn      :initarg :when-held-fn
                      :initform #'default-when-held-fn
                      :accessor when-held-fn-of)
   (when-unheld-fn    :initarg :when-unheld-fn
                      :initform #'default-when-unheld-fn
                      :accessor when-unheld-fn-of)
   (when-selected-fn  :initarg :when-selected-fn
                      :initform #'default-when-selected-fn
                      :accessor when-selected-fn-of)
   (items             :initarg :items
                      :initform NIL
                      :accessor items-of
                      :documentation "The list of menu items." )
   (super-menu        :accessor super-menu-of
                      :initarg :super-menu
                      :initform NIL
                      :documentation
                      "The menu which contains this menu ~
                       (or menu-item), if any.")
   (sub-menus         :initform NIL
                      :accessor sub-menus-of)
   (message           :initform NIL
                      :accessor message-of)
   (window            :initform NIL
                      :accessor window-of)
   (gcontext          :initform NIL
                      :accessor gcontext-of)
   (width             :initform 0
                      :accessor width-of)
   (enabled           :initform T
                      :accessor enabled-p)
   (checked-item      :initform NIL
                      :accessor checked-item-p)
   (item-window       :initform NIL
                      :accessor item-window-of)
   (sub-menus-alist   :initform NIL
                      :accessor sub-menus-alist-of)))

(defmethod initialize-instance :after ((self wb-menu)
				       &key
                                       sub-item-fn 
                                       when-selected-fn
                                       when-held-fn
                                       when-unheld-fn
				       (title NIL)
				       (type :pop-up)
				       (font *default-menu-font*)
				       (item-parent NIL)
				       (super-menu NIL)
				       &allow-other-keys)
  (declare (special *default-menu-font*))
  (let* ((display *default-display*)
         (parent-window 
          (if super-menu
            (parent-of (window-of super-menu))
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
     (menu-type self) type
     (title-of self) (if (stringp title) title (format nil "~a" title))
     (super-menu-of self) super-menu
     (gcontext-of self) 
     (if super-menu 
       (gcontext-of super-menu)
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
                                       :structure-notify
                                       ))
     (xlib:window-plist (window-of self))
     (acons :wb-class-instance self (xlib:window-plist (window-of self))))
    (unless item-parent (setf item-parent (window-of self)))
    (setf
     (item-window-of self)
     (xlib::create-window :parent item-parent
		          :x 0 :y 0 :width 16 :height 16
		          :background grey :colormap colormap
		          :event-mask (xlib::make-event-mask 
                                       :enter-window 
                                       :leave-window
                                       :button-press 
                                       :button-release
                                       :structure-notify
                                       :owner-grab-button))
     (xlib:window-plist (item-window-of self))
     (acons :wb-class-instance self (xlib:window-plist (item-window-of self)))
     (title-width-of self)
     (xlib::text-extents (xlib::gcontext-font
                          (gcontext-of self))
                         (title-of self)))
    (set-menu-fns self
                  :selected when-selected-fn
                  :held when-held-fn
                  :unheld when-unheld-fn
                  :sub-item sub-item-fn)
    self))

(defun set-menu-fns (menu
                     &key
                     (selected nil)
                     (held nil)
                     (unheld nil)
                     (sub-item nil))
  (if (and selected (functionp selected))
    (setf (when-selected-fn-of menu) selected))
  (if (and held     (functionp held))
    (setf (when-held-fn-of menu) held))
  (if (and unheld   (functionp unheld))
    (setf (when-unheld-fn-of menu) unheld))
  (if (and sub-item (functionp sub-item))
    (setf (sub-item-fn-of menu) sub-item)))

(defun menu-p (menu)
  (typep menu 'wb-menu))

(defun put-menu-prop (menu property value)
  "Stores value on property of menu"
  (setf (user-properties-of menu)
        (acons property value (user-properties-of menu))))


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
    (sub-menus-alist-of menu) 
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

(defun check-menu-item (menu item-string &optional (check? nil) )
 "Places a check-char beside item identified by item-string ~
  if check-char is nil then check mark is removed. ~
  if check-char is t a default check mark is used."
 (let ((sub-menu (get-menu-item menu item-string)))
    (when sub-menu (setf (checked-item-p sub-menu) check?))))


(defun set-menu-item-string (menu item-string new-string)
  "Changes the string for the menu item currently ~
   identified by item-string to new-string."
  (let ((item (get-menu-item menu item-string)))
    (when item
      (setf (title-of item) new-string)
      (menu-compute-geometry (super-menu-of item))
      (menu-compute-geometry item))))


(defun item-message (item)
  (if (message-of item)
    (message-of item)
    "This item will be selected when the button is released."))


(defun menu-set-items (menu &rest items)
  (let ((item-list NIL)
	(sub-menus-alist NIL))
    (dolist (item items) 
      (let* ((item-menu
              (make-instance 'wb-menu
                :title (first item)
                :super-menu menu
                :item-parent (window-of menu)
                :when-selected-fn (when-selected-fn-of menu)
                :when-held-fn (when-held-fn-of menu)
                :when-unheld-fn (when-unheld-fn-of menu)
                :sub-item-fn (sub-item-fn-of menu)
                :items item
                :type :pop-up
                :message (third item)))
             (item-window (item-window-of item-menu)))
        (push item-menu item-list)
        (push (list item-window item-menu) sub-menus-alist)
	(when (and (> (length item) 4) (eq (fourth item) :sub-items))
	  (nconc sub-menus-alist (apply #'menu-set-items item-menu (fifth item))))
	(setf (sub-menus-of menu) (reverse item-list))
	(menu-compute-geometry menu)))
	sub-menus-alist))

(defparameter *menu-item-margin* 4)
(defvar *check-box-width* 10)
(defvar *sub-menu-triangle-width* 10)
(defvar *menu-space* 2)

(defun max-item-width (menu)
  (declare (special *menu-space*
                    *check-box-width*
                    *sub-menu-triangle-width*
                    *menu-border-width*))
  (let ((sub-menus (sub-menus-of menu))
	 (max-width (title-width-of menu)))
    (dolist (item sub-menus)
      (setf max-width (max max-width (title-width-of item))))
    (+ *menu-border-width*
       *menu-space* 
       *check-box-width*
       *menu-space*
       max-width
       *menu-space*
       *sub-menu-triangle-width*
       *menu-space*
       *menu-border-width*)))

(defun update-item-window (item height width next-left next-top)
  (let ((window (item-window-of item)))
    (xlib::with-state (window)
      (setf (xlib::drawable-height window) height
	    (xlib::drawable-width  window) width
            (xlib::drawable-x      window) next-left 
            (xlib::drawable-y      window) next-top))))


(defvar *3d-menus* NIL "Variable determining whether menu items are to be flat or 3d")

(defvar *menu-border-width* 2)

(defun menu-downlight (item)
  (declare (special *3d-menus* *menu-border-width*))
  (if *3d-menus*
    (let* ((drawable (item-window-of item))
           (gcontext (gcontext-of item))
           (width (xlib::drawable-width drawable))
           (height (xlib::drawable-height drawable))
           (colormap (xlib::window-colormap (window-of item)))
           (light-gray (xlib::alloc-color colormap *light-gray-color*))
           (dark-gray (xlib::alloc-color colormap *dark-gray-color*))
           (line-width *menu-border-width*)
           (lw/2 (round line-width 2))
           (left-x 0)
           (top-y 1)
           (bottom-y (- height lw/2))
           )
      ;; Left and Top of menu item box
      (xlib:with-gcontext (gcontext :foreground light-gray :line-width line-width)
        (xlib:draw-lines drawable gcontext
                         (list left-x bottom-y
                               left-x top-y
                               right-x top-y )))
      ;; Bottom and right of menu item box
      (xlib:with-gcontext (gcontext :foreground dark-gray :line-width line-width)
        (xlib:draw-lines drawable gcontext
                         (list right-x (+ top-y lw/2)
                               right-x bottom-y
                               (+ left-x lw/2) bottom-y)))
      )
    (let* ((window (item-window-of item))
           (gcontext (gcontext-of item))
           (width (- (xlib::drawable-width window) *menu-border-width*))
           (height (- (xlib::drawable-height window) *menu-border-width*))
           (back (xlib:gcontext-background gcontext))
           )
      (xlib::with-gcontext (gcontext :line-width *menu-border-width* :foreground back)
        (xlib::draw-rectangle window gcontext 0 0
                              width height)))
    ))




(defun menu-highlight (item)
  (declare (special *3d-menus* *menu-border-width*))
  (if *3d-menus*
    (let* ((drawable (item-window-of item))
           (gcontext (gcontext-of item))
           (width (xlib::drawable-width drawable))
           (height (xlib::drawable-height drawable))
           (colormap (xlib::window-colormap (window-of item)))
           (light-gray (xlib::alloc-color colormap *light-gray-color*))
           (dark-gray (xlib::alloc-color colormap *dark-gray-color*))
           (line-width *menu-border-width*)
           (lw/2 (round line-width 2))
           (left-x 0)
           (top-y 1)
           (bottom-y (- height lw/2))
           )
      ;; Left and top of menu item box
      (xlib:with-gcontext (gcontext :foreground dark-gray :line-width line-width)
        (xlib:draw-lines drawable gcontext
                         (list left-x bottom-y
                               left-x top-y
                               right-x top-y )))
      ;; Bottom and right of menu item box
      (xlib:with-gcontext (gcontext :foreground light-gray :line-width line-width)
        (xlib:draw-lines drawable gcontext
                         (list right-x (+ top-y lw/2)
                               right-x bottom-y
                               (+ left-x lw/2) bottom-y)))
      )
    (let* ((window (item-window-of item))
           (gcontext (gcontext-of item))
           (width (- (xlib::drawable-width window) *menu-border-width*))
           (height (- (xlib::drawable-height window) *menu-border-width*)))
      (xlib::with-gcontext (gcontext :line-width *menu-border-width*)
        (xlib::draw-rectangle window gcontext 0 0 width height)))
    )
  )



(defun menu-refresh (menu)
  (declare (special *menu-space*
                    *check-box-width*
                    *sub-menu-triangle-width*
                    *menu-border-width*))
  (let* ((window (window-of menu))
	 (gcontext (gcontext-of menu))
	 (title-x (round (- (width-of menu)
                            (title-width-of menu))
                         2))
	 (baseline-y (+ (xlib::font-ascent
                         (xlib::gcontext-font gcontext))
                        *menu-item-margin*)))
    (unless (eq (menu-type menu) :title)
      (xlib::draw-image-glyphs
       window gcontext title-x baseline-y 
       (title-of menu)))
    (dolist (sub-menu (sub-menus-of menu))
      (let* ((x (+ *menu-border-width* *menu-space*))
             (drawable (item-window-of sub-menu))
             (text (title-of sub-menu))
             (height (xlib::drawable-height drawable))
             (width (xlib:drawable-width drawable))
             )
        (if ;;(and
             (enabled-p sub-menu)
             ;;*3d-menus*)
          (menu-downlight sub-menu))
        
        (if (checked-item-p sub-menu)
          ;; draw a check mark and move x over.
          (let* ((check-mark-arm-length (round height 3))
                 (left-y (- baseline-y  check-mark-arm-length))
                 (right-y (- left-y (* 2 check-mark-arm-length)))
                 (middle-x (+ x (round *check-box-width* 2)))
                 (right-x (+ x *check-box-width*)))
            (xlib:draw-lines drawable gcontext
                             (list x left-y
                                   middle-x baseline-y
                                   right-x right-y))
            ))
        (setf x (+ x *check-box-width* *menu-space*))
        (if (string-equal "-" text)
          (let ((y (round (/ (xlib::drawable-height drawable) 2)))
                (right-x (- (xlib:drawable-width drawable)
                            *menu-space*
                            *sub-menu-triangle-width*
                            *menu-space*
                            *menu-border-width*)))
            (xlib:draw-line drawable gcontext x y right-x y)
            (menu-item-disable sub-menu)
            )
          (xlib::draw-image-glyphs 
           drawable
           gcontext x baseline-y 
           text))
        (if (sub-menus-of sub-menu)
          (case (menu-type menu)
            (:title
             ;; Point arrow down
             (let* ((right-x (- width *menu-space* *menu-border-width*))
                    (left-x (- right-x *sub-menu-triangle-width*))
                    (top-y (- height baseline-y))
                    (middle-x (+ left-x (round *sub-menu-triangle-width* 2))))
               (xlib:draw-lines drawable gcontext
                                (list left-x top-y
                                      right-x top-y
                                      middle-x baseline-y
                                      left-x top-y)
                                :fill-p T
                                :shape :convex)))
            (otherwise
             ;; Point arrow right
             (let* ((right-x (- width *menu-space* *menu-border-width*))
                    (left-x (- right-x *sub-menu-triangle-width*))
                    (top-y (- height baseline-y))
                    (right-y (round height 2)))
               (xlib:draw-lines drawable gcontext
                                (list left-x baseline-y
                                      right-x right-y
                                      left-x top-y
                                      left-x baseline-y)
                                :fill-p T
                                :shape :convex)))
            
            
            ))
        ))))

(defmethod wb-refresh ((self wb-menu))
  (menu-refresh self))

(defun find-item-menu (window menu)
 (second (assoc window (sub-menus-alist-of menu))))

(defun find-menu (window menu)
  (second (find window 
                (sub-menus-alist-of menu)
                :test #'(lambda (x y) (eq x (window-of y)))
                :key #'cadr)))

(defmethod parent-of ((self xlib::window))
  (multiple-value-bind
    (children parent)
    (xlib::query-tree self)
    (declare (ignore children))
    parent))

(defmethod parent-of ((self wb-menu))
  (multiple-value-bind (children parent)
                       (xlib::query-tree (item-window-of self))
    (declare (ignore children))
    parent))

(defun recurse-unmap-children (menu)
  (xlib:unmap-subwindows (window-of menu)))
#|
  (dolist (item (sub-menus-of menu))
    (when (sub-menus-of item) 
      (recurse-unmap-children item))
    ;;(format t "Unmapping Window of Menu with Title: ~a~%" (title-of item))
    (when (eq (xlib::window-map-state (window-of item)) :viewable)
      (xlib::unmap-window (window-of item)))))
|#

(defun shutdown-menu (menu)
  (dolist (thing (sub-menus-of menu))
    (when (eq (xlib::window-map-state (window-of thing))
              :viewable)
      (shutdown-menu thing)
      ))
  (menu-downlight menu)
  (xlib::unmap-window (window-of menu)))

(defun unmap-submenus (menu)
  (dolist (submenu (sub-menus-of menu))
    (when (eq (xlib::window-map-state (window-of submenu)) :viewable)
      (unmap-submenus submenu)
      (menu-downlight submenu)
      (xlib::unmap-window (window-of submenu))))
  )

(defun destroy-menu (menu)
  (declare (ignore menu))
  (dolist (submenu (sub-menus-of menu))
    (destroy-menu submenu))
  (let ((w (window-of menu)))
    (xlib::destroy-subwindows w)
    (xlib::destroy-window w)))

(defun mouse-in-item-p (item)
  (multiple-value-bind (x y) (xlib::pointer-position (item-window-of item))
    (and (<= x (xlib::drawable-width (item-window-of item)))
         (<= y (xlib::drawable-height (item-window-of item)))
         (>= x 0)
         (>= y 0))))


(defun menu-present (menu &optional position)
  (let ((window (window-of menu))
        x y)
    (if position
      (setf x (aref position 0)
            y (aref position 1))
      (if (eq (menu-type menu) :title)
        (setf x 0
              y 0)
        (multiple-value-setq (x y)
          (xlib::global-pointer-position
           (xlib::drawable-display window)))))
    (xlib::with-state (window)
      (setf (xlib::drawable-x window) x
            (xlib::drawable-y window) y))
    (xlib::map-window window)))

(defun place-item-sub-menu (item)
  (let* ((sm (super-menu-of item))
	 position)
    (setf
     position 
     (case (menu-type sm)
       (:pop-up
        (let* ((window (item-window-of item))
               (parent (parent-of window))) 
          (make-position 
           (+ (xlib::drawable-x parent)
              (xlib::drawable-width parent)
              *menu-item-margin*)
           (+ (xlib::drawable-y parent)
              (xlib::drawable-y window))))
        )
       
       (:title
        (let* ((menu-bar-window (window-of sm))
               (menu-item-window (item-window-of item))
               (host-window (parent-of menu-bar-window)))
          (make-position
           (+ (xlib::drawable-x menu-bar-window)
              (xlib::drawable-x menu-item-window)
              (xlib::drawable-x host-window))
           (+ (xlib::drawable-y menu-bar-window)
              (xlib::drawable-y menu-item-window)
              (xlib::drawable-y host-window) 
              (xlib::drawable-height menu-bar-window)
              *menu-item-margin*)))
        )
       
       (otherwise
        (let* ((window (item-window-of item))
               (parent (parent-of window))) 
          (make-position
           (+ (xlib::drawable-x parent)
              (xlib::drawable-width parent)
              )
           (+ (xlib::drawable-y parent)
              (xlib::drawable-y window)
              ;;(xlib::drawable-height parent)
              *menu-item-margin*))))
       )
     )
    
    (menu-present item position)))




(defun menu (menu &optional position)
  (menu-present menu position)
  (xlib::display-finish-output
   (xlib::drawable-display (window-of menu))))

(defun menu-compute-geometry (menu)
  (case (menu-type menu)
    (:pop-up
     (let* ((menu-font (xlib::gcontext-font (gcontext-of menu)))
            (sub-menus (sub-menus-of menu))
            menu-width)
       (setf menu-width (+ (max-item-width menu)
                           *menu-item-margin* *menu-item-margin*))
       (let ((delta-y (+ (xlib::font-ascent menu-font)
                         (xlib::font-descent menu-font)
                         *menu-item-margin* *menu-item-margin*)))
         (xlib::with-state ((window-of menu))
           (setf (xlib::drawable-width (window-of menu)) menu-width
                 (xlib::drawable-height (window-of menu))
                 (+ 2 (* (+ 1 (length sub-menus)) delta-y))))
         (let ((next-item-top delta-y))
           (dolist (item sub-menus)
             (update-item-window item delta-y 
                                 (- menu-width 4) 0 next-item-top)
             (incf next-item-top delta-y))))
       (xlib::map-subwindows (window-of menu))
       (setf (width-of menu) menu-width))
     )

    (:title
     (let* ((menu-font (xlib::gcontext-font (gcontext-of menu)))
            (item-height (+ (xlib::font-ascent menu-font)
                            (xlib::font-descent menu-font)
                            *menu-item-margin*
                            *menu-item-margin*))
            (sub-menus-width NIL)
            (sub-menus (sub-menus-of menu))
            menu-width item-width)
       (setf item-width (+ (* *menu-item-margin* 2)
                           (max-item-width menu))
             sub-menus-width (* item-width (length sub-menus))
             menu-width (+ sub-menus-width
                           (* *menu-item-margin* 4)))
       (let ((delta-x (+ item-width *menu-item-margin*)))
         (xlib::with-state ((window-of menu))
           (setf (xlib::drawable-width  (window-of menu)) menu-width
                 (xlib::drawable-height (window-of menu)) item-height))
         (let ((item-left *menu-item-margin*))
           (dolist (next-item sub-menus)
             (update-item-window next-item
                                 item-height item-width item-left 0)
             (incf item-left delta-x))))
       (xlib::map-subwindows (window-of menu))
       (setf (width-of menu) menu-width))
     )

    (otherwise  ;; the old way
     (let* ((menu-font (xlib::gcontext-font (gcontext-of menu)))
            (item-height (+ (xlib::font-ascent menu-font)
                            (xlib::font-descent menu-font)
                            *menu-item-margin*
                            *menu-item-margin*))
            (sub-menus-width NIL)
            (sub-menus (sub-menus-of menu))
            menu-width item-width)
       (setf item-width (+ (* *menu-item-margin* 2)
                           (max-item-width menu))
             sub-menus-width (* item-width (length sub-menus))
             menu-width (+ sub-menus-width
                           (* *menu-item-margin* 4)))
       (let ((delta-x (+ item-width *menu-item-margin*)))
         (xlib::with-state ((window-of menu))
           (setf (xlib::drawable-width  (window-of menu)) menu-width
                 (xlib::drawable-height (window-of menu)) item-height))
         (let ((item-left *menu-item-margin*))
           (dolist (next-item sub-menus)
             (update-item-window next-item
                                 item-height item-width item-left 0)
             (incf item-left delta-x))))
       (xlib::map-subwindows (window-of menu))
       (setf (width-of menu) menu-width))
     )))

(defun make-menu
       (&key items
             sub-item-fn
             when-selected-fn
             when-held-fn
             when-unheld-fn
             (title NIL)
	     (font *default-menu-font*)
	     (change-offset-fn NIL)
	     (menu-type :pop-up)
	     &aux menu ;;display
             )
  (declare (ignore change-offset-fn)
	   (special *default-menu-font*))
  
  (setf menu (make-instance 'wb-menu
			    :title title
			    :type menu-type
			    :font font
                            :items items))
  (set-menu-fns menu :selected when-selected-fn
		     :held when-held-fn
		     :unheld when-unheld-fn
		     :sub-item sub-item-fn)
  (setf (sub-menus-alist-of menu) (apply #'menu-set-items menu items))
  (push (list (item-window-of menu) menu) (sub-menus-alist-of menu))
  menu)
