(in-package :wb)
(defclass host-menu-canvas ()
  ((menu-bar :initarg :menu-bar
	     :initform NIL
	     :accessor menu-bar-of)))

(defun install-menu-bar (canvas)
  (let ((left (canvas-get-title-menu canvas :key :left))
	(middle (canvas-get-title-menu canvas :key :middle))
	(right (canvas-get-title-menu canvas :key :right))
	(ctrl-left (canvas-get-title-menu canvas :key :ctrl-left))
	(ctrl-middle (canvas-get-title-menu canvas :key :ctrl-middle))
	(ctrl-right (canvas-get-title-menu canvas :key :ctrl-right))
	(display (xlib::drawable-display (host-window canvas)))
	menus menu-bar)
    (when ctrl-right
      (push ctrl-right menus))
    (when ctrl-middle
      (push ctrl-middle menus))
    (when ctrl-left
      (push ctrl-left menus))
    (when right
      (push right menus))
    (when middle
      (push middle menus))
    (when left
      (push left menus))
    (when menus
      (setf menu-bar (make-instance 'wb-menu
                                    :display display
				    :title NIL
				    :type :pop-up
				    :font *default-menu-font*
				    :canvas canvas))
      (set-menu-fns menu-bar :selected (when-selected-fn-of (first menus))
			     :held (when-held-fn-of (first menus))
			     :unheld (when-unheld-fn-of (first menus))
			     :sub-item (sub-item-fn-of (first menus)))
      (dolist (menu menus)
	(setf (parent-menu-of menu) menu-bar)
	(xlib::reparent-window (item-window-of menu) (window-of menu-bar) 0 0)
        (setf (items-alist-of menu-bar) (nconc (items-alist-of menu-bar) (items-alist-of menu))))
      (push (list (item-window-of menu-bar) menu-bar) (items-alist-of menu-bar))
      (setf (items-of menu-bar) menus)
      (menu-compute-geometry menu-bar)
      (setf (event-handler-of menu-bar)
        (mp:process-run-function "Quail Canvas Event Handler"
          #'event-handler canvas))
      (setf (menu-bar-of canvas) menu-bar)
      (menu menu-bar (make-position 300 300))
      menu-bar)))
