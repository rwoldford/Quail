;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          quail-menubar-clx.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     N.G. Bennett 1993.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(add-menu-in-quail-menubar
          remove-menu-from-quail-menubar
          install-quail-menubar
          *quail-menubar*)))

(defvar *quail-menubar*
  NIL
  "The default menubar put up for the Quail system.")

(defun add-menu-in-quail-menubar (menu)
  "Adds the menu to Quail's menubar. ~
   (:see-also install-quail-menubar remove-menu-from-quail-menubar)"
  (declare (special *quail-menubar*))
  (unless *quail-menubar* (install-quail-menubar))
  ;; Change menu to :pop-up. This ensures that it constructs
  ;; a vertical menu.
  (setf (wb::menu-type menu) :pop-up)
  (unless (member menu (wb::sub-menus-of *quail-menubar*))
    (setf (wb::sub-menus-of *quail-menubar*)
          (append (wb::sub-menus-of *quail-menubar*)
                  (list menu))))
  (xlib::reparent-window
   (wb::window-of menu)
   (wb::window-of *quail-menubar*)
   0
   (xlib::drawable-width (wb::window-of *quail-menubar*)))
  (wb::menu-compute-geometry *quail-menubar*)
  (xlib::unmap-window (wb::window-of *quail-menubar*))
  (xlib::map-window (wb::window-of *quail-menubar*))
  *quail-menubar*)

(defun remove-menu-from-quail-menubar (menu)
  "Removes the menu from Quail's menubar. ~
   (:see-also install-quail-menubar add-menu-in-quail-menubar)"
  (declare (special *quail-menubar*))
  (setf (wb::sub-menus-of *quail-menubar*)
        (remove menu (wb::sub-menus-of *quail-menubar*)))
  (wb::menu-compute-geometry *quail-menubar*)
  (xlib::unmap-window (wb::window-of *quail-menubar*))
  (xlib::map-window (wb::window-of *quail-menubar*))
  *quail-menubar*)

(defun install-default-quail-menubar ()
  "Creates and installs the default top level Quail menubar."
  (declare (special *quail-menubar*))
  (setf *quail-menubar* (quail-menubar))
  (install-quail-menubar))

(defun quail-menubar ()
  "Creates the default top level Quail menubar. Does not install it. ~
   (:see-also install-default-quail-menubar)"
  (declare (special *quail-menubar*))
  (let ((quail-menu (quail-menu))
        (quail-plot-menu (quail-plot-menu))
        the-menubar)
    
    ;; Change menus to :pop-up. This ensures that it constructs
    ;; a vertical menu.
    (setf (wb::menu-type quail-menu) :pop-up)
    (setf (wb::menu-type quail-plot-menu) :pop-up)
    (setf the-menubar
          (make-instance 'wb::wb-menu
            :title ""
            :type :title
            :font wb::*default-menu-font*
            ))
    (wb::set-menu-fns the-menubar
                      :selected (wb::when-selected-fn-of quail-menu)
                      :held (wb::when-held-fn-of quail-menu)
                      :unheld (wb::when-unheld-fn-of quail-menu)
                      :sub-item (wb::sub-item-fn-of quail-menu))
    (setf (wb::super-menu-of quail-menu) the-menubar)
    (setf (wb::super-menu-of quail-plot-menu) the-menubar)
    (xlib::reparent-window
     (wb::item-window-of quail-menu)
     (wb::window-of the-menubar)
     0 0)
    (xlib::reparent-window
     (wb::item-window-of quail-plot-menu)
     (wb::window-of the-menubar)
     (xlib::drawable-width (wb::item-window-of quail-menu))
     0)
    ;;(setf (wb::sub-menus-alist-of the-menubar)
    ;;      (nconc (wb::sub-menus-alist-of the-menubar)
    ;;             (wb::sub-menus-alist-of quail-menu)))
    (push (list (wb::item-window-of the-menubar)
                the-menubar)
          (wb::sub-menus-alist-of the-menubar))
    (setf (wb::sub-menus-of the-menubar)
          (list quail-menu quail-plot-menu))
    (wb::menu-compute-geometry quail-menu)
    (wb::menu-compute-geometry quail-plot-menu)
    (wb::menu-compute-geometry the-menubar)
    (xlib::unmap-window (wb::item-window-of the-menubar))
    the-menubar)
  )
#|
(defun install-quail-menubar ()
  "Installs the top-level Quail's menubar according to the ~
   value of *quail-menubar*."
  (declare (special *quail-menubar*))
  (if  *quail-menubar*
    (let* ((window (wb::window-of *quail-menubar*))) 
      (wb::menu-present *quail-menubar*
                        (wb::make-position
                         10
                         (+ (xlib::drawable-height window) 10)))
      )
    (install-default-quail-menubar))) 
|#

(defun install-quail-menubar ()
   "Installs the top-level Quail's menubar for the first time."
  (setf *quail-menubar*
        (wb::make-canvas
         :middle-title-items *quail-plot-menu-items*
         :title-middle "Plots"
         :left-title-items *quail-menu-items*
         :title-left "Quail"
         :title-right "Menubar canvas"
         :left 30
         :bottom (- (wb::screen-height) 100)
         :width 200
         :height 30
         :background-color wb::*gray-color*
         :title "Quail Menubar")))
