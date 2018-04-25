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
  "Adds the menu to Quail's default menubar."
  (declare (special *quail-menubar*))
  (unless (member menu (wb::items-of *quail-menubar*))
    (setf (wb::items-of *quail-menubar*)
          (append (wb::items-of *quail-menubar*)(list menu))))
  (xlib::reparent-window (wb::window-of menu)
                         (wb::window-of *quail-menubar*)  0 0)
  (wb::menu-compute-geometry *quail-menubar*)
  menu)

(defun remove-menu-from-quail-menubar (menu)
  "Removes the menu from Quail's default menubar."
  (declare (special *quail-menubar*))
  (setf (wb::items-of *quail-menubar*)
        (remove menu (wb::items-of *quail-menubar*)))
  (wb::menu-compute-geometry *quail-menubar*))

(defun install-quail-menubar ()
  "Installs Quail's default menubar."
  (declare (special *quail-menubar* *quail-menu*))
  (unless *quail-menu* (setf *quail-menu* (quail-menu)))
  (unless *quail-menubar* (setf *quail-menubar* *quail-menu*))
  (let* ((window (wb::window-of *quail-menubar*))
        ) 
    (wb::menu *quail-menubar*
              (wb::make-position 10
                                 (- (wb::screen-height)
                                    (xlib::drawable-height window) 10)))
     ))



