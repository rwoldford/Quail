;;; This is ~/RESERVE/lc2-Quail/source/window-basics/host/new-host-menu-canvas-sblx.lsp
;;; a re-type of h-m-c-sblx.lsp
;;; to try to get around the halt of the compile/load od that .fasl
;;; for reasons I do not understand
;;; 13 October 2020

(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*last-menubar* *system-default-menubar*)))

(defclass host-menu-canvas ()
	()
	(:documentation "A class to define how title menus should appear."))

(defvar *system-default-menubar*
	NIL
	"The default menubar of the system.")

#|
(defvar *quail-menubar-window* NIL
	"The window which hosts the Quail Menubar~
	needed also to host the font wrt menu calculations are performed.
	Its vaue will be established in make-default-system-menubar
	which is called by set-system-default-menubar.")
|#

(defvar *system-default-menubar-base-height* NIL
	"The height (pixels) of just the title strip of *system-default-menubar*.")

(defun set-system-default-menubar-base-height (&key (height 100))
	"Sets the height (pixels) of just the title strip of *system-default-menubar*."
	(setf *system-default-menubar-base-height* height))

(define-application-frame QUAILMENUBAR ()
	()
	(:menu-bar QUAILMENUBAR-command-table)
	(:panes
		(display :application))
	(:layouts
		(default display))
	(:geometry :height (set-system-default-menubar-base-height)))

(define-QUAILMENUBAR-command com-quail ()
	)

(define-QUAILMENUBAR-command com-plots ()
	)

(define-QUAILMENUBAR-command com-extra-menus ()
	)

(define-QUAILMENUBAR-command com-inactive ()
	)

(make-command-table 'quail-command-table 
	:errorp NIL
	:menu '(("Inactive" :command com-inactive)))

(make-command-table 'plots-command-table
	:errorp nil
	:menu '(("Inactive" :command com-inactive)))

(make-command-table 'extra-menus-command-table
	:errorp nil
	:menu '(("Inactive" :command com-inactive)))

(make-command-table 'QUAILMENUBAR-command-table
	:errorp nil
	:menu '(("Quail" :menu quail-command-table)
		("Plots" :menu plots-command-table)
		("Extra Menus" :menu extra-menus-command-table)))

(defvar *system-default-menubar-thread-name* "QMB")

(defun set-system-default-menubar-thread-name (&key (string "QMB"))
	(setf *system-default-menubar-thread-name* string))

(defun make-default-system-menubar (&key (thread-name *system-default-menubar-thread-name*))
		(flet ((run ()
			(let ((frame (make-application-frame 'QUAILMENUBAR :pretty-name "QUAIL MENUBAR")))
				;(setq *quail-menubar-window* frame)
				(setq *system-default-menubar* frame)
				(run-frame-top-level frame))))
		(sb-thread::make-thread #'run :name thread-name)))

(defvar *quail-window-base-height* NIL
	"The height (pixels) of just the title strip of *quail-menubar-window*.")

(defun set-quail-window-base-height (&key (height 100))
	"Sets the height (pixels) of just the title strip of *quail-menubar-window*."
	(setf *quail-window-base-height* height))

#|
(defun set-system-default-menubar ()
	"Sets the *system-default-menubar* to be *quail-menubar-window*
	as returned from make-default-system-menubar above"
	(unless *system-default-menubar*
		(setf *system-default-menubar* *quail-menubar-window*)))
|#

(eval-when (:load-toplevel)
	(unless *system-default-menubar*
		(make-default-system-menubar)))

(defun thread-from-name (name)
	(dolist (x (sb-thread::list-all-threads))
		(if (string-equal name (sb-thread::thread-name x))
			(return x))))

(defvar *system-default-menubar-thread* (thread-from-name *system-default-menubar-thread-name*))

(defun command-table-items (frame-symbol)
	"Get command table items from an application frame identified by frame-symbol"
	(let (foo)
		(clim-user::map-over-command-table-commands
			(lambda (elt) (push elt foo))
			(clim-user::frame-command-table (clim-user::find-application-frame frame-symbol)))
		(nreverse foo)))

(defvar *last-menubar* *system-default-menubar*
   "The list of elements of the last menubar.  To be updated ~
and downdated as canvases are activated and deactivated.") 
