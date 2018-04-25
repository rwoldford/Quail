;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        host-system-clx.lisp                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*window-manager*
          *known-window-managers*
          default-host
          default-display
          prompt-for-window-manager
          prompt-for-default-host
          open-default-host-display
          host-display)))

(defvar *default-host* NIL
  "The default host for the X window system.")

(defvar *default-display* NIL
  "The default display for the X window system.")

(defvar *window-manager* NIL
  "The window manager being used by your X window system.")

(defvar *known-window-managers* '(:4Dwm :twm :MacX)
  "The window managers we know how to handle.")

(defun prompt-for-window-manager (&optional wms)
  "Prompts for window manager ans sets *window-manager* to the ~
   input."
  (declare (special *known-window-managers*))
  (unless wms (setf wms *known-window-managers*))
  (format *query-io* "~&Please indicate which of the following ~
                      X window managers you are using:~%~
                      ~{ ~s~}~%~
                      Window-manager> " wms)
  (setf *window-manager* (read *query-io*)))

(defun prompt-for-default-host ()
  "Prompts for and sets the default host."
  (declare (special *default-host*))
  (format *query-io* "~&Please enter your X host name or IP address. ~
                      ~&(eg. my-machine ~
                      ~&~3Tor  my-machine.uwaterloo.ca~
                      ~&~3Tor 129.97.140.30~
                      ~&~3Tor the name of your X terminal):  "
          )
  (setf *default-host* (read-line *query-io*))
  (format *query-io* "~&*default-host* is now ~s " *default-host*)
  *default-host*)

(defun default-host ()
  "Returns value of *default-host* if it's non-NIL ~
   otherwise prompts for the default host, assigns the ~
   result to *default-host* and returns it."
  (declare (special *default-host*))
  (unless *default-host* (prompt-for-default-host))
  *default-host*)

(defun default-display ()
  "Returns the default display.  If *default-display* is NIL it is set."
   (unless *default-display*
     (setf *default-display* (xlib:open-display (default-host))))
  *default-display*)

#|

(defun open-default-host-display ()
  "Opens and returns a display on the default host."
  *default-display*)

(defun host-display (&optional host)
  "Returns a display for the optional argument host.  ~
   If host is NIL or not provided then the value of *default-host* ~
   is used."
  *default-display*)
|#

(defun doit (&optional (host (default-host)))
  "Does it (Well what did you think it did?!??)"
  (default-display)
  (mp:process-run-function "Quail Event Handler"
                           #'event-handler *default-display*))
