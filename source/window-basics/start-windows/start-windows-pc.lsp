;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          start-windows-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;     R.W. Oldford 1994
;;; 
;;; A copy of start-windows-excl.lisp <= ***
;;; There is no SI package  there is an Allegro  getenv  function !
;;; It needs a C pointer as an argument!!!
;;; I'm not sure what the MP package is either!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(start-windows stop-windows)))
(defun default-host ()
  "Returns value of *default-host* if it's non-NIL ~
   otherwise queries the system environment variable DISPLAY for the ~
   host, assigns the result to *default-host* and returns it."
  (declare (special *default-host*))
   #|
  (unless *default-host* 
    (let ((system-host (si:getenv "DISPLAY")))
      (setf *default-host*
            (subseq system-host 0 
                    (position #\: system-host)))))
  *default-host*
   |#
 )

(defun start-windows ()
  "Starts up the windowing environment for this implementation."
   #|
  (default-display)
  (mp:process-run-function "Quail Event Handler"
                           #'event-handler *default-display*)
   |#
   )

(defun stop-windows ()
  "Shuts down the windowing environment for this implementation."
   #|
  (let ((quail-event-handler
         (find-if #'(lambda (proc)
                      (string-equal "Quail Event Handler"
                                    (mp:process-name proc)))
                  mp:*all-processes*)))
    (if quail-event-handler
      (mp:process-kill quail-event-handler))
    (setf *current-canvas* NIL)
    (setf *default-display* (xlib:close-display *default-display*)))
   |#
   )
