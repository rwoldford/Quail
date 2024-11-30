;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               display-mode.lisp
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
;;;    N.G. Bennett 1992
;;;    R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :window-basics)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(display-mode-of with-display-mode)))

(defclass display-mode-mixin ()
  ((display-mode :initform :active :initarg  :display-mode
                 :accessor display-mode-of
                 :documentation "This slot determines the mode of display for ~
                                 the canvas. (ie :printer, :postscript or :active)")
   ))

(defvar display-mode)

(defmacro with-display-mode
          (canvas display-mode (command &rest args) &body forms)
  "If display-mode is :active (the default), then the forms are evaluated ~
   as usual.  If the display-mode is not :active, but rather is one of ~
   (:postscript :printer) then forms are not evaluated.  Instead, ~
   the command with its arguments is processed to find a like command ~
   for the given display-mode."
  `(let ((display-mode ,display-mode))
     (cond
      ((eq :active display-mode)
       (format t "~% In with-diaplay-mode :active clause")
       ,@forms)
      ((eq :postscript display-mode)
       (format t "~% In with-display-mode :postscript clause")
       (let*
         ((canvas ,canvas)
          (old-mode (display-mode-of canvas))
          result)
         (setf result (,(get-postscript-command command) ,@args))
         (setf (display-mode-of canvas) :active)
         (,command ,@args)
         (setf (display-mode-of canvas) old-mode)
         result))
      #|  Here's where we might need to define "printer-.." commands
          for some port besides the Mac.   .... rwo
      ((eq :printer display-mode)
       (let*
         ((canvas ,canvas)
          (old-mode (display-mode-of canvas))
          result)
         (setf result (,(get-printer-command command) ,@args))
         (setf (display-mode-of canvas) :active)
         (,command ,@args)
         (setf (display-mode-of canvas) old-mode)
         result))
      |#
      (T 
        (format t "~% in with-display-mode T clause")
        ,@forms)))
  )

(defun get-printer-command (command-name)
  "Returns the printer command name corresponding to the given ~
   command-name."
  (intern (concatenate 'string "PRINTER-" (string command-name))
           :window-basics))

(defun get-postscript-command (command-name)
  "Returns the postscript command name corresponding to the given ~
   command-name."
  (intern (concatenate 'string "PS-" (string command-name))
           :window-basics))

