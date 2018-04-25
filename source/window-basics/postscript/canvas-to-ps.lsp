;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               canvas-to-ps.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1991 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991 - 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :window-basics)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(canvas-to-ps)))

(defun canvas-to-ps (canvas &key (prompt? T))
  "Writes out to files the postscript commands necessary to ~
   reproduce the canvas as displayed.  ~
   (:key ~
   (:arg prompt? T If non-NIL, the user is prompted for ~
                         file and postscript information.))"
  (let ((old-mode (display-mode-of canvas)))
    (unwind-protect 
      (progn
        (when prompt?
          (when (and (prompt-for-ps-page-properties canvas)
                     (prompt-for-ps-filenames canvas))
            (setf (display-mode-of canvas) :postscript)
            (redisplay canvas)
            ;; this next line ensures that the concatenate-ps does
            ;; not redisplay the postscript file, but rather the screen
            (setf (display-mode-of canvas) old-mode)
            (concatenate-ps canvas)
            (delete-header-file canvas)
            (delete-body-file canvas)
            )))
      (setf (display-mode-of canvas) old-mode)
      (re-enable-menubars))))
