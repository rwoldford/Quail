;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               browser-mcl.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1988-1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     G. Desvignes 1988,1989
;;;     R.W. Oldford 1988-1992
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;
;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(push-in-buffer
          select-in-menu
          )))
;;;
;;; function definitions
;;;


(defun get-active-region (window)
  "Return the inside region of the window.  ~
   As the height and width of the clipping region are the inside dimensions ~
   of the window, we use it and the origin position of the window which is ~
   given by the outside region of the window to built the inside region."

       (let ((reg-1 (wb::clipping-region-of window))
             (reg-2 (wb::canvas-screen-region window)))
            (setf (wb::region-left reg-1)
                  (wb::region-left reg-2))
            (setf (wb::region-bottom reg-1)
                  (wb::region-bottom reg-2))
            (setf (wb::region-width reg-1)
                  (- (wb::region-width reg-1) 24))
            (setf (wb::region-height reg-1)
                  (- (wb::region-height reg-1) 16))
            reg-1))

(defun push-in-buffer (text)
  (print "push-in-buffer needs to be fixed.")
  (format *terminal-io* "Push-in-buffer called with ~s" text)
  (ccl:set-window-layer (ccl:front-window) 1)
 #|
  (or (stringp text)
      (setq text (format nil "~s" text)))
  (dotimes (k (length text))
     (#_PostEvent :errchk :a0 3 :d0  (char text k) :d0)
     (#_PostEvent :errchk :a0 4 :d0  (char text k) :d0)
     (event-dispatch))
  (#_PostEvent :errchk :a0 3 :d0 #\NewLine :d0)
  (#_PostEvent :errchk :a0 4 :d0 #\NewLine :d0)
|#
)
