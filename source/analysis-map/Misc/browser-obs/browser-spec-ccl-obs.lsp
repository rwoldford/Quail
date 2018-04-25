

;;; replaced by canvas-title
(defun get-window-title (window) (ask window (window-title)))



;;; Replaced by setf (canvas-title canvas) in window-basics
(defun put-window-title (window title)
  "Set a new title to window."
  (or (stringp title)
      (setq title (format nil "~a" title)))
  (ask window (set-window-title title)))

