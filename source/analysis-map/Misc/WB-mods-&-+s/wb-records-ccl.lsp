(in-package :window-basics)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  A useful function for interacting with mac :rect records
;;;
;;;


(defun make-rect (top left bottom right)
  "Makes a Mac CL :rect record."
  (let (rect)
     (setf rect (make-record :rect))
     (set-record-field rect :rect ':topleft (make-point left top))
     (set-record-field rect :rect ':bottomright (make-point right bottom))
     rect))
