;;;********* bug in MCL 2.0b1
;;;
;;; Do synonym streams really work?
;;;
;;; Consider the following:
;;;

(defun my-read ()
  (read))

(defun si-read ()
  (declare (special *standard-input*))
  (read *standard-input*))

(defvar *syn-standard-input*)

(setf *syn-standard-input* (make-synonym-stream '*standard-input*))

(defun syn-read ()
  (declare (special *syn-standard-input*))
  (read *syn-standard-input*))

;;;
;;;
;;; ANd in the listener:
;;;
? (my-read)
(+ 2 3)                        ;<--- as typed
(+ 2 3)                        ;<--- as returned

? (my-read)
(+ 2 3)                       ;<--- typed as (+ 2 300000000000000 & 14 backspaces)
(+ 2 3)                       ;<--- as returned

? (si-read)
(+ 2 3)                       ;<--- as typed
(+ 2 3)                       ;<--- as returned

? (si-read)
(+ 2 3)                       ;<--- typed as (+ 2 300000000000000 & 14 backspaces)
(+ 2 3)                       ;<--- as returned

? (syn-read)
(+ 2 3)                       ;<--- as typed
(+ 2 3)                       ;<--- as returned

? (syn-read)
(+ 2 3)                       ;<--- typed as (+ 2 300000000000000 & 14 backspaces)
(+ 2 300000000000000)         ;<--- as returned


;;;
;;;
;;;;;;;;;;;;;; Argggggggggggggggggggggh!!!!!!!!!!!!!
;;;
;;;
;;;  Explanations anyone?  MCL folk?