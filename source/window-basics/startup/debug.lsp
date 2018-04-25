(in-package :wb)
(defvar *debug-print-flag* NIL)

(defmacro *debug-print* (string)
  (declare (special *debug-print-flag*))
  (if *debug-print-flag*
    `(quail-print ,string)))
#|
(defun *debug-print* (string)
  (declare (special *debug-print-flag*))
  (if *debug-print-flag*
    (quail-print string)))
    |#
