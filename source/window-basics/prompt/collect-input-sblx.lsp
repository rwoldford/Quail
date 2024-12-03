;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; collect-input-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;  G.W. Bennett 1996-1997, 2020
;;;     
;;;----------------------------------------------------------------------------------

(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(collect-input)))

;;; a function needed in collect-input
(defun copy-update-list (list1 list2)
  "returns a copy of list1 with updated cdrs from list2 when the list2 element is not a seimcolon as a string"
  (let ((copy-list1 (copy-tree list1)))
    (dolist (x list2)
      (unless (string-equal x ";")
      (setf (cdr (elt copy-list1 (position x list2))) x)))
    (return-from copy-update-list copy-list1)))

;;; The querying function which needs a clim-listener to run
;;; but isn't used 12JAN2021
#|
(defun accepting-tag (item &key (stream *query-io*) (ow t))
  "item is a cons of two strings"
    (accepting-values
  (stream :initially-select-query-identifier 'the-tag :own-window ow)
      (terpri stream)
      (terpri stream)
        (accept 'string :prompt "A string" :stream stream)
    ))
|#

;;; (collect-input (list (cons "a" "a1") (cons "b" "b1") (cons "c" "c1")))
;;; click on a -> a11, on b -> b11, on c ; ==>> (("a" . "a11") ("b" . "b11") ("c" ."c1"))

;;; collect-input itself
(defun collect-input (items &key (columns 1) (prompt-text "Please enter responses ...")
                      (item-print-function NIL)
                      (action-function #'(lambda (i) i))
                      (select-text "select")
                      (cancel-text "cancel")
                      (stream *query-io*) (ow t))
;(declare (ignore columns item-print-function action-function select-text cancel-text))
  (let* ((result-list ()))
           (dolist (x items)
            (accepting-values
              (stream :initially-select-query-identifier 'the-tag :own-window ow :label prompt-text :height 250 :width 350)
              ;(terpri stream)
              (format stream "~%Asking for ~s , currently ~s " (car x) (cdr x))
              (format stream "~%To accept the current value, enter  ; ")
              (terpri stream)
              (push  (accept 'string :prompt (first x) :stream stream) result-list)
              )
            (setf result-list (remove-if #'NULL result-list))
            )
      (return-from collect-input (copy-update-list items (reverse result-list)))
      ))
