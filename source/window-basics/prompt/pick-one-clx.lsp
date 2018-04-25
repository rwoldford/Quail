;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               pick-one-clx.lisp
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
;;;     R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;  History:
;;;
;;;  - Construct a uniform prompting facility.
;;;

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(pick-one)))

(defun pick-one (items &key
                       (prompt-text "Pick one of the following.")
                       (item-print-function NIL)
                       (action-function #'(lambda (i) i))
                       (select-text "Select")
                       (cancel-text "Cancel"))
  "Prompts the user to pick one of the items in the sequence items, ~
   using the text supplied by the keyword prompt-text. ~
   If supplied, the function action-function is called on the selected item.  ~
   Default action just returns the item.  ~
   If supplied, item-print-function will be used on each item to display it ~
   in the table.  ~
   (:see-also prompt-for-items prompt-t-or-f)"
   (let ((selection (car (select-item-from-list items 
			   :prompt-text prompt-text
			   :select-text select-text
			   :cancel-text cancel-text
			   :selection-type :single
			   :item-print-function
			     (if (functionp item-print-function)
			       item-print-function
			       #'(lambda (x) (format nil "~a" x)))))))
    (funcall action-function selection)
    selection))
