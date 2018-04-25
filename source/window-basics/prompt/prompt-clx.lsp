;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               prompt-clx.lisp
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
;;;    N.G. Bennett 1993
;;;    R.W. Oldford 1994.
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(prompt-user save-value prompt-t-or-f prompt-true-or-false
           prompt-for-items)))

(defun prompt-user (&key (type T)
                         (read-type :string)
                         (prompt-string NIL)
                         (initial-string "")
                         (ok-text "OK")
                         (cancel-text "Cancel")
                         left top
                         width
                         (height 100))
  "Prompts user for input. ~
   (:key ~
   (:arg Prompt-string NIL The string used to query the user.) ~
   (:arg initial-string \"\" Appears to the user as the default answer.) ~
   (:arg ok-text \"OK\" String used to prompt user for confirmation.) ~
   (:arg cancel-text \"Cancel\" String used to prompt user for cancellation.) ~
   (:arg type T  Data type of item to be returned by user.  Any Common Lisp ~
   data-type is acceptable.) ~
   (:arg read-type :string  How to read the typed input.  ~
   Legal values are :string :eval or :read.) ~
   (:arg left NIL  Location of the left of the prompt window.) ~
   (:arg top NIL  Location of the top of the prompt window.) ~
   (:arg width NIL  Width of prompt window.) ~
   (:arg height 100  Height of prompt window.) ~
   )"
  (declare (ignore left top width height))
  (setq prompt-string
        (if prompt-string
          (if (stringp prompt-string)
            (format NIL prompt-string)
            (format NIL "~a" prompt-string))
          (format nil "Please enter a ~S" type)))
  (let* ((result (cdar
                  (collect-input 
                   (list ">>>")
                   :prompt-text prompt-string))))
    (setf result
          (case read-type
            (:string result)
            (:eval (eval (read-from-string result :preserve-whitespace t)))
            (:read (read-from-string result :preserve-whitespace t))
            )
          )
    (if (typep result type)
      result
      (quail-error "~S is not of type ~S" result type))))

(defun save-value (arg)
  "Saves the value of arg as the value of a symbol retrieved from a prompt."
  (eval
   `(setf ,(prompt-user  :read-type :read
                         :type 'symbol
                         :prompt-string
                         "Give a symbol to store the value on:")
          ,arg)))

(defun prompt-t-or-f (message
                      &key
                      (true-text "Yes")
                      (false-text "No")
                      (cancel-text "Cancel"))
  "Prompts the user to make a decision between two alternatives (true-text and false-text) ~
   in response to the information given in message.  T is returned if the first letter ~
   of the true-text (default the string Yes) is typed by the user; NIL is returned if the first letter ~
   of the false-text (default the string No) is typed by the user.  ~
   If cancelled by the user, processing ends. ~
   (:see-also prompt-for-items pick-one prompt-true-or-false)"
  (y-or-n-dialog message
                 :yes-text true-text
                 :no-text false-text
                 :cancel-text cancel-text))

(defun prompt-true-or-false (message
                             &key
                             (true-text "Yes")
                             (false-text "No")
                             (cancel-text "Cancel"))
  "Prompts the user to make a decision between two alternatives (true-text and false-text) ~
   in response to the information given in message.  T is returned if the ~
   true-text (default the string Yes) is typed by the user; NIL is returned if the ~
   false-text (default the string No) is typed by the user.  ~
   If cancelled by the user, processing ends. ~
   (:see-also prompt-for-items pick-one prompt-t-or-f)"
  (prompt-t-or-f message
                 :true-text true-text
                 :false-text false-text
                 :cancel-text cancel-text))

(defun prompt-for-items (list
                         &key (prompt-text "Choose")
                         (item-function NIL)
                         (selection-type :single))
  "Prompts user to select one or more items from the list.  ~
   Prompt-text is displayed to help the user choose.  ~
   Selection-type is one of :single (indicating only a single item ~
   can be selected), :contiguous  (indicating only contiguous items ~
   can be selected), and :disjoint (indicating that any subset of the items ~
   can be selected).  It returns a list of all items selected, NIL if no items ~
   were selected.  ~
   Item-print-function is the function called on each item to display it in ~
   selection table. ~
   (:see-also pick-one prompt-t-or-f)"
  (if (functionp item-function)
    (select-item-from-list list
                           :prompt-text prompt-text
                           :item-print-function 
                           #'(lambda(x) 
			       (format nil "~a" (funcall item-function x)))
                           :selection-type selection-type)
    (select-item-from-list list
                           :prompt-text prompt-text
                           :selection-type selection-type)))
