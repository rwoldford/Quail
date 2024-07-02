;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               prompt-mcl.lisp
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
;;;     Greg Anglin 1991
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
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
  (setq prompt-string
        (if prompt-string
          (if (stringp prompt-string)
            (format NIL prompt-string)
            (format NIL "~a" prompt-string))
          (format nil "Please enter a ~S" type)))
  (setq width (min  (- (screen-width)  40)
                    (or width (+ 180 (* 5 (length prompt-string))))))
  (setq left (or left (- (screen-mouse-x) (round (/ width 2)))))
  (setq left (min (max 10 left )  (- (screen-width) width 10)))
  (setq top (or top (+ (screen-mouse-y) (round (/ height 2)))))
  (setq top (max  (min  top (- (screen-height) 30)) (+ height 10)))
  (let* ((result (ccl:get-string-from-user 
                  prompt-string
                  :initial-string initial-string
                  :size (h-draw:make-point width height)
                  :ok-text ok-text
                  :cancel-text cancel-text
                  :position (h-draw:make-point left (screen-to-host-y top)))))
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
                         :result-type 'symbol
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
  (ccl::y-or-n-dialog message
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
    (ccl::select-item-from-list list
                                :window-title prompt-text
                                :table-print-function 
                                #'(lambda(x s) (princ (funcall item-function x) s))
                                :selection-type selection-type)
    (ccl::select-item-from-list list
                                :window-title prompt-text
                                :selection-type selection-type)))

  
  
