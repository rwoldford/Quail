;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prompt-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;  Authors:
;;;     Greg Anglin 1991
;;;     H.A. Chipman 1991
;;;     C.B. Hurley 1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford 1989-1992
;;;     J.O. Pedersen 1988-89
;;;     G.W. Bennett 1996-1997, 2020
;;;     
;;;----------------------------------------------------------------------------------
;;;
;;; This holds the building, creation, and processing
;;; of multi-item-lists all of which are needed for
;;; prompt-user files. They are here because of the
;;; limitation on ACL/PC's edit buffer!!
;;;
;;; G.W.Bennett  January 24 1997
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (export  '(prompt-user save-value prompt-t-or-f prompt-true-or-false
           prompt-for-items)))

;;; an application frame to do the prompting
(define-application-frame frame-which-accepts-values ()
  ((accepted-value :initform nil :accessor accepted-value)
    (result-type :initarg :result-type :accessor result-type)
    (read-type :initarg :read-type :accessor read-type)
    (prompt-string :initarg :prompt-string :accessor prompt-string)
    (ok-text :initarg :ok-text :accessor ok-text)
    (cancel-text :initarg :cancel-text :accessor cancel-text))
   (:panes (accept :application 
   :display-function (lambda (frame pane)
                      (terpri pane)
                      (terpri pane)
                       (format pane "Accepted object is  is: ~s~%" (accepted-value frame))
                       (terpri pane)
                       (with-output-as-gadget (pane)
                         (make-pane :push-button
                                    :label "Accept object !"
                                    :activate-callback
                                    (lambda (gadget)
                                      (terpri pane)
                                      (setf  (accepted-value frame)
                                            (accepting-values (stream :own-window t :width 300 :height 150 
                                              :label "Please enter your selection")
                                              (fresh-line stream)
                                              (accept (result-type *application-frame*) :prompt "An object  " :stream stream)))
                                      (terpri pane)
                                      (redisplay-frame-pane frame pane)
                                      )))
   ))
   (prompt-pane 
    (make-pane 'clim-stream-pane
     :height 30
     ;:scroll-bars nil ;27NOV2024
           :display-time t
           :display-function #'(lambda (frame pane) (draw-text* pane (prompt-string *application-frame*)
            10 25 :ink +blue+ :text-size 18))
           ))
   )
   (:layouts (default 
      (vertically (:height 200 :width 250) 
        prompt-pane 
        accept 
        (horizontally ()
        +fill+
        (make-pane 'push-button
          :label (string-upcase (ok-text *application-frame*))
          :activate-callback
          (lambda (ignore)
      ignore
      (frame-exit *application-frame*)))
        (make-pane 'push-button
          :label (string-upcase (cancel-text *application-frame*))
          :activate-callback
          (lambda (ignore)
      ignore
      (frame-exit *application-frame*))))
    ))))


;;; prompt-user itself
(defun prompt-user (&key (result-type 'string)
                         (read-type :string)
                         (prompt-string "")
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
   (:arg result-type T  Data type of item to be returned by user.  Any Common Lisp ~
    data-type is acceptable.) ~
   (:arg read-type :string  How to read the typed input.  ~
                            Legal values are :string :eval or :read.) ~
   (:arg left NIL  Location of the left of the prompt window.) ~
   (:arg top NIL  Location of the top of the prompt window.) ~
   (:arg width NIL  Width of prompt window.) ~
   (:arg height 100  Height of prompt window.) ~
   )"
  (let ((frame
    (make-application-frame 'frame-which-accepts-values :pretty-name "Your input .." :result-type result-type :read-type read-type :prompt-string prompt-string
      :initial-string initial-string :ok-text ok-text :cancel-text cancel-text :left left :top top :width width :height height)))
  (run-frame-top-level frame)
  (return-from prompt-user (accepted-value frame))))

;;; save-vaalue
(defun save-value (arg)
     "Saves the value of arg as the value of a symbol retrieved from a prompt."
     (eval
        `(setf ,(prompt-user  :read-type :read
                   :result-type 'symbol
                   :prompt-string 
                   "Give a symbol to store the value on:")
                 ,arg)))

;;; prompt-t-or-f
(defun prompt-t-or-f (message
                      &key
                      (true-text "Yes")
                      (false-text "No")
                      (cancel-text "Cancel"))
  "Prompts the user to make a decision between two alternatives (true-text and false-text) ~
   in response to the information given in message.  T is returned on pressing the button with label ~
   true-text (default the string Yes) ; NIL is returned on pressing the button with label ~
   false-text (default the string No).  ~
   If cancelled by the user, processing ends. ~
   (:see-also prompt-for-items pick-one prompt-true-or-false)"
  (let ((frame
      (make-application-frame 'prompt-t-or-f :pretty-name "Please check your response" :message message :true-text true-text :false-text false-text :cancel-text cancel-text)))
  (run-frame-top-level frame)
  (return-from  prompt-t-or-f (frame-result frame))))

;;; and its application frame
(define-application-frame prompt-t-or-f () 
  ((frame-result :initform nil :accessor frame-result)
    (message :initarg :message :accessor message)
    (true-text :initarg :true-text :accessor true-text)
    (false-text :initarg :false-text :accessor false-text)
    (cancel-text :initarg :cancel-text :accessor cancel-text))
  (:menu-bar nil)
  (:panes
    (prompt-pane 
    (make-pane 'clim-stream-pane
     :height 30
     :scroll-bars nil
           :display-time t
           :display-function #'(lambda (frame pane) (draw-text* pane  
            (message *application-frame*) 10 25 :ink +blue+ :text-size 18))
           )))
  (:layouts
   (default
    (vertically (:height 100 :width 350)
      prompt-pane
   (horizontally ()
     +fill+
     (make-pane 'push-button
          :label (string-upcase (true-text *application-frame*))
          :activate-callback
          (lambda (ignore)
            ignore
            (setf (frame-result *application-frame*) T)
      (frame-exit *application-frame*))
          )
     (make-pane 'push-button
          :label (string-upcase (false-text *application-frame*))
          :activate-callback
          (lambda (ignore)
      ignore
            (setf (frame-result *application-frame*) NIL)
      (frame-exit *application-frame*)))
     (make-pane 'push-button
          :label (string-upcase (cancel-text *application-frame*))
          :activate-callback
          (lambda (ignore)
      ignore
      (frame-exit *application-frame*)))
     )))))

;;; prompt-true-or-false
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

;;; prompt-for-items
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
    (cond ((eql selection-type :single)
      (pick-one list :prompt-text prompt-text))
    ((eql selection-type :disjoint)
    (check-items list :prompt-text prompt-text))
    ((eql selection-type :contiguous)
    (check-items list :prompt-text "For contiguous selection , hold down shift-key")))))
