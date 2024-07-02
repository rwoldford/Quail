;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prompt-pc.lsp
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
;;;     G.W. Bennett 1996-1997
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

;; Build the list of dialog-items
(defun build-multi-dialog-items (items &key (prompt-text "Pick one of the following") 
                                       (item-print-function NIL)
                                       (action-function #'(lambda (i) i))
                                       (select-text "select")
                                       (cancel-text "cancel"))
  ;(declare (ignore item-print-function action-function))
  (let* ((max-item-width (reduce #'max (mapcar #'(lambda (x) (quail-string-width x)) items)))
         
         (error-text "please choose an item first!") 
         (error-text-width (quail-string-width error-text))
         (prompt-text-width (quail-string-width prompt-text)) 
         (text-box-width (+ 5 (max prompt-text-width error-text-width))) 
         ;(text-box-height (+ 10 text-height))
         (prompt-box-height (+ 10  *text-height*))
         (button-width 
           (+ 5 
              (max (quail-string-width *select-text*)
                   (quail-string-width cancel-text)
                   )))
         ;; make the prompt stuff  
         (text-dialog 
           (make-instance  'cg::static-text
                          :box (cg::make-box-relative 
                                 (+ *dialog-outer-left* *min-separator*) 
                                 (+ *dialog-outer-top* *min-separator*) 
                                 text-box-width prompt-box-height)
                          :value  (format nil prompt-text)))
         ;; prompt stuff done
         ;; make the error stuff
         (error-dialog 
           (make-instance  'cg::static-text
                          :box (cg::make-box-relative
                                 (+ *dialog-outer-left* *min-separator*)
                                 (+ *dialog-outer-top* *min-separator*)
                                 text-box-width *text-box-height*)
                          :border :static
                          :value error-text))
         ;; error stuff done                          
         ;; seq-dialog should start here
         (seq-dialog 
           (make-instance  'cg::multi-item-list
                          :range items
                          :box (cg::make-box-relative 
                                 (cg::left  (cg::box text-dialog))
                                 (+ (cg::bottom (cg::box text-dialog)) *min-separator*  )
                                 (+  max-item-width *scroll-bar-width*)  
                                 (+ *min-separator* (* (min (length items) *displayed*) *text-height*)))
                          :title "Pick one"
                          :available-p T
                          )
           )
         ;; seq-dialog should end here          
         ;; cancel begins
         (cancel-button
           (make-instance  'cg::cancel-button
                          :box (cg::make-box-relative 
                                 (+ (cg::right  (cg::box  seq-dialog)) *min-separator*)
                                 (+ (cg::bottom  (cg::box  text-dialog)) *min-separator*)
                                 button-width *button-height*)
                          :title cancel-text
                          ))
         ;; cancel done          
         ;; make select stuff
         (select-button
           (make-instance  'cg::button
                          :box (cg::make-box-relative 
                                 (cg::left  (cg::box cancel-button))
                                 (+ (cg::bottom  (cg::box  cancel-button)) *min-separator*)                  
                                 button-width *button-height*)                              
                          :title select-text
                          :set-value-fn
                          #'(lambda (&rest args)
                             (cond ((not 
                                      (cg::value seq-dialog)
                                      )
                                    (cg::pop-up-message-dialog 
                                      (cg::window seq-dialog)
                                      "You choose ... " error-text cg::warning-icon "OK?")
                                    ;; Accept the button click but do not return from the
                                    ;; main dialog yet after popping up the warning
                                    (values t nil))
                                   ;; Return from the main dialog since we noted above that
                                   ;; a selection was made
                                   (t (values t t))))
                          ))
         ;; select done
         )
    (list text-dialog error-dialog seq-dialog select-button cancel-button)
    )) ;; end of build-multi-dialog-items
;; Create the dialog from what build-.. produces
(defun create-multi-dialog (items &key (prompt-text "Please choose one or more ... ")
                                  (item-print-function NIL)
                                  (action-function #'(lambda (i) i))
                                  (select-text "select")
                                  (cancel-text "cancel"))
  (let* ((list-of-dialog-items (build-multi-dialog-items items
                                                         :prompt-text prompt-text
                                                         :item-print-function item-print-function
                                                         :action-function action-function
                                                         :select-text select-text
                                                         :cancel-text cancel-text))
         (text-dialog (first list-of-dialog-items))
         (select-button (fourth list-of-dialog-items))
         (cancel-button (fifth list-of-dialog-items))
         (seq-dialog (third list-of-dialog-items))                               
         ;; Start creation of base-dialog here
         ;; make an empty dialog-item  
         (base-item 
           (list (make-instance  'cg::button
                                :box (cg::make-box 20 20 20 20))
                 ))
         ;; make an empty dialog from this item
         (base-dialog 
           (cg::open-dialog base-item 'cg::dialog 
                            (cg::screen cg::*system*) :title "You choose  .." 
                            :pop-up-p t  
                            :font (cg::make-font :roman "times\ new\ roman" 20);(canvas-font-to-host-font *prompt-normal-font*)
                            :window-exterior (cg::make-box-relative
                                               *dialog-outer-left*
                                               *dialog-outer-top*
                                               (+ *min-separator* 
                                                  (max (cg::right (cg::box text-dialog))
                                                       (cg::right (cg::box select-button))
                                                       (cg::right (cg::box cancel-button))))
                                               (+ (* 4 *min-separator*)
                                                  (max (cg::bottom (cg::box seq-dialog))
                                                       (cg::bottom  (cg::box cancel-button)))))
                            ))
         ;; End creation of empty base-dialog here
         )
    ;; add new items
    (cg::update-dialog base-dialog list-of-dialog-items)        
    base-dialog)
  ) ;; end of create-multi-dialog

;; Now process it
(defun process-multi-dialog (items &key (prompt-text "Please choose one or more .. ")
                                   (item-print-function nil) 
                                   (action-function #'identity)                              
                                   (select-text "select")
                                   (cancel-text "cancel"))
  ;; Make the dialog itself
  (let ((new-items '()))
    (cond
      ((functionp item-print-function)
       (setf new-items (mapcar item-print-function items)))
      ((null item-print-function)
       (setf new-items (mapcar #'(lambda (item)
                                  (format NIL "~s" item))
                               items)))
      (T items))
    ;)
    (let* ((a-dialog (create-multi-dialog new-items :prompt-text prompt-text
                                          :item-print-function item-print-function
                                          :action-function action-function
                                          :select-text select-text
                                          :cancel-text cancel-text))
           (error-text "Please choose an item first!")
           ;; Show the completed dialog
           (clicked-button 
             (cg::pop-up-modal-dialog a-dialog))
           (d-items 
             (mapcar #'cg::value (cg::dialog-items a-dialog))
             )
           (seq-v  (third d-items))
           (select-v (fourth d-items))
           (cancel-v  (fifth d-items)))
      (cond (cancel-v (abort) )
            (t (loop for item in items as new-item in new-items when
                     (member new-item seq-v :test #'string-equal)
                     collect (funcall action-function item))))
      ))) ;; end of process-multi-dialog

;;; Code in the mold of other dialogs follows
;;; build-prompt-user -> create-prompt-user -> prompt-user itself

(defun build-prompt-user (&key ;(result-type T) 
                               ;(read-type :string) ;07aug2023 type->result-type
                               (prompt-string "")
                               (initial-string "") 
                               (ok-text "OK")
                               (cancel-text "cancel")
                               )
  (let* ((response-width 100)
         (prompt-text-width (quail-string-width prompt-string))
         (text-box-width (+ 5 prompt-text-width ))
         (prompt-box-height (+ 10  *text-height*))
         (button-width 
           (+ 5 
              (max (quail-string-width ok-text)
                   (quail-string-width cancel-text)
                   )))
         ;; make the prompt stuff  
         (text-dialog 
           (make-instance  'cg::static-text
                          :box (cg::make-box-relative 
                                 (+ *dialog-outer-left* *min-separator*) 
                                 (+ *dialog-outer-top* *min-separator*) 
                                 text-box-width prompt-box-height)
                          :value (format nil prompt-string)
                          :name :text-d)
           )
         ;; prompt stuff done
         
         ;; Now set the top of the first item
         (first-item-y (cg::bottom (cg::box text-dialog)))         
         
         ;;; Make the response-dialog
         (response-dialog
           (make-instance  'cg::editable-text
                          :value initial-string
                          :box (cg::make-box-relative
                                 (+ *dialog-outer-left* *min-separator*)
                                 (+ first-item-y *min-separator*)
                                 (max response-width prompt-text-width)
                                 *text-box-height*)
                          :name :resp-d))
         
         ;;; Find two values needed for later sizing and positioning
         (collect-bottom (cg::bottom (cg::box
                                       response-dialog)))
         ;; cancel begins
         (cancel-button
           (make-instance  'cg::cancel-button
                          :box (cg::make-box-relative 
                                 (+ *dialog-outer-left* *min-separator*)
                                 (+ collect-bottom *min-separator*)
                                 button-width *button-height*)                              
                          :title cancel-text
                          :name :cancel-d
                          :on-click #'(lambda (x y)
                                       (declare (ignore x y))
                                       (abort))
                          ))
         (cancel-bottom (cg::bottom (cg::box cancel-button)))
         ;; cancel done 
         
         ;; make select stuff
         (select-button
           (make-instance  'cg::button
                          :box (cg::make-box-relative 
                                 (+ (cg::right (cg::box cancel-button))
                                    *min-separator*)
                                 (+ collect-bottom *min-separator*)
                                 button-width *button-height*)                              
                          :title ok-text
                          :name :select-d
                          :set-value-fn 
                          #'(lambda (&rest args)
                             (values t t))
                          ))
         ;; select done
         ;; compute overall sizes             
         (overall-width (+ *min-separator* (max (cg::right (cg::box 
                                                             text-dialog))
                                                (cg::right (cg::box
                                                             response-dialog))
                                                (cg::right (cg::box
                                                             select-button))
                                                (cg::right (cg::box
                                                             cancel-button)))))
         (overall-height (+ (* 4 *min-separator*) cancel-bottom ))
         )
    ;; return some sizes and the dialogs
    (list overall-width overall-height text-dialog response-dialog select-button cancel-button)
    ))

  ;; 18oct05 to replace just prompt-user itself
  (defun create-prompt-user (&key ;(result-type T)
                                  ;(read-type :string)
                                  (prompt-string "")
                                  (initial-string "")
                                  (ok-text "OK")
                                  (cancel-text "cancel")
                                  )
    (let* ((list-of-dialog-items (build-prompt-user ;:result-type result-type  ;07aug2023 type -> result-type
                                                    ;:read-type read-type 
                                                    :prompt-string prompt-string 
                                                    :initial-string initial-string
                                                    :ok-text ok-text :cancel-text cancel-text))
           (overall-width (first list-of-dialog-items))
           (overall-height (second list-of-dialog-items))
           ;; Start creation of base-dialog here
           ;; make an empty dialog-item  
           (base-item 
             (list 
               (make-instance  'cg::button
                              :box (cg::make-box 20 20 20 20))
               ))
           ;; make an empty dialog from this item
           (base-dialog 
             (cg::open-dialog base-item 
                              'cg::dialog 
                              (cg::screen cg::*system*) :title "Your input .." 
                              :pop-up-p t  
                              :font (cg::make-font :roman "times\ new\ roman"20);(canvas-font-to-host-font *prompt-normal-font*)
                              :window-exterior (cg::make-box-relative
                                                 *dialog-outer-left*
                                                 *dialog-outer-top*
                                                 overall-width
                                                 overall-height
                                                 )
                              ))
           ) 
      ;; add new items
      (cg::update-dialog base-dialog (cddr  list-of-dialog-items))
      base-dialog))

(defun prompt-user (&key (result-type T)
                         (read-type :string)
                         (prompt-string "")
                         (initial-string "")
                         (ok-text "OK")
                         (cancel-text "Cancel")
                         (left 10) (top 10)
                         width
                         (height 100))
  (declare (ignore left top width height)) ;17oct2023
  ;; Make the dialog itself
  (let* ((a-dialog (create-prompt-user ;:result-type result-type
                                       ;:read-type read-type
                                       :prompt-string prompt-string 
                                       :initial-string initial-string
                                       :ok-text ok-text :cancel-text cancel-text
                                       )))
    ;; Show the completed dialog
    (cg::pop-up-modal-dialog a-dialog)
    (let ((cancel-v (cg::value 
                      (cg::find-component :cancel-d a-dialog)
                      ))
          (select-v (cg::value (cg::find-component :resp-d a-dialog)
                               ))
          )
      (if cancel-v
          (abort)                  
          (let* ((result select-v))
            (setf result
                  (case read-type
                        (:string result)
                        (:eval (eval (read-from-string result :preserve-whitespace t)))
                        (:read (read-from-string result :preserve-whitespace t))
                        )
                  )
            (if (typep result result-type)
                result
                (quail-error "~S is not of type ~s" result result-type)))
          ))) 
  )
      
(defun save-value (arg)
        "Saves the value of arg as the value of a symbol retrieved from a prompt."
        (eval
          `(setf ,(prompt-user  :read-type :read
                               :result-type 'symbol
                               :prompt-string 
                               "Give a symbol to store the value on:")
                 ,arg)))

(defun prompt-t-or-f (message &key (true-text "Yes")
                                  (false-text "No")
                                  (cancel-text "Cancel"))
     "Prompts the user to choose between two alternatives ~
(true-text and false-text) in response to the information ~
given in message. T is returned if the first letter ~
of the true-text (default the string Yes) is typed by the ~
user; NIL if the first letter of the false-text is typed. ~
If cancel-text is selected, processing stops."

(let* ((answer (cg::ask-user-for-choice 
                        message true-text false-text cancel-text)))
    (cond ((eq answer true-text) T)
              ((eq answer false-text) NIL)
              ((eq answer cancel-text) NIL))))

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
  (declare (ignore selection-type))
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
  (process-multi-dialog list :prompt-text prompt-text :item-print-function
                        item-function
                        ))
