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
(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(prompt-user save-value prompt-t-or-f prompt-true-or-false
           prompt-for-items)))


(defun how-many (char string)
   "Counts the number of occurrences of char in string"
    (- (length string)
      (length (remove-if #'(lambda (x)
                             (eq x char)) string))))

(defun list-of-lengths (string)
   ;; Assume string does *not* start with ~%
   ;; Gets a list of the stream-string-width of the
   ;; text bits of string
   (let ((result '())
         (fmat (list #\~ #\% #\Newline))
         (alphabet (list #\a #\b #\c #\d #\e #\f #\g #\h
                 #\i #\j #\k #\l #\m #\n #\o #\p #\q
                 #\r #\s #\t #\u #\v #\w #\x #\y #\z
                 #\A #\B #\C #\D #\E #\F #\G #\H
                 #\I #\J #\K #\L #\M #\N #\O #\P #\Q
                 #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                 #\Space  #\. #\, #\; #\: #\( #\)))
         )
      (push 
       (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
       (cg::stream-string-width (cg::screen cg::*system*) string))) result)
      (do ((j 0 (+ j 1))
           (n (how-many #\Newline string)))
           ((= j n))
           (setf string (string-left-trim fmat 
                        (string-left-trim alphabet string)))
           (push 
         (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
         (cg::stream-string-width (cg::screen cg::*system*) string))) result)
      )
    result))

(defun max-text-segment (lengths) 
   " Gets the length of the maximum text segment~
   from a collection of cumulative lengths of a piece~
   of text assumed to have come from list-of-lengths."
   (let ((diffs '()))
      (do ((j 0 (+ j 1))
           (n (length lengths)))
          ((= j (- n 1)))
         (push (- (elt lengths j) (elt lengths (+ j 1))) diffs))
      (cond ((eq 1 (length lengths))
             (first lengths))
            (T (abs (first (sort diffs #'<))))
            )
      ))

;; Build the list of dialog-items
(defun build-multi-dialog-items (items &key (prompt-text "Pick one of the following") 
                                  (item-print-function NIL)
                                  (action-function #'(lambda (i) i))
                                  (select-text "select")
                                  (cancel-text "cancel"))
   (flet ((text-width (string) 
          (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
          (cg::stream-string-width (cg::screen cg::*system*) string))))
          (set-view-size (a-window) 
            (list (cg::exterior-width a-window) (cg::exterior-height a-window))))
     (let* ((view-font 
             (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
             (cg::font (cg::screen cg::*system*)))))
            (dialog-outer-left 10) ;; left of window-exterior of whole dialog
            (dialog-outer-top 10) ;; top of window-exterior of whole dialog
            (scroll-bar-width 25) ;; a guess at this
            (min-separator 15) ;; distance between widgets H and V
            (displayed 5) ;; Number of items visible
            (max-item-width (first (sort (mapcar #'(lambda (x)
                             (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
                                                     (cg::stream-string-width (cg::screen cg::*system*) x))))
                                           items) #'>)))
            (error-text "please choose an item first!") 
            (error-text-width (text-width error-text)) 
            (prompt-text-width (text-width prompt-text)) 
            (temp-font-info 
               (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
               (cg::fontmetrics (cg::screen cg::*system*)))))
            (text-height 
              (+ (cg::font-ascent temp-font-info) (cg::font-descent temp-font-info) 
                 (cg::font-leading temp-font-info)))
            (text-box-width (+ 5 (max prompt-text-width error-text-width))) 
            (text-box-height (+ 10 text-height))
            (prompt-box-height (+ 10 (* (+ 1 (how-many #\Newline
                                               prompt-text))
                                        text-height)))
            (button-height (+ 2 text-height)) 
            (button-width 
              (+ 5 
                 (max (text-width select-text) 
                   (text-width cancel-text))))
            (seq-height 200) 
            (seq-width 200)
            ;; make the prompt stuff  
            (text-dialog 
              (cg::make-dialog-item :widget 'cg::static-text 
               :box (cg::make-box-relative 
                     (+ dialog-outer-left min-separator) 
                     (+ dialog-outer-top min-separator) 
                     text-box-width prompt-box-height)
               :value  (format nil prompt-text)))
            ;; prompt stuff done
            ;; make the error stuff
            (error-dialog
              (cg::make-dialog-item :widget 'cg::static-text
               :box (cg::make-box-relative
                     (+ dialog-outer-left min-separator)
                     (+ dialog-outer-top min-separator)
                     text-box-width text-box-height)
               :border :static
               :value error-text))
            ;; error stuff done                          
            ;; seq-dialog should start here
            (seq-dialog
              (cg::make-dialog-item
               :widget cg::'multi-item-list
               :range items
               :box (cg::make-box-relative 
                     (cg::box-left  (cg::dialog-item-box text-dialog))
                     (+ (cg::box-bottom (cg::dialog-item-box text-dialog)) min-separator  )
                     ;+                 max-item-width
                     (+  max-item-width scroll-bar-width)  
                     (+ min-separator (* (min (length items) displayed) text-height)))
               :title "Pick one"
               :available-p T
               )
              )
            ;; seq-dialog should end here          
            ;; cancel begins
            (cancel-button
              (cg::make-dialog-item
               :widget cg::'cancel-button
               :box (cg::make-box-relative 
                     (+ (cg::box-right  (cg::dialog-item-box  seq-dialog)) min-separator)
                     (+ (cg::box-bottom  (cg::dialog-item-box  text-dialog)) min-separator)
                     button-width button-height)
               :title cancel-text
               ))
            ;; cancel done          
            ;; make select stuff
            (select-button
              (cg::make-dialog-item
               :widget cg::'button
               :box (cg::make-box-relative 
                     (cg::box-left  (cg::dialog-item-box cancel-button))
                     (+ (cg::box-bottom  (cg::dialog-item-box  cancel-button)) min-separator)                  
                     button-width button-height)                              
               :title select-text
               :set-value-fn
               #'(lambda (&rest args)
                   (cond ((not (cg::dialog-item-value seq-dialog))
                          (cg::pop-up-message-dialog (cg::dialog-item-window seq-dialog)
                           "You choose ... " error-text cg::warning-icon "OK?")
                          ;; Accept the button click but do not return from the
                          ;; main dialog yet after popping up the warning
                          (values t nil))
                         ;; Return from the main dialog since we noted above that
                         ;; a selection was made
                         (t (values t t)))
                   )
               ))
            ;; select done
            ) ;; end of bindings in let* .. body follows
        (list text-dialog error-dialog seq-dialog select-button cancel-button)
        ) ;; end of let* .. it returns a list
     ) ;; end of flet
   ) ;; end of build-multi-dialog-items
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
          (view-font 
          (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
          (cg::font (cg::screen cg::*system*)))))
          (dialog-outer-left 10)
          (dialog-outer-top 10)
          (scroll-bar-width 25)
          (min-separator 15)
          (text-dialog (first list-of-dialog-items))
          (select-button (fourth list-of-dialog-items))
          (cancel-button (fifth list-of-dialog-items))
          (seq-dialog (third list-of-dialog-items))                               
          ;; Start creation of base-dialog here
          ;; make an empty dialog-item  
          (base-item 
            (list (cg::make-dialog-item :widget 'cg::button :box (cg::make-box 20 20 20 20))))
          ;; make an empty dialog from this item
          (base-dialog 
            (cg::open-dialog base-item 'cg::dialog (cg::screen cg::*system*) :title "You choose  .." :pop-up-p t  
                             :font (canvas-font-to-host-font *prompt-normal-font*)
             :window-exterior (cg::make-box-relative
                               dialog-outer-left
                               dialog-outer-top
                               (+ min-separator 
                                  (max (cg::box-right (cg::dialog-item-box text-dialog))
                                    (cg::box-right (cg::dialog-item-box select-button))
                                    (cg::box-right (cg::dialog-item-box cancel-button))))
                               (+ (* 4 min-separator)
                                  (max (cg::box-bottom (cg::dialog-item-box seq-dialog))
                                    (cg::box-bottom  (cg::dialog-item-box cancel-button)))))
             ))
          ;; End creation of empty base-dialog here
          ) ;end of list of let* .. body is next
      ;; add new items
      (cg::update-dialog base-dialog list-of-dialog-items)        
      base-dialog) ;; end of let* .. it returns an actual dialog
   ) ;; end of create-multi-dialog

;; Now process it
(defun process-multi-dialog (items &key (prompt-text "Please choose one or more .. ")
                              (item-print-function nil) 
                              (action-function #'identity)                              
                              (select-text "select")
                              (cancel-text "cancel"))
   ;; Make the dialog itself
   (let* ((new-items
            (cond
                  ((functionp item-print-function)
                   (setf new-items (mapcar item-print-function items)))
                  ((null item-print-function)
                   (setf new-items (mapcar #'(lambda (item)
                                               (format NIL "~s" item))
                                     items)))
                  (T items))
            )
          (a-dialog (create-multi-dialog new-items :prompt-text prompt-text
                      :item-print-function item-print-function
                      :action-function action-function
                      :select-text select-text
                      :cancel-text cancel-text))
          (error-text "Please choose an item first!")
          ;; Show the completed dialog
          (clicked-button (cg::pop-up-dialog a-dialog))
          (d-items (mapcar #'cg::dialog-item-value (cg::dialog-items a-dialog)))
          (seq-v  (third d-items))
          (select-v (fourth d-items))
          (cancel-v  (fifth d-items)))
      (cond (cancel-v (abort) )
            (t (loop for item in items as new-item in new-items when
                 (member new-item seq-v :test #'string-equal)
                 collect (funcall action-function item))))
      ) ;; end let*
   ) ;; end of process-multi-dialog

;;; Code in the mold of other dialogs follows
;;; build-prompt-user -> create-prompt-user -> prompt-user itself

(defun build-prompt-user (&key (type T) (read-type :string)
                           (prompt-string "")
                           (initial-string "") 
                           (ok-text "OK")
                           (cancel-text "cancel")
                           left top
                           width
                           (height 100)
                           )
     (flet ((text-width (string) 
             (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
             (cg::stream-string-width (cg::screen cg::*system*) string)))) 
             (set-view-size (a-window) 
              (list (cg::exterior-width a-window) (cg::exterior-height a-window))))
       (let* ((view-font 
                (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
                (cg::font (cg::screen cg::*system*)))))
                (dialog-outer-left 10)
                (dialog-outer-top 10)
                (min-separator 15) ;; minimum distance between boxes
                (response-width 100) ;; width of editable-text boxes
                ;(prompt-text-width (text-width prompt-text)) 
                (prompt-text-width (max-text-segment 
                                    (list-of-lengths prompt-string))) ;<<
                (temp-font-info 
                (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
                (cg::fontmetrics (cg::screen cg::*system*))))) 
                (text-height 
                 (+ (cg::font-ascent temp-font-info) (cg::font-descent temp-font-info) 
                     (cg::font-leading temp-font-info)))
                (text-box-width (+ 5 prompt-text-width )) 
                (text-box-height (+ 10 text-height)) ;<<
                (prompt-box-height (+ 10 (* (+ 1 (how-many #\Newline 
                                                 prompt-string)) 
                                          text-height))) ;<<
                (text-box-x 20)
                (text-box-y 20)
                (button-height (+ 2 text-height)) 
                (button-width 
                 (+ 5 
                     (max (text-width ok-text) 
                         (text-width cancel-text))))
                (seq-height 200) 
                (seq-width 200)
                ;; The following are set AFTER text-dialog
                ;;  (first-item-y (+ text-box-y text-box-height ))
                ;;  (first-item-x  dialog-outer-left )
                ;;  (button-x (+ 50 first-item-x (* columns col-width)))
                ;;  (view-size-width (max 200
                ;;                (+ button-x button-width 2)
                ;;                (+ text-box-x text-box-width 2)))
                ;;  (view-size-height (max 100
                ;;                (+ first-item-y
                ;;                 (+ 2 (* (+ nrows 1) text-box-height))
                ;;                        2)
                ;;                 (+ text-box-height 
                ;;                       button-height
                ;;                        2)))
                ;; make the prompt stuff  
                (text-dialog 
                 (cg::make-dialog-item :widget 'cg::static-text 
                  :box (cg::make-box-relative 
                           (+ dialog-outer-left min-separator) 
                           (+ dialog-outer-top min-separator) 
                           text-box-width prompt-box-height)
                  :value (format nil prompt-string) ;<<
                  :name :text-d)
                 )
                ;; prompt stuff done
                
                ;; Now set the top of the first item
                (first-item-y (cg::box-bottom (cg::dialog-item-box
                                                                text-dialog)))         
                
                ;;; Make the response-dialog
                (response-dialog
                 (cg::make-dialog-item
                  :widget cg::'editable-text
                  :value initial-string
                  :box (cg::make-box-relative
                           (+ dialog-outer-left min-separator)
                           (+ first-item-y min-separator)
                           (max response-width prompt-text-width)
                           text-box-height)
                  :name :resp-d))
                
                ;;; Find two values needed for later sizing and positioning
                (rightmost (cg::box-right (cg::dialog-item-box
                                                          response-dialog))) 
                (collect-bottom (cg::box-bottom (cg::dialog-item-box
                                                                      response-dialog)))
                ;; cancel begins
                (cancel-button
                 (cg::make-dialog-item
                  :widget cg::'cancel-button
                  :box (cg::make-box-relative 
                           (+ dialog-outer-left min-separator)
                           (+ collect-bottom min-separator)
                           button-width button-height)                              
                  :title cancel-text
                  :name :cancel-d
                  ))
                (cancel-bottom (cg::box-bottom (cg::dialog-item-box cancel-button)))
                ;; cancel done 
                
                ;; make select stuff
                (select-button
                 (cg::make-dialog-item
                  :widget cg::'button
                  :box (cg::make-box-relative 
                           (+ (cg::box-right (cg::dialog-item-box cancel-button))
                               min-separator)
                           (+ collect-bottom min-separator)
                           button-width button-height)                              
                  :title ok-text
                  :name :select-d
                  :set-value-fn
                  #'(lambda (&rest args)
                       (values t t))
                  ))
                (select-bottom (cg::box-bottom (cg::dialog-item-box select-button)))
                ;; select done
                ;; compute overall sizes             
              (overall-width (+ min-separator (max (cg::box-right (cg::dialog-item-box 
                                                                   text-dialog))
                                                (cg::box-right (cg::dialog-item-box
                                                                response-dialog))
                                                (cg::box-right (cg::dialog-item-box
                                                                select-button))
                                                (cg::box-right (cg::dialog-item-box
                                                                cancel-button)))))
                (overall-height (+ (* 4 min-separator) cancel-bottom ))
                ) ;; end of bindings in let* .. body follows
           ;; return some sizes and the dialogs
           (list overall-width overall-height text-dialog response-dialog select-button cancel-button)
           ) ;; end of let* .. it returns a list
       ) ;; end of flet
     )

(defun create-prompt-user (&key (type T)
                                                (read-type :string)
                                                (prompt-string "")
                                                (initial-string "")
                                                (ok-text "OK")
                                                (cancel-text "cancel")
                                                left top
                                                width
                                                (height 100)
                                                )
     (let* ((list-of-dialog-items (build-prompt-user :type type 
                                                :read-type read-type :prompt-string prompt-string 
                                                :initial-string initial-string
                                                :ok-text ok-text :cancel-text cancel-text
                                                :left left :top top :width width
                                                :height height))
              (view-font 
              (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
              (cg::font (cg::screen cg::*system*)))))
              (dialog-outer-left 10)
              (dialog-outer-top 10)
              (overall-width (first list-of-dialog-items))
              (overall-height (second list-of-dialog-items))
              ;; Start creation of base-dialog here
              ;; make an empty dialog-item  
              (base-item 
               (list (cg::make-dialog-item :widget 'cg::button :box (cg::make-box 20 20 20 20))))
              ;; make an empty dialog from this item
              (base-dialog 
               (cg::open-dialog base-item 'cg::dialog (cg::screen cg::*system*) :title "Your input .." :pop-up-p t  
                :font (canvas-font-to-host-font *prompt-normal-font*)
                :window-exterior (cg::make-box-relative
                                              dialog-outer-left
                                              dialog-outer-top
                                              overall-width
                                              overall-height
                                              )
                ))
              ;; End creation of empty base-dialog here
              ) ;end of list of let* .. body is next
         ;; add new items
         (cg::update-dialog base-dialog (cddr  list-of-dialog-items))
         base-dialog) ;; end of let* .. it returns an actual dialog
     )

(defun prompt-user (&key (type T)
                                     (read-type :string)
                                     (prompt-string "")
                                     (initial-string "")
                                     (ok-text "OK")
                                     (cancel-text "Cancel")
                                     left top
                                     width
                                     (height 100))
     ;; Make the dialog itself
     (let* ((a-dialog (create-prompt-user :type type :read-type read-type
                               :prompt-string prompt-string 
                               :initial-string initial-string
                               :ok-text ok-text :cancel-text cancel-text
                               ))
              ;; Show the completed dialog
              (showit (cg::pop-up-dialog a-dialog))
              (cancel-v (cg::dialog-item-value 
                               (cg::widget :cancel-d a-dialog)))
              (select-v (cg::dialog-item-value
                              (cg::widget :resp-d a-dialog)))
              )
         (if cancel-v
            (abort)        ; was initial-string                    
            (let* ((result select-v))
                (setf result
                        (case read-type
                           (:string result)
                           (:eval (eval (read-from-string result :preserve-whitespace t)))
                           (:read (read-from-string result :preserve-whitespace t))
                           )
                        )
                (if (typep result type)
                   result
                   (quail-error "~S is not of type ~s" result type)))
            )
         ) ;; end let*
     )
      
(defun save-value (arg)
     "Saves the value of arg as the value of a symbol retrieved from a prompt."
     (eval
        `(setf ,(prompt-user  :read-type :read
                   :type 'symbol
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
 ;(if (functionp item-function)
   (process-multi-dialog list :prompt-text prompt-text :item-print-function
     item-function
    )
 ;   (process-multi-dialog list :prompt-text prompt-text)
 )
