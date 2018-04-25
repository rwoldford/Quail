;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prompt-user-expts.lsp
;;; started 18oct2005
;;; to try to get the prompt-user stuff to function correctly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define a temporary version of :wb
(defpackage "WINDOW-BASICS"
  (:use "COMMON-LISP")
  (:nicknames "WB"))
(in-package :wb)
;;;; I need the auxiliary functions at the top of prompt-pc.lsp
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
       (cg::stream-string-width scrn string))) result)
      (do ((j 0 (+ j 1))
           (n (how-many #\Newline string)))
           ((= j n))
           (setf string (string-left-trim fmat 
                        (string-left-trim alphabet string)))
           (push 
         (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
         (cg::stream-string-width scrn string))) result)
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

;;;; Here is the code from prompt-pc.lsp
;;;; First build-prompt-user

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
             (cg::stream-string-width scrn string)))) 
             (set-view-size (a-window) 
              (list (cg::exterior-width a-window) (cg::exterior-height a-window))))
       (let* ((scrn (cg::screen cg::*system*))
              (view-font 
               ;(let ((scrn (cg::screen cg::*system*))) 17oct05
                 (cg::with-device-context (hdc scrn)
                   (cg::font scrn)))
                (dialog-outer-left 10)
                (dialog-outer-top 10)
                (min-separator 15) ;; minimum distance between boxes
                (response-width 100) ;; width of editable-text boxes
                ;(prompt-text-width (text-width prompt-text)) 
                (prompt-text-width (max-text-segment 
                                    (list-of-lengths prompt-string))) ;<<
                (temp-font-info 
                ;(let ((scrn (cg::screen cg::*system*))) 17oct05
    (cg::with-device-context (hdc scrn)
                (cg::fontmetrics scrn))) 
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
                 ;(cg::make-dialog-item :widget 'cg::static-text 17oct05
                 (make-instance  'cg::static-text
                  :box (cg::make-box-relative 
                           (+ dialog-outer-left min-separator) 
                           (+ dialog-outer-top min-separator) 
                           text-box-width prompt-box-height)
                  :value (format nil prompt-string) ;<<
                  :name :text-d)
                 )
                ;; prompt stuff done
                
                ;; Now set the top of the first item
                (first-item-y (cg::bottom (cg::box
                                                                text-dialog)))         
                
                ;;; Make the response-dialog
                (response-dialog
                 ;(cg::make-dialog-item :widget cg::'editable-text 17oct05
                 (make-instance  'cg::editable-text
                  :value initial-string
                  :box (cg::make-box-relative
                           (+ dialog-outer-left min-separator)
                           (+ first-item-y min-separator)
                           (max response-width prompt-text-width)
                           text-box-height)
                  :name :resp-d))
                
                ;;; Find two values needed for later sizing and positioning
                (rightmost (cg::right (cg::box
                                                          response-dialog))) 
                (collect-bottom (cg::bottom (cg::box
                                                                      response-dialog)))
                ;; cancel begins
                (cancel-button
                 ;(cg::make-dialog-item :widget cg::'cancel-button 17oct05
                 (make-instance  'cg::cancel-button
                  :box (cg::make-box-relative 
                           (+ dialog-outer-left min-separator)
                           (+ collect-bottom min-separator)
                           button-width button-height)                              
                  :title cancel-text
                   :name :cancel-d
                   :on-click #'(lambda (x y) ;18oct05
                                 (abort)) ;18oct05
                  ))
                (cancel-bottom (cg::bottom (cg::box cancel-button)))
                ;; cancel done 
                
                ;; make select stuff
                (select-button
                 ;(cg::make-dialog-item :widget cg::'button 17oct05
                 (make-instance  'cg::button
                  :box (cg::make-box-relative 
                           (+ (cg::right (cg::box cancel-button))
                               min-separator)
                           (+ collect-bottom min-separator)
                           button-width button-height)                              
                  :title ok-text
                  :name :select-d
                  :set-value-fn
                  #'(lambda (&rest args)
                       (values t t))
                  ))
                (select-bottom (cg::bottom (cg::box select-button)))
                ;; select done
                ;; compute overall sizes             
              (overall-width (+ min-separator (max (cg::right (cg::box 
                                                                   text-dialog))
                                                (cg::right (cg::box
                                                                response-dialog))
                                                (cg::right (cg::box
                                                                select-button))
                                                (cg::right (cg::box
                                                                cancel-button)))))
                (overall-height (+ (* 4 min-separator) cancel-bottom ))
                ) ;; end of bindings in let* .. body follows
           ;; return some sizes and the dialogs
         (list overall-width overall-height text-dialog 
               response-dialog select-button cancel-button)
           ) ;; end of let* .. it returns a list
       ) ;; end of flet
     )

;;;; .. then create-prompt-user

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
              (cg::font scrn))))
              (dialog-outer-left 10)
              (dialog-outer-top 10)
              (overall-width (first list-of-dialog-items))
              (overall-height (second list-of-dialog-items))
              ;; Start creation of base-dialog here
              ;; make an empty dialog-item  
              (base-item 
               (list 
                ;(cg::make-dialog-item :widget 'cg::button 17oct05
                (make-instance  'cg::button
                                      :box (cg::make-box 20 20 20 20))
                     ))
              ;; make an empty dialog from this item
              (base-dialog 
               (cg::open-dialog base-item 
                                'cg::dialog 
                                (cg::screen cg::*system*) :title "Your input .." 
                                :pop-up-p t  
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

;;;; .. finally prompt-user itself

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
            (showit 
             ;(cg::pop-up-dialog a-dialog) 18oct05
             (cg::make-window (gensym)
             :class 'cg::dialog
             :owner (cg::development-main-window cg::*system*)
             :title "Choice Dialog"
             :visible-box (cg::box a-dialog)
             :widgets (cg::dialog-items a-dialog)
                          ))
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

;;;; Some code based on dialogs70.lsp
(setf *element4*
  (make-instance 'cg::multi-item-list
    :left 10 :top 10 :width 190 :height 100
    :range (list "First" "Second" "Third")
    ))
(setf *select4*
  (make-instance 'cg::static-text
    :left 10 :top 120 :width 190 :height 30
    :value "Select"
    :on-click #'(lambda (x y)
                  (format t "Results is ~s"
                    (cg::selected-object *element4*)))
    ))
(cg::make-window (gensym)
  :class 'cg::dialog
  :owner (cg::development-main-window cg::*system*)
  :title "DIALOG 1"
  :visible-box (cg::make-box 0 0 200 160)
  :widgets (list *element4* *select4*))
                    
