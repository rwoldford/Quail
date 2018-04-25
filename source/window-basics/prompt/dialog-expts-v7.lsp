;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dialog-expts-v7.lsp
;;;
;;; Consolidating some things from code-fragment.lsp
;;; in an attempt to sort out dialogs in v7
;;; I think I need to employ :on-click argument
;;; when making items
;;;
;;; Started 14JAN05
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;14 Jan 05
;;; collect some definitions
(setf displayed 5
  scroll-bar-width 25
  seq-height 200
  seq-width 200
  dialog-outer-left 10
  min-separator 15
  dialog-outer-top 10
  prompt-text "Here is a text-dialog"
  select-text "Select"
  cancel-text "Cancel"
  error-text "please choose an item first!"
  items (list "First" "Second" "Third")) ;; ok[1]
(setf temp-font-info 
               (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
      (cg::fontmetrics (cg::screen cg::*system*))))) ;; ok[2]
(setf text-height 
              (+ (cg::font-ascent temp-font-info) (cg::font-descent temp-font-info) 
                 (cg::font-leading temp-font-info))) ;; 19 ok[3] 
(setf button-height (+ 2 text-height)) ;; 21 ok[4]
(setf prompt-box-height (+ 10 (* 1 text-height))) ;; 29 ok[5]
(defun text-width (string) 
          (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
      (cg::stream-string-width (cg::screen cg::*system*) string)))) ;; text-width ok[6] 
(setf button-width 
              (+ 5 
                 (max (text-width select-text) 
                   (text-width cancel-text)))) ;; 49 ok[7] 
(setf prompt-text-width (text-width prompt-text)) ;; 131 ok[8]
(setf text-box-width (+ 5  prompt-text-width )) ;; 136 ok[9]
(setf text-box-height (+ 10 text-height)) ;; 29 ok[10]
(setf max-item-width (first (sort (mapcar #'(lambda (x)
                                              (let ((scrn (cg::screen cg::*system*)))
                                                (cg::with-device-context (hdc scrn)
                                                  (cg::stream-string-width (cg::screen cg::*system*) x))))
                                    items) #'>))) ;; 48 ok[11]
;;; And some forms too
(defun how-many (char string)
   "Counts the number of occurrences of char in string"
    (- (length string)
      (length (remove-if #'(lambda (x)
                             (eq x char)) string)))) ;; how-many ok[12]
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
     result)) ;; list-of-lengths ok[12]

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
     )) ;; max-text-segment ok[13]
;;; Finally, some dialogs too
(setf text-dialog 
            (make-instance cg::'static-text
              ;(cg::make-dialog-item :widget 'cg::static-text 10jan05
               :box (cg::make-box-relative 
                     (+ dialog-outer-left min-separator) 
                     (+ dialog-outer-top min-separator) 
                     text-box-width prompt-box-height)
              :value  (format nil prompt-text))) ;; ok[14]
;;; can I show this now ?
(pop-up-modal-dialog text-dialog :stream (screen *system*)
  :initial-focus NIL :bring-thread-to-front T) ;; error!
;;; I'll use create-multi-dialog with list-of-dialog-items just
;;; this text dialog
(setf list-of-dialog-items (list text-dialog)) ;; ok[14]

(setf error-dialog 
            (make-instance cg::'static-text
              ;(cg::make-dialog-item :widget 'cg::static-text 10jan05
               :box (cg::make-box-relative
                     (+ dialog-outer-left min-separator)
                     (+ dialog-outer-top min-separator)
                     text-box-width text-box-height)
               :border :static
              :value error-text)) ;; ok[16]

(setf seq-dialog
             (make-instance cg::'multi-item-list
              ;(cg::make-dialog-item :widget cg::'multi-item-list
               :range items
               :box (cg::make-box-relative 
                     ;(cg::box-left  (cg::dialog-item-box text-dialog)) 10jan05
                     (cg::left (cg::box  text-dialog))
                     (+ (cg::bottom 
                         (cg::box text-dialog))
                         ;(cg::dialog-item-box text-dialog)) 
                        min-separator  )
                     ;+                 max-item-width
                     (+  max-item-width scroll-bar-width)  
                     (+ min-separator (* (min (length items) displayed) text-height)))
               :title "Pick one"
               :available-p T
               )
 ) ;; ok[17]

(setf cancel-button
             (make-instance
             ; (cg::make-dialog-item :widget 
             cg::'cancel-button
               :box (cg::make-box-relative 
                     (+ (cg::right  
                         (cg::box  seq-dialog))
                         ;(cg::dialog-item-box  seq-dialog)) 
                        min-separator)
                     (+ (cg::bottom  
                         (cg::box seq-dialog))
                         ;(cg::dialog-item-box  text-dialog)) 
                        min-separator)
                     button-width button-height)
               :title cancel-text
               )) ;; ok[18]

(setf select-button
             (make-instance
             ;(cg::make-dialog-item :widget 
                                   cg::'button
               :box (cg::make-box-relative 
                     (cg::left 
                      (cg::box  cancel-button))
                      ;(cg::dialog-item-box cancel-button))
                      (+ (cg::bottom                         
                          (cg::box cancel-button))
                          ;(cg::dialog-item-box  cancel-button)
                         min-separator)                  
                     button-width button-height)                              
               :title select-text
               :set-value-fn
               #'(lambda (&rest args)
                   (cond ((not 
                           ;(cg::dialog-item-value seq-dialog)
                           (cg::value (cg::dialog-item seq-dialog))
                           )
                          (cg::pop-up-message-dialog (cg::dialog-item-window seq-dialog)
                           "You choose ... " error-text cg::warning-icon "OK?")
                          ;; Accept the button click but do not return from the
                          ;; main dialog yet after popping up the warning
                          (values t nil))
                         ;; Return from the main dialog since we noted above that
                         ;; a selection was made
                         (t (values t t)))
                   )
               )) ;; ok[19]

;;; That ought to be sufficient to get started!
;;; execute the long setf etc..
(defun create-multi-dialog (items &key (prompt-text "Please choose one or more ... ")
                             (item-print-function NIL)
                             (action-function #'(lambda (i) i))
                             (select-text "select")
                             (cancel-text "cancel"))
  (let* ((list-of-dialog-items 
          ;(build-multi-dialog-items items
          ;                       :prompt-text prompt-text
          ;                       :item-print-function item-print-function
          ;                       :action-function action-function
          ;                       :select-text select-text
          ;                                                :cancel-text cancel-text)
          (list text-dialog error-dialog seq-dialog select-button cancel-button) 
                                )
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
           (list (make-instance
                  ;cg::make-dialog-item :widget 
                  'cg::button :box (cg::make-box 20 20 20 20))))
          ;; make an empty dialog from this item
          (base-dialog 
            (cg::open-dialog base-item 'cg::dialog (cg::screen cg::*system*) :title "You choose  .." :pop-up-p t  
                             ;:font (canvas-font-to-host-font *prompt-normal-font*)
             :window-exterior (cg::make-box-relative
                               dialog-outer-left
                               dialog-outer-top
                               (+ min-separator 
                                  (max (cg::right 
                                        (cg::box text-dialog)
                                        ;(cg::dialog-item-box text-dialog)
                                        )
                                       (cg::right 
                                        (cg::box  text-dialog)
                                        ;(cg::dialog-item-box select-button)
                                        )
                                       (cg::right 
                                        (cg::box text-dialog)
                                        ;(cg::dialog-item-box cancel-button)
                                        )))
                               (+ (* 4 min-separator)
                                  (max (cg::bottom 
                                        (cg::box  seq-dialog)
                                        ;(cg::dialog-item-box seq-dialog)
                                        )
                                       (cg::bottom  
                                        (cg::box  cancel-button)
                                        ;(cg::dialog-item-box cancel-button)
                                        ))))
             ))
          ;; End creation of empty base-dialog here
          ) ;end of list of let* .. body is next
      ;; add new items
      (cg::update-dialog base-dialog list-of-dialog-items)        
      base-dialog) ;; end of let* .. it returns an actual dialog
  ) ;; create-multi-dialog ok[15]
(setf dialog1 (create-multi-dialog list-of-dialog-items))
;* Error: Attempt to take the value of the unbound variable `ERROR-DIALOG'.
;* [condition type: UNBOUND-VARIABLE]
;; so I need the other dialogs too
(setf the-items (list text-dialog error-dialog seq-dialog select-button cancel-button)) ;; ok[16]
(setf dialog1 (create-multi-dialog the-items)) ;; ok[17]
(pop-up-modal-dialog dialog1 :stream (screen *system*)
  :initial-focus NIL
  :bring-thread-to-front T)
;* barf as before
;; let's get select-button and modify it using :on-click:
(setf select-button
             (make-instance
             ;(cg::make-dialog-item :widget 
                                   cg::'button
               :box (cg::make-box-relative 
                     (cg::left 
                      (cg::box  cancel-button))
                      ;(cg::dialog-item-box cancel-button))
                      (+ (cg::bottom                         
                          (cg::box cancel-button))
                          ;(cg::dialog-item-box  cancel-button)
                         min-separator)                  
                     button-width button-height)                              
               :title select-text
               ;:on-click 
               :set-value-fn
               #'(lambda (&rest args)
                             (format t ~s "Here is the result from :on-click"))
               ;:set-value-fn  ;; replaced by the line above
               ;#'(lambda (&rest args)
                ;   (cond ((not 
                ;           ;(cg::dialog-item-value seq-dialog)
                ;           (cg::value (cg::dialog-item seq-dialog))
                ;           )
                ;          (cg::pop-up-message-dialog (cg::dialog-item-window seq-dialog)
                ;           "You choose ... " error-text cg::warning-icon "OK?")
                ;          ;; Accept the button click but do not return from the
                ;          ;; main dialog yet after popping up the warning
                ;          (values t nil))
                ;         ;; Return from the main dialog since we noted above that
                ;         ;; a selection was made
                ;         (t (values t t)))
                ;   )
               ))
;; done the redefinition and that of list the-items
;; and dialog1
;* Error: Cannot add (Unnamed) to You choose  .. because it is already on You choose  ...
;* [1] CG-USER(39): 
(setf seq-dialog
             (make-instance cg::'multi-item-list
              ;(cg::make-dialog-item :widget cg::'multi-item-list
               :range items
               :box (cg::make-box-relative 
                     ;(cg::box-left  (cg::dialog-item-box text-dialog)) 10jan05
                     (cg::left (cg::box  text-dialog))
                     (+ (cg::bottom 
                         (cg::box text-dialog))
                         ;(cg::dialog-item-box text-dialog)) 
                        min-separator  )
                     ;+                 max-item-width
                     (+  max-item-width scroll-bar-width)  
                     (+ min-separator (* (min (length items) displayed) text-height)))
               :title "Pick one"
               :available-p T
               :on-click #'(lambda (&rest args)
                             (format t ~s :range))
               )
 )