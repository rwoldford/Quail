;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code-fragment.lsp
;;; holding
;;; code fragment which needs fixing
;;; it's from prompt-pc.lsp
;;; specifically, how do I talk about the offset
;;; originally computed by (cg::dialog-item-box of some item) ??
;;; I think I shal try to build the stuff from prompt-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This seems OK as a way to carry about functions too
(defun one (x y)
               (let ((z (+ x y))
                     (w #'(lambda (w)
                            (* w 3))))
                 (list z w)))

(defun two (x y q)
               (list (funcall (second (one x y)) q) x y))

;*CG-USER(81): (two 1 2 3)
;*(9 1 2)
;;;;

;;; We begin with some functions:
(defun how-many (char string)
   "Counts the number of occurrences of char in string"
    (- (length string)
      (length (remove-if #'(lambda (x)
                             (eq x char)) string)))) ;;OK[1]

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
    result)) ;;OK[2]

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
     )) ;;OK[3]


;;; here are some dialogs
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
 ) ;; ok[27]
;; bit which found text-dialog too
(setf text-dialog 
            (make-instance cg::'static-text
              ;(cg::make-dialog-item :widget 'cg::static-text 10jan05
               :box (cg::make-box-relative 
                     (+ dialog-outer-left min-separator) 
                     (+ dialog-outer-top min-separator) 
                     text-box-width prompt-box-height)
              :value  (format nil prompt-text))) ;; ok[24]

(setf error-dialog 
            (make-instance cg::'static-text
              ;(cg::make-dialog-item :widget 'cg::static-text 10jan05
               :box (cg::make-box-relative
                     (+ dialog-outer-left min-separator)
                     (+ dialog-outer-top min-separator)
                     text-box-width text-box-height)
               :border :static
              :value error-text));; ok[27]

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
               )) ;; ok[28]

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
               )) ;; ok[29]

;; some things I need
(setf displayed 5);; OK[4]
(setf scroll-bar-width 25) ;; OK[5]
(setf seq-height 200) ;; ok[6] 
(setf seq-width 200);; ok[7]
(setf dialog-outer-left 10);; ok[8]
(setf min-separator 15);; ok[9]
(setf dialog-outer-top 10);; ok[10]
(setf prompt-text "Here is a text-dialog");; ok[11]
(setf select-text "Select");; ok[12]
(setf cancel-text "Cancel");; ok[13]
(setf error-text "please choose an item first!");; ok[14]
(setf temp-font-info 
               (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
      (cg::fontmetrics (cg::screen cg::*system*)))));; ok[15]
(setf text-height 
              (+ (cg::font-ascent temp-font-info) (cg::font-descent temp-font-info) 
                 (cg::font-leading temp-font-info))) ;; 19 ok[16] 
(setf button-height (+ 2 text-height));; 21 ok[17]
(setf prompt-box-height (+ 10 (* 1 text-height)));; 29 ok[18] 
(defun text-width (string) 
          (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
      (cg::stream-string-width (cg::screen cg::*system*) string)))) ;; ok[19] 
(setf button-width 
              (+ 5 
                 (max (text-width select-text) 
                   (text-width cancel-text)))) ;; 49 ok[20]
(setf prompt-text-width (text-width prompt-text));; 131 ok[21] 
(setf text-box-width (+ 5  prompt-text-width ));; 136 ok[22] 
;; result from  (setf text-dialog etc) from above
;; text-dialog is, then:
;#<STATIC-TEXT STATIC-TEXT @ #x20ccc752>
;(dialog-item-p text-dialog) 
;;(window text-dialog) 
(box text-dialog) 
;(setf text-dialog-box (box text-dialog)) 
;(left text-dialog-box) 
;; Sooo, dialogs do have boxes obtained by (box dialog)
;; and then I can ask for left, etc..
;;;; 11jan05 more things needed to get values correct
(setf items (list "First" "Second" "Third"));; ok[23]
;; make the dialogs: text-dialog ;; ok[24 above]
;; need this for error-dialog
(setf text-box-height (+ 10 text-height));; 29 ok[25]
;; error-dialog ;; ok[26 above]
;; need this for seq-dialog -- NOTE it uses items defined in [23]
(setf max-item-width (first (sort (mapcar #'(lambda (x)
                                              (let ((scrn (cg::screen cg::*system*)))
                                                (cg::with-device-context (hdc scrn)
                                                  (cg::stream-string-width (cg::screen cg::*system*) x))))
                                    items) #'>)));; 48 ok[26]
;; seq-dialog ;; ok[27above]
;; cancel-button ;; ok[28above]
;; select-button ;; ok[29above]
;; what are its items ?
(dialog-items seq-dialog)
;* = Error: No methods applicable for generic function
;*       #<STANDARD-GENERIC-FUNCTION DIALOG-ITEMS> with args
;*       (#<MULTI-ITEM-LIST MULTI-ITEM-LIST @ #x20b2918a>) of classes
;*       (MULTI-ITEM-LIST)
;*[condition type: PROGRAM-ERROR]
;; so what does seq-dialog have ?
(inspect seq-dialog) ;; ok
;* the list, which is items, is its range
(range seq-dialog) ; ok ("First" "Second" "Third")
;* and the question is now: how do I get the returned value from a dialog ?
;* do I need the on-click funtion which works on a dialog-item


;;; Here is the form to build a multi-dialog
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
            (make-instance cg::'static-text
              ;(cg::make-dialog-item :widget 'cg::static-text 10jan05
              :box (cg::make-box-relative 
                    (+ dialog-outer-left min-separator) 
                    (+ dialog-outer-top min-separator) 
                    text-box-width prompt-box-height)
              :value  (format nil prompt-text)))
           ;; prompt stuff done
           ;; make the error stuff
           (error-dialog 
            (make-instance cg::'static-text
              ;(cg::make-dialog-item :widget 'cg::static-text 10jan05
              :box (cg::make-box-relative
                    (+ dialog-outer-left min-separator)
                    (+ dialog-outer-top min-separator)
                    text-box-width text-box-height)
              :border :static
              :value error-text))
           ;; error stuff done                          
           ;; seq-dialog should start here
           (seq-dialog
            (make-instance 
                ;(cg::make-dialog-item :widget 
                cg::'multi-item-list
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
            )
           ;; seq-dialog should end here          
           ;; cancel begins
           (cancel-button
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
              ))
           ;; cancel done          
           ;; make select stuff
           (select-button
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
              ))
           ;; select done
           ) ;; end of bindings in let* .. body follows
      (list text-dialog error-dialog seq-dialog select-button cancel-button)
      ) ;; end of let* .. it returns a list
    ) ;; end of flet
  ) ;; end of build-multi-dilaog-items

;;; Here is the form to create a multi-dialog
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
         ;(view-font 
         ; (let ((scrn (cg::screen cg::*system*)))
         ;   (cg::with-device-context (hdc scrn)
         ;     (cg::font (cg::screen cg::*system*)))))
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
                                                      (cg::box  select-button)
                                                      ;(cg::dialog-item-box select-button)
                                                      )
                                                     (cg::right cancel-button)
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
  ) ;; ok[30]
;* I should setf something to this
(setf dialog1 (create-multi-dialog items)) ;; ok[31] NOTE I have ;'d out the font stuff
(dialogp dialog1);; T ok[31]
(dialog-items dialog1) ; lots of stuff
;*(#<STATIC-TEXT STATIC-TEXT @ #x20a1ee5a> #<STATIC-TEXT STATIC-TEXT @ #x20a210d2>
;* #<MULTI-ITEM-LIST MULTI-ITEM-LIST @ #x20a207a2> #<BUTTON BUTTON @ #x20a2c05a>
;* #<CANCEL-BUTTON CANCEL-BUTTON @ #x20a208a2>)
;* Here is where I think I need to sort out the value bit
;; code to display this dialog
(setf clicked-button (cg::pop-up-modal-dialog dialog1 
                            :stream (cg::screen cg::*system*)
                            :initial-focus NIL
                       :bring-thread-to-front T));; ok[32]
;; there it is .. but when I select First and click Select I get:
;*Error: No methods applicable for generic function #<STANDARD-GENERIC-FUNCTION VALUE> 
;*with args (NIL) of classes (NULL)
;*[condition type: PROGRAM-ERROR]
;* Clicking Cancel returns nil which is OK
(fourth (dialog-items dialog1)); #<BUTTON BUTTON @ #x20a2c05a>
(name (fourth (dialog-items dialog1))) ;NIL
(first (dialog-items dialog1)); #<STATIC-TEXT STATIC-TEXT @ #x20a1ee5a>
(name (first (dialog-items dialog1))) ;nil
;; here is process modified to swallow dialog1
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
          (a-dialog dialog1)
          (error-text "Please choose an item first!")
          ;; Show the completed dialog
          ;(clicked-button (cg::pop-up-dialog a-dialog)) 11jan05
          (clicked-button (cg::pop-up-modal-dialog a-dialog 
                            :stream (cg::screen cg::*system*)
                            :initial-focus NIL
                            :bring-thread-to-front T)) ;;11jan05
          (d-items  (mapcar 
                        ;#'cg::dialog-item-value 
                        #'cg::value
                      (cg::dialog-items a-dialog)))
          (seq-v  (third d-items))
          (select-v (fourth d-items))
          (cancel-v  (fifth d-items)))
      (cond (cancel-v (abort) )
            (t (loop for item in items as new-item in new-items when
                 (member new-item seq-v :test #'string-equal)
                 collect (funcall action-function item))))
      ) ;; end let*
  );; OK[33]
(process-multi-dialog items) 
;*Error: No methods applicable for generic function
;*       #<STANDARD-GENERIC-FUNCTION VALUE> with args (NIL) of classes
;*       (NULL)
;*[condition type: PROGRAM-ERROR]
;** I'm stuck - the dialog displays and Select kills things.