;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; collect-input-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;  G.W. Bennett 1996-1997
;;;     
;;;----------------------------------------------------------------------------------

(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(collect-input)))
;; Build the list of dialog-items
(defun build-collect-items (items &key (prompt-text "Please enter responses ...") 
                             (item-print-function nil) 
                             (action-function 
                               #'(lambda (i)
                                   i))
                             (select-text "select")
                             (cancel-text "cancel")
                             (columns 1)
                             )
   (flet ((text-width (string)
           (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn) 
            (if (stringp string)
              (cg::stream-string-width (cg::screen cg::*system*) string)
               (cg::stream-string-width (cg::screen cg::*system*)
                (format NIL "~s" string)))) 
      ))
          (set-view-size (a-window) 
            (list (cg::width (cg::exterior a-window)) (cg::height (cg::exterior a-window)))))
     (let* ((view-font 
            (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
            (cg::font (cg::screen cg::*system*)))))
            (dialog-outer-left 10)
            (dialog-outer-top 10)
            (min-separator 15) ;; minimum distance between boxes
            (response-width 100) ;; width of editable-text boxes
            (max-item-width (apply #'max (mapcar #'text-width items)))
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
                                        text-box-height)))
            (text-box-x 20)
            (text-box-y 20)
            (button-height (+ 2 text-height)) 
            (button-width 
              (+ 5 
                 (max (text-width select-text) 
                   (text-width cancel-text))))
            (seq-height 200) 
            (seq-width 200)
            (n-items  (length items))
            (col-width (+ 15 max-item-width ))
            (nrows (ceiling (/ n-items columns)))
            ;; The following are set AFTER text-dialog
            ;;               (first-item-y (+ text-box-y text-box-height ))
            ;;               (first-item-x  dialog-outer-left )
            ;;               (button-x (+ 50 first-item-x (* columns col-width)))
            ;;               (view-size-width (max 200
            ;;                                              (+ button-x button-width 2)
            ;;                                              (+ text-box-x text-box-width 2)))
            ;;               (view-size-height (max 100
            ;;                                                (+ first-item-y
            ;;                                                    (+ 2 (* (+ nrows 1) text-box-height))
            ;;                                                    2)
            ;;                                                (+ text-box-height 
            ;;                                                    button-height
            ;;                                                    2)))
            ;; make the prompt stuff  
            (text-dialog 
              (cg::make-dialog-item :widget 'cg::static-text 
               :box (cg::make-box-relative 
                     (+ dialog-outer-left min-separator) 
                     (+ dialog-outer-top min-separator) 
                     text-box-width prompt-box-height)
               :value (format nil prompt-text)
               :name :text-d
               ))
            ;; prompt stuff done
            
            ;; Now set the top of the first item
            (first-item-y (+ (cg::box-bottom (cg::dialog-item-box
                                              text-dialog))))
            (first-item-x  dialog-outer-left )
            (button-x (+ 50 first-item-x (* columns col-width)))
            (view-size-width (max 200
                               (+ button-x button-width 2)
                               (+ text-box-x text-box-width 2)))
            (view-size-height (max 100
                                (+ first-item-y
                                   (+ 2 (* (+ nrows 1) text-box-height))
                                   2)
                                (+ text-box-height 
                                   button-height
                                   2)))             
            ;; make the error stuff
            (error-dialog
              (cg::make-dialog-item :widget 'cg::static-text
               :box (cg::make-box-relative
                     (+ dialog-outer-left min-separator)
                     (+ dialog-outer-top min-separator)
                     text-box-width text-box-height)
               :border :static
               :value error-text
               :name :error-d))
            ;; error stuff done
            
            ;; collect-dialog should start here
            ;;; TOP OF CODE FROM -mcl FILE
            (collect-dialog
             (loop
               for item in items
               with y-start = first-item-y
               with x-start = first-item-x
               with row = 0
               with col = 0
               append
               (list 
                 (cg::make-dialog-item
                  :widget cg::'static-text
                  ;; Display must be in :value slot
                  ;; (car item) grabs the "prompt-piece"
                  :value (car item) ;item 
                  :box (cg::make-box-relative 
                        (+ x-start (* col col-width)  min-separator)
                        (+ y-start (* row text-box-height))
                        (+ min-separator max-item-width )
                        (+ text-box-height))
                  :available-p T
                  )
                 (cg::make-dialog-item
                  ;; This has implications for the return value.
                  ;; editable-test returns a genuine string
                  ;; lisp-text returns a lisp object
                  :widget cg::'editable-text;cg::'lisp-text 
                  ;; (cdr item) grabs the "default value"
                  :value (cdr item) ;NIL
                  :box (cg::make-box-relative 
                        (+ x-start (* col col-width)  min-separator
                           min-separator max-item-width)
                        (+ y-start (* row text-box-height))
                        response-width
                        (+ text-box-height))
                  :available-p T
                  ))
               do (cond
                        ((>= row (1- nrows))
                         (incf col 1)
                         (setq row 0)
                         (setq x-start (+ x-start response-width
                                          min-separator)))
                        (T (incf row 1))))
             )
            ;;; END OF CODE FROM -mcl FILE                   
            ;;; Find two values needed for later sizing and positioning
            (rightmost (cg::box-right (cg::dialog-item-box (first (last collect-dialog)))))
            (collect-bottom (apply #'max (mapcar #'cg::box-bottom
                                           (mapcar #'cg::dialog-item-box
                                             collect-dialog))))
            ;; cancel begins
            (cancel-button
              (cg::make-dialog-item
               :widget cg::'cancel-button
               :box (cg::make-box-relative 
                     (+ rightmost min-separator)
                     (+ (cg::box-bottom  (cg::dialog-item-box  text-dialog)) min-separator)
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
                     (+ rightmost min-separator)
                     (+ (cg::box-bottom  (cg::dialog-item-box  cancel-button)) min-separator)
                     button-width button-height)                              
               :title select-text
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
                                                              select-button))
                                              (cg::box-right (cg::dialog-item-box
                                                              cancel-button)))))
            (overall-height (+ (* 4 min-separator) (max collect-bottom cancel-bottom select-bottom)))
            ) ;; end of bindings in let* .. body follows
        ;; return some sizes and the dialogs
        (append (list overall-width overall-height text-dialog error-dialog)  collect-dialog  (list select-button cancel-button))
        ) ;; end of let* .. it returns a list
     ) ;; end of flet
   ) ;;end of build-collect-items

;; Create the real dialog from the output of build-collect-items
(defun create-collect-dialog (items &key (columns 1) (prompt-text "Please enter responses ...")
                               (item-print-function NIL)
                               (action-function #'(lambda (i) i))
                               (select-text "select") (cancel-text "cancel"))
   (let* ((list-of-dialog-items (build-collect-items items :columns columns 
                                  :prompt-text prompt-text
                                  :item-print-function item-print-function
                                  :action-function action-function
                                  :select-text select-text :cancel-text cancel-text))
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
            (cg::open-dialog base-item 'cg::dialog (cg::screen cg::*system*) :title "Enter responses ..." :pop-up-p t  
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
   ) ;; end of create-collect-dialog

;; Now deal with the dialog
(defun collect-input(items &key (columns 1) (prompt-text "Please enter responses ...")
                      (item-print-function NIL)
                      (action-function #'(lambda (i) i))
                      (select-text "select") (cancel-text "cancel"))
   ;; Make the dialog itself
   (let* ((a-dialog (create-collect-dialog items :columns columns
                      :item-print-function item-print-function
                      :action-function action-function
                      :select-text select-text :cancel-text cancel-text
                      :prompt-text prompt-text))
          ;; Show the completed dialog
          (showit (cg::pop-up-dialog a-dialog))
          ;; Check on what is returned
          (d-items (cg::dialog-items a-dialog))
          (t-items (mapcar #'cg::dialog-item-title d-items))
          (v-items (mapcar #'cg::dialog-item-value d-items))
          (n-items (length items))
          (d-length (length d-items))
          (cancel-v (cg::dialog-item-value 
                     (cg::widget :cancel-d a-dialog)))
          (select-v (cg::dialog-item-value
                     (cg::widget :select-d a-dialog)))
          )
      (if cancel-v
         items
         (loop for x from 0 to (- d-length 5)
           collect (cons (nth (+ x 2) v-items) 
                     (nth (+ x 3) v-items)
                     )
           do (incf x 1))
         )
      ) ;; end let*
   ) ;; end process-check-geometry itself
