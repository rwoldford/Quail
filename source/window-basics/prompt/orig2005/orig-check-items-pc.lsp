;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  check-items-pc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;  G.W.Bennett  1996-1997
;;;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(check-items)))
;; Build the necessary lists of dialog-items
(defun build-check-dialog-items (items &key (prompt-text "Check one or more ...") 
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
         (cg::stream-string-width (cg::screen cg::*system*) string)) 
        ))
             (set-view-size (a-window) 
              (list (cg::width (cg::exterior a-window)) (cg::height (cg::exterior a-window)))))
       (let* ((view-font 
              (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
                   (cg::font (cg::screen cg::*system*)))
      ))
                (dialog-outer-left 10)
                (dialog-outer-top 10)
                ;               (scroll-bar-width 25)
                (min-separator 15)
                (check-allowance 10)
              (max-item-width (first (sort (mapcar #'(lambda (x)
                                                       (if (stringp x)
                           (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
                                                          (cg::stream-string-width (cg::screen cg::*system*) x)
                                                          (cg::stream-string-width (cg::screen cg::*system*)
                                                           (format NIL "~s" x))))))
                                             items)
                                       #'>)))
                (error-text "please choose an item first!") 
                (error-text-width 
          (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
         (cg::stream-string-width (cg::screen cg::*system*) error-text)))) 
                (prompt-text-width 
           (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
      (cg::stream-string-width (cg::screen cg::*system*) prompt-text)))) 
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
                (text-box-x 20)
                (text-box-y 20)
                (button-height (+ 2 text-height)) 
                (button-width 
             (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
                 (+ 5 
                     (max (cg::stream-string-width (cg::screen cg::*system*) select-text) 
                         (cg::stream-string-width (cg::screen cg::*system*) cancel-text))))))
                (seq-height 200) 
                (seq-width 200)
                (n-items  (length items))
                (col-width (+ 15 max-item-width check-allowance))
                (nrows (ceiling (/ n-items columns)))
                (first-item-y (+ text-box-y text-box-height))
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
                ;; check-dialog should start here
                ;;; TOP OF CODE FROM -mcl FILE
                (check-dialog
                 (loop
                   for item in items
                   with y-start = first-item-y
                   with x-start = first-item-x
                   with row = 0
                   with col = 0
                   collect
                   (cg::make-dialog-item
                    :widget cg::'check-box
                    :title item
                    :box (cg::make-box-relative 
                             (+ x-start (* col col-width)  min-separator)
                             (+ y-start (* row text-box-height))
                             (+ min-separator max-item-width check-allowance)
                             (+ min-separator text-box-height))
                    :available-p T
                    )
                   do (cond
                                  ((>= row (1- nrows))
                                   (incf col 1)
                                   (setq row 0))
                                  (T (incf row 1))))
                 )
                ;;; END OF CODE FROM -mcl FILE                   
                (height-item-box (cg::box-height (cg::dialog-item-box (first check-dialog))))
                (width-item-box (cg::box-width (cg::dialog-item-box (first check-dialog))))
                (rightmost  (cg::box-right (cg::dialog-item-box (first (last check-dialog)))))
                ;; cancel begins
                (cancel-button
                 (cg::make-dialog-item
                  :widget cg::'cancel-button
                  :box (cg::make-box-relative 
                           (+ rightmost min-separator)
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
                           (+ rightmost min-separator)
                           (+ (cg::box-bottom  (cg::dialog-item-box  cancel-button)) min-separator)
                           button-width button-height)                              
                  :title select-text
                  :set-value-fn
                  #'(lambda (&rest args)
                       (values t t))
                  ))
                ;; select done
                (overall-width (+ (* 4  min-separator) (* columns width-item-box) button-width))
                (overall-height (+ (* 4  min-separator) (* nrows height-item-box) button-height))
                ) ;; end of bindings in let* .. body follows
           (append (list overall-width overall-height text-dialog error-dialog)  check-dialog  (list select-button cancel-button))
           ) ;; end of let* .. it returns a list
       ) ;; end of flet
     ) ;; end of build-check-items

;; Create the dialog itself from what build produces
(defun create-check-dialog (items &key (columns 1) (prompt-text "Choose one or more ...")
                             (item-print-function NIL)
                             (item-select-function #'(lambda (i) i))
                             (action-function #'(lambda (i) i))
                             (select-text "select") (cancel-text "cancel"))
   (let* ((list-of-dialog-items (build-check-dialog-items items :columns columns :prompt-text 
                                 :item-print-function item-print-function
                                 :action-function action-function
                                 prompt-text :select-text select-text :cancel-text cancel-text))
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
               (cg::open-dialog base-item 'cg::dialog (cg::screen cg::*system*) :title "Choose One .." :pop-up-p t  
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
     ) ;; end of create-check-dialog
;; Now process the whole thing

(defun check-items (items &key (columns 1) (prompt-text "Choose one or more ...")
                     (item-print-function NIL)
                     (action-function #'(lambda (i) i))
                     (select-text "select") (cancel-text "cancel"))
   ;; Make the dialog itself
   (let* ((a-dialog (create-check-dialog items :columns columns
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
          (cancel-v (nth (+ 3 n-items) v-items))
          (select-v (nth (+ 4 n-items) v-items)))
      (if cancel-v
         items
         (loop for x from 0 to (- n-items 1)
           collect (cons (nth (+ x 2) t-items) (nth (+ x 2) v-items)))
         )
      ) ;; end let*
   ) ;; end check-items itself

