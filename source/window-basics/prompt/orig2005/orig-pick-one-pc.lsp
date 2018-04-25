;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pick-one-pc.lsp
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
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(pick-one)))
;; Build the list of dialog-items
(defun build-single-dialog-items (items &key (prompt-text "Pick one of the following") 
                                                     (item-print-function NIL) 
                                                     (action-function 
                                                      #'(lambda (i)
                                                           i))
                                                     (select-text "select")
                                                     (cancel-text "cancel"))
     (flet ((text-width (string) 
               (let ((scrn (cg::screen cg::*system*)))
    (cg::with-device-context (hdc scrn)
               (cg::stream-string-width (cg::screen cg::*system*) string)))) 
             (set-view-size (a-window) 
              (list (cg::width (cg::exterior a-window)) (cg::height (cg::exterior a-window)))))
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
                                                       (if (stringp x)
                                                          (cg::stream-string-width (cg::screen cg::*system*) x)
                                                          (cg::stream-string-width (cg::screen cg::*system*)
                                                           (format NIL "~s" x))))))
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
                                      text-box-height)))
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
                  :value (format nil prompt-text)))
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
                 (if (functionp item-print-function)
                    (cg::make-dialog-item
                     :widget cg::'single-item-list
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
                    (cg::make-dialog-item
                     :widget cg::'single-item-list
                     :range items
                     :box (cg::make-box-relative 
                              (cg::box-left (cg::dialog-item-box  text-dialog))
                              (+ (cg::box-bottom  (cg::dialog-item-box  text-dialog)) min-separator  )
                              ;+                    max-item-width
                              (+  max-item-width scroll-bar-width)
                              (+ min-separator (* (min (length items) displayed) text-height)))
                     :title "Pick one"
                     :available-p T
                     )))
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
                                   "Choose one ... " error-text cg::warning-icon "OK?")
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
     ) ;; end of build-single-dialog-items

;; Now create the actual dialog from what build-.. produces
(defun create-single-dialog (items &key  (prompt-text "Pick one of the following")
                                             (item-print-function NIL)
                                             (action-function #'(lambda (i) i))
                                             (select-text "Select")
                                             (cancel-text "Cancel"))
     (let* ((list-of-dialog-items (build-single-dialog-items items
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
               (cg::open-dialog base-item 'cg::dialog (cg::screen cg::*system*) :title "Choose One .." :pop-up-p t  
                                :font (canvas-font-to-host-font *prompt-normal-font*)
                :window-exterior (cg::make-box-relative
                                              dialog-outer-left
                                              dialog-outer-top
                                              (+ (* 3 min-separator) 
                                                  (max (cg::box-right (cg::dialog-item-box text-dialog))
                                                      (cg::box-right (cg::dialog-item-box select-button))
                                                      (cg::box-right (cg::dialog-item-box cancel-button))))
                                              (+ (* 3 min-separator)
                                                  (max (cg::box-bottom (cg::dialog-item-box seq-dialog))
                                                      (cg::box-bottom  (cg::dialog-item-box cancel-button)))))
                ))
              ;; End creation of empty base-dialog here
              ) ;end of list of let* .. body is next
         ;; add new items
         (cg::update-dialog base-dialog list-of-dialog-items)        
         base-dialog) ;; end of let* .. it returns an actual dialog
     ) ;; end of create-single-dialog

;; Now deal with the dialog
(defun pick-one (items &key (prompt-text "Pick one of the following")
                           (item-print-function NIL)
                           (action-function #'(lambda (i) i))
                           (select-text "Select")
                           (cancel-text "Cancel"))
     ;; Make the dialog itself
     (let* ((a-dialog (create-single-dialog items
                               :prompt-text prompt-text
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
         (cond (cancel-v (abort))
                   (t seq-v))
         ;         ) ;; end of  let*
         ) ;; end let*
     ) ;; end process-single-dialog itself
