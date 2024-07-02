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

;;; check-items takes (list "a" "b" ..) for items and returns (list "a" ("b".T) ..) if "b" is selected
;;;    and so on for other selections.

;; Build the necessary lists of dialog-items
(defun build-check-dialog-items (items &key (prompt-text "Check one or more ...") 
                                       (select-text "select")
                                       (cancel-text "cancel")
                                       (columns 1)
                                       )
  (let* ((check-allowance 10)
         (max-item-width (reduce #'max (mapcar #'(lambda (x) (quail-string-width x)) items)))
         (error-text "please choose an item first!") 
         (error-text-width 
           (quail-string-width error-text)) 
         (prompt-text-width 
           (quail-string-width prompt-text))
         (text-box-width (+ 5 (max prompt-text-width error-text-width))) 
         (prompt-box-height (+ 10 *text-height*))
         (text-box-y 20)
         (button-width 
           (+ 5 
              (max (quail-string-width select-text)
                   (quail-string-width cancel-text))))
         (n-items  (length items))
         (col-width (+ 15 max-item-width check-allowance))
         (nrows (ceiling (/ n-items columns)))
         (first-item-y (+ text-box-y *text-box-height*))
         (first-item-x  *dialog-outer-left* )
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
             (make-instance  'cg::check-box
                            :title item
                            :box (cg::make-box-relative 
                                   (+ x-start (* col col-width)  *min-separator*)
                                   (+ y-start (* row *text-box-height*))
                                   (+ *min-separator* max-item-width check-allowance)
                                   (+ *min-separator* *text-box-height*))
                            :available-p T
                            )
             do (cond
                  ((>= row (1- nrows))
                   (incf col 1)
                   (setq row 0))
                  (T (incf row 1))))
           )
         ;;; END OF CODE FROM -mcl FILE                   
         (height-item-box (cg::height (cg::box (first check-dialog))))
         (width-item-box (cg::width (cg::box (first check-dialog))))
         (rightmost  (cg::right (cg::box (first (last check-dialog)))))
         ;; cancel begins
         (cancel-button
           (make-instance  'cg::cancel-button
                          :box (cg::make-box-relative 
                                 (+ rightmost *min-separator*)
                                 (+ (cg::bottom  (cg::box  text-dialog)) *min-separator*)
                                 button-width *button-height*)                              
                          :title cancel-text
                          ))
         ;; cancel done                          
         ;; make select stuff
         (select-button
           (make-instance  'cg::button
                          :box (cg::make-box-relative 
                                 (+ rightmost *min-separator*)
                                 (+ (cg::bottom  (cg::box  cancel-button)) *min-separator*)
                                 button-width *button-height*)                              
                          :title select-text
                          :set-value-fn
                          #'(lambda (&rest args)
                             (values t t))
                          ))
         ;; select done
         (overall-width (+ (* 4  *min-separator*) (* columns width-item-box) button-width))
         (overall-height (+ (* 4  *min-separator*) (* nrows height-item-box) *button-height*))
         )
    (append (list overall-width overall-height text-dialog error-dialog) 
            check-dialog  (list select-button cancel-button))
    )
  ) ;; end of build-check-items

;; Create the dialog itself from what build produces
(defun create-check-dialog (items &key (columns 1) (prompt-text "Choose one or more ...")
                                  (select-text "select") (cancel-text "cancel"))
  (let* ((list-of-dialog-items (build-check-dialog-items items :columns columns :prompt-text prompt-text
                                                         :select-text select-text :cancel-text cancel-text))
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
                            'cg::dialog (cg::screen cg::*system*) 
                            :title "Choose One .." 
                            :pop-up-p t  
                            :font (cg::make-font :roman "times\ new\ roman" 20);(canvas-font-to-host-font *prompt-normal-font*)
                            :window-exterior (cg::make-box-relative
                                               *dialog-outer-left*
                                               *dialog-outer-top*
                                               overall-width
                                               overall-height
                                               )))
         )
    ;; add new items
    (cg::update-dialog base-dialog (cddr  list-of-dialog-items))
    base-dialog) ;; end of let* .. it returns an actual dialog
  ) ;; end of create-check-dialog
;; Now process the whole thing

(defun check-items (items &key (columns 1) (prompt-text "Choose one or more ...")
                          (item-print-function NIL)
                          (action-function #'(lambda (i) i))
                          (select-text "select") (cancel-text "cancel"))
  (declare (ignore item-print-function action-function))
  ;; Make the dialog itself
  (let ((a-dialog (create-check-dialog items :columns columns
                                       :select-text select-text :cancel-text cancel-text
                                       :prompt-text prompt-text)))
    (cg::pop-up-modal-dialog a-dialog)
    (let*(
          (d-items (cg::dialog-items a-dialog))
          (t-items (mapcar #'cg::title d-items))
          (v-items (mapcar #'cg::value d-items))
          (n-items (length items))
          (cancel-v (nth (+ 3 n-items) v-items)))(defun collect-input(items &key (columns 1) (prompt-text "Please enter responses ...")
                           (item-print-function NIL)
                           (action-function #'(lambda (i) i))
                           (select-text "select") (cancel-text "cancel"))
  (declare (ignore item-print-function action-function)) ;18oct2023
  ;; Make the dialog itself
  (let ((a-dialog (create-collect-dialog items :columns columns
                                         :select-text select-text :cancel-text cancel-text
                                         :prompt-text prompt-text)))
    ;; Show the completed dialog
    (cg::pop-up-modal-dialog a-dialog)
    ;; Check on what is returned
    (let* ((d-items (cg::dialog-items a-dialog))
           (v-items (mapcar #'cg::value d-items))
           (d-length (length d-items))
           (cancel-v 
             (cg::value  (cg::find-component :cancel-d a-dialog)))
           )
      (if cancel-v
          items
          (loop for x from 0 to (- d-length 5)
                collect (cons (nth (+ x 2) v-items) 
                              (nth (+ x 3) v-items)
                              )
                do (incf x 1))
          ))))
      (if cancel-v
          items
          (loop for x from 0 to (- n-items 1)
                collect (cons (nth (+ x 2) t-items) (nth (+ x 2) v-items))))))
  ) ;; end check-items itself

