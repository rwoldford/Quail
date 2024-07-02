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

;;; collect-input takes (list (cons "a" "a1") (cons "s" "s1") ..)
;;; the elements are open for editing, so using (list (cons "a" "a1") (cons "s" "s1"))
;;;   and changing a1 to a11 + select -> (list (cons "a" "a11") (cons "s" "s1"))
;;;   since s1 was unedited

;; Build the list of dialog-items
(defun build-collect-items (items &key (prompt-text "Please enter responses ...") 
                                  (select-text "select")
                                  (cancel-text "cancel")
                                  (columns 1)
                                  )
  (flet ((text-width (string)
                     (quail-string-width string)))
        (let* ((response-width 100) ;; width of editable-text boxes
               (max-item-width (apply #'max (mapcar #'text-width items)))
               (error-text "please choose an item first!") 
               (error-text-width (text-width error-text)) 
               (prompt-text-width (text-width prompt-text)) 
               (text-box-width (+ 5 (max prompt-text-width error-text-width)))
               (prompt-box-height (+ 10  *text-box-height*))
               (button-width 
                 (+ 5 
                    (max (text-width select-text) 
                         (text-width cancel-text))))
               (n-items  (length items))
               (col-width (+ 15 max-item-width ))
               (nrows (ceiling (/ n-items columns)))
               ;; make the prompt stuff  
               (text-dialog 
                 (make-instance  'cg::static-text
                                :box (cg::make-box-relative 
                                       (+ *dialog-outer-left* *min-separator*) 
                                       (+ *dialog-outer-top* *min-separator*) 
                                       text-box-width prompt-box-height)
                                :value (format nil prompt-text)
                                :name :text-d
                                ))
               ;; prompt stuff done
               
               ;; Now set the top of the first item
               (first-item-y (+ (cg::bottom (cg::box
                                              text-dialog))))
               (first-item-x  *dialog-outer-left* )
               ;; make the error stuff
               (error-dialog
                 (make-instance  'cg::static-text
                                :box (cg::make-box-relative
                                       (+ *dialog-outer-left* *min-separator*)
                                       (+ *dialog-outer-top* *min-separator*)
                                       text-box-width *text-box-height*)
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
                     (make-instance  'cg::static-text
                                    ;; Display must be in :value slot
                                    ;; (car item) grabs the "prompt-piece"
                                    :value (car item) ;item 
                                    :box (cg::make-box-relative 
                                           (+ x-start (* col col-width)  *min-separator*)
                                           (+ y-start (* row *text-box-height*))
                                           (+ *min-separator* max-item-width )
                                           (+ *text-box-height*))
                                    :available-p T
                                    )
                     (make-instance  'cg::editable-text
                                    ;; This has implications for the return value.
                                    ;; editable-test returns a genuine string
                                    ;; lisp-text returns a lisp object
                                    ;; (cdr item) grabs the "default value"
                                    :value (cdr item) ;NIL
                                    :box (cg::make-box-relative 
                                           (+ x-start (* col col-width)  *min-separator*
                                              *min-separator* max-item-width)
                                           (+ y-start (* row *text-box-height*))
                                           response-width
                                           (+ *text-box-height*))
                                    :available-p T
                                    ))
                   do (cond
                        ((>= row (1- nrows))
                         (incf col 1)
                         (setq row 0)
                         (setq x-start (+ x-start response-width
                                          *min-separator*)))
                        (T (incf row 1))))
                 )
               ;;; END OF CODE FROM -mcl FILE                   
               ;;; Find two values needed for later sizing and positioning
               (rightmost (cg::right 
                            (cg::box 
                              (first (last collect-dialog)))))
               (collect-bottom (apply #'max (mapcar #'cg::bottom
                                                    (mapcar #'cg::box
                                                            collect-dialog))))
               ;; cancel begins
               (cancel-button
                 (make-instance  'cg::cancel-button
                                :box (cg::make-box-relative 
                                       (+ rightmost *min-separator*)
                                       (+ (cg::bottom  (cg::box  text-dialog)) *min-separator*)
                                       button-width *button-height*)                              
                                :title cancel-text
                                :name :cancel-d
                                ))
               (cancel-bottom (cg::bottom (cg::box cancel-button)))
               ;; cancel done 
               
               ;; make select stuff
               (select-button
                 (make-instance  'cg::button
                                :box (cg::make-box-relative 
                                       (+ rightmost *min-separator*)
                                       (+ (cg::bottom  (cg::box  cancel-button)) *min-separator*)
                                       button-width *button-height*)                              
                                :title select-text
                                :name :select-d
                                :set-value-fn
                                #'(lambda (&rest args)
                                   (values t t))
                                ))
               (select-bottom (cg::bottom (cg::box select-button))
                              )
               ;; select done
               ;; compute overall sizes             
               (overall-width 
                 (+ *min-separator* 
                    (max (cg::right (cg::box  text-dialog))
                         (cg::right (cg::box
                                      select-button))
                         (cg::right (cg::box
                                      cancel-button)))))
               (overall-height 
                 (+ (* 4 *min-separator*) 
                    (max collect-bottom cancel-bottom select-bottom)))
               )
          ;; return some sizes and the dialogs
          (append (list overall-width overall-height text-dialog error-dialog)  collect-dialog  
                  (list select-button cancel-button))
          )
        )
  ) ;;end of build-collect-items

;; Create the real dialog from the output of build-collect-items
(defun create-collect-dialog (items &key (columns 1) (prompt-text "Please enter responses ...")
                                    ;(item-print-function NIL)
                                    ;(action-function #'(lambda (i) i))
                                    (select-text "select") (cancel-text "cancel"))
  ;(declare (ignore action-function))
  (let* ((list-of-dialog-items (build-collect-items items :columns columns 
                                                    :prompt-text prompt-text
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
           (cg::open-dialog base-item 'cg::dialog (cg::screen cg::*system*) 
                            :title "Enter responses ..." :pop-up-p t  
                            :font (cg::make-font :roman "times\ new\ roman" 20);(canvas-font-to-host-font *prompt-normal-font*)
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
    base-dialog)
  ) ;; end of create-collect-dialog

;; Now deal with the dialog
(defun collect-input(items &key (columns 1) (prompt-text "Please enter responses ...")
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
          )))) ;; end collect-input
