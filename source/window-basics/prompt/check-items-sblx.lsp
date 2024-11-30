;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  check-items-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;  G.W.Bennett  1996-1997, 2020
;;;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(check-items)))
;;; two functions needed later

(defun pos-list (a-list a-sublist)
  "returns the positions of the elements of a-sublist in a-list"
  (mapcar #'(lambda (x) (position x a-list :test #'equal)) a-sublist))

(defun copy-result-list (plist1 list2)
  "returns a copy of list2 with cdrs updated to T at positions plist"
  (let ((copy-list2 (copy-tree list2)))
  (dolist (x plist1 (return copy-list2))
    (setf (elt copy-list2 x)
      (cons (car (elt copy-list2 x)) t)))
  (return-from copy-result-list  copy-list2)))

;;; check-items itself
(defun check-items (items &key (prompt-text "Check one or more ...") 
                                        (item-print-function NIL) 
                                        (action-function 
                                         #'(lambda (i)
                                             i))
                                        (select-text "Select")
                                        (cancel-text "cancel"))
   "returns a copy of items with cdrs updated to T at positions pos-list obtained from the matching application-frame"
  (let ((frame
      (make-application-frame 'check-items :pretty-name prompt-text :items items :prompt-text prompt-text :item-print-function item-print-function
        :action-function action-function :select-text select-text :cancel-text cancel-text)))
  (run-frame-top-level frame)
  (return-from check-items (copy-result-list (pos-list items (reverse (frame-result frame)))
    items))
  ))

;;; The application frame to collect the elements selected
(define-application-frame check-items () 
  ((frame-result :initform nil :accessor frame-result)
    (items :initarg :items :accessor items)
    (prompt-text :initarg :prompt-text :accessor prompt-text)
    (item-print-function :initarg :item-print-function :accessor item-print-function)
    (action-function :initarg :action-function :accessor action-function)
    (select-text :initarg :select-text :accessor select-text)
    (cancel-text :initarg :cancel-text :accessor cancel-text)
    )
  (:menu-bar nil)
  (:panes
    (prompt-pane 
    (make-pane 'clim-stream-pane
     :height 30
     ;:scroll-bars nil
           :display-time t
           :display-function #'(lambda (frame pane) (draw-text* pane (prompt-text *application-frame*)
            10 25 :ink +blue+ :text-size 18))
           ))
   (options
    (make-pane 'list-pane
      :mode :nonexclusive
               :items (items *application-frame*)
               :name-key #'car
               )))
  (:layouts
   (default
    (vertically (:height 300 :width 350)
      prompt-pane
   (horizontally ()
     (labelling (:label "Options") (scrolling () options)))
   (horizontally ()
     +fill+
     (make-pane 'push-button
          :label (string-upcase (select-text *application-frame*))
          :activate-callback
          (lambda (ignore)
            ignore
            (setf (frame-result *application-frame*) (gadget-value options))
      (frame-exit *application-frame*))
          )
     (make-pane 'push-button
          :label (string-upcase (cancel-text *application-frame*))
          :activate-callback
          (lambda (ignore)
      ignore
      (frame-exit *application-frame*)))
     )))))




