;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pick-one-sblx.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;  Authors:
;;;  G.W. Bennett 1996-1997, 2020
;;;     
;;;----------------------------------------------------------------------------------

(in-package :wb)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(pick-one)))

;;; pipck-one itself
(defun pick-one (items &key (prompt-text "Pick one of the following") 
                                        (item-print-function NIL) 
                                        (action-function 
                                         #'(lambda (i)
                                             i))
                                        (select-text "Select")
                                        (cancel-text "cancel"))
"returns a single cons cell from items through the matching application-frame"
  (let ((frame
      (make-application-frame 'pick-one :pretty-name prompt-text :prompt-text prompt-text :items items :item-print-function item-print-function
        :action-function action-function :select-text select-text :cancel-text cancel-text)))
  (run-frame-top-level frame)
  (return-from  pick-one (frame-result frame))))

;;; the matching application frame
(define-application-frame pick-one () 
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
           ;'draw-pick-one-prompt-text
           ))
   (options
    (make-pane 'list-pane
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
            ;(format t " pick-one (gadget-value options) returns ~s ~%" (gadget-value options))
      (frame-exit *application-frame*))
          )
     (make-pane 'push-button
          :label (string-upcase (cancel-text *application-frame*))
          :activate-callback
          (lambda (ignore)
      ignore
      (frame-exit *application-frame*)))
     )))))
