;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               collect-input-clx.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     R.W. Oldford 1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------
;;;
;;;  History:
;;;
;;;  - Construct a uniform prompting facility.
;;;

(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(collect-input)))

(defun collect-input (prompt-default-pairs
                      &key
                      (prompt-text "Enter text and/or exit dialog.")
                      (select-text "OK")
                      (cancel-text "Cancel")
                      (columns 1)
                      )
  "Prompts user for value of several items as given by the ~
   argument prompt-default-pairs.  Prompt-text, select-text, and cancel-text ~
   are strings which will appear in the message, select button, and cancel ~
   button, respectively.   Columns is the number of columns to be used in the ~
   display (default 1). ~
   (:returns An updated list of prompt-value-pairs.  Values are strings or NIL ~
   (if no value was specified))."
  (setq prompt-default-pairs
        (loop
          for pair in prompt-default-pairs
          collect
          (cond
           ((consp pair) pair)
           (T (cons pair NIL)))))
  
  (let
    ((d (make-instance 'collect-input-dialog 
          :items prompt-default-pairs
          :text prompt-text
          :ok-text select-text
          :cancel-text cancel-text
          :columns columns)))
    (when-dialog-done (d :timeout NIL)
      (loop for item in (slot-value d 'collect-input)
            collect
            (cons
             (dialog-original-item (car item))
             (if (zerop (length (dialog-text-of (cdr item))))
               NIL
               (dialog-text-of (cdr item))))))
    ))
  

(defclass collect-input-dialog (dialog)
  ((ok-text                :initform "OK"
                           :initarg :ok-text
                           :accessor ok-text-of)
   (cancel-text            :initform "Cancel"
                           :initarg :cancel-text
                           :accessor cancel-text-of)
   (ok-button              :initform NIL
                           :accessor ok-button-of)
   (cancel-button          :initform NIL
                           :accessor cancel-button-of)
   (item-pairs             :initarg :items 
                           :initform NIL)
   (collect-input          :initform NIL)
   (collect-input-windows  :initform NIL)
   (columns                :initform 1
                           :initarg :columns)
   )
  (:default-initargs :text "Enter text and/or exit dialog."
    :title "Input dialog")
  (:documentation "A dialog that allows the user to set ~
                   the logical value of several parameters at once.")
  )

(defmethod dialog-compute-interior ((self collect-input-dialog))
  (let* ((font (dialog-font-of self))
         (ok-text (ok-text-of self))
         (cancel-text (cancel-text-of self))
         (gcontext (gcontext-of self))
         (item-pairs (slot-value self 'item-pairs))
         (n-items (length item-pairs))
         (collect-input (make-list n-items))
         (collect-input-windows (make-list n-items))
         (window (window-of self))
         )
    (setf (xlib::gcontext-font gcontext)
          (canvas-font-to-host-font font))
    (setf (ok-button-of self)
          (make-text-button
           :text ok-text
           :gcontext gcontext
           :font font
           :border-width 2
           :on-action 
           #'(lambda ()
               (shutdown-dialog self)
               (dialog-done self))
           )
          (cancel-button-of self)
          (make-text-button
           :text cancel-text
           :gcontext gcontext
           :font font
           :border-width 2
           :on-action 
           #'(lambda ()
               (shutdown-dialog self)
               (format *quail-terminal-io* "~&Cancelled.~%")
               (dialog-done self)
               (throw :cancel T))
           ))
          
    (let (text-item edit-text)
      (loop
        for i from 0 to (- n-items 1)
        as item in item-pairs
        do
        (setf text-item 
              (make-instance 'text-dialog-item
                :gcontext gcontext
                :font font
                :item (car item)
                :text (string (car item))))
        (setf edit-text
              (make-instance 'editable-text-dialog-item
                :box-size 20
                :foreground-color *black-color*
                :background-color *gray-color*
                :border-width 2
                :text (or (cdr item) "")))
        (setf (elt collect-input i)
              (cons text-item edit-text))
        (setf (elt collect-input-windows i)
              (cons (window-of text-item)
                    (window-of edit-text)))))
    (setf
     (slot-value self 'collect-input)
     collect-input
     (slot-value self 'collect-input-windows)
     collect-input-windows)
    )
  )
(defmethod dialog-compute-geometry ((self collect-input-dialog))
  (let* ((ok-b (ok-button-of self))
         (c-b (cancel-button-of self))
         (ok-bw (window-of ok-b))
         (c-bw (window-of c-b))
         (gcontext (gcontext-of self))
         (window (window-of self))
         (border-width (dialog-border-width-of self))
         (h-space (dialog-h-space self))
         (v-space (dialog-v-space self))
         (font (xlib::gcontext-font gcontext))
         (font-ascent (xlib::font-ascent font))
         (text-height
          (+ font-ascent (xlib::font-descent font)))
         (title-width (xlib::text-extents font (dialog-title-of self)))
         (text-width (xlib::text-extents font (dialog-text-of self)))
         (ok-width (xlib::drawable-width ok-bw))
         (c-width (xlib::drawable-width c-bw))
         (button-width (max ok-width c-width))
         (button-height (xlib::drawable-height ok-bw))
         (collect-input-windows (slot-value self 'collect-input-windows))
         (max-item-width
          (+ h-space
             (reduce #'(lambda (width collect-input)
                         (max width 
                              (+ (xlib::drawable-width 
                                  (car collect-input))
                                 (xlib::drawable-width 
                                  (cdr collect-input)))))
                     collect-input-windows
                     :initial-value 0)))
         (max-item-height
          (+ v-space
             (reduce #'(lambda (height collect-input)
                         (max height 
                              (xlib::drawable-height
                               (car collect-input))
                              (xlib::drawable-height
                               (cdr collect-input))))
                  collect-input-windows
                  :initial-value 0)))
         (ncols (slot-value self 'columns))
         (nrows (ceiling (length collect-input-windows) ncols))
         (list-width (- (* ncols
                           (+ h-space max-item-width))
                        h-space))
         (list-height (- (* nrows
                            max-item-height)
                         v-space))
         (self-width
          (+ border-width border-width h-space h-space
             (max
              list-width
              title-width
              text-width
              (+ button-width h-space button-width))))
         (list-y (+ border-width
                    v-space
                    text-height
                    v-space
                    text-height
                    v-space))
         (self-height 
          (+ list-y
             list-height
             v-space
             button-height
             v-space
             border-width))
         (button-y (- self-height
                      button-height
                      v-space 
                      border-width))
         
         )
    
    
    ;; The dialog:
    (xlib::with-state (window)
      (setf (xlib::drawable-width window) self-width
            (xlib::drawable-height window) self-height))


    ;; The buttons
    (setf (xlib::drawable-width ok-bw) button-width
          (xlib::drawable-width c-bw) button-width)
    (xlib::reparent-window ok-bw window
                           (+ h-space border-width)
                           button-y)
    (xlib::reparent-window c-bw window
                           (+ border-width h-space button-width h-space)
                           button-y)
    
   
    
    ;; The collect-input-windows
    (setf max-item-width
          (floor (- self-width
                    (* 2 (+ border-width h-space))
                    (* (- ncols 1) 2 h-space))
                 ncols))
    (loop
      for item in collect-input-windows
      as dialog-item in (slot-value self 'collect-input)
      with y-start = list-y
      with x-start = (+ border-width h-space)
      with row = 0
      with col = 0
      with col-width = (+ max-item-width h-space h-space)
      do
      (let ((tw (car item))
            (cbw (cdr item)))
        (xlib::reparent-window tw window
                               (+ x-start (* col col-width))
                               (+ y-start
                                  (dialog-border-width-of
                                   (cdr dialog-item))
                                  (* row max-item-height)))
        (xlib::reparent-window cbw window
                               (- (+ x-start
                                     (* col col-width)
                                     max-item-width)
                                  (xlib::drawable-width cbw))
                               (+ y-start (* row max-item-height)))
        (cond
         ((>= row (1- nrows))
          (incf col 1)
          (setq row 0))
         (T (incf row 1))))
      )

   ))
    


#|
(setf items
      (collect-input (list 'heycheckmeOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze")))
(setf items (collect-input items))
(collect-input (list 'heycheckmeOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze")
             :columns 2)
(collect-input (list 'heycheckmeOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze")
             :columns 3)
(collect-input (list 'heycheckmeOut!! "try this" "Or this" "or this"
                                       "Gold" "Silver" "Bronze"
                                       "platinum" "Tin" "lead" "zinc"
                                       "iron")
             :prompt-text "Give the man a medal!"
             :select-text "DO IT"
             :cancel-text "Forget it." :columns 3)
|#
