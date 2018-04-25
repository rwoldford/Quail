;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               selection-list.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1995 Maynooth
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '( display-list-selection-mixin single-selection-list
            multi-selection-list select-from-list)))

(in-package :views)


(defclass display-list-selection-mixin ()
  ((display-selection :initform nil
                      :initarg :display-selection
                      :accessor display-selection-of)))

(defclass single-selection-list (display-list-selection-mixin display-list)
  ())


(defclass multi-selection-list (display-list-selection-mixin display-list)
  ())

(defgeneric display-selection-function (display-list-selection-mixin)
  (:documentation "Returns a function which is invoked with a left click ~
                   on subview of the display list"))

(defgeneric selected-data-of (display-list-selection-mixin)
  (:documentation "Returns a the items which have been selected from ~
                   the display list."))

(defmethod display-selection-function ((self display-list-selection-mixin))
  nil)

(defmethod selected-data-of ((self display-list-selection-mixin))
  nil)




(defmethod construct-sub-views :after ((self display-list-selection-mixin) &rest args )
  (declare (ignore args))
  (let* ((f1 (display-selection-function self)))
    (when f1   
      (loop for s in (subviews-of self) do
            (setf (left-fn-of s) f1)))))


(defmethod selected-data-of ((self single-selection-list))
  (let ((d (display-selection-of self)))
    (if d
      (viewed-object-of d))))

(defmethod display-selection-function ((self single-selection-list))
  #'(lambda(sub)
      (let ((old (display-selection-of self)))
        (when old
          (deselect-menu-label old)
          (if (eql sub old) (setq sub nil)))
        (if sub
          (select-menu-label sub))
        (setf (display-selection-of self) sub)
        )))

(defmethod selected-data-of ((self multi-selection-list))
  (let ((d (display-selection-of self)))
    (mapcar #'viewed-object-of d)))


(defmethod display-selection-function ((self multi-selection-list))
  #'(lambda(sub)
      (let ((ans (display-selection-of self)))
        (if (member sub ans)
          (progn
            (deselect-menu-label sub)
            (setf (display-selection-of self) (delete sub ans)))
          (progn
            (select-menu-label sub)
            (setf (display-selection-of self) (append ans (list sub))))
          ))))

;;----------------------------------------------------------------------------------------

(defun select-from-list (list
                   &key prompt-text
                   (item-function NIL)
                   (selection-type :single)
                   (selection-item-type 'marked-menu-label)
                   selection-display-type)
  "This pops up a window so the user can choose from the list. ~
   If selection-type is :single (the default) only a single ~
   selection is allowed, any other value of selection-type permits ~
   multiple selections. ~
   Further control of selection behavior is possible by giving a suitable ~
   class name  for :selection-display-type, and of menu label appearance ~
   through the :selection-item-type keyword. "
   

  (if (null prompt-text)
    (setq prompt-text
          (if (eq selection-type :single)
            "Choose One"
            "Choose")))
  (if (null selection-display-type)
    (setq selection-display-type
          (if (eq selection-type :single)
            'single-selection-list
            'multi-selection-list)))

  (let* ((okw 40) (okh 20) (indent 10)
         (scroll 15)
          (menu-list (display-list
                     :display-type selection-display-type
                     :data list
                     :labels item-function
                     :display-list-border 2
                     :item-type selection-item-type
                     :draw? nil))
         (ok (view :type 'close-button
                   :text "OK"
                     :rectangle-width okw
                   :rectangle-height okh))
         (dw (draw-region-width menu-list))
         (dh (draw-region-height menu-list))
         (bigr-wid (max 150 (+ (* 2 indent) dw)))
         (bigr-hgt (max 150 (+ (* 2 indent) dh (* 2 okh))))
         (l (max scroll
                 (- (truncate (wb:screen-width) 2)
                    (truncate bigr-wid 2))))
         (tp (- (wb:screen-height) 100))
         (bigr (make-region l (+ l bigr-wid)
                            (max scroll   (- tp  bigr-hgt))
                            tp))
         (select-view 
          (view-layout :box-views? t
                       :subviews (list menu-list ok)
                       :bounding-region bigr
                       :positions
                       (list
                        (make-region (+ indent l) (- (right-of bigr) indent)
                                     (- (top-of bigr) indent dh)
                                     (- (top-of bigr) indent))
                        (make-region (+ indent l) (- (right-of bigr) indent)
                                     (+ (bottom-of bigr) indent)
                                     (+ (bottom-of bigr) indent okh)))))
                        
                                     
         (w (make-view-window 
             :background-color wb:*light-grey-color*
             :pen-color wb:*black-color*
            :title  prompt-text
            :left (- (left-of bigr) scroll)
            :bottom (- (bottom-of bigr) scroll)
            :right (right-of bigr)
            :top (top-of bigr)))
         )
      (draw-view select-view :viewport (make-viewport w)) 
      
  (loop ;;with start-time = (get-universal-time)
        ;; with wait-time = 20
         until (not (wb:canvas-visible-p   w))
        do 
          (ccl::event-dispatch)
          )
 
    (selected-data-of menu-list)
                                          
    
    ))

