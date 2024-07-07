;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                       help-display.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     C.B. Hurley 1992 George Washington University
;;;     R.W. Oldford 1992
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :quail)
(eval-when (:compile-toplevel :load-toplevel :execute) (export '(help-display)))

(defgeneric help-display (thing &optional help-type)
  (:documentation "Displays in an interactive window ~%
                   help information of type help-type on thing."))

;;;--------------------------------------------------------------------------
;;;
;;;  T
;;;
;;;--------------------------------------------------------------------------


(defmethod help-display (thing &optional help-type)
  (declare (ignore  help-type))
  (inform-user (format NIL 
                       "Sorry, no help is yet available for:~% ~s ."
                       thing)))


;;;--------------------------------------------------------------------------
;;;
;;;  Numbers
;;;
;;;--------------------------------------------------------------------------


(defmethod help-display ((thing number) &optional help-type)
  (declare (ignore  help-type))
  (inform-user (format NIL 
                       "Sorry, not much help here.~% ~s is simply a number."
                       thing)))


;;;--------------------------------------------------------------------------
;;;
;;;  lists
;;;
;;;--------------------------------------------------------------------------

(defmethod help-display ((items list) &optional (help-type NIL))
  "If help-type is non-NIL, then help is called repeatedly on the items ~%
   in the list with the given help-type.  If help-type is NIL and there ~%
   are exactly two items in the items list and the second item is a legitimate ~%
   doc-type, then the ~%
   second item in the list is taken to be the help type.  ~%
   Otherwise help is called ~%
   on each item in the list with no type specified.  ~%
   (:see-also doc-type-p quail-doc-types)"
  (cond
   (help-type
    (loop for i in items do (help i help-type)))
   ((= (length items) 2)
    (if (qk::doc-type-p (second items))
      (help (first items) (second items))
      (loop for i in items do (help i))))
   (T (loop for i in items do (help i)))))



;;;--------------------------------------------------------------------------
;;;
;;;  strings
;;;
;;;--------------------------------------------------------------------------

(defmethod help-display ((thing string) &optional (help-type NIL))
  (help-display (destring thing) help-type))


;;;--------------------------------------------------------------------------
;;;
;;;  Symbols
;;;
;;;--------------------------------------------------------------------------


(defmethod help-display ((sym symbol) &optional (help-type NIL))
  (let ((help-types (if help-type
                      (list help-type)
                      (qk::help-types sym (symbol-package sym)))))
    (if help-types
      
      
      ;;then
      (if (> (length help-types) 1)
        
        ;; then
        (let
          ((selected-help-type
            (wb::pick-one (cons :none help-types)
                          :prompt-text
                          (format
                           NIL
                           "Help is available on ~s for the following types:~%~%
                            Please specify which one you would like (or :NONE to cancel ~%
                            request). "
                           sym)
                          )))
          (if (and selected-help-type
                   (not (eq selected-help-type
                            :NONE)))
            ;; then
            (let ((doc-object (doc sym selected-help-type)))
              (if doc-object
                ;; then
                (help-display doc-object)
                ;; else
                (inform-user (format NIL 
                                     "Sorry, the symbol - ~s - has no help available for help-type ~s."
                                     sym selected-help-type))))
            ;; else do nothing
            ))
        
        ;; else 
        (let ((doc-object (doc sym (car help-types))))
          (if doc-object
            ;; then
            (help-display doc-object)
            ;; else
            (inform-user (format NIL 
                                 "Sorry, the symbol - ~s - has no help available for help-type ~s."
                                 sym help-type))))
        )
      
      
      ;;else see if there is a general topic corresponding to this symbol
      (let ((general-topic? (doc sym :topic)))
        (if general-topic?
          (help-display general-topic?)
          (inform-user (format NIL 
                               "Sorry, the symbol - ~s - has no help available."
                               sym))))))
  
  )



;;;--------------------------------------------------------------------------
;;;
;;;  documentation-object
;;;
;;;--------------------------------------------------------------------------

(defmethod help-display ((thing qk::documentation-object)
                         &optional (help-type NIL))
  (let* ((h-v (make-instance 'help-view
                :viewed-object thing
                :help-type
                (or help-type
                    (qk::doc-type thing))))
         (vw (make-help-window :width  (help-window-width h-v)
                               :height (help-window-height h-v)))
         (vp (make-viewport vw)))
    (draw-view h-v :viewport vp)
    h-v))

