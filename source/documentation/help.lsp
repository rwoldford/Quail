;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                help.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1991, 1992
;;; Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991-3
;;;
;;;
;;;----------------------------------------------------------------------------



(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(help *help-in-windows* help-types)))

;;;----------------------------------------------------------------------------
;;;
;;;  The online help function:
;;;
;;;        help
;;;
;;;---------------------------------------------------------------------------

(defvar *help-in-windows*
  NIL
  "Boolean value T or NIL. If non-NIL the help function uses ~%
   windows to produce help information.  If NIL, help is displayed directly ~%
   in the listener.")

(defun help (&optional (thing nil thing?) (help-type NIL))
  "Quail's online help function.  Gives help documentation ~%
   on its optional argument (typically a symbol).  ~%
   A second optional argument allows the user to identify the help-type.  ~%
   If more than one type of help is available, the user is ~%
   prompted for a choice.  ~%
   Try (help 'Quail :topic) for example.  ~%
   Help returns no value. ~%
   (:optional ~%
    (:arg thing nil A symbol or documentation object.) ~%
    (:arg help-type NIL The help type you want on thing.  For example ~%
                        :function or :macro or :constant.  If NIL and ~%
                        more than one type is available on that symbol, ~%
                        then you will be asked to choose one.)) ~%
   (:see-also help-types  *help-in-windows* (documentation :topic)) ~%
   (:returns No value.)"
  (declare (special *help-in-windows*))
  (if thing?
    (if *help-in-windows* 
      (if help-type
        (quail::help-display thing help-type)
        (quail::help-display thing))
      (if help-type
        (help-object thing help-type)
        (help-object thing)))
    (let ((selected-help
           (quail-query
            "Help is available on many symbols.  ~%
             If you want help on the help function just enter help. ~%
             Otherwise enter the symbol ~
             (or list of symbols) on which you would like help.  ~%~%
             Enter symbol now:  ")))
      (help selected-help)))
  (values))

(defun help-types  (sym &optional package)
  "Returns the list of help types for this symbol in the package ~%
   (if specified).  ~%
   For example as a variable, a class name, a function, a macro, et cetera."
  (documentable-uses sym package))

(defgeneric help-object (thing &optional help-type)
  (:documentation "Produces help description for its argument.  ~%
                   If the optional argument help-type is given, it produces ~%
                   the description of the symbol for that help-type.  ~%
                   Otherwise it produces documentation on the available help-type ~%
                   of the first argument.   If more than one help-type is ~%
                   available, the user is asked to choose one."))



;;;--------------------------------------------------------------------------
;;;
;;;  T
;;;
;;;--------------------------------------------------------------------------


(defmethod help-object (thing &optional help-type)
  (declare (ignore  help-type))
  (vw::inform-user (format NIL 
                          "Sorry, no help is yet available for:~% ~s ."
                          thing)))



;;;--------------------------------------------------------------------------
;;;
;;;  Numbers
;;;
;;;--------------------------------------------------------------------------


(defmethod help-object ((thing number) &optional help-type)
  (declare (ignore  help-type))
  (vw::inform-user (format NIL 
                          "Sorry, not much help here.~% ~s is simply a number."
                          thing)))


;;;--------------------------------------------------------------------------
;;;
;;;  lists
;;;
;;;--------------------------------------------------------------------------


(defmethod help-object ((items list) &optional help-type)
  (if help-type
    (loop for i in items do (help i help-type))
    (loop for i in items do (help i))))


;;;--------------------------------------------------------------------------
;;;
;;;  strings
;;;
;;;--------------------------------------------------------------------------

(defmethod help-object ((thing string) &optional (help-type NIL))
  (help-object (destring thing) help-type))
  

;;;--------------------------------------------------------------------------
;;;
;;;  Symbols
;;;
;;;--------------------------------------------------------------------------


(defmethod help-object ((sym symbol) &optional help-type)
  (let ((help-types (if help-type
                      (list help-type)
                      (help-types sym))))
    (if help-types
      
      
      ;;then
      (if (> (length help-types) 1)
        
        ;; then
        (let
          ((selected-help-type
            (quail-query
             "Help is available on ~s for the following types: ~{~& ~s ~}. ~%
              Please specify which one you would like (or :NONE to cancel ~%
              request).~%>> "
             sym
             help-types)))
          (if (and selected-help-type
                   (not (eq selected-help-type
                            :NONE)))
            ;; then
            (let ((doc-object (doc sym selected-help-type)))
              (if doc-object
                ;; then
                (help-object doc-object)
                ;; else
                (vw::inform-user (format NIL 
                                        "Sorry, the symbol - ~s ~%
                                         - has no help available for ~%
                                         help-type ~s."
                                        sym selected-help-type))))
            ;; else do nothing
            ))
        
        ;; else 
        (let ((doc-object (doc sym (car help-types))))
          (if doc-object
            ;; then
            (help-object doc-object)
            ;; else
            (vw::inform-user (format NIL 
                                 "Sorry, the symbol - ~s - has no help available for help-type ~s."
                                 sym help-type))))
        )
      
      
      ;;else see if there is a general topic corresponding to this symbol
      (let ((general-topic? (doc sym :topic)))
        (if general-topic?
          (help-object general-topic?)
          (vw::inform-user (format NIL 
                               "Sorry, the symbol - ~s - has no help available."
                               sym)))))))



;;;--------------------------------------------------------------------------
;;;
;;;  documentation-object
;;;
;;;--------------------------------------------------------------------------


(defmethod help-object ((q-d documentation-object) &optional help-type)
  (declare (special *quail-help-io*)
           (ignore  help-type))
  (format-doc *quail-help-io* q-d))
