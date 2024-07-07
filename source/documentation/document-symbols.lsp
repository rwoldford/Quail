;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          document-symbols.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1991 statistical computing laboratory, university of waterloo
;;;
;;;
;;;  authors:
;;;     m.e. lewis 1991.
;;;     r.w. oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(document-symbols *user-doc-out-path-function*)))

(defvar *user-doc-out-path-function*
  NIL
  "A variable whose value is either NIL or is a function of two arguments, ~
   a symbol and a doc-type (e.g. :function :macro  :topic, et cetera).  ~
   When funcalled on a symbol and doc-type, the function must return a complete ~
   path name for the output documentation file that will be automatically generated ~
   by document-symbols.  ~
   If NIL, the path used will be the same as for the quail system documentation.  ~
   This variable allows the user to generate quail documentation for ~
   his or her own symbols, or even to override the provided quail documentation."
  )

(defun doc-by-types (sym &key (package nil)
                         (types nil))
  "Document the symbol in the specified (or home) package according to ~
   its use in the corresponding list of types."
  (declare (special *user-doc-out-path-function*))
  ;;
  ;;  Get the types to be a non-NIL list if possible.
  ;;
  (if types
    (setf types (intersection types (documentable-uses sym package)))
    (setf types (documentable-uses sym package)))
  (unless (listp types) (setf types (list types)))
  ;;
  ;; Now document it for all the types mentioned.
  ;;
  (if types
    (let
      ((ofile-fun (if (functionp *user-doc-out-path-function*)
                    *user-doc-out-path-function*
                    #'doc-auto-path-name)))
      
      (loop for type in types do
            (with-open-file (ofile (funcall ofile-fun sym type)
                                   :direction :output 
                                   :if-exists :append
                                   :if-does-not-exist :create)
              (write-doc ofile (doc sym type)))))))



;;;-----------------------------------------------------------------------------------------------
;;;
;;;  Produce document files for all symbols in a list or all symbols
;;;  in a package.
;;;
;;;-----------------------------------------------------------------------------------------------



(defun document-symbols (&key package symbol-list types)
  "Constructs documentation for all external symbols given by ~
   package or only those specified ~
   by symbol-list.  If given, types is a list of the ~
   type of symbol usage to be documented (e.g. '(function macro class)).  ~
   Otherwise all documentable types will be constructed."
  
  (if symbol-list
    (if package
      (loop for sym in symbol-list
            do
            (if (null sym)
              (doc-by-types sym :types types)
              (if
                (setf sym (find-symbol (string sym) package))
                (doc-by-types sym :package package :types types))))
      (loop for sym in symbol-list
            do
            (doc-by-types sym :types types)))
    (do-external-symbols (sym package)
      (doc-by-types sym :package package :types types))
    ))
