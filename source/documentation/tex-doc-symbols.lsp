;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           tex-doc-symbols.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(tex-doc-symbols make-tex-doc-driver
          *user-tex-out-path-function*)))

;;;-----------------------------------------------------------------------


(defvar *user-tex-out-path-function*
  NIL
  "A variable whose value is either NIL or is a function of two arguments, ~
   a symbol and a doc-type (e.g. :function :macro  :topic, et cetera).  ~
   When funcalled on a symbol and doc-type, the function must return a complete ~
   path name for the output documentation file that will be automatically generated ~
   by tex-doc-symbols.  ~
   If NIL, the path used will be the same as for the quail system tex documentation.  ~
   This variable allows the user to generate quail documentation for ~
   his or her own symbols, or even to override the provided quail documentation."
  )

(defun tex-doc-by-types (sym &key
                             (package nil)
                             (types nil))
  (declare (special *user-tex-out-path-function*))
  "Produce TeX commands that ~
   document the symbol in the specified (or home) package according to ~
   its use in the corresponding list of types."
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
      ((ofile-fun (if (functionp *user-tex-out-path-function*)
                    *user-tex-out-path-function*
                    #'mk::doc-tex-path-name)))
      (loop for type in types do
            (with-open-file
              (ofile (funcall ofile-fun sym type)
                     :direction :output 
                     :if-exists :append
                     :if-does-not-exist :create)
              (write-tex-doc ofile (doc sym type)))))))


(defun tex-doc-symbols (&key package symbol-list types)
  "Constructs TeX documentation for all external symbols given by the ~
   keyword package or only those specified ~
   by the keyword symbol-list.  If given, types is a list of the ~
   type of symbol usage to be documented (e.g. '(function macro class)).  ~
   Otherwise all documentable types will be constructed."
  
  (if symbol-list
    (if package
      (loop for sym in symbol-list
            do
            (setf sym (find-symbol (string sym) package))
            (tex-doc-by-types sym :package package :types types))
      (loop for sym in symbol-list
            do
            (tex-doc-by-types sym :types types)))
    (do-external-symbols (sym package)
      (tex-doc-by-types sym :package package :types types))
    ))


(defun include-tex-doc-by-types (destination sym
                                 &key (package nil)
                                      (types nil))
  "Produce and write to the destination ~
   the TeX \include commands for those files which contain the symbol's~
   documentation as determined by the specified (or home) package according to ~
   its use in the corresponding list of types."
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
    (loop for type in types do
          (format destination "~&\\include{~a}" (mk::doc-file-name sym type)))))



(defun make-tex-doc-driver
       (destination &key package
                    symbol-list
                    (types '(:function))
                    (document-type "article")
                    (document-styles '("fullpage"))
                    (point-size 12)
                    (input-files nil))
  "Produces a LaTeX file (destination) ~
   that can be run to get a .dvi documentation file for the ~
   given package's external symbols (or those specified by symbol-list).  ~
   If supplied, only documentation in the list of types will be produced ~
   (e.g. Class, function, macro, etc.).~
   Point-size (a single fixnum), document-type (\"default article\"), document-styles ~
   (default '(\"fullpage\")), and extra input-files can be specified ~
   as the values of those keyword arguments."
  
  (declare (special tex-doc-style))
  
  (setf document-styles
        (concatenate 'list
                     (list (format nil "~apt" point-size))
                     (if (listp document-styles)
                       document-styles
                       (list document-styles))
                     (if (listp tex-doc-style)
                       tex-doc-style
                       (list tex-doc-style))))
  (with-open-file (ofile destination
                         :direction :output
                         :if-does-not-exist :create)
    ;;
    ;; Set up the header of the document.
    ;;
    (format ofile "~&\\documentstyle")
    (write-tex-args ofile document-styles)
    (format ofile "{~a}" document-type)
    (format ofile "~&\\begin{document}")
    ;;
    ;;  Set up the input files
    ;;
    (when input-files
      (if (not (listp input-files))
        (setf input-files (list input-files)))
      (loop for file in input-files
            do (format ofile "~&\\input{~a}" file)))
    ;;
    ;;  Now determine the desired documentation, construct the
    ;;  corresponding filename and include it in the
    ;;  driver file
    (cond
     (symbol-list
      (if package
        (loop for sym
              in (sort symbol-list
                       #'string<  :key #'(lambda (x) (string x)))
              do
              (setf sym (find-symbol (string sym) package))
              (include-tex-doc-by-types ofile sym :package package :types types))
        (loop for sym
              in (sort symbol-list
                       #'string<  :key #'(lambda (x) (string x)))
              do
              (include-tex-doc-by-types ofile sym :package package :types types))))
     (T
      (do-external-symbols (sym package) (push sym symbol-list))
      (loop for sym
            in (sort symbol-list
                     #'string<  :key #'(lambda (x) (string x)))
            do
            (include-tex-doc-by-types ofile sym :package package :types types)))
     )
    ;;
    ;;  And end the file
    ;;
    (format ofile "~&\\end{document}")))
    
                                 
