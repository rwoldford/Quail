;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                doc-index.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; copyright (c) 1990 statistical computing laboratory, university of waterloo
;;;
;;;
;;;  authors:
;;;     m.e. lewis 1991.
;;;     r.w. oldford 1991.
;;;     e.e. cummings 1933.
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-documentation-index make-sorted-documentation-index sort-doc
          view-doc-index)))

(defun view-doc-index (&key package pathname)
  "Produces a display of the list of symbols found in the file ~
   given by the pathname.  If no pathname is specified, then the index ~
   file for the given package is examined."
  (cond
   ((and package pathname)
    (quail-error "VIEW-DOC-INDEX -- Must supply package ~
                  or pathname, not both."))
   (package
    (setf pathname (mk::doc-index-filename :package package)))
   ((null pathname)
    (quail-error "VIEW-DOC-INDEX -- Must supply one of package ~
                  or pathname.")
    )
   )
  (setf pathname (probe-file pathname))
  (if pathname
    (cond
     ((string=
       "As a file."
       (wb:pick-one '("As a file."
                      "As a help window. (longer)")
                    :prompt-text
                    (format NIL
                            "Choose one.~%~
                             A help window will take more resources.")))
      (q:edit-file pathname))
     (T
      (vw:inform-user (format NIL "This may take a little while ~%~
                                  as these indices are very long."))
      (q::help
       (make-instance
         'topic-documentation
         :name (format NIL "~s symbol index" (or package ""))
         :doc-capsule
         (format NIL "Here is the index of symbols for ~s."
                 (or package pathname))
         :sub-topics
         (with-open-file (ifile pathname
                                :direction :input
                                :if-does-not-exist nil)
           (read ifile NIL NIL))
         ))
      )
     )
    (vw:inform-user (format NIL
                           "Sorry, can't find the index. ~%~
                            You might try recreating it with ~%~
                            ~%~5T(make-sorted-documentation-index :package ~s)"
                           (or package :quail)))
    )
  )


(defun process-symbol (sym ofile &optional package)
  (setf sym (find-symbol (string sym) 
                         (or (package-name package)
                             (symbol-package sym))))
  (if (eql :function (function-information sym))
    (format ofile "~&(~a   ~s)" sym 
            :function))
  (if (eql :macro (function-information sym))
    (format ofile "~&(~a   ~s)" sym 
      :macro))
  (if (eql :special-form (function-information sym))
    (format ofile "~&(~a   ~s)" sym 
            :special-form))
  (if (eql :special-operator (function-information sym)) ;27oct05
    (format ofile "~&(~a   ~s)" sym 
      :special-operator)) ;27oct05 
  (if (and (boundp sym)
           (constantp sym))
    (format ofile "~&(~a   ~s)" sym 
            :constant))
  (if (and (boundp sym)
           (not (constantp sym)))
    (format ofile "~&(~a   ~s)" sym 
            :variable))
  (if (structure-p sym)
    (format ofile "~&(~a   ~s)" sym 
            :structure))
  (if (find-class sym nil)
    (format ofile "~&(~a   ~s)" sym 
            :class)))

    
(defun make-documentation-index (&key package symbol-list)
  "Constructs documentation index for all external symbols ~
   or for those specified in the optional list argument. ~
   The result is stored in the doc-index file in the documentation ~
   directory."
  (with-open-file (ofile (mk::doc-index-filename :package package)
                         :direction :output 
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (format ofile "~&~a" (make-string 80 :initial-element #\;))
    (format ofile "~&;;;")
    (format ofile "~&;;;")
    (format ofile "~&;;;")
    (if package
      (format ofile "~5TIndex file for symbols in the ~s package." package)
      (format ofile "~5TIndex file of symbols")
      )
    (format ofile "~&;;;")
    (format ofile "~5T~a" (current-date))
    (format ofile "~&;;;")
    (format ofile "~&;;;")
    (format ofile "~&~a" (make-string 80 :initial-element #\;))
    (if symbol-list
      (loop for sym in symbol-list
            do
            (process-symbol sym ofile package))
      (if package
        (do-external-symbols (sym package)
          (process-symbol sym ofile package))
        (error "Must specify at least one of SYMBOL-LIST and PACKAGE ~
                -- MAKE-DOCUMENTATION-INDEX")))
    ))

(defun sort-doc (&key package (symbol-list ()))
  (let ((filename (mk::doc-index-filename :package package)))
    (with-open-file (ifile filename
                           :direction :input
                           :if-does-not-exist nil)
      (do ((pair (read ifile nil nil) 
                 (read ifile nil nil))
           )
          ((null pair) symbol-list)
        (push pair symbol-list)))
    (setf symbol-list (sort symbol-list #'string<
                            :key #'(lambda (x) (concatenate 'string  
                                                            (string (first x))
                                                            (string (second x))))))
    (delete-file filename)
    (with-open-file (ofile filename
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
      (format ofile "~&~a" (make-string 80 :initial-element #\;))
      (format ofile "~&;;;")
      (format ofile "~&;;;")
      (if package
        (format ofile "~5TIndex file for symbols in the ~s package." package)
        (format ofile "~5TIndex file of symbols")
        )
      (format ofile "~&;;;")
      (format ofile "~5TAll symbols sorted in alphabetical order ~
                     and stored in a list.")
      (format ofile "~&;;;")
      (format ofile "~5T~a" (current-date))
      (format ofile "~&;;;")
      (format ofile "~&;;;")
      (format ofile "~&~a" (make-string 80 :initial-element #\;))
      (format ofile "~&~%(")
      (if package
        (dolist (item symbol-list)
          (format ofile
                  "~&~5T(~a:~s ~s)"
                  package
                  (first item)
                  (second item)))
        (dolist (item symbol-list)
          (format ofile "~&~5T~s" item))
          )
      (format ofile "~&)")
      ))
  )

(defun make-sorted-documentation-index (&key package (symbol-list ()))
  "Constructs documentation index for all external symbols ~
   or for those specified in the optional list argument. ~
   Symbols are sorted first by name  ~
   and then by name within use (class, function, etc.).  ~
   The result is stored in a doc-index file in the documentation ~
   indices directory for that package."
  (make-documentation-index :package package :symbol-list symbol-list)
  (sort-doc :package package)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Some examples
;;;
;;;
#|
(make-documentation-index :package :window-basics)
(sort-doc (mk::doc-index-filename :package :window-basics))


(make-sorted-documentation-index :package :quail-kernel)

(loop for p in '(:window-basics :views :quail-kernel :quail :new-math)
      do
      (make-sorted-documentation-index :package p))


|#
