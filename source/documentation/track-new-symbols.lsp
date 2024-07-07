;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                         track-new-symbols.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1992 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1992.
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(get-new-symbol-uses pp-to-file file-subtopics get-old-subtopics
          get-deprecated-symbol-uses)))

(defun get-new-symbol-uses
       (old-list &key
                 package
                 symbol-list
                 types
                 (sorted-topics? T))
  
  "Constructs a list of list pairs.  The first element in each list pair is ~
   a symbol; the second element is the symbol's documentable use.  ~
   Only new symbols, or new uses of previously identified symbols are ~
   constructed.  ~
   All symbols in either the specified package or the symbol-list are ~
   compared to the old-list.  ~
   The required parameter old-list must be a similar list of list pairs.  ~
   Returns a list containing the differences."
  
  (let ((grand-list
         (generate-subtopics :package package
                             :symbol-list symbol-list
                             :types types
                             :sorted-topics? sorted-topics?))
        result
        )
    (flet
      ((eq-test (a b)
         (and (eq (first a) (first b))
              (eq (second a) (second b))))
       (sort-key (x) (string-upcase (car x))))
      
      (setf result (set-difference grand-list old-list :test #'eq-test))
      
      (if sorted-topics?
        (cl:sort result #'string< :key #'sort-key)
        result))))

(defun get-deprecated-symbol-uses
       (old-list &key
                 package
                 symbol-list
                 types
                 (sorted-topics? T))
  
  "Constructs a list of list pairs.  The first element in each list pair is ~
   a symbol; the second element is the symbol's documentable use.  ~
   Only symbols, or uses of previously identified symbols, which no longer ~
   appear in the symbol-list are returned.  ~
   All symbols in either the specified package or the symbol-list are ~
   compared to the old-list.  ~
   The required parameter old-list must be a similar list of list pairs.  ~
   Returns a list containing the differences."
  
  (let ((grand-list
         (generate-subtopics :package package
                             :symbol-list symbol-list
                             :types types
                             :sorted-topics? sorted-topics?))
        result
        )
    (flet
      ((eq-test (a b)
         (and (eq (first a) (first b))
              (eq (second a) (second b))))
       (sort-key (x) (string-upcase (car x))))
      
      (setf result (set-difference old-list grand-list :test #'eq-test))
      
      (if sorted-topics?
        (cl:sort result #'string< :key #'sort-key)
        result))))

(defun pp-to-file (thing file &key (if-exists :append))
  "Pretty-prints the first argument to the file given by the second ~
   argument.  The keyword if-exists specifies ~
   the action to be taken if the file already exists.  ~
   The default is to append to the existing file (i.e. :append); ~
   other possibilities are :supersede, :error, and others as for ~
   the CL with-open-file."
  
  (with-open-file (ofile file
                         :direction :output 
                         :if-exists if-exists
                         :if-does-not-exist :create)
    (pprint thing ofile)))

(defun file-subtopics (sub-topics &key name directory path (if-exists :append))
  "Prints the sub-topics to a specified file.  ~
   The keyword if-exists specifies ~
   the action to be taken if the file already exists.  ~
   The default is to append to the existing file (i.e. :append); ~
   other possibilities are :supersede, :error, and others as for ~
   the CL with-open-file.  ~
   The file is determined by the value of the keyword parameters ~
   name, directory, and path.  ~
   Either the full path name of the file is used as given ~
   by the value of the path parameter, or a pathname is constructed using ~
   the default doc path for current-symbols, the optional sub-directory ~
   given by directory, and the filename given by name."
  
  (unless path
    (unless name (setf name "sub-topic-list"))
    (setf path (doc-current-symbols-path-name name :directory directory)))
  
  (with-open-file (ofile path
                         :direction :output 
                         :if-exists if-exists
                         :if-does-not-exist :create)
    (format ofile "~&(")
    (loop for st in sub-topics do
          (format ofile "~&~s" st )
          finally
          (format ofile "~&)")))
  NIL)



(defun get-old-subtopics (&key name directory path)
  "Returns the list of sub-topics from a specified file.  ~
   The file is determined by the value of the keyword parameters ~
   name, directory, and path.  ~
   Either the full path name of the file is used as given ~
   by the value of the path parameter, or a pathname is constructed using ~
   the default doc path for current-symbols, the optional sub-directory ~
   given by directory, and the filename given by name."

  (unless path
    (unless name (setf name "sub-topic-list"))
    (setf path (doc-current-symbols-path-name name :directory directory)))

  (with-open-file (ofile path
                         :direction :input 
                         :if-does-not-exist :error)
    (read ofile)))
