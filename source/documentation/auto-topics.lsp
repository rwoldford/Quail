;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                         auto-topics.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1992 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1993.
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(create-topic-info-from-files write-topic-info-to-file
          topics-list-to-topic-files auto-create-and-file-topics)))


(defun pathname-to-file-info (pathname-or-list-of-names)
  (if (not (listp pathname-or-list-of-names))
    (setf pathname-or-list-of-names (list pathname-or-list-of-names)))
  (loop for pn in pathname-or-list-of-names
        collect
        (list (rest (pathname-directory pn))
              (pathname-name pn)
              pn)))
        
(defun files-in-quail ()
  (loop for system in mk::*quail-systems*
        append
        (let ((files (mk::files-in-system system)))
          (loop for fs in files
                collect (list (rest (pathname-directory fs))
                              (pathname-name fs)
                              fs)))))

(defun find-exported-syms (file-info)
  (let
    ((file-path (third file-info))
     (form nil)
     (eof nil)
     (original-package *package*)
     (exported-syms NIL))
    (prog1
      (butlast
       (with-open-file (in file-path :direction :input)
         (loop while (not eof)
               do 
               (prog1
                 (setf form (read in nil :eof nil))
                 (setf eof (eq form :eof))
                 (if (listp form)
                   (cond
                    ((equal (string (first form)) "IN-PACKAGE")
                     (setf *package* (find-package (string (second form)))))
                    ((equal (string (first form)) "EXPORT")
                     (setf exported-syms (union (second (second form)) exported-syms))))))
               )))
       (setf *package* original-package))
   exported-syms
    ))

(defun generate-topics-list-from-files (&optional (file-pathnames NIL))
  
  (let ((exported-syms NIL)
        (files-info (if file-pathnames
                      (pathname-to-file-info file-pathnames)
                      (files-in-quail))))
    (loop
      for file-info in files-info
      when  (car
             (last
              (setf exported-syms
                    (let ((directories (cons "Quail" (rest (first file-info))))
                          (file-name-sans-.lisp (second file-info)))
                      (list directories
                            file-name-sans-.lisp
                            (find-exported-syms file-info))))))
      collect
      exported-syms)))


(defun create-topic-info-from-files (&optional (file-pathnames NIL))
  "Reads the source files as identified by the optional argument file-pathnames ~
   and constructs a list of topics and sub-topics information that can be parsed ~
   to construct topic documentation.  Topics and sub-topics are identified with the names of ~
   the directories and files as found in the file-pathnames.  ~
   Each subdirectory will be identified as a sub-topic of its parent directory.  ~
   At the bottommost level, each file is scanned for symbols appearing in an export ~
   statement.  Each such symbol found is a subtopic of the topic defined by the name ~
   of the file in which it is found; its documentation type is whatever is found as ~
   a documentable-use in its home package. ~
   (:optional ~
    (:arg file-pathnames NIL Either a single pathname of a file or a ~
   list of file-pathnames.  If absent, all source files of the current ~
   Quail system are assumed.)) ~
   (:returns An organized list of topic information.  Each topic list ~
   is itself a list whose first element is a symbol naming the ~
   topic and whose second element is a list containing sensible subtopics.) ~
   (:see-also (documentable-uses :function))"

  (let* ((topics-list  (generate-topics-list-from-files file-pathnames))
         (topics-hash-array (make-hash-table :size (length topics-list)
                                             :test #'equalp))
         (directories   NIL)
         (remaining-directories NIL)
         (file-name     NIL)
         (file-contents NIL)
         (final-list NIL)
         )
    (flet
      ((add-sub-topic-to (key sub &optional (type :topic))
         (unless (and (eq type :topic)
                      (string-equal key (string sub)))
           (let ((contents (gethash key topics-hash-array))
                 (sub-topic (list (if (stringp sub)
                                    (destring sub)
                                    sub)
                                  type)))
             (if contents
               (unless
                 (member sub-topic contents :test #'equal)
                 (push sub-topic (gethash key topics-hash-array)))
               (setf (gethash key topics-hash-array) (list sub-topic)))))))
      
      (dolist (info topics-list)
        (setf directories (first info))
        (setf remaining-directories directories)
        (setf file-name (second info))
        (setf file-contents (third info))
        (dolist (dir directories)
          (setf remaining-directories (rest remaining-directories))
          (when remaining-directories
            (add-sub-topic-to dir (first remaining-directories))))
        
        (if file-contents
          (cond
           ((> (length file-contents) 1)
            (dolist (sym file-contents)
              (dolist (use (documentable-uses sym
                                              (symbol-package sym)))
                (add-sub-topic-to file-name sym use)))
            (add-sub-topic-to (first (last directories)) file-name))
           (T
            (dolist (use (documentable-uses (first file-contents)
                                            (symbol-package (first file-contents))))
              (add-sub-topic-to (first (last directories))
                                (first file-contents)
                                use))))
          (format *terminal-io* "No file-contents! : ~a" file-name)
          ))
      (maphash #'(lambda (key value)
                   (push (list (destring key)
                               (sort value
                                     #'(lambda (a b)
                                         (string-lessp (string (first a))
                                                       (string (first b))))
                                     ))
                         final-list))
               topics-hash-array)
      (sort final-list #'(lambda (a b)
                           (string-lessp (string (first a))
                                         (string (first b))))))))


(defun write-topic-info-to-file (topic-info-list &optional 
                                                 (full-pathname NIL)
                                                 (outfile "topic-info"))
  (loop for info in topic-info-list do
        (with-open-file (ofile (if full-pathname
                                 full-pathname
                                 (mk::doc-auto-topics-path-name outfile))
                               :direction :output 
                               :if-exists :append
                               :if-does-not-exist :create)
          (format ofile "~&(~s ~%~5T(" (first info))
          (loop for sub in (second info) do
                (format ofile "~&~6T~s" sub))
          (format ofile "~&~5T))" )
          )))


(defun topics-list-to-topic-files (&optional
                                   (topics-list NIL))
  (declare (special *user-doc-out-path-function*))
  (let
    ((ofile-fun (if (functionp *user-doc-out-path-function*)
                  *user-doc-out-path-function*
                  #'mk::doc-auto-path-name))
     sub-topics
     name)
    (loop for topic in topics-list do
          (vw::inform-user (format NIL "~&processing topic ~s" (first topic)))
          (setf name (first topic))
          (setf sub-topics (second topic))
          (setf (doc name :topic)
                (make-instance
                  'qk::topic-documentation
                  :name (format nil "~a" name)
                  :sub-topics 
                  sub-topics
                  :package (package-name (symbol-package (first topic)))
                  ))
          (with-open-file (ofile (funcall ofile-fun name :topic)
                                 :direction :output 
                                 :if-exists :append
                                 :if-does-not-exist :create)
            (format ofile "~&(setf (doc '~a :topic)~%" name)
            (write-doc ofile (doc name :topic))
            (format ofile "~&)~%"))
          )))

(defun auto-create-and-file-topics (&optional (file-pathnames NIL))
  "Reads the source files as identified by the optional argument file-pathnames ~
   and constructs a list of topics and sub-topics information.  ~
   This is then parsed to construct topic documentation which is in turn ~
   written directly to the files in the doc;auto directory.  A different ~
   location is effected by setting the value of the symbol ~
   *user-doc-out-path-function* to be a function which will return a complete ~
   pathname for each topic.  That function takes two arguments, the topic name ~
   and its doc-type, i.e. :topic.  ~%~
   Topics and sub-topics are identified with the names of ~
   the directories and files as found in the file-pathnames.  ~
   Each subdirectory will be identified as a sub-topic of its parent directory.  ~
   At the bottommost level, each file is scanned for symbols appearing in an export ~
   statement.  Each such symbol found is a subtopic of the topic defined by the name ~
   of the file in which it is found; its documentation type is whatever is found as ~
   a documentable-use in its home package. ~
   (:optional ~
    (:arg file-pathnames NIL Either a single pathname of a file or a ~
   list of file-pathnames.  If absent, all source files of the current ~
   Quail system are assumed.)) ~
   (:see-also (documentable-uses :function) ~
    (*user-doc-out-path-function* :parameter) ~
    (write-topic-info-to-file  :function) ~
    (topics-list-to-topic-files  :function))"
  (topics-list-to-topic-files (create-topic-info-from-files file-pathnames)))
