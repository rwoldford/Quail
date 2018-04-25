;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                install-topics.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(install-topics load-topics make-?-topic ensure-loaded-topics
          load-topics-file topics-file-to-topic-files
          export-topic-syms)))


(defun load-topics (&key package symbol-list)
  "Loads the topic documentation (if any) ~
   for all external symbols given by the ~
   keyword package or only those specified ~
   by the keyword symbol-list."
  (if symbol-list
    (if package
      (loop for sym in symbol-list
            do
            (setf sym (find-symbol (string sym) package))
            (if sym (doc sym :topic))
            (vw::inform-user-busy))
      (loop for sym in symbol-list
            do
            (doc sym :topic)
            (vw::inform-user-busy)))
    (do-external-symbols (sym package)
      (doc sym :topic)
      (vw::inform-user-busy))
    )
  (vw:inform-user "Topics Loaded"))

(let ((already-loaded? NIL))
  (defun ensure-loaded-topics (&key package symbol-list)
    (declare (special *quail-terminal-io*))
    (unless already-loaded?
      (vw::inform-user
       (format NIL
              "~&~%OK, I have to load some topic information.~%~
               This may take a while ...  ~%~
               The good news is that I will eventually finish and ~%~
               I only have to do it once.~%"))
      (load-topics :package package :symbol-list symbol-list)
      (setf already-loaded? T)))
  )


(defun install-topics (&key package symbol-list)
  "Installs the topic documentation (if any) ~
   as a super-topic of all its sub-topics.  ~
   Which topics to be installed are specified by the values ~
   of the keyword parameters package and symbol-list.
   If only the package is specified, then  all external symbols ~
   having a value for (doc symbol \:topic ) are installed.  ~
   Otherwise only the symbols on the symbol-list which are in ~
   the package (if specified) are installed."
  
  
  (flet
    ((install-topic (topic)
                    (if topic
                      (loop for st in (sub-topics topic)
                            do (install-super-topic topic st)))))
    (if symbol-list
      (if package
        (loop for sym in symbol-list
              do
              (setf sym (find-symbol (string sym) package))
              (if sym
                (install-topic (doc sym :topic))))
        (loop for sym in symbol-list
              do
              (install-topic (doc sym :topic))))
      (do-external-symbols (sym package)
        (install-topic (doc sym :topic)))
      )
    "Topics Installed"))



(defun make-?-topic (package &optional topics)
  "Creates the topic documentation for the ? symbol in the given package.  ~
   If a list of topics are specified as the value of the optional argument ~
   topics, then these topics are installed as the sub-topics of the general ~
   question-mark topic.  ~
   If topics are not specified, then all symbols in this package which have ~
   topic documentation on them and are themselves not the sub-topic of another ~
   topic are included as sub-topics of the question-mark topic."
  
  
  (let*
    ((question-mark-sym (find-symbol "?" package))
     (question-mark-topic (doc question-mark-sym :topic))
     
     ;; The following is pretty ugly, but easy.
     (do-something? T))
    
    (if question-mark-topic
      
      ;;then
      ;; Should it be used or not?
      (if
        (not
         (quail-yes-or-no-p
          "Topic documentation for the question-mark already exists in the package ~a !   ~
           Do you want to add to it? " (package-name package)))
        
        ;; then
        (if
          (quail-yes-or-no-p
           "Do you want to over-ride the existing documentation and create a new topic? ")
          ;; then
          ;; make a new one and set it.
          (setf question-mark-topic
                (setf (doc question-mark-sym :topic)
                      (make-doc question-mark-sym :topic)))
          ;;else
          ;; We're not going to do anything.
          (setf do-something? NIL))
        
        ;; no else, use the topic we found.
        )
      
      ;;else
      (setf question-mark-topic
            (setf (doc question-mark-sym :topic)
                  (make-doc question-mark-sym :topic))))
    
    ;; now blow out or do something
    (when do-something?
      
      (let (current-topic)
        (if topics
          ;; then
          (loop
            for sym in topics
            when (setf current-topic (doc sym :topic))
            do (install-sub-topic question-mark-topic current-topic)
            )
          (do-external-symbols (sym package)
            (setf current-topic (doc sym :topic))
            (if
              (and
               ;; we have a topic
               current-topic
               ;; and it isn't known to be a sub-topic of something else
               (not (super-topics current-topic)))
              ;; then we'll take it
              (install-sub-topic question-mark-topic current-topic)))))
      
      (if (null (doc-capsule question-mark-topic))
        (setf
         (doc-capsule question-mark-topic)
         (format nil
                 "General help topic for the package ~a.  See the sub-topics for ~
                  information on various areas." (package-name package))))
      
      (if (null (doc-elaboration question-mark-topic))
        (setf
         (doc-elaboration question-mark-topic)
         (format nil
                 "The sub-topics listed below contain information on different topics.  ~
                  This information can be accessed on any sub-topic by invoking the help ~
                  command on the symbol that names it.")))
      )
    )
  NIL)

(defun load-topics-file (filename &optional (package :quail))
  (with-open-file (ofile filename
                         :direction :input)
    (let ((topic (read ofile nil nil))
          name
          sub-topics)
      (loop while topic do
            (setf name (first topic))
            (setf sub-topics (second topic))
            (setf (doc name :topic)
                  (make-instance
                   'qk::topic-documentation
                   :name (format nil "~a" name)
                   :sub-topics 
                   sub-topics
                   :package package
                   ))
            (setf topic (read ofile nil nil))))))

(defun topics-file-to-topic-files (filename
                                   &optional
                                   (package :quail))
  (declare (special *user-doc-out-path-function*))
  (let*
    ((ofile-fun (if (functionp *user-doc-out-path-function*)
                  *user-doc-out-path-function*
                  #'mk::doc-auto-path-name))
     (the-package (find-package package))
     (package-use-list (package-use-list package))
     (package-used-by-list (package-used-by-list package))
     symbol-package)
    (with-open-file (ifile filename
                           :direction :input)
      (let ((topic (read ifile nil nil))
            name
            sub-topics)
        (loop while topic do
              (setf symbol-package (symbol-package (first topic)))
              ;;(unless (or (eql the-package symbol-package)
              ;;            (member  symbol-package package-use-list))
              ;;  (format *quail-terminal-io* "~&Topic = ~s" topic)
                (format *quail-terminal-io* "~&name = ~s" (first topic))
                (format *quail-terminal-io* "~&symbol-package = ~s" symbol-package)
               ;; (format *quail-terminal-io* "~&package-use-list = ~s~%~%" package-use-list)
               ;; (import (first topic) the-package))
               ;;(unless (member symbol-package package-used-by-list)
               ;;  (eval-when (:compile-toplevel :load-toplevel :execute) (export (first topic) the-package)))
              (setf name (first topic))
              (setf sub-topics (second topic))
              (setf (doc name :topic)
                    (make-instance
                     'qk::topic-documentation
                     :name (format nil "~a" name)
                     :sub-topics 
                     sub-topics
                     :package (package-name the-package)
                     ))
              (with-open-file (ofile (funcall ofile-fun name :topic)
                                     :direction :output 
                                     :if-exists :append
                                     :if-does-not-exist :create)
                (format ofile "~&(setf (doc '~a :topic)~%" name)
                (write-doc ofile (doc name :topic))
                (format ofile "~&)~%"))
              (setf topic (read ifile nil nil)))))))
        


(defun export-topic-syms (&key (package :quail) symbol-list)
  "Exports from the specified package (default :quail) ~
   either all symbols, or only those appearing in symbol-list, which ~
   have topic documentation attached to them."
  (flet
    ((topic-p (s) (member :topic (documentable-uses s package))))
    (if symbol-list
      (loop
        for sym in symbol-list
        when (topic-p sym)
        do
        (unless (eq (find-package package) (symbol-package sym))
          (import sym package))
        (eval-when (:compile-toplevel :load-toplevel :execute) (export sym package)))
      (do-all-symbols (sym package)
        (when (topic-p sym)
          (when (eq (find-package package) (symbol-package sym))
            (import sym package))
          (eval-when (:compile-toplevel :load-toplevel :execute) (export sym package))))
      ))
  )
