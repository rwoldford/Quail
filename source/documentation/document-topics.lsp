;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          document-topics.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(document-topics)))

(defun document-topics (package)
  "Document-topics interactively produces topic-documentation for the given package.  ~
   All external symbols of the named package are looped over and the user prompted ~
   to assign each to one topic at a time (as named by the user).  ~
   Many passes through the list of symbols and their documentation type are made.  ~
   At each iteration the list is reduced to the list of those symbols which have not ~
   yet been assigned.  ~
   Iteration stops when either the list or the user is exhausted.  ~
   Returns multiple values: either the topmost topic-documentation object or a list ~
   of the topic-documentation objects that were created, ~
   the list of sub-topics that were considered, and the list of sub-topics that ~
   remain unassigned."
  (let (first-doc-obj sub-topics all-topics)
    (multiple-value-setq (first-doc-obj sub-topics)
      (generate-topic  (quail-query "~&Please give a name for the first topic:  ")
                       :package package :prompt? T))
    (let (current-doc-obj
          (remaining-topics
           (set-difference sub-topics (sub-topics first-doc-obj))))
      (setf all-topics
            (append
             (list first-doc-obj)
             (loop for i from 1
                   until
                   (or (not (quail-yes-or-no-p
                             "~&Do you want to assign any of the remaining subtopics to ~
                              a new topic?"))
                       (null remaining-topics))
                   do
                   (setf current-doc-obj
                         (generate-topic
                          (quail-query "~&Please give a name for the next topic:  ")
                          :symbol-list remaining-topics
                          :package package
                          :prompt? T))
                   (setf remaining-topics
                         (set-difference  remaining-topics (sub-topics current-doc-obj)))
                   collect current-doc-obj)))
      (if
        (quail-yes-or-no-p "~&Should all of the topic-documentation objects constructed here ~
                            be stored as sub-topics of some larger topic?")
        
        ;;then
        (let*
          ((top-doc-sym 
            (quail-query "~&Please give a name for this larger topic:  "))
           (top-doc-obj (doc top-doc-sym :topic)))
          (if top-doc-obj
            
            ;;then
            (if (quail-yes-or-no-p
                 "~&A topic documentation of this name (~a) already exists.  ~
                  Should I just add the new topics to it?"
                 top-doc-sym)
              
              ;;then
              (progn
                (loop
                  for st in all-topics
                  do (install-sub-topic top-doc-obj st))
                (values top-doc-obj sub-topics remaining-topics))
              
              ;;else
              (if (quail-yes-or-no-p
                   "~&Should I clear the old topic documentation for ~a and ~
                    install a new one in its place?"
                   top-doc-sym)
                
                ;;then
                (progn
                  (setf top-doc-obj (make-doc top-doc-sym :topic))
                  (setf (doc top-doc-sym :topic) top-doc-obj)
                  (loop
                    for st in all-topics
                    do (install-sub-topic top-doc-obj st))
                  (values top-doc-obj sub-topics remaining-topics))
                
                ;;else
                (values all-topics sub-topics remaining-topics)))
            
            ;;else
            (progn
              (setf top-doc-obj (make-doc top-doc-sym :topic))
              (setf (doc top-doc-sym :topic) top-doc-obj)
              (loop
                for st in all-topics
                do (install-sub-topic top-doc-obj st))
              (values top-doc-obj sub-topics remaining-topics))))
        
        ;;else
        (values all-topics sub-topics remaining-topics))
      )))
                                    
