;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                         generate-topic.lisp                               
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

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(generate-subtopic generate-topic generate-package-topics)))

(defun generate-subtopics (&key package symbol-list types
                            (sorted-topics? T))
  "Constructs a list of list pairs suitable as sub-topics to a topic-documentation ~
   object.  The elements of the list are ~
   determined by the keyword parameters :package, :symbol-list, and :types.  ~
   If no sub-topics are found to be appropriate, then no topic is generated.  ~
   The package specifies the package where the symbols are to be found;  if ~
   missing the symbols are taken where found. Then all documentable uses ~
   of all external symbols of the package are given as sub-topics.  ~
   The keyword :symbol-list is a list of the symbols to appear as sub-topics; ~
   if missing all external symbols of the package argument are used.  ~
   The keyword :types is a list of the ~
   type of symbol usage to be documented (e.g. '(function macro class)); ~
   if missing, all documentable uses of each symbol are used.  ~
   Returns the list of subtopics."
  (let (sub-topics)
    (flet
      ((merge-topic-and-type (topic type-list)
         (if type-list
           (loop
             for type in type-list
             collect (list topic type))))
       (get-types (symbol package type-list)
         (if type-list
           (intersection type-list (documentable-uses symbol package))
           (documentable-uses symbol package))))
      
      (if symbol-list
        (setf sub-topics
              (if package
                (loop
                  for sym in symbol-list
                  nconc
                  (if (null sym)
                    (merge-topic-and-type
                     sym
                     (get-types sym package types))
                    (cond
                     ((and (listp sym)
                           (not (null sym))
                           (setf (first sym )(find-symbol (string (first sym)) package)))
                      (list sym))
                     ((setf sym (find-symbol (string sym) package))
                      (merge-topic-and-type
                       sym
                       (get-types sym package types))))))
                (loop
                  for sym in symbol-list
                  nconc
                  (if (and (listp sym)
                           (not (null sym)))
                    (list sym)
                    (merge-topic-and-type
                     sym
                     (get-types sym package types))))))
        (do-external-symbols (sym package)
          (setf sub-topics
                (nconc (if (and (listp sym)
                                (not (null sym)))
                         (list sym)
                         (merge-topic-and-type
                          sym
                          (get-types sym package types)))
                       sub-topics)))
        ))
    (if sorted-topics?
      (setf sub-topics
            (cl:sort  sub-topics  #'string<
                   :key #'(lambda (x) (string-upcase (car x))))))
    sub-topics)
  )


(defun generate-topic (name &key package symbol-list types (prompt? T)
                            (sorted-topics? T))
  "Constructs a topic documentation called name containing sub-topics as ~
   determined by the keyword parameters :package, :symbol-list, and :types.  ~
   If no sub-topics are found to be appropriate, then no topic is generated.  ~
   The package specifies the package where the symbols are to be found;  if ~
   missing the symbols are taken where found. Then all documentable uses ~
   of all external symbols of the package are given as sub-topics.  ~
   The package of the topic-documentation is package or the current package ~
   if package is not given.  ~
   The keyword :symbol-list is a list of the symbols to appear as sub-topics; ~
   if missing all external symbols of the package argument are used.  ~
   The keyword :types is a list of the ~
   type of symbol usage to be documented (e.g. '(function macro class)); ~
   if missing, all documentable uses of each symbol are used.  ~
   Returns multiple values: the topic-documentation object and the ~
   list of subtopics."
  (declare (special *package*))
  (let (sub-topics result)
    (setf sub-topics
          (generate-subtopics :package package
                              :symbol-list symbol-list
                              :types types
                              :sorted-topics? sorted-topics?))
    (when sub-topics
      (if (and prompt?
               (quail-yes-or-no-p
                "Do you want to iteractively edit the sub-topics to be included?"))
        (setf result (setf (doc name :topic)
                           (make-instance
                            'topic-documentation
                            :name (format nil "~a" name)
                            :sub-topics 
                            (loop for st in sub-topics
                                  when
                                  (quail-y-or-n-p
                                   (format nil "Include this topic: ~s?" st))
                                  collect st)
                            :package (or (and package
                                              (package-name package))
                                         (package-name *package*))
                            )))
        (setf result
              (setf (doc name :topic)
                    (make-instance 'topic-documentation
                                   :name (format nil "~a" name)
                                   :sub-topics sub-topics
                                   :package (or (and package
                                                     (package-name package))
                                                (package-name *package*)))))
        )
      (values result sub-topics))
    ))
  
(defun type-as-group-topic (type)
       (cond
        ((eq type :all)
         '(:global-var :procedure :data-structure :data-set))
        ((eq type :global-var)
         '(:constant :variable :parameter))
        ((eq type :procedure)
         '(:function :macro :generic-function :method :special-form))
        ((eq type :data-structure)
         '(:class :structure :built-in-class))
        (T NIL)))



(defvar *package-topics*
  '(:all 
    :global-var
    :constant :variable :parameter
    :procedure
    :function :macro :generic-function :method :special-form
    :data-structure
    :class :structure :built-in-class
    :data-set)
  "List of keywords that can be used to generate package-topics.")

(defun produce-symbol-for-type (package-name type interned-package)
  (if (eq type :all)
    (intern (string package-name) interned-package)
    (intern
     (concatenate 'string
                  (string package-name)
                  "-"
                  (let ((type-name (string-upcase (string type))))
                    (cond
                     ((or (string-equal type-name "CLASS")
                          (string-equal type-name "BUILT-IN-CLASS"))
                      (concatenate 'string type-name "ES"))
                     (T (concatenate 'string type-name "S")))))
     interned-package)))


(defun produce-doc-capsule-for-type (package type)
  (let ((package-name (package-name package)))
    (if (eq type :all)
      (format nil 
              "The package ~a.  Like all other packages, this is an ~
               internally consistent collection of symbols."
              package-name)
      (format 
       nil
       "The set of ~a in the package ~a."
       (let ((type-name (string-upcase (string type))))
                      (cond
                       ((or (string-equal type-name "CLASS")
                            (string-equal type-name "BUILT-IN-CLASS"))
                        (concatenate 'string type-name "ES"))
                       (T (concatenate 'string type-name "S"))))
       package-name))))


(defun generate-package-topics (package
                                &key
                                (type :all)
                                (interned-package *package*))
  (declare (special *package*))
  
  ;; Error checking
  (if (not (member type *package-topics*))
    (quail-error "Sorry, don't know how to build topic documentation for ~
                  this type: ~s" type))
  
  
  (let* ((package-name (package-name package))
         (symbol (produce-symbol-for-type package-name type interned-package))
         (sub-topics (type-as-group-topic type)))
    (cond
     (sub-topics
      (if (eq type :all)
        (setf (doc symbol :package)
                (make-instance 'package-documentation
                               :name (string symbol)
                               :package (package-name interned-package)
                               :doc-capsule
                               (produce-doc-capsule-for-type package type)
                               :sub-topics
                               (loop
                                 for type in sub-topics
                                 do (generate-package-topics package :type type)
                                 collect
                                 (list
                                  (produce-symbol-for-type
                                   package-name type interned-package)
                                  :topic))
                               ))
        (setf (doc symbol :topic)
              (make-instance 'topic-documentation
                             :name (string symbol)
                             :package (package-name interned-package)
                             :doc-capsule
                             (produce-doc-capsule-for-type package type)
                             :sub-topics
                             (loop
                               for type in sub-topics
                               do (generate-package-topics package :type type)
                               collect
                               (list
                                (produce-symbol-for-type
                                 package-name type interned-package)
                                :topic))
                             ))))
     (T 
      (generate-topic symbol
                      :package package
                      :types (list type)
                      :prompt? NIL)))))
