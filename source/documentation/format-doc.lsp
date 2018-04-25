;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            format-doc.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1991 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991, 1992.
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(format-doc)))    

;;;----------------------------------------------------------------------------
;;;
;;;  The generic  online documentor function:
;;;
;;;        format-doc
;;;
;;;---------------------------------------------------------------------------


(defgeneric format-doc (destination documentation-object)
  (:documentation "Writes documentation appropriate for the ~
                   given kind of documentation-object ~
                   to the destination."))

;;;--------------------------------------------------------------------------
;;;
;;;  T
;;;
;;;--------------------------------------------------------------------------

(defmethod format-doc (destination thing)
  (format destination "~a" thing))


(defmethod format-doc :around (destination thing)
  (if (null destination)
    (with-output-to-string (var)
      (call-next-method var thing))
    (call-next-method)))

;;;--------------------------------------------------------------------------
;;;
;;;  Built-in-class-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod format-doc (destination (bic-d built-in-class-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics))
    bic-d
    
    (format-header-box destination 
                       :left name 
                       :right 'Built-in-class)
    
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    
    (if package
      (format-doc-paragraph destination
                            :title "Home Package"
                            :body (format nil "~a"
                                          (string-downcase-object package))))
    
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body
                            (list-names-of-docs super-topics)))
    
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))


;;;--------------------------------------------------------------------------
;;;
;;;  structure-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod format-doc (destination (s-d structure-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics))
    s-d
    
    (format-header-box destination 
                       :left name 
                       :right 'structure)
    
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    
    (if package
      (format-doc-paragraph destination
                            :title "Home Package"
                            :body (format nil "~a"
                                          (string-downcase-object package))))
    
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body
                            (list-names-of-docs super-topics)))
    
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))



;;;--------------------------------------------------------------------------
;;;
;;;  Variable-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod format-doc (destination (v-d variable-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (value value)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics))
    v-d
    
    (format-header-box destination 
                       :left name 
                       :right 'Variable)
    
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    
    (if value
      (format-doc-paragraph destination
                            :body value
                            :title "Last value"))
    
    (if package
      (format-doc-paragraph destination
                            :title "Home Package"
                            :body (format nil "~a"
                                          (string-downcase-object package))))
    
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body 
                            (list-names-of-docs super-topics)))
    
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))

;;;--------------------------------------------------------------------------
;;;
;;;  Constant-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod format-doc (destination (c-d constant-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (value value)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics))
    c-d
    
    (format-header-box destination 
                       :left name 
                       :right 'Constant)
    
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    
    (if value
      (format-doc-paragraph destination
                            :body value
                            :title "Value"))
    
    (if package
      (format-doc-paragraph destination
                            :title "Home Package"
                            :body (format nil "~a"
                                          (string-downcase-object package))))
    
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body 
                            (list-names-of-docs super-topics)))
    
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))


;;;--------------------------------------------------------------------------
;;;
;;;  Parameter-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod format-doc (destination (p-d parameter-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (value value)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics))
    p-d
    
    (format-header-box destination 
                       :left name 
                       :right 'Parameter)
    
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    
    (if value
      (format-doc-paragraph destination
                            :body value
                            :title "Last value"))
    
    (if package
      (format-doc-paragraph destination
                            :title "Home Package"
                            :body (format nil "~a"
                                          (string-downcase-object package))))
    
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body 
                            (list-names-of-docs super-topics)))
    
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))


;;;--------------------------------------------------------------------------
;;;
;;;  Function-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod format-doc (destination (f-d function-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (lam-list lambda-list)
     (arg arguments)
     (returns returns)
     (side-effects side-effects))
    f-d
    (format-header-box destination 
                       :left name 
                       :right 'Function)
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    (if lam-list
      (format-doc-paragraph destination
                            :title "Lambda List"
                            :body lam-list
                            ))
    
    (if (and arg (arg-info-p arg))
      (format-doc destination arg))
    
    (if returns
      (format-doc-paragraph destination
                            :title "Returns"
                            :body returns))
    (if side-effects
      (format-doc-paragraph destination
                            :title "Side Effects"
                            :body side-effects))
    (if package
      (format-doc-paragraph destination
                            :title "Home Package"
                            :body
                            (format nil ":~a"
                                    (string-downcase-object package))))
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body 
                            (list-names-of-docs super-topics)))
    
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))



;;;--------------------------------------------------------------------------
;;;
;;;  Generic-function-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod format-doc (destination (f-d generic-function-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (lam-list lambda-list)
     (arg arguments)
     (returns returns)
     (side-effects side-effects))
    f-d
    (format-header-box destination 
                       :left name 
                       :right 'Generic-function)
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    (if lam-list
      (format-doc-paragraph destination
                            :title "Lambda List"
                            :body lam-list))
    
    (if (and arg (arg-info-p arg))
      (format-doc destination arg))
    
    (if returns
      (format-doc-paragraph destination
                            :title "Returns"
                            :body returns))
    (if side-effects
      (format-doc-paragraph destination
                            :title "Side Effects"
                            :body side-effects))
    (if package
      (format-doc-paragraph destination
                            :title "Home Package"
                            :body (format nil "~a"
                                          (string-downcase-object package))))
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body 
                            (list-names-of-docs super-topics)))
    
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))



;;;--------------------------------------------------------------------------
;;;
;;;  Macro-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod format-doc (destination (f-d macro-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (lam-list lambda-list)
     (arg arguments)
     (returns returns)
     (side-effects side-effects))
    f-d
    (format-header-box destination 
                       :left name 
                       :right 'Macro)
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    (if lam-list
      (format-doc-paragraph destination
                            :title "Lambda List"
                            :body lam-list))
    
    (if (and arg (arg-info-p arg))
      (format-doc destination arg))
    
    (if returns
      (format-doc-paragraph destination
                            :title "Returns"
                            :body returns))
    (if side-effects
      (format-doc-paragraph destination
                            :title "Side Effects"
                            :body side-effects))
    (if package
      (format-doc-paragraph destination
                            :title "Home Package"
                            :body (format nil "~a"
                                          (string-downcase-object package))))
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body 
                            (list-names-of-docs super-topics)))
    
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))





;;;--------------------------------------------------------------------------
;;;
;;;  Special-form-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod format-doc (destination (f-d Special-form-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (lam-list lambda-list)
     (arg arguments)
     (returns returns)
     (side-effects side-effects))
    f-d
    (format-header-box destination 
                       :left name 
                       :right 'Special-form)
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    (if lam-list
      (format-doc-paragraph destination
                            :title "Lambda List"
                            :body lam-list))
    
    (if (and arg (arg-info-p arg))
      (format-doc destination arg))
    
    (if returns
      (format-doc-paragraph destination
                            :title "Returns"
                            :body returns))
    (if side-effects
      (format-doc-paragraph destination
                            :title "Side Effects"
                            :body side-effects))
    (if package
      (format-doc-paragraph destination
                            :title "Home Package"
                            :body (format nil "~a"
                                          (string-downcase-object package))))
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body 
                            (list-names-of-docs super-topics)))
    
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))



;;;--------------------------------------------------------------------------
;;;
;;;  Argument-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod format-doc (destination (a-d argument-documentation))
  (with-accessors 
    ((&required &required)
     (&rest &rest)
     (&key &key)
     (&optional &optional)
     (&aux &aux)
     (&allow-other-keys &allow-other-keys))
    
    a-d
    
    (format-begin-titled-list destination :title "Arguments")
    
    (when &required
      (format-arg-element destination
                          :title "Required"
                          :items &required))
    (when &rest
      (format-arg-element destination
                          :title "&rest"
                          :items &rest))
    (when &key
      (format-arg-element destination
                          :title "&key"
                          :items &key))
    (when &optional
      (format-arg-element destination
                          :title "&optional"
                          :items &optional))
    (when &aux
      (format-arg-element destination
                          :title "&aux"
                          :items &aux))
    (when &allow-other-keys
      (format-arg-element destination
                          :title "&allow-other-keys"
                          :items &allow-other-keys))
    
    (format destination  "~%")))


;;;--------------------------------------------------------------------------
;;;
;;;  Class-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod format-doc (destination (c-d class-documentation))
  (with-accessors 
      ((name                   name)
       (symbol                 doc-symbol)
       (package                package)
       (doc-capsule            doc-capsule)
       (doc-elaboration        doc-elaboration)
       (examples               examples)
       (references             references)
       (see-also               see-also)
       (super-topics           super-topics)
       (class                  doc-class-obect)
       (supers                 doc-super-classes)
       (class-precedence-list  doc-class-precedence-list )
       (subs                   doc-sub-classes)
       (slots                  doc-slots)
       (accessors              doc-accessors)
       )
      c-d
    
    (format-header-box destination 
                       :left name 
                       :right 'Class)
    
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    
    (if supers
      (format-doc-paragraph
       destination
       :title "Supers"
       :body
       (string-downcase 
        (cond
         ((= (length supers) 1)
          (format NIL "(~a)"
                  (class-name (first supers))))
         (T
          (format NIL "(~a~{   ~a~})"
                  (class-name (first supers))
                  (mapcar #'class-name (rest supers))))))))
    (if slots
      (let*
        ((direct-class-slots    (doc-direct-class-slots slots))
         (direct-instance-slots (doc-direct-instance-slots slots))
         (class-slots           (doc-class-slots slots))
         (instance-slots        (doc-instance-slots slots)))
        (if class-slots
          (format-doc-paragraph
           destination
           :title "Class slots"
           :body
           (string-downcase 
            (cond
             ((= (length class-slots) 1)
              (format NIL "~a "
                      (slot-definition-name (first class-slots))))
             (T
              (format NIL "~{~&~a~}"
                      (mapcar #'slot-definition-name
                              class-slots))))))
          )
        (if instance-slots
          (format-doc-paragraph
           destination
           :title "Instance slots"
           :body
           (string-downcase 
            (cond
             ((= (length instance-slots) 1)
              (format NIL "~a "
                      (slot-definition-name (first instance-slots))))
             (T
              (format NIL "~{~&~a~}"
                      (mapcar #'slot-definition-name
                              instance-slots))))))
          )
        (if direct-class-slots
          (format-doc-paragraph
           destination
           :title "Directly defined class slots"
           :body
           (string-downcase 
            (cond
             ((= (length direct-class-slots) 1)
              (format NIL "~a "
                      (slot-definition-name (first direct-class-slots))))
             (T
              (format NIL "~{~&~a~}"
                      (mapcar #'slot-definition-name
                              direct-class-slots))))))
          )
        (if direct-instance-slots
          (format-doc-paragraph
           destination
           :title "Directly defined instance slots"
           :body
           (string-downcase 
            (cond
             ((= (length direct-instance-slots) 1)
              (format NIL "~a "
                      (slot-definition-name (first direct-instance-slots))))
             (T
              (format NIL "~{~&~a~}"
                      (mapcar #'slot-definition-name
                              direct-instance-slots))))))
          )
        
        )
      )
     ;;;; No accessors done yet!!! ... rwo
    (if subs
      (format-doc-paragraph
       destination
       :title "Sub-Classes"
       :body
       (string-downcase 
        (cond
         ((= (length subs) 1)
          (format NIL "(~a)"
                  (class-name (first subs))))
         (T
          (format NIL "(~a~{   ~a~})"
                  (class-name (first subs))
                  (mapcar #'class-name (rest subs)))))))
      )
    
    (if class-precedence-list
      (format-doc-paragraph
       destination
       :title "Class precedence list"
       :body
       (string-downcase 
        (cond
         ((= (length class-precedence-list) 1)
          (format NIL "(~a)"
                  (class-name (first class-precedence-list))))
         (T
          (format NIL "(~a~{   ~a~})"
                  (class-name (first class-precedence-list))
                  (mapcar #'class-name (rest class-precedence-list))))))
       )
      )
    
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    
    (if package
      (format-doc-paragraph destination
                            :title "Home Package"
                            :body (format nil "~a"
                                          (string-downcase-object package))))
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body 
                            (list-names-of-docs super-topics)))
    
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))

;;;---------------------------------------------------------------
;;;
;;;  Slot-documentation
;;;
;;;---------------------------------------------------------------
#|
(defmethod format-doc (destination (s-d slot-documentation))
  (with-accessors 
    ((name name)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (initform slot-initform)
     (initarg slot-initargs)
     (reader slot-readers)
     (writer slot-writers)
     (type slot-type)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics))
    
    s-d
    
    (format destination "~a" (string-downcase-object name))
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    (if initarg
      (format-doc-paragraph destination
                            :title "Initarg"
                            :body initarg))
    (if initform
      (format-doc-paragraph destination
                            :title "Initform"
                            :body initform))
    (if type
      (format-doc-paragraph destination
                            :title "Type"
                            :body type))
    (if reader
      (format-doc-paragraph destination
                            :title "Reader"
                            :body reader))
    
    (if writer
      (format-doc-paragraph destination
                            :title "Writer"
                            :body writer))
    
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body 
                            (list-names-of-docs super-topics)))
    
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))
    
    (format destination "~%")))

|#

;;;--------------------------------------------------------------------------
;;;
;;;  Topic-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod format-doc (destination (t-d topic-documentation))
  (with-accessors 
    ((name name)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (sub-topics sub-topics)
     (super-topics super-topics))
    t-d
    (format-header-box destination 
                       :left name 
                       :right 'Topic)
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    (if sub-topics
      (format-titled-alist destination
                           :title "Sub-topics"
                           :items
                           (loop
                             for i from 0 to (- (length sub-topics) 1)
                             collect
                             (let* ((s (elt sub-topics i))
                                    (st (topic s)))
                               (if (not (eq s st))
                                 (setf (elt sub-topics i) st))
                               (cons 
                                (string-downcase (name st))
                                (let ((short-doc
                                       (string-capitalize 
                                        (string-first-n... (doc-capsule st))
                                        :start 0 :end 1)))
                                  (if short-doc
                                    (format nil short-doc))))))))
    
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body 
                            (list-names-of-docs super-topics)))
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))


;;;--------------------------------------------------------------------------
;;;
;;;  Package-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod format-doc (destination (p-d package-documentation))
  (with-accessors 
    ((name name)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (sub-topics sub-topics))
    p-d
    (format-header-box destination 
                       :left name 
                       :right 'Package)
    (if doc-capsule
      (format-doc-paragraph destination
                            :title "Description"
                            :body doc-capsule))
    
    (let ((nicknames (nicknames p-d)))
      (if nicknames
        (format-doc-paragraph destination
                              :title "Nicknames"
                              :body (string-downcase-list nicknames))))
    
    (let ((packages-used (packages-used p-d)))
      (if packages-used
        (format-doc-paragraph destination
                              :title (format nil "Packages used by ~a" name)
                              :body (string-downcase-list packages-used))))
    
    (let ((packages-which-use (packages-which-use p-d)))
      (if packages-which-use
        (format-doc-paragraph destination
                              :title (format nil "Packages which use ~a" name)
                              :body (string-downcase-list packages-which-use))))
    
    (if examples
      (format-doc-examples destination
                           :title "Examples"
                           :examples examples))
    (if doc-elaboration
      (format-doc-paragraph destination
                            :title "Elaboration"
                            :body doc-elaboration))
    
    (if sub-topics
      (format-titled-alist destination
                           :title "Sub-topics"
                           :items
                           (loop
                             for i from 0 to (- (length sub-topics) 1)
                             collect
                             (let* ((s (elt sub-topics i))
                                    (st (topic s)))
                               (if (not (eq s st))
                                 (setf (elt sub-topics i) st))
                               (cons 
                                (string-downcase (name st))
                                (let ((short-doc
                                       (string-capitalize
                                        (string-first-n... (doc-capsule st))
                                        :start 0 :end 1)))
                                  (if short-doc
                                    (format nil short-doc))))))))
    
    
    (if super-topics
      (format-doc-paragraph destination
                            :title "See Topics"
                            :body 
                            (list-names-of-docs super-topics)))
    (if see-also
      (format-doc-paragraph destination
                            :title "See Also"
                            :body see-also))
    (if references
      (format-doc-paragraph destination
                            :title "References"
                            :body references))))
