;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           write-tex-doc.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1991 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------


(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(write-tex-doc)))        

;;;----------------------------------------------------------------------------
;;;
;;;  The generic TeX documentor function:
;;;
;;;        write-tex-doc
;;;
;;;---------------------------------------------------------------------------


(defgeneric write-tex-doc (destination documentation-object)
  (:documentation "Writes TeX commands appropriate for the ~
                   given kind of documentation-object ~
                   to the destination."))

;;;--------------------------------------------------------------------------
;;;
;;;  Topic-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod write-tex-doc (destination (t-d topic-documentation))
  (with-accessors 
    ((name name)
     (package package)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (super-topics super-topics)
     (sub-topics sub-topics))
    t-d
    
    (write-tex-header-box destination 
                          :left name 
                          :right 'Topic)
    
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    
    (if package
      (write-tex-doc-paragraph destination
                               :title "Home Package"
                               :body package))
    
    (if examples
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    (if sub-topics
      (write-tex-titled-alist destination
                              :title "Sub-topics"
                              :items
                              (loop
                                for i from 0 to (- (length sub-topics) 1)
                                collect
                                (let* ((s (elt sub-topics i))
                                       (st (topic s)))
                                  (cons (concatenate 'string
                                                     (string-downcase (name st))
                                                     "   ...   a "
                                                     (string-downcase (string (doc-type st)))
                                                     ".")
                                        (string-first-n... (doc-capsule st)
                                                           200))))))
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body super-topics))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))




;;;--------------------------------------------------------------------------
;;;
;;;  Package-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod write-tex-doc (destination (p-d package-documentation))
  (with-accessors 
    ((name name)
     (doc-capsule doc-capsule)
     (doc-elaboration doc-elaboration)
     (examples examples)
     (references references)
     (see-also see-also)
     (sub-topics sub-topics)
     (super-topics super-topics))
    p-d
    
    (write-tex-header-box destination 
                          :left name 
                          :right 'Package)
    
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    
    (let ((nicknames (nicknames p-d)))
      (if nicknames
        (write-tex-doc-paragraph destination
                                 :title "Nicknames"
                                 :body (string-downcase-list nicknames))))
    
    (let ((packages-used (packages-used p-d)))
      (if packages-used
        (write-tex-doc-paragraph destination
                                 :title (format nil "Packages used by ~a" name)
                                 :body (string-downcase-list packages-used))))
    
    (let ((packages-which-use (packages-which-use p-d)))
      (if packages-which-use
        (write-tex-doc-paragraph destination
                                 :title (format nil "Packages which use ~a" name)
                                 :body (string-downcase-list packages-which-use))))
    
    
    (if examples
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    (if sub-topics
      (write-tex-titled-alist
       destination
       :title "Sub-topics"
       :items
       (loop
         for i from 0 to (- (length sub-topics) 1)
         collect
         (let* ((s (elt sub-topics i))
                (st (topic s)))
           (cons (concatenate 'string
                              (string-downcase (name st))
                              "   ...   a "
                              (string-downcase (string (doc-type st)))
                              ".")
                 (string-first-n... (doc-capsule st)
                                    200))))))
    
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body super-topics))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))




;;;--------------------------------------------------------------------------
;;;
;;;  structure-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod write-tex-doc (destination (s-d structure-documentation))
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
    
    (write-tex-header-box destination 
                          :left name 
                          :right 'Structure)
    
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    
    (if package
      (write-tex-doc-paragraph destination
                               :title "Home Package"
                               :body package))
    
    (if examples
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body super-topics))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))







;;;--------------------------------------------------------------------------
;;;
;;;  Built-in-class-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod write-tex-doc (destination (bic-d built-in-class-documentation))
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
    
    (write-tex-header-box destination 
                          :left name 
                          :right 'Built-in-class)
    
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    
    (if package
      (write-tex-doc-paragraph destination
                               :title "Home Package"
                               :body package))
    
    (if examples
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body super-topics))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))



;;;--------------------------------------------------------------------------
;;;
;;;  Variable-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod write-tex-doc (destination (v-d variable-documentation))
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
    
    (write-tex-header-box destination 
                          :left name 
                          :right 'Variable)
    
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    
    (if value
      (write-tex-doc-paragraph destination
                               :body value
                               :title "Default value"))
    
    (if package
      (write-tex-doc-paragraph destination
                               :title "Home Package"
                               :body package))
    
    (if examples
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body super-topics))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))

;;;--------------------------------------------------------------------------
;;;
;;;  Constant-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod write-tex-doc (destination (c-d constant-documentation))
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
    
    (write-tex-header-box destination 
                          :left name 
                          :right 'Constant)
    
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    
    (if value
      (write-tex-doc-paragraph destination
                               :body value
                               :title "Value"))
    
    (if package
      (write-tex-doc-paragraph destination
                               :title "Home Package"
                               :body package))
    
    (if examples
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body super-topics))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))


;;;--------------------------------------------------------------------------
;;;
;;;  Parameter-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod write-tex-doc (destination (p-d parameter-documentation))
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
    
    (write-tex-header-box destination 
                          :left name 
                          :right 'Parameter)
    
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    
    (if value
      (write-tex-doc-paragraph destination
                               :body value
                               :title "Default value"))
    
    (if package
      (write-tex-doc-paragraph destination
                               :title "Home Package"
                               :body package))
    
    (if examples
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body super-topics))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))


;;;--------------------------------------------------------------------------
;;;
;;;  Function-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod write-tex-doc (destination (f-d function-documentation))
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
    (write-tex-header-box destination 
                          :left name 
                          :right 'Function)
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    (if lam-list
      (write-tex-doc-paragraph destination
                               :title "Lambda List"
                               :body lam-list))
    (if (and arg (arg-info-p arg))
      (write-tex-doc destination arg))
    (if returns
      (write-tex-doc-paragraph destination
                               :title "Returns"
                               :body returns))
    (if side-effects
      (write-tex-doc-paragraph destination
                               :title "Side Effects"
                               :body side-effects))
    (if package
      (write-tex-doc-paragraph destination
                               :title "Home Package"
                               :body package))
    (if examples
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body super-topics))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))



;;;--------------------------------------------------------------------------
;;;
;;;  Generic-function-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod write-tex-doc (destination (f-d generic-function-documentation))
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
    (write-tex-header-box destination 
                          :left name 
                          :right 'Generic-function)
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    (if lam-list
      (write-tex-doc-paragraph destination
                               :title "Lambda List"
                               :body lam-list))
    (if (and arg (arg-info-p arg))
      (write-tex-doc destination arg))
    (if returns
      (write-tex-doc-paragraph destination
                               :title "Returns"
                               :body returns))
    (if side-effects
      (write-tex-doc-paragraph destination
                               :title "Side Effects"
                               :body side-effects))
    (if package
      (write-tex-doc-paragraph destination
                               :title "Home Package"
                               :body package))
    (if examples
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body super-topics))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))



;;;--------------------------------------------------------------------------
;;;
;;;  Macro-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod write-tex-doc (destination (f-d macro-documentation))
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
    (write-tex-header-box destination 
                          :left name 
                          :right 'Macro)
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    (if lam-list
      (write-tex-doc-paragraph destination
                               :title "Lambda List"
                               :body lam-list))
    (if (and arg (arg-info-p arg))
      (write-tex-doc destination arg))
    (if returns
      (write-tex-doc-paragraph destination
                               :title "Returns"
                               :body returns))
    (if side-effects
      (write-tex-doc-paragraph destination
                               :title "Side Effects"
                               :body side-effects))
    (if package
      (write-tex-doc-paragraph destination
                               :title "Home Package"
                               :body package))
    (if examples
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body super-topics))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))




;;;--------------------------------------------------------------------------
;;;
;;;  Special-form-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod write-tex-doc (destination (f-d Special-form-documentation))
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
    (write-tex-header-box destination 
                          :left name 
                          :right 'Special-form)
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    (if lam-list
      (write-tex-doc-paragraph destination
                               :title "Lambda List"
                               :body lam-list))
    (if (and arg (arg-info-p arg))
      (write-tex-doc destination arg))
    (if returns
      (write-tex-doc-paragraph destination
                               :title "Returns"
                               :body returns))
    (if side-effects
      (write-tex-doc-paragraph destination
                               :title "Side Effects"
                               :body side-effects))
    (if package
      (write-tex-doc-paragraph destination
                               :title "Home Package"
                               :body package))
    (if examples
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body super-topics))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))



;;;--------------------------------------------------------------------------
;;;
;;;  Argument-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod write-tex-doc (destination (a-d argument-documentation))
  (with-accessors 
    ((&required &required)
     (&rest &rest)
     (&key &key)
     (&optional &optional)
     (&aux &aux)
     (&allow-other-keys &allow-other-keys))
    
    a-d
    
    (write-tex-doc-paragraph destination :title "Arguments")
    
    (if &required
      (write-tex-arg-element destination
                             :title "Required"
                             :items &required))
    (if &rest
      (write-tex-arg-element destination
                             :title "&rest"
                             :items &rest))
    (if &key
      (write-tex-arg-element destination
                             :title "&key"
                             :items &key))
    (if &optional
      (write-tex-arg-element destination
                             :title "&optional"
                             :items &optional))
    (if &aux
      (write-tex-arg-element destination
                             :title "&aux"
                             :items &aux))
    (if &allow-other-keys
      (write-tex-arg-element destination
                             :title "&allow-other-keys"
                             :items &allow-other-keys))
    ))


;;;--------------------------------------------------------------------------
;;;
;;;  Class-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod write-tex-doc (destination (c-d class-documentation))
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

    (write-tex-header-box destination 
                          :left name 
                          :right 'Class)
    
    (if doc-capsule
      (write-tex-doc-paragraph destination
                               :title "Description"
                               :body doc-capsule))
    
    (if supers
      (write-tex-doc-paragraph
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
        (;;(direct-class-slots    (doc-direct-class-slots slots))
         ;;(direct-instance-slots (doc-direct-instance-slots slots))
         (class-slots           (doc-class-slots slots))
         (instance-slots        (doc-instance-slots slots)))
        (if class-slots
          (write-tex-doc-paragraph
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
          (write-tex-doc-paragraph
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
        #|
        (if direct-class-slots
          (write-tex-doc-paragraph
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
          (write-tex-doc-paragraph
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
        |#
        )
      )
    ;;;; No accessors done yet!!! ... rwo
    (if subs
      (write-tex-doc-paragraph
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
      (write-tex-doc-paragraph
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
      (write-tex-doc-examples destination
                              :title "Examples"
                              :examples examples))
    
    (if package
      (write-tex-doc-paragraph destination
                               :title "Home Package"
                               :body (format nil "~a"
                                             (string-downcase-object package))))
    (if doc-elaboration
      (write-tex-doc-paragraph destination
                               :title "Elaboration"
                               :body doc-elaboration))
    
    (if super-topics
      (write-tex-doc-paragraph destination
                               :title "See Topics"
                               :body 
                               (list-names-of-docs super-topics)))
    
    (if see-also
      (write-tex-doc-paragraph destination
                               :title "See Also"
                               :body see-also))
    
    (if references
      (write-tex-doc-paragraph destination
                               :title "References"
                               :body references))))
