;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              help-sub-views.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     C.B. Hurley 1992 George Washington University
;;;     R.W. Oldford 1992+
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(help-sub-views)))

(defgeneric help-sub-views (documentation-object &key type)
  (:documentation "Returns a list of views containing the information ~%
                   in the documentation object. ~%
                   Type may be used to determine the type of subviews to be ~%
                   constructed.  Typically it is ignored."))

;;;--------------------------------------------------------------------------
;;;
;;;  T
;;;
;;;--------------------------------------------------------------------------



(defmethod help-sub-views ((thing T) &key type)
  (let ((views NIL))
    (push
     (header-box-view :left (format  NIL "~s" thing)
                      :right (or type 'Unknown))
     views)
    (nconc views
           (make-view-paragraph :title "Description"
                                :body (format NIL "No help is available on ~s." thing)
                                )))
  
  )

;;;--------------------------------------------------------------------------
;;;
;;;  Built-in-class-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod help-sub-views ((bic-d qk::built-in-class-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    
    (with-accessors 
      ((name qk::name)
       (package qk::package)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (super-topics qk::super-topics))
      bic-d
      
      (push
       (header-box-view
        :left name 
        :right 'Built-in-class)
       views)
      
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      (if package
        (nconc views
               (make-view-paragraph 
                :title "Home Package"
                :body (format nil "~a" (string-downcase-object package)))))
      
      
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)
               ))
      
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references)))
      views)))


;;;--------------------------------------------------------------------------
;;;
;;;  structure-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod help-sub-views ((s-d qk::structure-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    
    (with-accessors 
      ((name qk::name)
       (package qk::package)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (super-topics qk::super-topics))
      s-d
      
      (push
       (header-box-view 
        :left name 
        :right 'structure)
       views)
      
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      (if package
        (nconc views
               (make-view-paragraph 
                :title "Home Package"
                :body (format nil "~a" (string-downcase-object package)))))
      
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)))
      
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references))))
    views))



;;;--------------------------------------------------------------------------
;;;
;;;  Variable-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod help-sub-views ((v-d qk::variable-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    
    (with-accessors 
      ((name qk::name)
       (package qk::package)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (value qk::value)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (super-topics qk::super-topics))
      v-d
      
      (push
       (header-box-view :left name :right 'Variable)
       views)
      
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      (if value
        (nconc views
               (make-view-paragraph 
                :body value
                :title "Last value")))
      
      (if package
        (nconc views
               (make-view-paragraph 
                :title "Home Package"
                :body (format nil "~a" (string-downcase-object package)))))
      
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)))
      
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references))))
    views))

;;;--------------------------------------------------------------------------
;;;
;;;  Constant-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod help-sub-views ((c-d qk::constant-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    
    (with-accessors 
      ((name qk::name)
       (package qk::package)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (value qk::value)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (super-topics qk::super-topics))
      c-d
      
      (push
       (header-box-view :left name :right 'Constant)
       views)
      
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      (if value
        (nconc views
               (make-view-paragraph 
                :body value
                :title "Value")))
      
      (if package
        (nconc views
               (make-view-paragraph 
                :title "Home Package"
                :body (format nil "~a" (string-downcase-object package)))))
      
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)))
      
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references))))
    views))


;;;--------------------------------------------------------------------------
;;;
;;;  Parameter-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod help-sub-views ((p-d qk::parameter-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    
    (with-accessors 
      ((name qk::name)
       (package qk::package)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (value qk::value)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (super-topics qk::super-topics))
      p-d
      
      (push
       (header-box-view 
        :left name 
        :right 'Parameter)
       views)
      
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      (if value
        (nconc views
               (make-view-paragraph 
                :body value
                :title "Last value")))
      
      (if package
        (nconc views
               (make-view-paragraph 
                :title "Home Package"
                :body (format nil "~a" (string-downcase-object package)))))
      
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)))
      
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references))))
    views))


;;;--------------------------------------------------------------------------
;;;
;;;  Function-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod help-sub-views ((self qk::function-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    (with-accessors 
      ((name qk::name)
       (package qk::package)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (super-topics qk::super-topics)
       (lam-list qk::lambda-list)
       (arg qk::arguments)
       (returns qk::returns)
       (side-effects qk::side-effects))
      self
      (push
       (header-box-view :left name 
                        :right 'Function)
       views)
      (if lam-list
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a~{     ~a~}"
                              (string-downcase name)
                              (string-downcase-list lam-list))))
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a"
                              (string-downcase name)
                              ))))
      (if doc-capsule
        (nconc views
               (make-view-paragraph :title "Description"
                                    :body doc-capsule)))
      (if (and arg (qk::arg-info-p arg))
          (nconc views
              (help-sub-views arg)))
      
      (if returns
        (nconc views
               (make-view-paragraph  :title "Returns"
                                     :body returns)))
      (if side-effects
        (nconc views
               (make-view-paragraph  :title "Side Effects"
                                     :body side-effects)))
      (if package
        (nconc views
               (make-view-paragraph  :title "Home Package"
                                     :body
                                     (format nil "~a" (string-downcase-object package)))))
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      (if doc-elaboration
        (nconc views
               (make-view-paragraph  :title "Elaboration"
                                     :body doc-elaboration)))
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "See topics"
                :items super-topics)))
      
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)))
      (if references
        (nconc views
               (make-view-paragraph  :title "References"
                                     :body references))))
    views))



;;;--------------------------------------------------------------------------
;;;
;;;  Generic-function-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod help-sub-views ((f-d qk::generic-function-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    (with-accessors 
      ((name                  qk::name)
       (package               qk::package)
       (symbol                qk::doc-symbol)
       (doc-capsule           qk::doc-capsule)
       (doc-elaboration       qk::doc-elaboration)
       (examples              qk::examples)
       (references            qk::references)
       (see-also              qk::see-also)
       (super-topics          qk::super-topics)
       (lam-list              qk::lambda-list)
       (arg                   qk::arguments)
       (returns               qk::returns)
       (side-effects          qk::side-effects)
       (methods               qk::method-list)
       )
      f-d
      (push
       (header-box-view 
        :left name 
        :right 'Generic-function)
       views)
      
      (if lam-list
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a~{     ~a~}"
                              (string-downcase name)
                              (string-downcase-list lam-list))))
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a"
                              (string-downcase name)
                              ))))
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      (if (and arg (qk::arg-info-p arg))
          (nconc views
              (help-sub-views arg)))
      
      (if returns
        (nconc views
               (make-view-paragraph 
                :title "Returns"
                :body returns)))
      (if side-effects
        (nconc views
               (make-view-paragraph 
                :title "Side Effects"
                :body side-effects)))

      (if methods
        (nconc views
               (make-methods-display-list
                :symbol symbol
                :title "Methods"
                :methods methods
                :group? T)))
      
      (if package
        (nconc views
               (make-view-paragraph 
                :title "Home Package"
                :body (format nil "~a" (string-downcase-object package)))))
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)))
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references)))
      views)))



;;;--------------------------------------------------------------------------
;;;
;;;  Macro-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod help-sub-views ((f-d qk::macro-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    
    (with-accessors 
      ((name qk::name)
       (package qk::package)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (super-topics qk::super-topics)
       (lam-list qk::lambda-list)
       (arg qk::arguments)
       (returns qk::returns)
       (side-effects qk::side-effects))
      f-d
      (push (header-box-view :left name :right 'Macro) views)
      
      (if lam-list
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a~{     ~a~}"
                              (string-downcase name)
                              (string-downcase-list lam-list))))
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a"
                              (string-downcase name)
                              ))))
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      (if (and arg (qk::arg-info-p arg))
          (nconc views
              (help-sub-views arg)))
      
      (if returns
        (nconc views
               (make-view-paragraph 
                :title "Returns"
                :body returns)))
      (if side-effects
        (nconc views
               (make-view-paragraph 
                :title "Side Effects"
                :body side-effects)))
      (if package
        (nconc views
               (make-view-paragraph 
                :title "Home Package"
                :body (format nil "~a" (string-downcase-object package)))))
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)))
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references)))
      views)))





;;;--------------------------------------------------------------------------
;;;
;;;  Special-form-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod help-sub-views ((f-d qk::Special-form-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    
    (with-accessors 
      ((name qk::name)
       (package qk::package)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (super-topics qk::super-topics)
       (lam-list qk::lambda-list)
       (arg qk::arguments)
       (returns qk::returns)
       (side-effects qk::side-effects))
      f-d
      (push (header-box-view :left name :right 'Special-form) views)

      (if lam-list
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a~{     ~a~}"
                              (string-downcase name)
                              (string-downcase-list lam-list))))
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a"
                              (string-downcase name)
                              ))))
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      (if (and arg (qk::arg-info-p arg))
          (nconc views
              (help-sub-views arg)))
      (if returns
        (nconc views
               (make-view-paragraph 
                :title "Returns"
                :body returns)))
      (if side-effects
        (nconc views
               (make-view-paragraph 
                :title "Side Effects"
                :body side-effects)))
      (if package
        (nconc views
               (make-view-paragraph 
                :title "Home Package"
                :body (format nil "~a" (string-downcase-object package)))))
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)))
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references)))
      views)))



;;;--------------------------------------------------------------------------
;;;
;;;  Argument-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod help-sub-views ((a-d qk::argument-documentation) &key type)
  (declare (ignore type))
  (with-accessors 
    ((required-args qk::required-args)
     (rest-args qk::rest-args)
     (key-args qk::key-args)
     (optional-args qk::optional-args)
     (aux-args qk::aux-args)
     (body-args qk::body-args)
     (allow-other-keys-args qk::allow-other-keys-args))
    
    a-d
    (let ((body "None."))
      
      (when (or required-args rest-args key-args body-args
              optional-args aux-args allow-other-keys-args)
        (setf body
              (with-output-to-string (destination)
                (when required-args
                  (qk::format-arg-element destination
                                          :line-size 1000
                                          :title "Required"
                                          :items required-args))
                (when rest-args
                  (qk::format-arg-element destination
                                          :line-size 1000
                                          :title "Rest"
                                          :items rest-args))
                (when key-args
                  (qk::format-arg-element destination
                                          :line-size 1000
                                          :title "Key"
                                          :items key-args))
                (when optional-args
                  (qk::format-arg-element destination
                                          :line-size 1000
                                          :title "Optional"
                                          :items optional-args))
                (when aux-args
                  (qk::format-arg-element destination
                                          :line-size 1000
                                          :title "Auxilliary"
                                          :items aux-args))
                (when body-args
                  (qk::format-arg-element destination
                                          :line-size 1000 ;27NOV2024
                                          :title "Body"
                                          :items body-args))
                (when allow-other-keys-args
                  (qk::format-arg-element destination
                                          :line-size 1000
                                          :title "allow-other-keys"
                                          :items allow-other-keys-args)))))
      (make-view-paragraph :title "Arguments" :body body)
      )))
  


;;;--------------------------------------------------------------------------
;;;
;;;  Class-documentation
;;;
;;;--------------------------------------------------------------------------


(defmethod help-sub-views ((c-d qk::class-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    
    (with-accessors 
      ((name                   qk::name)
       (symbol                 qk::doc-symbol)
       (package                qk::package)
       (doc-capsule            qk::doc-capsule)
       (doc-elaboration        qk::doc-elaboration)
       (examples               qk::examples)
       (references             qk::references)
       (see-also               qk::see-also)
       (super-topics           qk::super-topics)
       (class                  qk::doc-class-object)
       (supers                 qk::doc-super-classes)
       (class-precedence-list  qk::doc-class-precedence-list )
       (subs                   qk::doc-sub-classes)
       (slots                  qk::doc-slots)
       (accessors              qk::doc-accessors)
       )
      c-d
      
      (push (header-box-view :left name :right 'Class) views)
      
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      (when supers
        (nconc views
               (make-view-paragraph 
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
        (nconc views
               (make-titled-display-list
                :title ""
                :items supers
                :item-display-fn
                #'(lambda (x) (string-downcase (class-name x)))
                :item-action-fn
                #'(lambda (x)
                    (help (class-name x) :class))
                )
               )
        )
      
      
      (when slots
        (nconc
         views
         (make-titled-display-list
          :title "Slots"
          :items
          (list
           (list "Class slots" slots :class-slots)
           (list "Instance slots" slots :instance-slots)
           (list "Directly defined class slots" slots :direct-class-slots)
           (list "Directly defined instance slots" slots :direct-instance-slots)
           )
          :item-display-fn
          #'(lambda (x)
              (format NIL " ~a" (first x)))
          :item-action-fn
          #'(lambda (x)
              (apply #'help (rest x))
              )
          )
         )
        )
      
      (when accessors
        (nconc views
               (make-accessors-display-list
                class
                :title "Accessors"
                :accessors (cons
                            (cons "All types" accessors)
                            accessors)
                :ungroup? NIL)))
      
      (when subs
        (nconc views
               (make-titled-display-list
                :title "Sub-Classes"
                :items subs
                :item-display-fn
                #'(lambda (x) (string-downcase (class-name x)))
                :item-action-fn
                #'(lambda (x)
                    (help (class-name x) :class))
                )
               )
        )

      (when class-precedence-list
        (nconc views
               (make-titled-display-list
                :title "Class Precedence"
                :items class-precedence-list
                :item-display-fn
                #'(lambda (x) (string-downcase (class-name x)))
                :item-action-fn
                #'(lambda (x)
                    (help (class-name x) :class))
                )
               )
        )
      
      
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
               ))
      
      (if package
        (nconc views
               (make-view-paragraph 
                :title "Home Package"
                :body (string-downcase-object package))))
      
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)))
      
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references)))
      views)))

;;;--------------------------------------------------------------------------
;;;
;;;  Accessor-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod help-sub-views ((a-d qk::accessor-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    (with-accessors 
      ((name              qk::name)
       (class             qk::doc-class-object)
       (symbol            qk::doc-symbol)
       (doc-capsule       qk::doc-capsule)
       (accessor          qk::doc-accessor)
       (accessor-type     qk::doc-accessor-type)
       (see-also          qk::see-also)
       )
      a-d
      (push
       (header-box-view 
        :left name 
        :right (if accessor-type
                 accessor-type
                 'Accessor))
       views)
      
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      
      
      
        (let ((add-to-see-also
               (list (list symbol :generic-function)
                     (list (class-name class) :class))))
          (nconc views
                 (if see-also
                   (vw::make-titled-display-list 
                    :title "See Also"
                    :items (append add-to-see-also see-also))
                   (vw::make-titled-display-list 
                    :title "See Also"
                    :items add-to-see-also)))
          )
      
      views)))


;;;--------------------------------------------------------------------------
;;;
;;;  Accessors-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod help-sub-views ((a-d qk::accessors-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    (with-accessors 
      ((name qk::name)
       (class qk::doc-class-object)
       (symbol qk::doc-symbol)
       (doc-capsule qk::doc-capsule)
       (accessors qk::doc-accessors)
       )
      a-d
      (push
       (header-box-view 
        :left name 
        :right 'Accessors)
       views)
      
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      
      (if accessors
        (let (title)
          (loop for alist in accessors
                do 
                (setf title (car alist))
                (setf title
                      (cond
                       ((stringp title) title)
                       ((symbolp title) (string-capitalize (string title)))
                       (T (string-capitalize
                           (format NIL "~s" (car alist))))
                       )
                      )
                (nconc views
                       (make-accessors-display-list
                        class
                        :title  title
                        :accessors (cdr alist)
                        :ungroup? T)
                       )))
        )
      
      views)))


;;;---------------------------------------------------------------
;;;
;;;  Slots-documentation
;;;
;;;---------------------------------------------------------------

(defmethod help-sub-views ((ss-d qk::slots-documentation) &key (type :all))
  (let ((views NIL))
    (with-accessors 
      ((name qk::name)
       (symbol qk::doc-symbol)
       (class qk::doc-class-object)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (super-topics qk::super-topics)
       (direct-class-slots qk::doc-direct-class-slots)
       (direct-instance-slots qk::doc-direct-instance-slots)
       (class-slots qk::doc-class-slots)
       (instance-slots qk::doc-instance-slots))
      
      ss-d
      
      
      (push (header-box-view :left name :right 'Slots)
            views)

      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body
                (concatenate
                 'string
                 (format NIL
                         "The collection of ~a for the class: ~a.~%"
                         (if (eq type :all)
                           "all slots"
                           (string-downcase
                            (format NIL "~a" type)))
                         (string-downcase
                          (format NIL "~s" (class-name class))))
                 doc-capsule))
               )
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body
                (format NIL
                         "The collection of ~a for the class: ~a."
                         (if (eq type :all)
                           "all slots"
                           (string-downcase
                            (format NIL "~a" type)))
                         (string-downcase
                          (format NIL "~s" (class-name class))))
                )))
      (let (display-list)
        (case type
          (:all
           (setf display-list
                 (make-titled-display-list
                  :title "Slots"
                  :items
                  (list
                   (list "Class slots" ss-d :class-slots)
                   (list "Instance slots" ss-d :instance-slots)
                   (list "Directly defined class slots" ss-d :direct-class-slots)
                   (list "Directly defined instance slots" ss-d :direct-instance-slots)
                   )
                  :item-display-fn
                  #'(lambda (x)
                      (format NIL " ~a" (first x)))
                  :item-action-fn
                  #'(lambda (x)
                      (apply #'help (rest x))
                      )
                  )
                 )
           )
          (:class-slots
           (setf
            display-list
            (append
             (if class-slots
               (make-titled-display-list
                :title "Class slots"
                :items
                class-slots
                :item-display-fn
                #'(lambda (x)
                    (format NIL " ~a" (qk::slot-definition-name x)))
                :item-action-fn
                #'(lambda (x)
                    (help 
                     (make-instance 'qk::slot-documentation :slot x :class class))
                    )
                )
               (make-view-paragraph
                :title "Class slots"
                :body (format NIL "No class slots are defined for ~%
                                   the class ~a."
                              (string-downcase (class-name class)))
                )
               )
             (make-titled-display-list
              :title "Other slots"
              :items
              (list
               (list "Instance slots" ss-d :instance-slots)
               (list "Directly defined class slots" ss-d :direct-class-slots)
               (list "Directly defined instance slots" ss-d :direct-instance-slots)
               )
              :item-display-fn
              #'(lambda (x) (format NIL " ~a" (first x)))
              :item-action-fn
              #'(lambda (x) (apply #'help (rest x)))
              )
             )
            )
           )
          (:instance-slots
           (setf
            display-list
            (append
             (if instance-slots
               (make-titled-display-list
                :title "Instance slots"
                :items
                instance-slots
                :item-display-fn
                #'(lambda (x)
                    (format NIL " ~a" (qk::slot-definition-name x)))
                :item-action-fn
                #'(lambda (x)
                    (help 
                     (make-instance 'qk::slot-documentation :slot x :class class))
                    )
                )
               (make-view-paragraph
                :title "Instance slots"
                :body (format NIL "No instance slots are defined for ~%
                                   the class ~a."
                              (string-downcase
                               (format NIL "~s"
                                       (class-name class))))
                )
               )
             (make-titled-display-list
              :title "Other slots"
              :items
              (list
               (list "Class slots" ss-d :class-slots)
               (list "Directly defined class slots" ss-d :direct-class-slots)
               (list "Directly defined instance slots" ss-d :direct-instance-slots)
               )
              :item-display-fn
              #'(lambda (x) (format NIL " ~a" (first x)))
              :item-action-fn
              #'(lambda (x) (apply #'help (rest x)))
              )
             )
            )
           )
          (:direct-class-slots
           (setf
            display-list
            (append
             (if direct-class-slots
               (make-titled-display-list
                :title "Direct class slots"
                :items
                direct-class-slots
                :item-display-fn
                #'(lambda (x)
                    (format NIL " ~a" (qk::slot-definition-name x)))
                :item-action-fn
                #'(lambda (x)
                    (help 
                     (make-instance 'qk::slot-documentation :slot x :class class))
                    )
                )
               (make-view-paragraph
                :title "Direct class slots"
                :body (format NIL "No class slots are defined directly on ~%
                                   the class ~a."
                              (string-downcase
                               (format NIL "s"
                                       (class-name class))))
                )
               )
             (make-titled-display-list
              :title "Other slots"
              :items
              (list
               (list "Class slots" ss-d :class-slots)
               (list "Instance slots" ss-d :instance-slots)
               (list "Directly defined instance slots" ss-d :direct-instance-slots)
               )
              :item-display-fn
              #'(lambda (x) (format NIL " ~a" (first x)))
              :item-action-fn
              #'(lambda (x) (apply #'help (rest x)))
              )
             )
            )
           )
          (:direct-instance-slots
           (setf
            display-list
            (append
             (if direct-instance-slots
               (make-titled-display-list
                :title "Direct instance slots"
                :items
                direct-instance-slots
                :item-display-fn
                #'(lambda (x)
                    (format NIL " ~a" (qk::slot-definition-name x)))
                :item-action-fn
                #'(lambda (x)
                    (help 
                     (make-instance 'qk::slot-documentation :slot x :class class))
                    )
                )
               (make-view-paragraph
                :title "Direct instance slots"
                :body (format NIL "No instance slots are defined directly on ~%
                                   the class ~a."
                              (string-downcase
                               (format NIL "s"
                                       (class-name class))))
                )
               )
             (make-titled-display-list
              :title "Other slots"
              :items
              (list
               (list "Class slots" ss-d :class-slots)
               (list "Instance slots" ss-d :instance-slots)
               (list "Directly defined class slots" ss-d :direct-class-slots)
               )
              :item-display-fn
              #'(lambda (x) (format NIL " ~a" (first x)))
              :item-action-fn
              #'(lambda (x) (apply #'help (rest x)))
              )
             )
            )
           )
          )
        (nconc views display-list)
        )
        
      
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
               ))
      
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)))
      
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references)))
      views)))


;;;---------------------------------------------------------------
;;;
;;;  Slot-documentation
;;;
;;;---------------------------------------------------------------

(defmethod help-sub-views ((s-d qk::slot-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    
    (with-accessors 
      ((name             qk::name)
       (class            qk::doc-class-object)
       (slot             qk::doc-slot-descriptor)
       (doc-capsule      qk::doc-capsule)
       (doc-elaboration  qk::doc-elaboration)
       (examples         qk::examples)
       (references       qk::references)
       (see-also         qk::see-also)
       (super-topics     qk::super-topics))
      
      s-d
      
      ;;(if (or (null name)
      ;;        (string-equal (string-upcase name)
      ;;                      "NIL"))
       ;; (setf name (qk::slot-definition-name slot)))
      (push (header-box-view :left name :right 'Slot)
            views)

      (let 
        ((allocation      (qk::slot-definition-allocation slot))
         (initform        (qk::slot-definition-initform slot))
         (initargs        (qk::slot-definition-initargs slot))
         (initfunction    (qk::slot-definition-initfunction slot))
         (readers         (qk::slot-definition-readers slot))
         (writers         (qk::slot-definition-writers slot))
         (slot-type       (qk::slot-definition-type slot))
         (defined-class   (qk::slot-definition-type slot))
         )
        
        (unless doc-capsule
          (setf doc-capsule (documentation slot doc-type))) ;27NOV2024
        (if doc-capsule
          (nconc views
                 (make-view-paragraph 
                  :title "Description"
                  :body doc-capsule))
          )
        
        (if defined-class
          (nconc views
                 (make-view-paragraph 
                  :title "Defined on class"
                  :body defined-class)))
        
        (if class
          (nconc views
                 (make-view-paragraph 
                  :title "Class"
                  :body (string-downcase (format NIL "~s" (class-name class))))))
        
        (if allocation
          (nconc views
                 (make-view-paragraph 
                  :title "Allocation"
                  :body allocation)))
        
        (if initargs
          (nconc views
                 (if (listp initargs)
                   (make-view-paragraph 
                    :title (format NIL "Initarg~P" (length initargs))
                    :body (format NIL "~{~s ~}" initargs))
                   (make-view-paragraph 
                    :title "Initarg"
                    :body (format NIL "~s" initargs))
                   )
                 ))
        (if initform
          (nconc views
                 (make-view-paragraph 
                  :title "Initform"
                  :body initform
                  )))
        
        (if initfunction
          (nconc views
                 (make-view-paragraph 
                  :title "Initfunction"
                  :body initfunction)))
        (if slot-type
          (nconc views
                 (make-view-paragraph 
                  :title "Type"
                  :body slot-type)))
        (if readers
          (nconc views
                 (make-view-paragraph 
                  :title (format NIL "Reader~P" (length readers))
                  :body (format NIL "~{~&~s ~}" readers))
                 )
          )
        
        (if writers
          (nconc views
                 (make-view-paragraph 
                  :title (format NIL "Writer~P" (length writers))
                  :body (format NIL "~{~&~s ~}" writers))
                 ))
        
        (if doc-elaboration
          (nconc views
                 (make-view-paragraph 
                  :title "Elaboration"
                  :body doc-elaboration)))
        
        (if examples
          (nconc views
                 (make-examples-display-list
                  :title "Examples"
                  :examples examples)
                 ))
        
        (if super-topics
          (nconc views
                 (vw::make-titled-display-list 
                  :title "Super-topics"
                  :items super-topics)))
        
        (if see-also
          (nconc views
                 (vw::make-titled-display-list 
                  :title "See Also"
                  :items see-also)))
        
        (if references
          (nconc views
                 (make-view-paragraph 
                  :title "References"
                  :body references)))
        views)))
  )



;;;--------------------------------------------------------------------------
;;;
;;;  Topic-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod help-sub-views ((self qk::topic-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    
    (with-accessors 
      ((name qk::name)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (sub-topics qk::sub-topics)
       (super-topics qk::super-topics))
      self
      
      (push
       (header-box-view :left name 
                        :right 'Topic)
       views)
      
      (if doc-capsule
        (nconc views
               (make-view-paragraph :title "Description"
                                    :body doc-capsule)))
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      (if doc-elaboration
        (nconc views
               (vw::make-view-paragraph
                :title "Elaboration"
                :body doc-elaboration)))
      (if sub-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Sub-topics"
                :items sub-topics)))
      
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      (if see-also
        (nconc views
               (vw::make-titled-display-list
                :title "See Also" :items see-also)))
      (if references
        (nconc views
               (make-view-paragraph :title "References"
                                    :body references))))
    
    views))


;;;--------------------------------------------------------------------------
;;;
;;;  Package-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod help-sub-views ((p-d qk::package-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    
    (with-accessors 
      ((name qk::name)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (super-topics qk::super-topics)
       (sub-topics qk::sub-topics))
      p-d
      (push (header-box-view :left name :right 'Package) views)
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      (let ((nicknames (qk::nicknames p-d)))
        (if nicknames
          (nconc views
                 (make-view-paragraph 
                  :title "Nicknames"
                  :body (string-downcase-list nicknames)))))
      
      (let ((packages-used (qk::packages-used p-d)))
        (if packages-used
          (nconc views
                 (make-view-paragraph 
                  :title (format nil "Packages used by ~a" name)
                  :body (string-downcase-list packages-used)))))
      
      (let ((packages-which-use (qk::packages-which-use p-d)))
        (if packages-which-use
          (nconc views
                 (make-view-paragraph 
                  :title (format nil "Packages which use ~a" name)
                  :body (string-downcase-list packages-which-use)))))
      
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      
      
      (if sub-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Sub-topics"
                :items sub-topics)))
      
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      (if see-also
        (nconc views
               (vw::make-titled-display-list 
                :title "See Also"
                :items see-also)))
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references)))
      views)))





;;;--------------------------------------------------------------------------
;;;
;;;  Methods-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod help-sub-views ((ms-d qk::methods-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))

    (with-accessors 
      ((name qk::name)
       (symbol qk::doc-symbol)
       (doc-capsule qk::doc-capsule)
       (lam-list qk::lambda-list)
       (method-list qk::method-list)
       )
      ms-d
      (push
       (header-box-view 
        :left name 
        :right 'Methods)
       views)
      
      (if lam-list
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a~{     ~a~}"
                              (string-downcase name)
                              (string-downcase-list lam-list))))
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a"
                              (string-downcase name)
                              ))))
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule)))
      
      
      (if method-list
        (loop for mlist in method-list
              do 
              (nconc views
                     (make-methods-display-list
                      :title
                      (format
                       NIL "~{~a ~}"
                       (loop
                         for s in (car mlist)
                             collect
                             (string-capitalize
                              (if (symbolp s)
                               (string-downcase (string s))
                               (format NIL "~s"  s)))))
                      :symbol symbol
                      :methods (cdr mlist)
                      :group? NIL)
                     )))
      
      views)))


;;;--------------------------------------------------------------------------
;;;
;;;  Method-documentation
;;;
;;;--------------------------------------------------------------------------

(defmethod help-sub-views ((m-d qk::method-documentation) &key type)
  (declare (ignore type))
  (let ((views NIL))
    (with-accessors 
      ((name qk::name)
       (package qk::package)
       (symbol qk::doc-symbol)
       (doc-capsule qk::doc-capsule)
       (doc-elaboration qk::doc-elaboration)
       (examples qk::examples)
       (references qk::references)
       (see-also qk::see-also)
       (super-topics qk::super-topics)
       (lam-list qk::lambda-list)
       (arg qk::arguments)
       (returns qk::returns)
       (side-effects qk::side-effects)
       (qualifiers qk::doc-method-qualifiers)
       (specializers qk::doc-method-specializers)
       (mo qk::doc-method-object)
       )
      m-d
      (push
       (header-box-view 
        :left name 
        :right (format NIL "~{~s ~} METHOD" qualifiers)
        )
       views)
      
      (if lam-list
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a~{     ~a~}"
                              (string-downcase name)
                              (string-downcase-list lam-list))))
        (nconc views
               (make-view-paragraph 
                :title ""
                :body (format NIL
                              "~a"
                              (string-downcase name)
                              ))))
      
      (when specializers
        (nconc views
               (make-view-paragraph 
                :title "Specializers"
                :body
                (format NIL
                        "~{~a   ~}"
                        (loop for s in (method-specializers  mo)
                              collect 
                              (string-downcase
                               (format NIL "~s"
                                       (cond
                                        ((listp  s) s)
                                        ((symbolp s) s)
                                        (T (class-name s))))
                               ))))
               )
        )

      ;; Borrow doc-capsule from the generic-fuction if necessary
      (unless doc-capsule
        (setf doc-capsule (qk::doc-capsule (doc symbol :generic-function)))
        )
      (if doc-capsule
        (nconc views
               (make-view-paragraph 
                :title "Description"
                :body doc-capsule))
        )
      
      (if (and arg (qk::arg-info-p arg))
          (nconc views
              (help-sub-views arg)))
      
      (if returns
        (nconc views
               (make-view-paragraph 
                :title "Returns"
                :body returns)))
      (if side-effects
        (nconc views
               (make-view-paragraph 
                :title "Side Effects"
                :body side-effects)))
      
      (if package
        (nconc views
               (make-view-paragraph 
                :title "Home Package"
                :body (format nil "~a" (string-downcase-object package)))))
      (if examples
        (nconc views
               (make-examples-display-list
                :title "Examples"
                :examples examples)
                ))
      (if doc-elaboration
        (nconc views
               (make-view-paragraph 
                :title "Elaboration"
                :body doc-elaboration)))
      (if super-topics
        (nconc views
               (vw::make-titled-display-list 
                :title "Super-topics"
                :items super-topics)))
      (let ((add-to-see-also
             (cons (list symbol :methods)
                   (loop for thing in specializers
                         when (class-p thing)
                         collect (list (class-name thing) :class)))))
        (nconc views
               (if see-also
                 (vw::make-titled-display-list 
                  :title "See Also"
                  :items (append add-to-see-also see-also))
                 (vw::make-titled-display-list 
                  :title "See Also"
                  :items add-to-see-also)))
        )
      (if references
        (nconc views
               (make-view-paragraph 
                :title "References"
                :body references)))
      views)))
