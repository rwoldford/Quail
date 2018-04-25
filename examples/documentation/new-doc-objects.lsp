;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;                     Defining new documentation objects.
;;;
;;;
;;;   Author: R.W. Oldford 1994.   ... under construction.
;;;

#| This file contains instructions on how to extend the Quail documentation
   system by introducing new classes of documentation objects.
   
   The file is not really of general interest.

1. DOCUMENTATION-OBJECT:
   Create a new documentation object by specializing one of
   the existing documentation classes.
   To see existing doc classes: 
      (quail-doc-classes)
      or 
      (class-browse 'qk::documentation-object)


2. DOC-TYPE: You will need Quail documentation type to go with the new
   documentation-object class.  Quail documentation types are keyword symbols
   like :function, :macro, :generic-function, etc.
   Doc types are part of the primary user interface with the Quail documentation
   system as in (help 'help :function).

   Existing doc-types are found from:
       (quail-doc-types)

   To connect your new doc type (:my-new-type) with the new documentation object class
   my-new-doc-class

    (set-doc-type-to-doc-class :my-new-type 'my-new-doc-class)

   This installs the connection in a translation table used by the Quail documentation
   system and the doc class can be retrieved from the doc type as in

    (quail-doc-class :my-new-type)

   For your type of new documentation there may in fact be some information that
   is available from the Common Lisp documentation function.  (This is doubtful as
   Quail already uses all that is provided by the language definition. Nevertheless...)
   If so, you will need a CL doc type for the CL documentation function that you
   believe matches your new Quail doc type.
   Again, the connection is made through a translation table.
   For example if your :my-new-type should really grab the 'function documentation
   from a symbol, you set up the translation like this:

   (set-quail-to-cl-doc-type :my-new-type 'function)

   This allows the internal quail-kernel function cl-type to return the
   correct translation as in

   (qk::cl-type :my-new-type)


3. MAKE-DOC:
   Define a new method that will create the new documentation object by
   specializing the generic function make-doc.
   As an example, here is the general code for make-doc for a symbol.
   It is written in the package quail-kernel (in-package :quail-kernel).

(defmethod make-doc ((thing symbol) type)
  (let* ((doc-class (quail-doc-class type))
         (doc-type (cl-type type))
         (document
          (interpret-quail-documentation
           (if doc-type
             (documentation thing doc-type)
             (documentation thing))))
         )
    (make-instance doc-class
           :name (string-downcase (format NIL "~s" thing))
           :document document)
    ))

4. HELP-SUB-VIEWS:
   Define a new method to display the new documentation object in a help
   window by specializing the generic function help-sub-views.
   As an example here is some code for function-documentation.
   It is written within the Quail package (in-package :quail)

(defmethod help-sub-views ((self qk::function-documentation))
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
      (if doc-capsule
        (nconc views
               (make-view-paragraph :title "Description"
                                    :body doc-capsule)))
      (if lam-list
        (nconc views
               (make-view-paragraph  :title "Lambda List"
                                     :body (string-downcase-list lam-list))))
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
                                     (format nil ":~a"
                                             (string-downcase-object package)))))
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
    

5. 
