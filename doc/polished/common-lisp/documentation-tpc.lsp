(setf (doc 'documentation :topic)
      (make-instance 'quail-kernel::topic-documentation
                     :name
                     "documentation"
                     :doc-capsule
                     "How documentation is accessed and created in Quail."
                     :doc-elaboration
                     "Documentation is accessed online primarily through the ~
                      function help.  There are several kinds of documentation ~
                      available, often on the same symbol.  All types are ~
                      specified by a keyword (e.g. :topic, :function, :class).  ~
                      A list of ~
                      the documentable types for any symbol can be found by ~
                      calling the function documentable-uses on that symbol."
                     :examples
                     nil
                     :references
                     nil
                     :see-also
                     nil
                     :sub-topics
                     '((help                                   :function)
                       (documentable-uses                      :function)
                       (make-doc                               :generic-function)
                       (write-tex-doc                          :function)
                       (make-documentation-index               :function)
                       (make-sorted-documentation-index        :function)
                       (sort-doc                               :function)
                       (documentation-objects                  :topic))
                     :super-topics
                     NIL
                     ))