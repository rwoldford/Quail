(setf (doc 'getting-started :topic)
      (make-instance 'quail-kernel::topic-documentation
                     :name
                     "getting-started"
                     :doc-capsule
                     "How to get started in Quail."
                     :doc-elaboration
                     nil
                     :examples
                     nil
                     :references
                     nil
                     :see-also
                     nil
                     :sub-topics
                     '((common-lisp  :topic)
                       (quail        :topic))
                     :super-topics
                     NIL
                     ))

(setf (doc 'topics :topic)
      (make-instance 'quail-kernel::topic-documentation
                     :name
                     "topics"
                     :doc-capsule
                     "The set of topics documented in Quail."
                     :doc-elaboration
                     nil
                     :examples
                     nil
                     :references
                     nil
                     :see-also
                     nil
                     :sub-topics
                     '((arrays     :topic)
                       (numerical  :topic)
                       (graphics   :topic)
                       (views      :topic)
                       (statistics :topic)
                       (quaff      :topic))
                     :super-topics
                     NIL
                     ))

(setf (doc 'arrays :topic)
      (make-instance 'quail-kernel::topic-documentation
                     :name
                     "arrays"
                     :doc-capsule
                     "Arrays in Quail."
                     :doc-elaboration
                     nil
                     :examples
                     nil
                     :references
                     nil
                     :see-also
                     nil
                     :sub-topics
                     '((array    :built-in-class)
                       (sequence :built-in-class)
                       (ref :generic-function)
                       (dimensioned-ref-object :class)
                       (ref-array :class)
                       (setf :macro)
                       (setq :special-form))
                     :super-topics
                     NIL
