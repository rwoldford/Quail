(setf (doc 'example-topic :topic)
      (make-instance
        'quail-kernel::topic-documentation
        :name
        "example-topic"
        :doc-capsule
        "How to build a topic document in Quail."
        :doc-elaboration
        "A little more info"
        :examples
        (list (cons :root "Some root info")
              (cons :text "Misc text")
              (list :files
                    (list "extended arith"
                          "eg:Mathematics;extended-arithmetic.lsp")
                    (list "extended arith2"
                          "eg:Mathematics;extended-arithmetic.lsp")
                    (list "extended arith3"
                          "eg:Mathematics;extended-arithmetic.lsp")
                    )
              )
        :references
        "CLtL by Guy Steele."
        :see-also
        '(+ - * / NaN +infinity -infinity infinity)
        :sub-topics
        '((common-lisp  :topic)
          (quail        :topic))
        :super-topics
        NIL
