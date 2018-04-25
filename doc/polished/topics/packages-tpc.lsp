(setf (doc 'packages :topic)
      (make-instance 'quail-kernel::topic-documentation
                     :name
                     "packages"
                     :doc-capsule
                     "A package is a data structure that establishes a ~
                      mapping from print names (strings) to symbols."
                     :doc-elaboration
                     "Each package can be thought of as a collection of ~
                      symbols.  At any given time only one package (the ~
                      value of *package*) is the current package.  ~
                      Typically, a package will use the symbols of another ~
                      package and so might ~
                      in turn be used by other packages.  ~
                      If package A \"uses\" package B, then those symbols ~
                      in package B which have been \"exported\" from B are said ~
                      to be \"external symbols\" of B.  Symbols in package B ~
                      which have not been exported are said to be \"internal ~
                      \" symbols of B.  When in the package A, any external ~
                      symbol of B will be visible to A as if it were an internal ~
                      symbol of A."
                     :examples
                     (let ((p-names (loop for p in (list-all-packages)
                                          collect (package-name p))))
                       
                       (list (cons :root
                                   (format NIL "~a~{ ~a~}."
                                     (first p-names)
                                     (rest p-names)))
                             NIL
                             NIL)
                       )
                     :references
                     quail-kernel::*CLtL*
                     :see-also
                     nil
                     :sub-topics
                     '((*package*   :variable)
                       (list-all-packages :function)
                       (symbol-package :function)
                       (make-package :function)
                       (delete-package :function)
                       (in-package :macro)
                       (use-package :function)
                       (unuse-package :function)
                       (find-package :function)
                       (package-name :function)
                       (package-nicknames :function)
                       (rename-package :function)
                       (package-use-list :function)
                       (package-used-by-list :function)
                       (package-shadowing-symbols :function)
                       (intern :function)
                       (find-symbol :function)
                       (unintern :function)
                       (export :function)
                       (unexport :function)
                       (import :function)
                       (shadowing-import :function)
                       (shadow :function)
                       (defpackage :macro)
                       (find-all-symbols :function)
                       (do-symbols :macro)
                       (do-external-symbols :macro)
                       (do-all-symbols :macro)
                       ;;(with-package-iterator :macro)
                       )
                       :super-topics
                       NIL
                       :see-also
                       (loop for p in (list-all-packages)
                           collect (list (package-name p) :package))
                       ))