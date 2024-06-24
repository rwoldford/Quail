;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-kernel.asd                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1991.
;;;     R.W. Oldford 1989 +
;;;     Bob White
;;;     Greg Bennett 2017
;;;
;;;
;;;----------------------------------------------------------------------------

(asdf:defsystem "quail-kernel"
    :default-component-class cl-source-file.lsp
    :components ((:file "quail-kernel/quail-kernel-package")
                 #+:sbcl-linux(:file  "quail-kernel/quail-kernel-system-sblx")
                 #+:ccl-1.11(:file  "quail-kernel/quail-kernel-system-ccl")
                 #+:aclpc-linux(:file "quail-kernel/quail-kernel-system-pc")

               (:module "quail-kernel/mop"
                        :components (#+:sbcl-linux(:file "mop-sblx")
                                     #+:ccl.1-11(:file "mop-ccl")
                                     #+:aclpc-linux(:file "mop-pc")
                                     (:file "mixin-to-quail")
                                     #+:sbcl-linux(:file "function-info-sblx")
                                     #+:ccl-1.11(:file "function-info-ccl")
                                     #+:aclpc-linux(:file "function-info-pc")
                                     ))

               (:module "quail-kernel/basic"
                        :components ((:file "defmethod-multi")
                                     (:file "special-vars")
                                     (:file "synonym")
                                     (:file "seq")
                                     (:file "tree")
                                     (:file "search-tree")
                                     (:file "utility"
                                            :depends-on ("special-vars"))
                                     #+:sbcl-linux(:file "utility-sblx")
                                     #+:ccl-1.11(:file "utility-ccl")
                                     #+:aclpc-linux(:file "utility-pc")
                                     (:file "seq-utilities")
                                     (:file "symbols")
                                     (:file "quail-object")
                                     (:file "proto-mixin")
                                     (:file "open-mixin")
                                     (:file "return-class")
                                     (:file "make-result")
                                     #+:sbcl-linux(:file "defconstant")))
               (:module "quail-kernel/io"
                        :components ((:file "quail-io")
                                     (:file "quail-file")
                                     (:file "scan")
                                     (:file "slots")
                                     #+:sbcl-linux(:file "save-sblx")
                                     #+:ccl-1.11(:file "save-ccl")
                                     #+:aclpc-linux(:file "save-pc")
                                     (:file "restore")
                                     ;; (:file (add-system-extension "restore"))
                                     )
                         :depends-on ("quail-kernel/basic")
                        )

               (:module "quail-kernel/ref"
                        :components ((:file "ref-object")
                                     (:file "eref")
                                     (:file "ref")
                                     (:file "indices")
                                     (:file "ref-if")
                                     (:file "ref-eq")
                                     (:file "setf-ref")
                                     (:file "sel")
                                     (:file "with-ref")
                                     (:file "number-of-elements")
                                     (:file "number-of-slices")
                                     (:file "subscript-utility")
                                     (:file "row-major-ops")
                                     (:file "column-major-ops")
                                     ;; this wasn't doing anything anywhere ... dga 94 03
                                     ;; (:file "ref-behavior")
                                     )
                         :depends-on ("quail-kernel/basic" "quail-kernel/io")
                        )
               (:module "quail-kernel/math"
                        :components (#+(not (or :sbcl-linux :ccl-1.11))(:file "extended-ops")
                                     #+:sbcl-linux(:file "extended-ops-sblx")
                                     #+:ccl-1.11(:file "extended-ops-ccl")
                                     #+:aclpc-linux(:file "extended-ops-pc")
                                     (:file "matrix-multiply")) 
                         :depends-on ("quail-kernel/basic" "quail-kernel/io" "quail-kernel/ref")
                        )
               (:module "quail-kernel/array"
                        :components ((:file "map-element")
                                     (:file "map-slices")
                                     (:file "array")
                                     (:file "ref-array")
                                     (:file "mk-array")
                                     (:file "copy-dispatch")
                                     (:file "num-array")
                                     (:file "ones-array")
                                     (:file "matrix")
                                     (:file "file-matrix")
                                     (:file "collapse")
                                     (:file "tp")
                                     (:file "glue")
                                     (:file "slice")
                                     (:file "sort-object"
                                            :depends-on ("slice"))
                                     (:file "sort")
                                     (:file "sort-position"
                                            :depends-on ("slice"))
                                     (:file "ranks")
                                     (:file "order")
                                     (:file "remove-slices")
                                     (:file "find-slices")
                                     (:file "count-slices"
                                            :depends-on ("slice"))
                                     (:file "substitute-slices")
                                     (:file "replace-slices")
                                     (:file "slice-positions")
                                     (:file "reduce-slices")
                                     )
                         :depends-on ("quail-kernel/basic" "quail-kernel/io" "quail-kernel/ref" "quail-kernel/math")
                        ))

)

