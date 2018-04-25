;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               statistics.asd                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     D.G. Anglin 1992.
;;;     R.W. Oldford 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------

;(in-package :make)

;(eval-when (compile load eval)
;  (load-pathnames "statistics")
;  )
        

(asdf:defsystem "statistics"
    :default-component-class cl-source-file.lsp
   :components
   ((:module "statistics/basic-statistics"
             :components
             ((:file "summary-statistics")
              (:file "sweep")))
    #| contains wb:
    (:module "stat-graphics"
             :components
             ((:file "projection-trace")
              (:file "qq-plot")
              ;; --> split between Views and Stat-Sessions (:file "display")
              (:file "display-probability")
              (:file "stem")
              (:file "hue-light-view")
              (:file "imagel")
              (:file "image")
              )
             )
    |#
    (:module "statistics/models"
             :components ((:file "parse")
                          (:file "formula-reduce")
                          (:file "additive-formula-semantics")
                          (:file "formula-object")
                          (:file "factor")
                          (:file "contrasts")
                          (:file "response-matrix")
                          (:file "data-object")
                          (:file "data-frame")
                          ;;(:file "fn")   ---> gone to Mathematics directory
                          (:file "link")
                          (:file "family")
                          (:file "weight-fn")
                          (:file "model-object")
                          (:file "model-frame")
                          (:file "fit")
                          (:file "deviance")
                          (:file "residuals")
                          (:file "model-matrix")
                          (:file "gam")
                          (:file "gamfit-1") ;; temporary name, probably
                          (:file "glm")
                          (:file "least-squares-mixin")
                          (:file "linear-model")
                          (:file "lsfit")
                          ;; (:file "models-topic-doc")
                          (:file "new-terms")
                          ;;(:file "display") << has wb:
                          )
             )
     #| contains wb:
    (:module "statistics/stat-sessions"
             :components
             ((:file "response-session"))
             )
    |#
    ))