;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           models-topic-doc.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1992.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;;

(setf (doc 'models :topic)
      (make-instance 'quail-kernel::topic-documentation
                     :name
                     "models"
                     :doc-capsule
                     "Statistical Models in Quail"
                     :doc-elaboration
                     "Quail provides natural software representations of ~
                      statistical models through CLOS hierarchies of model, ~
                      formula, data, and fitted-model classes."
                     :examples
                     nil
                     :references
                     '("Chambers & Hastie (1991) ~
                        Statistical Models in S"
                       "Anglin & Oldford (1993) ~
                        'Modelling Response Models in Software', ~
                        4th Intl. Workshop on AI and Statistics"
                       )
                     :see-also
                     nil
                     :sub-topics
                     '(
                       (response-models :topic)
                       (model :generic-function)
                       (model-object :class)
                       (formula-object :class)
                       (data-frame :generic-function)
                       (data-frame :class)
                       (fit :generic-function)
                       (fit-object :class)
                       )
                     :super-topics
                     NIL
                     ))

(setf (doc 'model-formulae :topic)
      (make-instance 
       'quail-kernel::topic-documentation
       :name
       "model-formulae"
       :doc-capsule
       "Formulae for statistical models"
       :doc-elaboration
       "Quail provides a rich syntax of model formulae. ~
        Important special cases are
        additive formulae, which represent adaptive predictor functions ~
        which are additive in individual predictors, ~
        and their subclass of linear formulae for non-adaptive predictor functions."
       :examples
       nil
       :references
       '("Chambers & Hastie (1991) ~
          Statistical Models in S")
       :see-also
       '((formula-object :class))
       :sub-topics
       '((additive-formulae :topic)
         (linear-formulae :topic))
       :super-topics
       NIL
       ))

(setf (doc 'linear-formulae :topic)
      (make-instance 
       'quail-kernel::topic-documentation
       :name
       "linear-formulae"
       :doc-capsule
       "Formulae for additive, non-adaptive response models"
       :doc-elaboration
       "Linear formulae represent only non-adaptive predictor functions ~
        which are additive in individual predictors.  Each predictor enters ~
        the formula as the predictor ~
        multiplied by a parameter.  The parameters are not specified ~
        explicitly, rather there is implicitly one associated with each ~
        additive term."
       :examples
       nil
       :references
       '("Chambers & Hastie (1991) ~
          Statistical Models in S")
       :see-also
       '((linear-formula :class))
       :sub-topics
       nil
       :super-topics
       NIL
       ))

(setf (doc 'additive-formulae :topic)
      (make-instance 
       'quail-kernel::topic-documentation
       :name
       "additive-formulae"
       :doc-capsule
       "Formulae for additive, adaptive response models"
       :doc-elaboration
       "Additive formulae represent adaptive predictor functions ~
        which are additive in individual predictors.  Predictors are ~
        permitted to enter the formula as (conceptually) arguments to some class of ~
        functions for which non-parametric estimates can be obtained from the ~
        predictor and a response.  Usually the specification of each additive term ~
        indicates which estimator is to be used; for example, for smoothing spline ~
        estimates of a predictor x with approximately 3 degrees of freedom, one would ~
        use s(x,3)."
       :examples
       nil
       :references
       '("Chambers & Hastie (1991) ~
          Statistical Models in S"
         "Tibshirani & Hastie (1991) ~
          Generalized Additive Models")
       :see-also
       '((additive-formula :class))
       :sub-topics
       nil
       :super-topics
       NIL
       ))

(setf (doc 'response-models :topic)
      (make-instance 'quail-kernel::topic-documentation
                     :name
                     "response-models"
                     :doc-capsule
                     "Models for which a response variable is ~
                      separable from the predictor variables."
                     :doc-elaboration
                     nil
                     :examples
                     nil
                     :references
                     nil
                     :see-also
                     '((generalized-additive-models :topic)
                       (additive-model :class)
                       (generalized-linear-models :topic)
                       (linear-model :class)
                       (gaussian-linear-model :class))
                     :sub-topics
                     '((response-model :class)
                       (response-formula :class))
                     :super-topics
                     NIL
                     ))

(setf (doc 'generalized-additive-models :topic)
      (make-instance 'quail-kernel::topic-documentation
                     :name
                     "generalized-additive-models"
                     :doc-capsule
                     "Generalized Additive Models"
                     :doc-elaboration
                     "Generalized Additive Models are CURRENTLY UNIMPLEMENTED."
                     :examples
                     nil
                     :references
                     '(("Tibshirani & Hastie (1991) ~
                         Generalized Additive Models"))
                     :see-also
                     nil
                     :sub-topics
                     '((generalized-additive-model :class)
                       (additive-formula :class)
                       (family-object :class)
                       (link-object :class))
                     :super-topics
                     NIL
                     ))

(setf (doc 'generalized-linear-models :topic)
      (make-instance 'quail-kernel::topic-documentation
                     :name
                     "generalized-linear-models"
                     :doc-capsule
                     "Generalized Linear Models"
                     :doc-elaboration
                     nil
                     :examples
                     nil
                     :references
                     '(("McCullagh & Nelder (1983, 1989) ~
                         Generalized Linear Models"))
                     :see-also
                     nil
                     :sub-topics
                     '((generalized-linear-model :class)
                       (linear-formula :class)
                       (family-object :class)
                       (link-object :class))
                     :super-topics
                     NIL
                     ))

      
