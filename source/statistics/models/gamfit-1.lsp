(in-package :quail)

(defmethod special-formula-operator-handler
           ((operator-symbol (eql :s)) formula operator-name &rest ensured-args)
  "Creates an smoothing-spline-variable for the variable given ~
   by (first ensured-args), which must be univariate. ~
   (rest ensured-args) may contain keywords and ~
   arguments for either :spar (smoothing parameter) or ~
   :df (equivalent degrees of freedom), ~
   which otherwise will be chosen by the fitting routine ~
   by cross-validation."
  ;(declare (ignore operator-symbol formula operator-name ensured-args))
  (declare (ignorable operator-symbol formula operator-name ensured-args))
  (error "Special formula operator s(...) has not yet ~
          been implemented.")
)

;;; parse and additive-semantics are ready to go ... just do this stuff now!!
