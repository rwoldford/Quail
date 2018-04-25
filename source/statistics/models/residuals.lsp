(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(residuals compute-residuals)))

(defgeneric residuals (fit-obj &key type &allow-other-keys))

(defmethod residuals (fit-obj &key (type :default)
                              &allow-other-keys)
  (compute-residuals fit-obj type))

(defgeneric compute-residuals (fit-obj type &key &allow-other-keys))

(defmethod compute-residuals ((fit-obj response-model-fit)
                              (type (eql :deviance)) &key)
  (with-slots
    ((mu pred) (y response-matrix) (w weight)) fit-obj
    (* (map-element #'signum nil (- y mu))
       (map-element #'sqrt nil
                    (* w (fn-call 
                          (deviances-fn-of (family-of fit-obj))
                          mu y))))))

(defmethod compute-residuals ((fit-obj response-model-fit)
                              (type (eql :working)) &key)
  (with-slots
    (resid) fit-obj
    resid))

(defmethod compute-residuals ((fit-obj response-model-fit)
                              (type (eql :response)) &key)
  (with-slots
    ((y response-matrix) (mu pred)) fit-obj
    (- y mu)))

(defmethod compute-residuals ((fit-obj response-model-fit)
                              (type (eql :pearson)) &key)
  ;; need working weights, which we haven't stored ...
  ;; also assume resid has been filled
  (with-slots ((w weight) (mu pred) resid) fit-obj
    (* (sqrt (fn-call (find-weight-fn (family-of fit-obj)
                                      (link-of fit-obj))
                      mu w))
       resid))
  )





