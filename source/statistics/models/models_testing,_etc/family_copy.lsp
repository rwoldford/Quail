(in-package :quail)

(export '(family gaussian binomial))

(defclass family (quail-object)
  ((name :reader name-of :initarg :name)
   (variance-fn :reader variance-fn-of :initarg :variance-fn)
   (deviance-fn :reader deviance-fn-of :initarg :deviance-fn))
  (:documentation
   "
      [name]  name of the family
      [variance-fn]  a fn producing variance as a function of the mean
                     for a single observation from this family
      [deviance-fn]  a fn producing deviance as a function of means,
                     observed responses, and weights for collection of
                     observations from this family."))

(defmethod print-object ((family family) stream)
  (cond ((slot-boundp family 'name)
         (format stream "#<FAMILY subclass ~S>"
                 (name-of family)))
        (t
         (format stream "#<~S ~S>"
                 (class-name (class-of family))
                 (qk::system-get-pointer family)))))
        
(defmacro def-family (symbol name &key variance-fn deviance-fn)
  "A macro which simplifies creation of family subclasses."
  `(progn
     (defclass ,symbol (family)
       ((name :allocation :class :reader name-of :initform (quote ,name))
        (variance-fn :allocation :class 
                     :reader variance-fn-of :initform ,variance-fn)
        (deviance-fn :allocation :class 
                     :reader deviance-fn-of :initform ,deviance-fn)))
     (defvar ,symbol NIL ,name)
     (setf ,symbol (make-instance (quote ,symbol)))))

(def-family gaussian
  "Gaussian (constant variance function)"
  :variance-fn 
  (fn (mu) 1)
  :deviance-fn
  (fn (mu y w &optional residuals)
      (let ((e (- y mu)))
        (if residuals
          e
          (.* w (* e e))))))
#|
(def-family gaussian
  "Gaussian (constant variance function)"
  :variance-fn 
  (fn (mu) 1)
  :deviances-fn
  (fn (mu y) (- y mu)))
|#

(def-family binomial
  "Binomial"
  :variance-fn 
  (fn (mu) (* mu (- 1 mu)))
  :deviance-fn
  (fn (mu y w &optional residuals)
      (labels ((xlogy (x y) (if (zerop y)
                              0
                              (* x (base-lisp::log y))))
               (xlogx (x) (xlogy x x))
               (devxy (x y) (+ (xlogy x y)
                               (xlogy (- 1 x) (- 1 y))))
               (devx (x) (+ (xlogx x)
                            (xlogx (- 1 x)))))
        (let* (devy devmu devi)
          (setf devy (map-element #'devx nil y))
          ;;  This computation is currently unsafe when mu[i] is 0 or 1
          (setf devmu (map-element #'devxy nil y mu))
          (setf devi (* 2 (- devy devmu)))
          (if residuals
            (* (map-element #'signum nil (- y mu))
               (map-element #'sqrt nil (* devi w)))
            (.* w devi))))))

;; these need some work to help preserve eq-ness when possible

(defmethod initialize-response ((f family) response weight)
  (missing-method 'initialize-response f response weight))

(defmethod initialize-response ((f gaussian) response weight)
  (values (ref response) (ref weight) (sel response)))

(defmethod initialize-response ((f binomial) response weight)
  (let (n mu)
    (setf n (.* response '(1 1)))
    (setf response (/ (ref response t 0) n))
    (setf weight (* weight n))
    (setf mu (+ response (/ (- 0.5 response) n)))
    (values (ref response) (ref weight) (ref mu))))


(def-family poisson
  "Poisson"
  :variance-fn 
  (fn (mu) mu)
  :deviance-fn
  (fn (mu y w &optional residuals)
      ))


(def-family gamma
  "Gamma"
  :variance-fn 
  (fn (mu) (* mu mu))
  :deviance-fn
  (fn (mu y w &optional residuals)
      ))


(def-family inverse-gaussian
  "Inverse Gaussian"
  :variance-fn 
  (fn (mu) (expt mu 3))
  :deviance-fn
  (fn (mu y w &optional residuals)
      ))