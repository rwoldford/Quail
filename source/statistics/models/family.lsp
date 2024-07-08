;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                            family.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   (c) Copyright Statistical Computing Laboratory
;;;       University of Waterloo
;;;       1995
;;;
;;;  Authors:
;;;     D.G. Anglin 1992
;;;     R.W. Oldford 1995
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*families*
          family-object
          def-family
          find-family)))

(defvar *families*
  NIL
  "A variable containing all currently defined families.")

(defclass family-object (quail-object)
  ((name :reader name-of :initarg :name)
   (variance-fn :reader variance-fn-of :initarg :variance-fn)
   (deviances-fn :reader deviances-fn-of :initarg :deviances-fn))
  (:documentation
   "
      [name]  name of the family
      [variance-fn]  a fn producing variance as a function of the mean
                     for a single observation from this family
      [deviances-fn]  a fn producing deviances as a function of means and
                     observed responses for collection of
                     observations from this family."))

(defmethod print-object ((family family-object) stream)
  (cond ((slot-boundp family 'name)
         (format stream "#<FAMILY ~S>"
                 (name-of family)))
        (t
         (format stream "#<~S ~S>"
                 (class-name (class-of family))
                 (qk::system-get-pointer family)))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun family-symbol (family-identifier)
    (intern (concatenate 'string 
                         (string-upcase 
                          (if (listp family-identifier) ;; ie (quote foo) is a list
                            (second family-identifier)
                            family-identifier))
                         "-FAMILY")
            :quail)))

(defmethod find-family ((family-identifier t))
  ;; assume it's the right one
  (missing-method 'find-family family-identifier))

(defmethod find-family ((family-identifier symbol))
  (let (family)
    (if (keywordp family-identifier)
      (setq family-identifier (intern (symbol-name family-identifier) :quail)))
    (if (and (boundp family-identifier) 
             (typep (setf family (symbol-value family-identifier)) 'family-object))
      family
      (symbol-value (family-symbol family-identifier)))))

(defmethod-multi find-family ((family-identifier string))
  (find-family (intern (string-upcase family-identifier) :quail)))

(defmethod find-family ((family-identifier family-object))
  ;; assume it's the right one
  family-identifier)

(defmacro def-family (family-identifier name &key variance-fn deviances-fn)
  "A macro which simplifies creation of family subclasses."
  (let ((symbol (family-symbol family-identifier))) 
    `(progn
       ;; (declare (special *families*))
       ;; this first defclass is certain to destroy previous versions 
       ;; this is necessary due to :initform weirdness
       ;(defclass ,symbol () ())
       (defclass ,symbol (family-object)
         ((name :allocation :class :reader name-of :initform (quote ,name))
          (variance-fn :allocation :class 
                       :reader variance-fn-of :initform ,variance-fn)
          (deviances-fn :allocation :class 
                        :reader deviances-fn-of :initform ,deviances-fn)))
       (defvar ,symbol NIL ,name)
       (unless (member ,symbol *families*)
         (push ,symbol *families*))
       (setf ,symbol (make-instance (quote ,symbol)))
       (eval-when (:compile-toplevel :load-toplevel :execute)
       (export (quote ,symbol) :quail))
       ,symbol)))

(def-family :gaussian
  "Gaussian (constant variance function)"
  :variance-fn 
  (fn (mu) (declare (ignore mu)) 1)
  :deviances-fn
  (fn (mu y) (expt (- y mu) 2)))

(defun log{x}-or-zero (x)
  (if (and (numberp x) (zerop x))
    0.0
    (qk::ext_log x)))

(defun log{x/u}-or-zero (x u)
  (if (and (numberp x) (zerop x))
    0.0
    (qk::ext_log (qk::ext_/ x u))))

(def-family :binomial
  "Binomial"
  :variance-fn 
  (fn (mu) (* mu (- 1 mu)))
  :deviances-fn
  (fn (mu y)
      (labels ((alogb (a b) (* a (log{x}-or-zero b)))
               (aloga (a) (alogb a a))
               (fdevym (yi mi) (+ (alogb yi mi)
                                  (alogb (- 1 yi) (- 1 mi))))
               (fdevy (yi) (+ (aloga yi)
                              (aloga (- 1 yi)))))
        (let* (devy devym)
          (setf devy (map-element #'fdevy nil y))
          ;;  This computation is currently unsafe when mu[i] is 0 or 1
          (setf devym (map-element #'fdevym nil y mu))
          (* 2 (- devy devym))))))

(def-family :poisson
  "Poisson"
  :variance-fn 
  (fn (mu) mu)
  :deviances-fn
  (fn (mu y)
      (labels 
        ((devym (yi mi) 
           (+ mi (- yi) (* yi (log{x/u}-or-zero yi mi)))))
        (map-element #'devym nil y mu))))

(def-family :gamma
  "Gamma"
  :variance-fn 
  (fn (mu) (* mu mu))
  :deviances-fn
  (fn (mu y)
      (labels 
        ((devym (yi mi)
           (- (qk::ext_/ (qk::ext_- yi mi) mi)
              (log{x/u}-or-zero yi mi))))
        (map-element #'devym nil y mu))
      ))

(def-family :inverse-gaussian
  "Inverse Gaussian"
  :variance-fn 
  (fn (mu) (expt mu 3))
  :deviances-fn
  (fn (mu y)
      (/ (- y mu) 1/2 (* mu mu) y)
      ))

(defgeneric initialize-response (family response weight)
  (:documentation
   "Returns three values: response-matrix, weights, and starting values for ~
    the glm fit.")
  ;; cf. fit method for generalized-linear-model-fit in glm.lisp
  )

(defmethod initialize-response ((f family-object) response weight)
  (missing-method 'initialize-response f response weight))

;; In the following methods:
;; the ref collapses the dimensions -- could be done easier maybe.
;; the sel provides a new instance to hold starting values.

;; these need some work to help preserve eq-ness when possible
#|
;;; Mapply is gone.  The following function is replaced
;;; by substitute-slices-if in initialize-response
;;; Seems like a bizarre initialization ... rwo
(defun replace-zeros-with-1/6 (1d-object)
  (mapply #'(lambda (r)
              (let ((rr (eref r)))
                (if (> rr 0)
                  rr
                  1/6)))
          (list 1d-object t)))
|#

(defmethod initialize-response ((f gaussian-family) response weight)
  (values (ref response) (ref weight) (sel response)))

(defmethod initialize-response ((f binomial-family) response weight)
  (let (n mu)
    (setf n (.* response '(1 1)))
    (setf response (/ (ref response t 0) n))
    (setf weight (* weight n))
    (setf mu (+ response (/ (- 1/2 response) n)))
    (values (ref response) (ref weight) (ref mu))))

(defmethod-multi initialize-response ((f (poisson-family
                                          gamma-family
                                          inverse-gaussian-family))
                                      response
                                      weight)
  (values (ref response)
          (ref weight)
          (substitute-slices-if 1/6 
                                #'(lambda (r) (<= (eref r) 0))
                                response)))
