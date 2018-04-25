(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(link 
          def-link
          find-link
          power-link
          )))

(defclass link-object (quail-object)
  ((name :reader name-of :initarg :name)
   (link :reader link-of :initarg :link)
   (inverse-link :reader inverse-link-of :initarg :inverse-link))
  (:documentation 
   "
      [link]  a fn instance
      [inverse-link]  a fn instance which is the inverse of link"))

(defmethod print-object ((link link-object) stream)
  (cond ((slot-boundp link 'name)
         (format stream "#<LINK ~S>"
                 (name-of link)))
        (t
         (format stream "#<~S ~S>"
                 (class-name (class-of link))
                 (qk::system-get-pointer link)))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun link-symbol (link-identifier)
    (intern (concatenate 'string 
                         (string-upcase 
                          (if (listp link-identifier) ;; ie (quote foo) is a list
                            (second link-identifier)
                            link-identifier))
                         "-LINK")
            :quail)))

(defmethod find-link ((link-identifier t))
  ;; assume it's the right one
  (missing-method 'find-link link-identifier))

(defmethod find-link ((link-identifier symbol))
  (let (link)
    (if (keywordp link-identifier)
      (setq link-identifier (intern (symbol-name link-identifier) :quail)))
    (if (and (boundp link-identifier) 
             (typep (setf link (symbol-value link-identifier)) 'link-object))
      link
      (symbol-value (link-symbol link-identifier)))))

(defmethod find-link ((link-identifier string))
  (find-link (intern (string-upcase link-identifier) :quail)))

(defmethod find-link ((link-identifier link-object))
  ;; assume it's the right one
  link-identifier)

(defmacro def-link (link-identifier name link
                                    &key inverse-link link-deriv inverse-link-deriv)
  "A macro which simplifies creation of link subclasses."
  (let ((foo #'(lambda (fn fn-deriv)
                 (when fn-deriv (add-deriv fn fn-deriv '(0)))
                 fn))
        (symbol (link-symbol link-identifier)))
    `(progn
       ;; undefine previous versions ... allows easy redefine
       (defclass ,symbol () ())
       (defclass ,symbol (link-object)
         ((name :allocation :class :reader name-of 
                :initform (quote ,name))
          (link :allocation :class :reader link-of 
                :initform (funcall ,foo ,link ,link-deriv))
          (inverse-link :allocation :class
                        :reader inverse-link-of
                        :initform (funcall ,foo ,inverse-link ,inverse-link-deriv))))
       (defvar ,symbol NIL ,name) ;18JUNE2004
       (setf ,symbol (make-instance (quote ,symbol)))
       (eval-when (:compile-toplevel :load-toplevel :execute)
       (export (quote ,symbol) :quail)))))


#|  Unnecessary ...


(defun in-bounds-error-chk (x lower upper
                              &key
                              (lower-compare-fn #'>=)
                              (upper-compare-fn #'<=))
  "Determines whether the first argument is between the second and third.  ~
   Comparison functions are given by the keyword arguments ~
   lower-compare-fn (default (>= x lower)) ~
   and upper-compare-fn (default (<= x upper))."
  (if 
    (and (funcall lower-compare-fn x lower)
         (funcall upper-compare-fn x upper))
    T
    (quail-error "In-bounds-error-chk: Argument ~s out of bounds.  ~
                  lower = ~s   upper = ~s." x lower upper)))

|#


(def-link :identity
  "Identity: mu"
  (fn (mu) mu)
  :inverse-link
  (fn (eta) eta)
  :link-deriv
  (fn (mu) 1)
  :inverse-link-deriv
  (fn (eta) 1))


(def-link :logit
  "Logit: (log (/ mu (- 1 mu)))"
  (fn (mu)
      (log (/ mu (- 1 mu))))
  :inverse-link
  (fn (eta)
      (let ((e-eta (exp eta)))
        (/ e-eta (+ 1 e-eta))))
  :link-deriv
  (fn (mu)
      (/ (* mu (- 1 mu))))
  :inverse-link-deriv
  (fn (eta)
      (let* ((e-eta (exp eta))
             (1+e-eta (+ 1 e-eta)))
        (/ e-eta (* 1+e-eta 1+e-eta))))
  )

(def-link :cloglog
  "Complimentary log-log: (log (- (log (- 1 mu))))"
  (fn (mu) (log (- (log (- 1 mu)))))
  :inverse-link
  (fn (eta) (let ((e-eta (exp (- eta))))
              (- (exp  e-eta))))
  :link-deriv
  (fn (mu)
      (let ((1-mu (- 1 mu)))
        (- 1 (* 1-mu (log 1-mu)))))
  :inverse-link-deriv
  (fn (eta)
      (- (exp (- (+ eta (exp (- eta)))))))
  )


(def-link :log
  "Log: (log mu)"
  (fn (mu) (log mu))
  :inverse-link
  (fn (eta) (exp eta))
  :link-deriv
  (fn (mu) (/ 1 mu))
  :inverse-link-deriv
  (fn (eta) (exp eta))
  )





(def-link :reciprocal
  "Reciprocal: (/ 1 mu)"
  (fn (mu) (/ 1 mu))
  :inverse-link
  (fn (eta) (/ 1 eta))
  :link-deriv
  (fn (mu) (/ -1 (* mu mu)))
  :inverse-link-deriv
  (fn (eta) (/ -1 eta))
  )



(def-link :reciprocal-square
  "Reciprocal square: (/ 1 (* mu mu))"
  (fn (mu) (/ 1 (* mu mu)))
  :inverse-link
  (fn (eta) (/ 1 (sqrt eta)))
  :link-deriv
  (fn (mu) (/ -2 (expt mu 3)))
  :inverse-link-deriv
  (fn (eta) (* -.5 (expt eta -1.5)))
  )



(def-link :sqrt
  "Square root: (sqrt mu)"
  (fn (mu) (sqrt mu))
  :inverse-link
  (fn (eta) (* eta eta))
  :link-deriv
  (fn (mu) (/ 0.5 (sqrt mu)))
  :inverse-link-deriv
  (fn (eta) (* 2 eta))
  )


(defun power-link (power)
  "Returns a link-object that is a power transformation mu^power ~
   or log(mu) if power is zero."
  (let ((link-name (format NIL "Power = ~s: ~s"
                           power (if (zerop power)
                                   '(log mu)
                                   `(expt mu ,power))))
        link-fn inverse-link deriv-link deriv-inverse-link)
    (cond
     ((zerop power)
      (setf link-fn (fn (mu) (log mu)))
      (setf inverse-link (fn (eta) (exp eta)))
      (setf deriv-link (fn (mu) (/ 1 mu)))
      (setf deriv-inverse-link (fn (eta) (exp eta))))
     (T
      (setf link-fn            (eval
                                `(fn (mu) (expt mu , power))))
      (setf inverse-link       (eval
                                `(fn (eta) (expt eta ,(/ 1 power)))))
      (setf deriv-link         (eval
                                `(fn (mu) (* , power
                                               (expt mu ,(- power 1))))))
      (setf deriv-inverse-link (eval
                                `(fn (eta) (/ (expt eta
                                                    ,(/ (- 1 power) power))
                                              ,power))))))
    (flet ((foo (fn fn-deriv)
             (when fn-deriv (add-deriv fn fn-deriv '(0)))
             fn))
      (make-instance 'link-object
                     :name link-name
                     :link (foo link-fn deriv-link)
                     :inverse-link (foo inverse-link deriv-inverse-link))))
  )

#|
(def-link :probit
  "Probit: (qnorm mu)"
  (fn (mu) (qnorm mu))
  :inverse-link
  (fn (eta) (pnorm eta))
  ;;:link-deriv
  ;;(fn (mu) (/ 1 mu))
  :inverse-link-deriv
  (fn (eta) (norm eta))
  )
|#
