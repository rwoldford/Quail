;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               fn.lisp                              
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
;;;-------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(fn fn-call)))

(defclass fn (quail-object)
  ((name :reader name-of :initarg :name)
   (arglist :reader arglist-of :initarg :arglist)
   (body :reader body-of :initarg :body)
   (deriv-list :accessor deriv-list-of :initarg :deriv-list)
   (compiled-fn :reader compiled-fn-of :initarg :compiled-fn)))

(defun fn-call (fn-object &rest args)
  "Calls function associated with the first argument on the remaining arguments.  ~
   The first argument can be a function, a symbol, or an object of class fn."
  (typecase fn-object
   (fn
    (apply (compiled-fn-of fn-object) args))
   (function
    (apply fn-object args))
   (symbol
    (apply (symbol-function fn-object) args))
   (ref-array
    (array (collect-slices (f fn-object)
             (apply #'fn-call f args))
           :dimensions (dimensions-of fn-object)))
   ))

(defmacro fn (&rest args)
  (let* ((split (position-if-not #'(lambda (x) (and x (atom x)))
                                 args))
         (anonymous (eq split 0))
         (lambda-list (if split
                        (elt args split)
                        (error "Malformed fn: ~
                                no lambda-list provided.")))
         (function-name (if anonymous
                          nil
                          (first args)))
         (body (subseq args (+ 1 split)))
         (compiled-fn (if anonymous
                        `(function (lambda ,lambda-list ,@body))
                        `(setf (symbol-function (quote ,function-name))
                                 (function (lambda ,lambda-list ,@body))))))
    (if function-name
      `(setf (get (quote ,function-name) :FN)
        (make-instance 'fn 
                       :name (quote ,function-name)
                       :arglist (quote ,lambda-list)
                       :body (quote ,body)
                       :deriv-list nil
                       :compiled-fn ,compiled-fn))
      `(make-instance 'fn 
                      :name nil
                      :arglist (quote ,lambda-list)
                      :body (quote ,body)
                      :deriv-list nil
                      :compiled-fn ,compiled-fn))))

(defmethod add-deriv ((fn fn) deriv &optional (deriv-id '(0)))
  (with-slots (deriv-list) fn
    (setf deriv-list (append deriv-list (list (list deriv-id deriv))))))

(defmethod find-deriv ((fn fn) &optional (deriv-id '(0)))
  (with-slots (deriv-list) fn
    (second (find deriv-id deriv-list :test #'equal :key #'first))))

(defmethod print-object ((f fn) stream)
  (with-slots (name arglist body) f
    (let* ((dim (dimensions-of body))
           (len (length dim)))
      (format stream 
              "#<FN ")   
      (if name (format stream "~S " name))
      (format stream
              "~S "
              arglist)
      (qk::ref-print-recursion body
                               dim
                               (make-sequence 'list len)
                               len
                               0
                               stream
                               :left nil :right nil)
      (format stream ">")))
  f)
