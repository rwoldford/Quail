(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(formula-object response-formula 
          additive-formula linear-formula)))

(defun string-space (x)
  (string-downcase (format nil "~S " x)))

;; mf stands for model formula

(defmacro mf (&rest args)
  `(apply #'concatenate 'string (mapcar #'string-space (quote ,args))))

(defclass formula-object (quail-object)
  ((literal :accessor literal-of :initarg :literal
            :documentation "string representing the formula.")
   (variable-table :accessor variable-table-of :initarg :variable-table
                   :initform (make-hash-table :test #'equal)
                   :documentation "hash-table of information ~
                                   about variables in formula."))
  (:documentation
   "The class of model formulae. ~
    (:see-also (model-formulae :topic))")
  )

;; for interactive debugging ... see additive-formula-semantics.lisp
(defmethod dump-vtable ((f formula-object))
  (dump-vtable (variable-table-of f)))

(defmethod print-object ((f formula-object) stream)
  (cond ((slot-boundp f 'literal)
         (format stream "#<~S ~S>"
                 (class-name (class-of f))
                 (literal-of f)))
        (t
         (format stream "#<~S ~S>"
                 (class-name (class-of f))
                 (qk::system-get-pointer f)))))
        
(defclass response-formula (formula-object)
  ((response :accessor response-of :initarg :response
             :documentation "name of response in table.")
   (predictor-semantics :accessor predictor-semantics-of
                        :initarg :predictor-semantics
                        :documentation "semantics of predictor, unreduced."))
  (:documentation 
   "The class of model formulae with an explicit response variable.")
  )

(defclass additive-formula (response-formula)
  ()
  (:documentation
   "The class of model formulae relating a response to an additive ~
    expression in the variables.")
  )

(defclass linear-formula (additive-formula)
  ((reduced-predictors :accessor reduced-predictors-of :initform nil)
   (reduced-variables :accessor reduced-variables-of :initform nil)
   (coding :accessor coding-of :initform nil))
  (:documentation 
   "The class of model formulae relating to non-adaptive ~
    predictor functions which are additive in individual ~
    predictors, and each predictor enters the formula as the predictor ~
    multiplied by a parameter.  The parameters are not specified ~
    explicitly, rather there is implicitly one associated with each ~
    additive term."))

(defmethod initialize-instance :after ((f additive-formula)
                                       &rest initargs)
  (declare (ignore initargs))
  (with-slots 
    (literal variable-table response predictor-semantics) f
    (vregister-intercept variable-table)
    (let* ((parse-tree (parse :infix literal 0)))
      (multiple-value-bind
        (resp pred)
        (additive-formula-semantics-1 parse-tree variable-table)
        (setf response resp)
        (setf predictor-semantics (add-intercept! pred)))))
  f)

(defmethod initialize-instance :after ((f linear-formula)
                                       &rest initargs)
  (declare (ignore initargs))
  (formula-encode f)
  f)

(defun add-intercept! (pred)
  (list 'f+ *intercept* pred))

(defun make-variable-order< (f)
  (with-slots (variable-table) f
    ;; This predicate is probably pretty slow ... could build a faster one.
    #'(lambda (x y)
        (< (vcount (vfind x variable-table))
           (vcount (vfind y variable-table))))))

(defun make-careful-variable-order< (f)
  (flet ((formula-variable-not-found (formula variable)
           (error "The variable ~S is not part of the formula ~S."
                  variable formula)))
    (with-slots (variable-table) f
      ;; This predicate is probably pretty slow ... could build a faster one.
      #'(lambda (x y)
          (< (vcount (or (vfind x variable-table) (formula-variable-not-found f x)))
             (vcount (or (vfind y variable-table) (formula-variable-not-found f y))))))))

(defmethod formula-encode ((f linear-formula))
  (with-slots (response
               predictor-semantics
               reduced-predictors
               reduced-variables
               coding) f
    (let ((vo< (make-variable-order< f))
          ipredictors)
      (setf reduced-predictors (f-reduce predictor-semantics
                                         :f< vo<))
      (setf reduced-variables (append (f-variables response)
                                      (f-variables reduced-predictors)))
      (setf ipredictors
            (mapcar #'(lambda (x)
                        (mapcar #'(lambda (xx)
                                    (position xx reduced-variables :test #'equal))
                                x))
                    (f-remove-ops reduced-predictors)))
      (setf coding (compute-coding ipredictors
                                   (length reduced-variables))))))

(defun compute-coding (predictors numvar)
  (let* ((numpred (length predictors))
         (coding (array nil
                        :dimensions (list numpred numvar)
                        :class 'ref-array))
         margin
         margin-present)
    ; (loop for i from 0 to (- numpred 1) do
    (do ((i 0 (incf i)))
        ((= i numpred))
      ; (loop for j from 0 to (- numvar 1) do 
      (do ((j 0 (incf j)))
          ((= j numvar))
        (when (member j (elt predictors i))
          (setf margin (remove j (elt predictors i)))
          ;; if margin is empty, we have a main effect
          (setf margin-present (not margin))
          ; (loop for ii from 0 to (- i 1) 
          ;     while (not margin-present) do
          (do ((ii 0 (incf ii)))
              ((or (= ii i) margin-present))
            (progn
              (setf margin-present 
                    (equal margin (elt predictors ii)))))
          (setf (eref coding i j) (if margin-present
                                    :contrasts
                                    :dummies)))))
    coding))

(defun dotted-term-position (dotted-term formula)
  (position dotted-term (rest (reduced-predictors-of formula))
            :test #'equal))

(defun same-coding-p (dotted-term formula1 formula2 &optional pos1 pos2)
  (or pos1 
      (setq pos1 (dotted-term-position dotted-term formula1)))
  (or pos2 
      (setq pos2 (dotted-term-position dotted-term formula2)))
  (flet ((pos-variables (dotted-term formula)
           (let ((reduced-variables (reduced-variables-of formula)))
             (mapcar #'(lambda (xx)
                         (position xx reduced-variables :test #'equal))
                     (rest dotted-term)))))
    (let ((vpos1 (pos-variables dotted-term formula1))
          (vpos2 (pos-variables dotted-term formula2)))
      (loop with same = T
            with coding1 = (coding-of formula1)
            with coding2 = (coding-of formula2)
            while same
            for vpos1i in vpos1
            as vpos2i in vpos2
            do (setq same (eq (eref coding1 pos1 vpos1i)
                              (eref coding2 pos2 vpos2i)))
            finally (return same)))))

(defun compare-dotted-terms-semantics (formula1 formula2 &key (warn T))
  ;; assumes that formula1 and formula2 have variables sorted in the same order
  (let* ((the-same T)
         (rp1 (reduced-predictors-of formula1))
         (rp2 (reduced-predictors-of formula2))
         (dotted-terms-in-both
          (loop with pos2 
                for term in (rest rp1)
                as pos1 upfrom 0
                when (and (dotted-term-p term)
                          (setq pos2 (position term (rest rp2) :test #'equal)))
                collect (list term pos1 pos2))))
    (loop for (dotted-term pos1 pos2) in dotted-terms-in-both
          when (not (same-coding-p dotted-term formula1 formula2 pos1 pos2))
          do (if warn
               (warn "Term ~S has potentially different semantics (and hence encoding) in ~
                      the two formulae ~S and ~S."
                     dotted-term formula1 formula2))
          (setq the-same NIL))
    the-same))


  

