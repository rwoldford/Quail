;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               new-terms.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1993.
;;;     Greg Anglin 1995.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(drop add
          ;; fit-nested-models
          )))

(defgeneric drop (object-with-formula formula-fragment 
                                      &rest keys &key &allow-other-keys))

(defgeneric add  (object-with-formula formula-fragment 
                                      &rest keys &key &allow-other-keys))


;; Add do-nothing primary on drop FEB 04 1998
(defmethod drop (thing formula-fragment &rest keys &key &allow-other-keys)
   (declare (ignore thing formula-fragment keys))
   (call-next-method))

(defmethod drop :around (thing formula-fragment &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (if (null formula-fragment)
    thing
    (call-next-method)))

#|
;; a version somewhere between rwo's and dga's (current) version
;;
(defmethod drop ((formula linear-formula) formula-fragment &key)
  (let*
    ((variable-order< (make-careful-variable-order< formula))
     (old-preds (reduced-predictors-of formula))
     (response (response-of formula))
     (formula-class-name (class-name (class-of formula)))
     (temp-formula
      (make-instance formula-class-name
        :literal
        (concatenate 'string  response " ~ " formula-fragment)))
     (dropped-preds (f-reduce (reduced-predictors-of temp-formula)
                              :f< variable-order<))
     (new-preds
      (cons (first old-preds)
            (cl::sort
             (set-difference
              old-preds dropped-preds
              :test #'equal)
             variable-order<)))
     (new-formula-string
      (concatenate 'string  response " ~ "
                   (let
                     ((result (gather-reduced-predictors new-preds)))
                     (if (string-equal "" result)
                       "1"
                       result)))))
     (make-instance 'linear-formula :literal new-formula-string)
     )
    )
|#

;; a less intuitive but more direct version of drop for linear-formula
;; (relative to commented one above) 
;; also fixes some bugs
;;
;; won't handle e.g. dropping "x[t 3]" from  "y ~ x",
;; since formula-reducing code can't deal with "y ~ x - x[t 3]".
;;
(defmethod drop ((formula linear-formula) formula-fragment 
                 &rest keys &key (semantics-warning T) &allow-other-keys)
  (declare (ignore keys))
  (let*
    ((remove-intercept NIL)
     (variable-table (variable-table-of formula))
     (variable-order< (make-careful-variable-order< formula))
     (old-preds (reduced-predictors-of formula))
     (response (response-of formula))
     (dropped-preds (f-reduce (list 'f+
                                    (additive-formula-semantics-1
                                     (parse :infix formula-fragment 0)
                                     variable-table))
                              :f< variable-order<))
     (new-preds
      (cons 'f+
            (let ((new-preds (copy-list (rest old-preds))))
              (loop for dp in (rest dropped-preds)
                    do 
                    (when (equal dp "1")
                      (setq remove-intercept T))
                    (if (find dp new-preds :test #'equal)
                      (setq new-preds (delete dp new-preds :test #'equal))
                      (error "Can't find dropped predictor ~S in formula ~S."
                             dp formula)))
              new-preds)))
     (new-formula-string
      (concatenate 'string response " ~ "
                   (let ((result (gather-reduced-predictors new-preds)))
                     (if (string-equal "" result)
                       (if remove-intercept
                         (error "Null formula results from dropping intercept in formula ~S."
                                formula)
                         "1")
                       (if remove-intercept
                         (concatenate 'string "-1 + " result)
                         result)))))
     (new-formula 
      (make-instance 'linear-formula 
        :literal new-formula-string 
        :variable-table variable-table)))
    (values new-formula
            (compare-dotted-terms-semantics formula new-formula :warn semantics-warning))
    )
  )

(defmethod drop ((model generalized-linear-model) formula-fragment
                 &rest keys &key &allow-other-keys)
  (multiple-value-bind (new-formula semantics-ok)
                       (apply #'drop (formula-of model) formula-fragment keys)
    (values
     (model (class-of model)
            new-formula
            :link (link-of model)
            :family (family-of model)
            :weight-fn (weight-fn-of model)
            :offset (offset-of model))
     semantics-ok)))

(defmethod drop ((fit generalized-linear-model-fit) formula-fragment
                 &rest keys &key &allow-other-keys)
  (multiple-value-bind (new-model semantics-ok)
                       (apply #'drop (model-of fit) formula-fragment keys)
    (values
     (fit new-model 
          (data-frame-of fit)
          :weight (weight-of fit)
          :relevant-model-matrix (model-matrix-of fit))
     semantics-ok)))

;;; Add do-nothing primary on add FEB 04 1998
(defmethod add (thing formula-fragment &rest keys &key &allow-other-keys)
   (declare (ignore thing formula-fragment keys))
   (call-next-method))

(defmethod add :around (thing formula-fragment &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (if (null formula-fragment)
    thing
    (call-next-method)))

(defmethod add ((formula linear-formula) formula-fragment &rest keys 
                &key (semantics-warning T) &allow-other-keys)
  (declare (ignore keys))
  (let*
    ((new-variable-table (loop with new-variable-table = (make-hash-table :test #'equal)
                               for key being each hash-key of (variable-table-of formula)
                               using (hash-value value)
                               do (setf (gethash key new-variable-table) value)
                               finally (return new-variable-table)))
     (old-preds (reduced-predictors-of formula))
     (response (response-of formula))
     (added-preds (additive-formula-semantics-1
                   (parse :infix formula-fragment 0)
                   new-variable-table))
     (variable-order< #'(lambda (x y)
                          (< (vcount (vfind x new-variable-table))
                             (vcount (vfind y new-variable-table)))))
     (new-preds (f-reduce (append old-preds  ;; old-preds includes leading 'f+.
                                  (list added-preds))
                          :f< variable-order<))
     (new-formula-string
      (concatenate 'string  response " ~ " (gather-reduced-predictors new-preds)))
     (new-formula 
      (make-instance 'linear-formula 
        :literal new-formula-string 
        :variable-table new-variable-table)))
    (values new-formula
            (compare-dotted-terms-semantics formula new-formula :warn semantics-warning))
    )
  )

(defmethod add ((model generalized-linear-model) formula-fragment
                &rest keys &key &allow-other-keys)
  (multiple-value-bind (new-formula semantics-ok)
                       (apply #'add (formula-of model) formula-fragment keys)
    (values
     (model (class-of model)
            new-formula
            :link (link-of model)
            :family (family-of model)
            :weight-fn (weight-fn-of model)
            :offset (offset-of model))
     semantics-ok)))

(defmethod add ((fit generalized-linear-model-fit) formula-fragment
                 &rest keys &key &allow-other-keys)
  (multiple-value-bind (new-model semantics-ok)
                       (apply #'add (model-of fit) formula-fragment keys)
    (values
     (fit new-model 
          (data-frame-of fit)
          :weight (weight-of fit)
          :relevant-model-matrix (model-matrix-of fit))
     semantics-ok)))

(defun gather-reduced-predictors (preds)
  (if (listp preds)
    (cond
     ((= (length preds) 1) "")
     ((= (length preds) 2) (gather-reduced-predictors (cadr preds)))
     ((eq (first preds) 'F+)
      (concatenate 'string (gather-reduced-predictors (cadr preds))
                   " + " (gather-reduced-predictors (cons 'F+ (cddr preds)))))
     ((eq (first preds) 'F.)
      (concatenate 'string
                   (gather-reduced-predictors (cadr preds))
                   "."
                   (gather-reduced-predictors (cons 'F. (cddr preds)))))
     ((eq (first preds) 'F-)
      (concatenate 'string (gather-reduced-predictors (cadr preds))
                   " - " (gather-reduced-predictors (cons 'F- (cddr preds)))))
     (preds (format NIL "~a" preds))
     (T ""))
    (string preds)))

#|
;; rwo code which I haven't looked at since modifying drop/add ... dga 950704

(defgeneric list-nested-terms (modelling-thing)
  (:documentation "Returns a list of dotted pairs. ~
                   The list is ordered from smallest to largest nested model. ~
                   Not all possible nested models are included.  Rather the ~
                   ordering depends on that found in the given argument.  ~
                   The first of each pair is a description of the terms included ~
                   in that nested model.  The second describes ~
                   the term that was dropped from the next largest model to ~
                   arrive at the associated nested model."))

(defmethod list-nested-terms ((fit fit-object))
  (list-nested-terms (model-of fit)))

(defmethod list-nested-terms ((model response-model))
  (list-nested-terms (formula-of model)))

(defmethod list-nested-terms ((formula linear-formula))
  (let*
    ((preds (reduced-predictors-of formula))
     (full-model (gather-reduced-predictors preds))
     (nested-models (list (cons full-model NIL)))
     )
    
    (loop while (not (equal preds '(F+)))
          do
          (push (cons (gather-reduced-predictors (butlast preds))
                      (gather-reduced-predictors (car (last preds))))
                nested-models)
          (setf preds (butlast preds))
          )
    (rest nested-models)))

(defmethod fit-nested-models ((fit generalized-linear-model-fit))
  (let ((nested-models (cdr (reverse (list-nested-terms fit))))
        (current-fit fit))
    (loop for model in nested-models
          collect
          (let ((model-formula (car model))
                (term (cdr model)))
            (inform-user (format NIL "Now fitting ~a ~%Patience please ..." model-formula))
            (setf current-fit (drop current-fit term))
            (list term model-formula current-fit)))))
|#
