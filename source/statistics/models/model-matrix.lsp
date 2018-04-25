;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           model-matrix.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1992.
;;;     R.W. Oldford 1995.
;;;
;;;--------------------------------------------------------------------------------
;;;

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(model-matrix)))

;; a first shot at creating a model matrix from the model frame

(defclass model-matrix (matrix)
  ((formula :accessor formula-of :initarg :formula
            :documentation "an object of class formula.")
   (predictor-columns :accessor predictor-columns-of
                      :initform nil)
   (factor-contrasts :accessor factor-contrasts-of :initform nil)))

(defmethod compute-model-matrix ((fit response-model-fit) relevant-model-matrix)
  (with-slots
    (model model-frame model-matrix response-matrix)
              fit
    (let ((identifiers (list-identifiers model-frame)))
      (with-slots (formula) model
        (setf response-matrix (eref model-frame (response-of formula)))
        (dataset response-matrix
                 :identifiers identifiers
                 :variates (list (response-of formula)))
        (setf model-matrix (make-instance 'model-matrix :formula formula))
        (with-slots (predictor-columns
                     factor-contrasts
                     (dimensions qk::dimensions)
                     (contents qk::ref-contents)) model-matrix
          (setf factor-contrasts (make-sequence 'list
                                                (second 
                                                 (dimensions-of
                                                  (coding-of formula)))))
          (setf predictor-columns (compute-predictor-columns (coding-of formula)
                                                             factor-contrasts
                                                             model-frame))
          (setf dimensions (list (data-size model-frame)
                                 (apply #'cl:+
                                        (mapcar #'(lambda (x)
                                                    (apply #'cl:* x))
                                                predictor-columns))))
          (setf contents (make-array dimensions :initial-element nan))
          (fill-model-matrix fit relevant-model-matrix))
        )
      (dataset model-matrix :identifiers identifiers)
      )
      model-matrix))

(defun compute-predictor-columns (coding factor-contrasts model-frame)
  ;; Here I rely on the columns of the coding corresponding exactly
  ;; to those on the model-matrix ... this should be the case right now.
  (let ((dims (dimensions-of coding))
        (data (data-of model-frame)))
    (loop for i from 0 to (cl:- (first dims) 1)
          collect 
          (loop for j from 0 to (cl:- (second dims) 1)
                collect 
                (prog1
                  (coding-columns (eref coding i j)
                                  (eref data j)
                                  factor-contrasts j)
                  ;;(format *terminal-io* "~&Matrix for i=~S j=~S:~%" i j)
                  ;;(qk::print-2d factor-contrasts)
                  )))))

(defun dummy-delta (ii jj)
  (if (eq ii jj)
    1
    0))

#|
(defun coding-columns (coding-ij data-j factor-contrasts j)
"Used when determining the dimensions of the model-matrix.  ~
Produces the number of columns this variable (j) will contribute (multiplicatively) ~
to the columns for the current predictor (i) .  As a side effect, calculates and ~
caches the factor-contrasts for variables which appear in the model as factors."
  (cond ((null coding-ij) 1)
        ((typep data-j 'factor-object)
         (case coding-ij
           (:contrasts 
            (progn
              (or (elt factor-contrasts j)
                  (setf (elt factor-contrasts j) (contrasts data-j)))
              (cl:- (num-levels data-j) 1)))
           (:dummies
            (num-levels data-j))))
        (t (second (matrix-dimensions-of data-j)))))
|#
;;;

(defgeneric coding-columns (coding-ij data-j &optional factor-contrasts j)
  (:documentation 
   "Used when determining the dimensions of the model-matrix.  ~
    Produces the number of columns this variable (j) will contribute (multiplicatively) ~
    to the columns for the current predictor (i).  A very important side effect is the coding ~
    of a predictor (eg 'A.B') when bothe 'A' and 'B' are factors (ie. factor-arrays)."
   ))

(defmethod coding-columns ((coding-ij null) data-j &optional factor-contrasts j)
  "This method represents the case where the variate does not contribute to the predictor."
  (declare (ignore data-j factor-contrasts j))
  1)

(defmethod coding-columns ((coding-ij (eql :contrasts)) (data-j t) &optional factor-contrasts j)
  "This is the most common method for contribution of continuous covariates to the predictor."
  (declare (ignore factor-contrasts j))
  (second (matrix-dimensions-of data-j)))

(defmethod coding-columns ((coding-ij (eql :contrasts)) (data-j factor-array) &optional factor-contrasts j)
  "This method, as a side-effect, calculates and ~
   caches the factor-contrasts for variables which appear in the model as factors. ~
   Applies to cases of coding-ij = 'A.B' and data-j = 'A' or 'B'."
  (or (elt factor-contrasts j)
      (setf (elt factor-contrasts j) (contrasts data-j)))
  (cl:- (num-levels data-j) 1))

(defmethod coding-columns ((coding-ij (eql :dummies)) (data-j factor-array) &optional factor-contrasts j)
  "This method handles the case for 'B within A', where 'B' is coded separately for each level of 'A'. ~
   Here the argument coding-ij will refer to 'B within A', and data-j will be 'A'."
  (declare (ignore factor-contrasts j))
  (num-levels data-j))

;;;

#|
(defun encode-the-observation (coding-ij
                               data-j
                               obsnum
                               variable-coding-index
                               factor-contrasts-j)
"Used when filling the model matrix.  Returns a multiplicative ~
contribution of observation obsnum of this variable (j) to that column of the ~
current predictor (i) conceptually indexed by variable-coding-index." 
  (cond ((null coding-ij) 1)
        ((typep data-j 'factor-object)
         (with-slots (levels) data-j
           (let ((level (position (eref data-j obsnum) levels :test #'equal)))
             (case coding-ij
               (:contrasts 
                (eref factor-contrasts-j level variable-coding-index))
               (:dummies
                (dummy-delta level variable-coding-index))))))
        (t (eref data-j obsnum variable-coding-index))))
|#

(defgeneric encode-the-observation (coding-ij
                                    data-j
                                    obsnum
                                    variable-coding-index
                                    factor-contrasts-j)
  (:documentation
   "Used when filling the model matrix.  Returns a multiplicative ~
    contribution of observation obsnum of this variable (j) to that column of the ~
    current predictor (i) conceptually indexed by variable-coding-index."
   ))

(defmethod encode-the-observation ((coding-ij null)
                                    data-j
                                    obsnum
                                    variable-coding-index
                                    factor-contrasts-j)
  (declare (ignore data-j obsnum variable-coding-index factor-contrasts-j))
  1)

(defmethod encode-the-observation ((coding-ij (eql :contrasts))
                                   (data-j t)
                                   obsnum
                                   variable-coding-index
                                   factor-contrasts-j)
  (declare (ignore factor-contrasts-j))
  (eref data-j obsnum variable-coding-index))

(defmethod encode-the-observation ((coding-ij (eql :contrasts))
                                   (data-j factor-array)
                                   obsnum
                                   variable-coding-index
                                   factor-contrasts-j)
  (with-slots (levels) data-j
    (let ((level (position (eref data-j obsnum) levels :test #'equal)))
      (eref factor-contrasts-j level variable-coding-index))))

(defmethod encode-the-observation ((coding-ij (eql :dummies))
                                   (data-j factor-array)
                                   obsnum
                                   variable-coding-index
                                   factor-contrasts-j)
  (declare (ignore factor-contrasts-j))
  (with-slots (levels) data-j
    (let ((level (position (eref data-j obsnum) levels :test #'equal)))
      (dummy-delta level variable-coding-index))))
        

(defun fill-model-matrix (fit &optional relevant-model-matrix)
  ;;
  ;; egad !!  But I couldn't see a way to split it up ...
  ;;
  ;; NOTE: the "relevant-model-matrix" must have a formula with variables sorted in the
  ;; same order as in model-matrix's formula.  This'll happen OK with drop/add.
  ;;
  (with-slots (model-frame model-matrix) fit
    (with-slots (data) model-frame
      (with-slots (factor-contrasts
                   predictor-columns
                   formula
                   (mm-dims qk::dimensions)) model-matrix
        (with-slots (coding reduced-predictors) formula
          (let 
            ((numobs (first mm-dims))
             (numpreds (first (dimensions-of coding)))
             (numvars (second (dimensions-of coding)))
             (first-contributing-var
              ;;  Hop over the response and the intercept; they only
              ;;  contribute a multiplicative factor of 1. 
              ;;  Relies on model-frame having response in pos 0,
              ;;  and intercept in pos 1, if present.
              (if (typep (elt data 1) 'ones-array) 2 1))
             (current-column 0)
             variable-coding-max-indices
             variable-coding-indices
             relevant-predictors
             relevant-predictor-columns
             relevant-formula
             rpos
             )
            (when relevant-model-matrix
              (setq relevant-formula (formula-of relevant-model-matrix)
                    relevant-predictors (rest (reduced-predictors-of relevant-formula))
                    relevant-predictor-columns (predictor-columns-of relevant-model-matrix)))
            (loop
              for prednum from 0 to (cl:- numpreds 1)
              as pred in (rest reduced-predictors)
              do
              (if (and relevant-model-matrix
                       (setq rpos (position pred relevant-predictors :test #'equal))
                       (or (not (dotted-term-p pred))
                           (same-coding-p pred formula relevant-formula prednum rpos)))
                ;;
                ;; in this case, the relevant-model-matrix has an identically coded predictor as
                ;; model-matrix will have.  So, copy it verbatim from there.
                ;;
                ;; NOTE:  This code relies on (eq (elt relevant-predictor-columns rpos)
                ;;                                (elt predictor-columns prednum))
                ;; 
                (let* ((relevant-start-column (loop for i upto (- rpos 1)
                                                    sum (elt relevant-predictor-columns i)))
                       (relevant-end-column (+ relevant-start-column (elt relevant-predictor-columns rpos) -1))
                       end-column)
                  ;; (format T "~&BINGO: ~S" pred)
                  (setf (elt predictor-columns prednum)
                        (apply #'cl:* 
                               (elt predictor-columns prednum)))
                  (setq end-column (+ current-column
                                      (elt predictor-columns prednum) 
                                      -1))
                  (setf (ref model-matrix T (iseq current-column end-column))
                        (ref relevant-model-matrix T (iseq relevant-start-column relevant-end-column)))
                  (setq current-column (1+ end-column)))
                (progn
                  (setf variable-coding-max-indices
                        (elt predictor-columns prednum))
                  (setf (elt predictor-columns prednum)
                        (apply #'cl:* 
                               (elt predictor-columns prednum)))
                  (setf variable-coding-indices
                        (make-sequence 'list
                                       (length variable-coding-max-indices)
                                       :initial-element 0))
                  ; (loop for count from 1 to (elt predictor-columns prednum) do
                  (do ((count 1 (incf count)))
                      ((> count (elt predictor-columns prednum)))
                    (progn
                      ; (loop for obsnum from 0 to (cl:- numobs 1) do
                      (do ((obsnum 0 (incf obsnum)))
                          ((> obsnum (cl:- numobs 1)))
                        (setf 
                         (eref model-matrix obsnum current-column)
                         (apply #'* 
                                (loop for var 
                                      from first-contributing-var
                                      to (cl:- numvars 1)
                                      collect 
                                      (encode-the-observation
                                       (eref coding prednum var)
                                       (eref data var)
                                       obsnum
                                       (eref variable-coding-indices var)
                                       (eref factor-contrasts var)
                                       )))))
                      (setf 
                       variable-coding-indices
                       (qk::column-major-next-subscript
                        variable-coding-indices
                        variable-coding-max-indices
                        ))
                      (incf current-column))))))))))))
