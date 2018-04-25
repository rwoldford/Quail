(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(response-matrix 
          multinomial-categorical-counts binomial-categorical-counts
          categorical-proportion-matrix
          multinomial-cumulative-counts binomial-cumulative-counts
          cumulative-proportion-matrix
          binary-response-vector)))
          
(defclass response-matrix (matrix)
  ()
  (:documentation 
   "Subclasses of response-matrix represent response data in generalized ~
    linear/additive models.  Each is a matrix, with rows as observations ~
    and multinomial categories as columns.  Covariates are assumed to be in ~
    separate storage, associated with each row.")
)

;;; Classes for nominal data

(defclass multinomial-categorical-counts (response-matrix)
  ()
  (:documentation 
   "multinomial-categorical-counts are n x m matrices with n observations of ~
    m-nomial variables.  This subclass of response-matrix is used primarily to ~
    represent nominal (as opposed to ordinal) polytomous responses. ~
    The representation is that column i contains ~
    the count for category i, i = 0, ... , m-1.")
  )

(defclass binomial-categorical-counts (multinomial-categorical-counts)
  ()
  (:documentation 
   "binomial-categorical-counts are the special case of ~
    multinomial-categorical-counts for m=2.")
  )

#|
;; I don't think we really need these ... use binary-response-vector

(defclass bernoulli-categorical-counts (binomial-categorical-counts)
  ()
  (:documentation 
   "bernoulli-categorical-counts are a special case of binomial-categorical-counts. ~
    in which there are only two possible values ~
    in column 0 of any row: 0 or 1.  Column 1 is therefore constrained to have ~
    value 1 - column 0, and accordingly is optional.")
  )
|#

(defclass categorical-proportion-matrix (response-matrix)
  ()
  (:documentation
   "categorical-proportion-matrix is a numerical representation of the proportion of ~
    observations in each category of a nominal polytomous response.  ~
    For m categories, this matrix has (m-1) columns representing the proportion in ~
    category i, i=0, ..., m-2; 1 - (sum of all columns) gives ~
    the proportion in category (m-1)"
   )
)

;;; Classes for ordinal data

(defclass multinomial-cumulative-counts (response-matrix)
  ()
  (:documentation 
   "multinomial-cumulative-counts are n x m matrices with n observations of ~
    m-nomial variables.  This subclass of response-matrix is used primarily to ~
    represent ordinal (as opposed to nominal) polytomous responses. ~
    The representation is that column i contains ~
    the number of observations which are in categories <= i,  i = 0, ... , m-1. ~
    Accordingly, column (m-1) contains the total number of individuals for that ~
    row.")
  )

(defclass binomial-cumulative-counts (multinomial-cumulative-counts)
  ()
  (:documentation 
   "binomial-cumulative-counts are the special case of ~
    multinomial-cumulative-counts for m=2.")
  )

#|
;; I don't think we really need these ... use binary-response-vector

(defclass bernoulli-cumulative-counts (binomial-cumulative-counts)
  ()
  (:documentation
   "bernoulli-cumulative-counts are a special case of binomial-counts. ~
    bernoulli-cumulative-counts ~
    are special in that there are only two possible values ~
    in column 0 of any row: 0 or 1.  Column 1 is therefore constrained to have ~
    value 1, and accordingly is optional.")
  )
|#

(defclass cumulative-proportion-matrix (response-matrix)
  ()
  (:documentation
   "cumulative-proportion-matrix is a numerical representation of the proportion of ~
    observations in category of an ordinal polytomous response.  ~
    For m categories, this matrix has (m-1) columns representing the proportion in ~
    categories <= i, i=0, ..., m-2; the proportion in categories <= (m-1) is 1."
   )
)

;;; Class for categorical and binary data

(defclass categorical-response-vector (factor-array response-matrix)
  ())

(defclass binary-response-vector (categorical-response-vector)
  ()
  (:documentation
   "binary-response-vector is used to represent a univariate bernoulli ~
    (binary) response variable.  Since it is a subclass of factor-array, ~
    specification of the two 'levels' of the variable is can be made with arbitrary ~
    strings, symbols, or numbers.  If x is a binary-response-vector, ~
    (first (levels-of x)) is coded as 0.")
  )
  
(defmethod make-ref-array ((self binary-response-vector) dimensions &rest rest)
  (multiple-value-bind (levels extra)
                       (qk::interpret-keys-only rest
                                            '(:levels)
                                            'make-ref-array
                                            t)
    (if (> (length levels) 2)
      (error "Binary response vectors may have at most two levels, hence ~S ~
              is an illegal levels specifier." levels))
    (setf (levels-of self) levels)
    (apply #'call-next-method self dimensions extra)))

(defmethod update-instance-for-different-class :before 
           ((old matrix)
            (new binary-response-vector)
            &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value new 'levels) '(0 1)))

(defmethod print-object ((brv binary-response-vector) stream)
  (qk::print-dim-ref-object brv 'B stream)
  brv)

;;;  Conversions to (c/c)-proportion-matrix

(defgeneric convert-to-proportion-matrix (response-matrix)
  (:documentation
   "Converts response-matrix to a categorical-proportion-matrix, a ~
    cumulative-proportion-matrix, or a 0/1 (only) binary-response-vector ~
    for use by fitting procedures.  Returns two ~
    values: the proportion-matrix and the weight for each row (ie. total ~
    counts.")
)

(defmethod convert-to-proportion-matrix ((y t))
  (missing-method 'convert-to-proportion-matrix y))

(defmethod-multi convert-to-proportion-matrix ((y (multinomial-categorical-counts
                                                   matrix)))
  (let* ((dims (dimensions-of y))
         (num-categories (first (last dims)))
         (weight (.* y (make-list num-categories :initial-element 1)))
         (proportion-matrix (/ (ref y t (iseq 0 (- num-categories 2))) weight)))
    (change-class proportion-matrix 'categorical-proportion-matrix)
    (values proportion-matrix weight)))

(defmethod convert-to-proportion-matrix ((y multinomial-cumulative-counts))
  (let* ((dims (dimensions-of y))
         (num-categories (first (last dims)))
         (weight (ref y t (- num-categories 1)))
         (proportion-matrix (/ (ref y t (iseq 0 (- num-categories 2))) weight)))
    (change-class proportion-matrix 'cumulative-proportion-matrix)
    (values proportion-matrix weight)))

(defmethod convert-to-proportion-matrix ((y binary-response-vector))
  (let* ((dims (dimensions-of y))
         (levels (levels-of y))
         (num-obs (first dims))
         (weight (array 1 :dimensions (list num-obs) :class 'ones-array))
         (proportion-matrix (sel y)))
    (if (eq (length dims) 1)
      ; (loop for i from 0 to (1- num-obs) do
      (do ((i 0 (incf i)))
          ((= i num-obs))
        (setf (eref proportion-matrix i)
              (position (eref y i) levels :test #'equal)))
      ; (loop for i from 0 to (1- num-obs) do
      (do ((i 0 (incf i)))
          ((= i num-obs))
        (setf (eref proportion-matrix i)
              (position (eref y i 0) levels :test #'equal))))
    (values proportion-matrix weight)))
  



