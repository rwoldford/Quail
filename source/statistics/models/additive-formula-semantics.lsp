(in-package :quail)

#|

;; some documentation

"additive-formula-semantics is responsible for taking the tree produced
by parse (which should really have a more specific name), interpreting
it into 

1. references to variables or functions of variables, which are
   subsequently stored on vtable (which is variable-table slot of the
   formula-object)
2. the semantics of the formula, in a Polish (ie. lisp-like) notation
   (which ends up as predictor-semantics slot of formula-object)

This process does NOT depend on the
particular data set of interest.  It is the responsibility of the
model fitting code to further interpret vtable in the context of the
particular model and data.

There are two basic types of entries in a vtable (which is a hash table):
direct and aliased. Both are keyed by strings.  The value of an aliased
entry is a string which is the key to another entry; if aliased entries
are followed recursively, eventually they will key a direct entry.
The value of a direct entry is a list, with elements:

0:   the number of entries before this one in the hash table 
     (provides an ordering on the
     position of first appearance of the variable in the formula)
1:   an operator, which is one of VINTERCEPT, VREF1, VREF2, VREF3, or VREF4
2:   arguments to the operator

Each of the VREFs has a different meaning when it comes to building a
model frame (which see) from a dataset; the intent is

VINTERCEPT    just provide a column of 1's
VREF1         a variable, say \"x\"
VREF2         an indexed variable, say \"z[t 3]\"
VREF3         a function of variable(s), say \"foo(x, z[t 3])\"
VREF4         a lisp operation on the variable(s) \"{(/ z[t 2] z[t 1])}\"

Sometimes the function for VREF3 will be special, and refer to a process
rather than a fixed function.  For example, for generalized additive
models, \"s(x)\" will mean an adaptive smoothing spline fit of the
partial residuals of the response and other fitted terms to x.
(See Hastie and Tibshirani, Generalized Additive Models,
Chapman and Hall, 1990).

The current implementation of the VREFs is in model-frame.lisp.

EXAMPLE:
To see this in action, evaluate the following form:
(let ((f (make-instance 'q::additive-formula
           :literal \"y ~ x + z[t 3] + foo(x) + {(/ z[t 2] z[t 1])}\"))
      )
  (inspect f)
  (q::dump-vtable (q::variable-table-of f))
  f)
"

;; Note that the above EXAMPLE is inside a string ... if you are
;; reading *this* message, then you'll need to change each \" to "
;; in the example form.

|#

(defun additive-formula-semantics-1 (tree vtable)
  (additive-formula-semantics-2 (kind-of tree) tree vtable))

;; This next method is reponsible for the two values ultimately returned
;; to initialize-instance :after for additive-formula

(defmethod additive-formula-semantics-2 ((kind (eql :formula)) tree vtable)
  (let ((s (subtrees-of tree)))
    (values
     (additive-formula-semantics-1 (first s) vtable)
     (additive-formula-semantics-1 (second s) vtable))))

(defmethod additive-formula-semantics-2 ((kind (eql :whole-number)) tree vtable)
  (declare (ignore vtable))
  (node-of tree))

(defmethod additive-formula-semantics-2 ((kind (eql :lisp-read)) tree vtable)
  (declare (ignore vtable))
  (node-of tree))

(defmethod additive-formula-semantics-2 ((kind (eql :arg-number)) tree vtable)
  (declare (ignore vtable))
  (node-of tree))

(defmethod additive-formula-semantics-2 ((kind (eql :arg-keyword)) tree vtable)
  (declare (ignore vtable))
  (node-of tree))

(defmethod additive-formula-semantics-2 ((kind (eql :variable)) tree vtable)
  (if (equal (node-of tree) "1")
    (vregister-intercept vtable)  ;; not really necessary, it's already there
    (vregister (node-of tree)
               'vref1
               (list (node-of tree))
               vtable)))

(defmethod additive-formula-semantics-2 ((kind (eql :null)) tree vtable)
  (declare (ignore vtable tree))
  (error "Semantic error during formula interpretation. ~
          ~&Encountered null where a variable was expected."))

(defmethod additive-formula-semantics-2 ((kind (eql :number)) tree vtable)
  (if (eq (node-of tree) 1)
    (vregister-intercept vtable)
    (error "Semantic error during formula interpretation. ~
            ~&Encountered the number ~S where a variable was expected."
           (node-of tree))))

(defun name-and-args (tree vtable)
  ;; for special types of trees :vble-select, :function, :lisp-op
  (list* (node-of (first (subtrees-of tree)))
         (loop for arg in (subtrees-of (second (subtrees-of tree)))
               collect (additive-formula-semantics-2
                        ;; this allows a number or a keyword inside () and [] in formulae, but
                        ;; not as a :term
                        (case (kind-of arg)
                          (:number :arg-number)
                          (:keyword :arg-keyword)
                          (t (kind-of arg)))
                        arg
                        vtable))))

(defmethod additive-formula-semantics-2 ((kind (eql :vble-select)) tree vtable)
  (vregister (node-of tree)
             'vref2
             (name-and-args tree vtable)
             vtable))

(defmethod additive-formula-semantics-2 ((kind (eql :function)) tree vtable)
  (vregister (node-of tree)
             'vref3
             (name-and-args tree vtable)
             vtable))

(defmethod additive-formula-semantics-2 ((kind (eql :lisp-op)) tree vtable)
  (vregister (node-of tree)
             'vref4
             (name-and-args tree vtable)
             vtable))

(defmethod additive-formula-semantics-2 ((kind (eql :lisp)) tree vtable)
  (let* ((lisp-op (second (subtrees-of tree)))
         (name (first (subtrees-of tree)))
         (expansion (additive-formula-semantics-2 :lisp-op lisp-op vtable)))
    (if (eq (kind-of name) :variable)
      (valias (node-of name) expansion vtable)
      expansion)))

(defun additive-formula-infix-semantics (fsymbol tree vtable)
  (let ((s (subtrees-of tree)))
    (list fsymbol
          (additive-formula-semantics-1 (first s) vtable)
          (additive-formula-semantics-1 (second s) vtable))))

(defmethod additive-formula-semantics-2 ((kind (eql :expstar)) tree vtable)
  (additive-formula-infix-semantics 'f** tree vtable))

(defmethod additive-formula-semantics-2 ((kind (eql :star)) tree vtable)
  (additive-formula-infix-semantics 'f* tree vtable))

(defmethod additive-formula-semantics-2 ((kind (eql :slash)) tree vtable)
  (additive-formula-infix-semantics 'f/ tree vtable))

(defmethod additive-formula-semantics-2 ((kind (eql :dot)) tree vtable)
  (additive-formula-infix-semantics 'f. tree vtable))

(defmethod additive-formula-semantics-2 ((kind (eql :terms)) tree vtable)
  (additive-formula-infix-semantics 'f+ tree vtable))

(defmethod additive-formula-semantics-2 ((kind (eql :remove)) tree vtable)
  ;; this'll catch the special case of -1, and leaves it for 
  ;; f-reduce to deal with properly
  (if (eq (kind-of (first (subtrees-of tree))) :null)
    (list 'f- 
          (additive-formula-semantics-1 (second (subtrees-of tree))
                                        vtable))
    (additive-formula-infix-semantics 'f- tree vtable)))

(defmethod additive-formula-semantics-2 ((kind (eql :remove-star)) tree vtable)
  (additive-formula-infix-semantics 'f-* tree vtable))

(defmethod additive-formula-semantics-2 ((kind (eql :remove-slash)) tree vtable)
  (additive-formula-infix-semantics 'f-/ tree vtable))

;------------------------------------------------------------------------------

;;; stuff to do with variable hash tables

(defmacro vcount (vval)
  `(first ,vval))

(defmacro vget (vval)
  `(second ,vval))

(defmacro vargs (vval)
  `(third ,vval))

(defmethod dump-vtable ((vtable hash-table))
  (qk::dump-hash-table vtable))

(defun valias (new-ident old-ident vtable)
  (if (not (string= new-ident old-ident))
    (setf (gethash new-ident vtable) old-ident))
  new-ident)

(defun vregister (parse vget args vtable)
  (let ((ident (format nil "(~S~{ ~S~})" vget args))
        (count (hash-table-count vtable)))
    (if (not (gethash ident vtable))
      (setf (gethash ident vtable) (list count vget args)))
    (valias parse ident vtable)))

(defun vregister-intercept (vtable)
  (declare (special *intercept*))
  (vregister *intercept*
             'vintercept
             nil
             vtable))

(defun vfind (ident vtable &key (recurs t))
  (let ((v (gethash ident vtable)))
    (if (and recurs (stringp v))
      (multiple-value-bind (vv idents)
                           (vfind v vtable :recurs recurs)
        (values vv (list* ident idents)))
      (values v (list ident)))))

        


