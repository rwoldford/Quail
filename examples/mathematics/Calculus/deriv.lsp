;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;               Symbolic differentiation and simplification
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.

(in-package :quail-user)

;;;
;;;
;;;     In this file we consider some examples of the use of
;;;     symbolic differentiation in Quail.
;;;     In particular, the following functions are treated:
;;;
;;;      deriv
;;;      numerical-deriv
;;;      simplify
;;;      deriv-wrt
;;;     


;;;
;;;
;;;   DERIV
;;;

;;;
;;;   deriv is a generic function that has been specialized via methods
;;;   to handle a variety of arguments.
;;;   The intention is that deriv should take some mathematical argument
;;;   and return a similar result that represents the derivative of that
;;;   argument.
;;;   The derivative is taken with respect to the variable given as the
;;;   value of the keyword :wrt or, if this is not supplied, with respect to
;;;   whatever is returned by the generic function deriv-wrt when applied
;;;   to the first argument of deriv.
;;;
;;;   Some examples follow.
;;;

;;;   Symbols and numbers

(deriv 'x :wrt 'x)
(deriv 'x :wrt 'y)
(deriv 10 :wrt 'y)

;;;   Lists
(deriv '(* x x)
       :wrt 'x)

;;;   Lists with lambda
(deriv '(lambda (x y) (* x x (expt y 4)))
       :wrt 'x)

;;;   Lambda expressions (anonymous functions)
(deriv #'(lambda (x y) (* x x (expt y 4)))
       :wrt 'x)

;;;   Named functions
(defun foo (x y) (* x x (expt y 4)))
(deriv #'foo
       :wrt 'x)

;;;  Deriv can always be called on whatever it returns.

(deriv  (deriv '(* x x x)
               :wrt 'x)
        :wrt 'x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   More detailed examples on DERIV
;;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   differentiating list structure returns list structure
;;;

(setf f '(* x (+ x 3)))
(deriv f :wrt 'x)

;;;
;;;   Note that deriv can be called on anything returned by it.
;;;   For example, the second derivative of f is had by simply
;;;   differentiating twice.

(deriv (deriv f :wrt 'x) :wrt 'x)

;;;   Third and higher derivatives are zero for this example.
(deriv
 (deriv
  (deriv f :wrt 'x)
  :wrt 'x)
 :wrt 'x)

(deriv
 (deriv
  (deriv
   (deriv f :wrt 'x)
   :wrt 'x)
  :wrt 'x)
 :wrt 'x)

;;;  Of course this means that cross partial derivatives
;;;  may be had as well.
;;;

(setf g '(* x y (log x)))

(setf dgx (deriv g :wrt 'x))
(setf dgy (deriv g :wrt 'y))
(setf dgxy (deriv dgx :wrt 'y))
(setf dgyx (deriv dgy :wrt 'x))

;;;
;;;  Using list structure like this is very handy as it keeps the
;;;  symbolic representation intact.  
;;;
;;;  If x has a value then the derivative can be evaluated by evaluating
;;;  the list structure as in

(setf f '(* x (+ x 3)))
(setf x 5)
(setf df (deriv f :wrt 'x))
(eval df)

;;;  NOTE: Be wary of eval though! Eval insists on evaluation at the
;;;  `toplevel'.  Compare

(setf x 5)
(eval df)

;;; with
(let ((x 10000000))
  (eval df))

;;; Grrr.


;;;  It is generally more useful to have deriv return a function.
;;;  This can be had handing deriv a function.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Differentiating a function returns a function.
;;;

(defun f (x) (* x (+ x 3)))

(setf df (deriv #'f))

;;;
;;;  df is now an anonymous function that can be funcall'ed or apply'ed
;;;  like any other.

(funcall df 5)

;;;  Which will of course work properly within a let form

(setf x 5)
(let ((x 10000))
  (funcall df x))

;;;  Note that the answer to

(funcall df 5) 

;;;  is (likely to be) different from the one calculated previously!
;;;  Why?
;;;  Because f here was a function (typically already compiled and so no source
;;;  was available to the deriv function), a NUMERICAL derivative was calulated
;;;  instead.
;;;  Consequently, deriv returned a function which would calculate
;;;  the numerical derivative of the original function at any value given.
;;;

;;;  Sometimes this is not desired.
;;;  In Quail there are two solutions to this.
;;;
;;;  The first is to work with objects we call fn's in place of functions.
;;;  The second is to work directly with list structure and hand craft
;;;  functions as you need them.
;;;  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Differentiating an FN returns an FN
;;;
;;;  
;;;  Working with FNs.
;;;
;;;  FNS are objects that represent richer versions of functions.
;;;  (You might look at (help 'fn))
;;;
;;;  FNs are constructed in Quail in a manner entirely analogous to
;;;  functions.  In place of the defun macro, you use Quail's fn macro.
;;;  As in

(fn f (x) (* x (+ x 3)))

;;;  This does *two* things.  First it constructs a defun of f using the
;;;  body of the fn macro.  So it is very important to use fn only
;;;  as you would defun!
;;;  Second it constructs and returns an instance of the class fn.
;;;  Amongst other things an FN stores the source code that was used to
;;;  construct it as well as the compiled code.
;;;  It also has an empty slot for its derivative(s), should it (they) ever
;;;  be evaluated.

(setf f-fn (fn f (x) (* x (+ x 3))))
(describe f-fn)

;;;  Now differentiate
;;;
(setf df (deriv f-fn :wrt 'x))
(describe df)

;;;  And look at f-fn.  It now has df tucked away on its derivative list.
;;;
(describe f-fn)

;;;  Caching the results directly on f-fn means that we need only actually
;;;  determine the derivative once.


;;;
;;;  To invoke an FN on some arguments, the Quail function fn-call
;;;  is used.
;;;  FN-CALL behaves very much like the CL function FUNCALL
;;;  except that it works on both functions like #'f and on FNs
;;;  like f-fn.
;;;

(fn-call #'f 3)
(fn-call f-fn 3)

;;;  And because fn-call is a function, it can be applied as in
;;;

(apply #'fn-call f-fn '(3))

;;;  which is sometimes useful when the number of arguments to f-fn is unknown
;;;  but can be gathered up in a list.
;;;
;;;  FNs together with FN-CALL are well suited to applications where arbitrary
;;;  derivatives need to be calculated and symbolic differentiation is
;;;  preferred over numerical differentiation.
;;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Hand crafting functions from list structure.
;;;
;;;  This is the second method for building functions using the
;;;  symbolic derivatives instead of numerical ones.
;;;  It requires more book-keeping from the user and so is generally more
;;;  troublesome to use.
;;;  However, it does avoid the overhead involved in constructing FNs and
;;;  so may be preferable in some situations.
;;;
;;;  A single example should do.
;;;
;;;  Suppose we have the list structure 
;;;

(setf f '(lambda (x) (* x (+ x 3))))

;;;  And take its derivative

(setf df (deriv f :wrt 'x))

;;;
;;;  The simplest way to turn this into a function that can be funcall'ed
;;;  is to use the backquote operator.

(setf fun-df (eval `(function ,df)))
;;;
;;;  Then the value of fun-df is a function and can be funcalled as in

(funcall fun-df 5)

;;;
;;;  Should further symbolic derivatives be needed, they would be constructed
;;;  from df and not fun-df.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;    NUMERICAL DERIVATIVES
;;;
;;;
;;;  Should you want to calculate the numerical derivative of a function
;;;  at a particular point, you can do so directly with numerical-deriv
;;;

(defun f (x) (/ (* x x x) (- x 5)))

(numerical-deriv #'f :x 4)

;;;  Compare this to the symbolic result:

(setf x 4)
(eval (deriv '(/ (* x x x) (- x 5)) :wrt 'x))

;;;
;;;  The keyword x identifies the point at which the function is to be evaluated.
;;;

;;;
;;;  Another keyword parameter is :eps which specifies
;;;  the size of the region around x to be used in the calculation
;;;  x - eps to x + eps.
;;;  The function is approximated by a cubic polyionomial in this region.
;;;  Which region is best will depend on the function f and the point x
;;;  but often smaller regions work better.

(numerical-deriv #'f :x 4 :eps .01)
(numerical-deriv #'f :x 4 :eps .0000000001)
(numerical-deriv #'f :x 4 :eps 10)
(numerical-deriv #'f :x 4 :eps .5)







