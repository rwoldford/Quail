;;; -*- File: infix.lisp,  Mode: LISP, Syntax: MCL 2.0, Package: "QUAIL"

(in-package :quail)

;; Alterations: M.E.Lewis.
;; My thanks to the author.

;; Some Lisp programers who have experienced FORTRAN or C programming
;; often complain that Lisp is weak at mathematical formula expression.
;; Here is a read macro program which accepts C-like infix formulae
;; and translates them into prefixed lisp forms.
;; 
;; (1 + 2 * 3 / 4.0)  is expanded to something like
;; (+ 1 (/ (* 2 3) 4.0) and 2.5 is resulted.
;; 
;; Fuction call and array reference are also handled.
;; (sin(x) + a[1])  --> (+ (sin x) (sel a 1))
;; 
;; Also, a simple optimization to reduce function call and array 
;; reference is performed.
;; ((sin(x) + cos(x)) / (sin(x) - cos(x)))
;; --> (let* ((t1 (sin x)) (t2 (cos x))) (/ (+ t1 t2) (- t1 t2)))
;; For this optimization, all the expressions are assumed to have
;; no side-effect.
;; 
;; Assignment and relative operator are also available.
;; (a != b)     --> (/= a b)
;; (a <- x + 1)  --> (setf a (+ x 1))
;; (a[0] <- (x + y + z) )	--> (setf (sel a 0) (+ x y z))
;; 
;; Note that every factor or operator needs to be delimited by spaces,
;; since terms like '2*a' and 'a+pi' are valid symbols for Lisp although they
;; are recognized three different symbols by C.
;; 
;; This program was developed on euslisp, which is a subset of CommonLisp
;; and has lots of extended features for three-dimensional solid modeling,
;; especially for robot programming.  However, I have confirmed this program
;; correctly works in KCL environments both on our sun3 and sun4.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	mathtran.l
;;;;	convert C-like arithmethic expressions into lisp notation.
;;;;	1987-Sep
;;;;	Copyright (c) 1987,
;;;;	Toshihiro MATSUI, Electrotechnical Laboratory
;;;;	Tsukuba-city, Ibaraki, 305 JAPAN
;;;;	matsui%etl.jp@relay.cs.net

(defmacro infix-while (cond &rest body)
   `(do ()
        ((not ,cond))
	. ,body))

(defun infix-memq (x l) (member x l :test #'eq))

(defun expression (exp &optional (lhs nil) &aux result letvar-alist)
  (labels
     (
      (letvar (form)
	   (let ((v (assoc form letvar-alist :test #'equal)))
		(cond (v (incf (third v)) (second v))
		      (t (setf v (gensym))
			 (push (list form v 1) letvar-alist)
			 v))))
      (factor1 (exp)
	(let* ((sy (pop exp)) (arglist) form)
	   (cond
	     ((consp sy)
		(multiple-value-setq (sy form) (expr sy))
		(if form (error "illegal math expression for infix macro"))
		(values sy exp))
	     ((consp (first exp))	;function call or array ref.
		(setf arglist (pop exp))
		(cond ((eq (first arglist) 'sel)
		        (setf arglist (expr-list (rest arglist)))
			(setf form (cons 'sel (cons sy arglist)))
			(unless lhs (setf form (letvar form)))
			(values form exp))
		      (t
			(values (letvar (cons sy (expr-list arglist))) exp) )) )
	     (t (values sy exp)))))
      (factor2 (exp)
        (let* ((left) (right) (form))
          (multiple-value-setq (left exp) (factor1 exp))
          (cond
           ((infix-memq (first exp) '(** ^))
            (multiple-value-setq (right exp) (factor1 (rest exp)))
            (cond
             ((and (integerp right) (< right 10))
              (setf form (list '*))
              (cond ((atom left) 
                     (dotimes (i right) (nconc form (list left)))
                     (values (letvar form) exp))
                    (t 
                     (dotimes (i right) (nconc form (list 'temp)))
                     (values `(let ((temp ,left)) ,form) exp))))
             (t	(values (list 'expt left right) exp)))) 
           ((numberp left) (values left exp))
           (t (values left exp) ))))
      (term (exp)
	  (let* ((left) (op) (right))
	    (multiple-value-setq (left exp) (factor2 exp))
	    (setf op (first exp))
	    (cond
		(;;(infix-memq op '(* /))
                 ;;(infix-while (infix-memq op '(* /))
                 (infix-memq op '(* /  .*))
                 (infix-while (infix-memq op '(* /  .*))
		    (setf left (list op left))
		    (infix-while (eq (first exp) op)
		       (multiple-value-setq (right exp) (factor2 (rest exp)))
		       (nconc left (list right))   )
		    (setf op (first exp)))
		 (values left exp))
		(t (values left exp)))))
      (expr (exp)
	  (let* ((op (first exp)) (left) (right))
	    (if (infix-memq op '(+ -))	;+- as unary operator
		(setf  exp (rest  exp)))
	    (multiple-value-setq (left exp) (term exp))
	    (if (eq op '-)
		(setf left (list op  left)))
	    (when (infix-memq (first exp) '(+ -))
		 (setf left (list '+ left))
	         (infix-while (infix-memq (setf op (first exp)) '(+ -))
		     (multiple-value-setq (right exp) (term (rest exp)))
		     (if (eq op '-) (setf right (list '- right)))
		     (nconc left (list right))))
	     (values left exp)))
      (expr-list (exp)
	  (let (temp result)
	    (infix-while exp
		(multiple-value-setq (temp exp) (expr exp))
		(push temp result))
	    (nreverse result)))
      (rel-expr (exp)
	  (let ((left) (op) (right))
	    (multiple-value-setq (left exp) (expr exp))
	    (setf op (pop exp))
	    (when (infix-memq op '(== != /= < >  <= >=))
		(multiple-value-setq (right exp) (expr exp))
	        (setf left
		    (list (second (assoc op '((== =) (!= /=) (/= /=) (< <)
					      (<= <=) (> >) (>= >=))))
		          left right)))
	    (values left exp)))	
      (reconstruct-form (exp)
        (setf exp (list exp))
	(let ((letpairs))
	   (dolist (lv letvar-alist)
	      (if (> (third lv) 1)	;referenced more than once
		  (push (list (second lv) (first lv)) letpairs)
		  (nsubst (first lv) (second lv) exp)))
	   (if letpairs
	       `(let* ,letpairs  . ,exp)
	       (first exp)))))
   (multiple-value-setq (result exp) (rel-expr exp))
   (if exp (error "illegal expression in infix macro"))
   (let ((reconstruction (reconstruct-form result)))
     (if (and (symbolp reconstruction)
              (function-information reconstruction))
       (list reconstruction)
       reconstruction)) ))

#|
(defun infix2prefix (file &optional char)
  (let ((exp (read file)))
     (cond
	((symbolp exp)	;probably a left-hand-side array ref.
	 (expression (list exp (read file)) t))
	((eq (second exp) '<-)
	 (list 'setf (car exp) (expression (cddr exp) nil)))
	((eq (third exp) '<-)
	 (list 'setf (expression (list (first exp) (second exp)) t)
		     (expression (cdddr exp) nil)))
        (t  (expression exp nil)))) )
|#

(defun infix2prefix (form &optional subchar arg)
  (declare (ignore subchar arg))
  (with-input-from-string (stream (format nil "~a" form))
    (unless (char= (read-char stream nil nil) #\()
      (error "Bad syntax.  Infix notation is (2 + 3)."))
    (let ((exp (read-delimited-list #\) stream)))
      (cond
       ((symbolp exp) exp)
       ((numberp exp) exp)
       ((eq (second exp) '<-)
        (list 'setf (car exp) (expression (cddr exp) nil)))
       ((eq (third exp) '<-)
        (list 'setf (expression (list (first exp) (second exp)) t)
              (expression (cdddr exp) nil)))
       (t  (expression exp nil))))))

(defun read-sel (file &optional char)
  (declare (ignore char))
  (cons 'sel (read-delimited-list #\] file)))

#| 
(set-macro-character #\% 'infix2prefix)

(set-dispatch-macro-character #\# #\% #'infix2prefix)
|#

(set-macro-character #\[ 'read-sel)
(set-syntax-from-char #\] #\))

;; -- 
;; matsui@etl.junet (domestic), matsui%etl.jp@relay.cs.net (overseas)


