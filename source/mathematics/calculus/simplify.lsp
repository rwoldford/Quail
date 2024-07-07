;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               simplify.lisp                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1994.
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(simplify)))


(defun simplify-difference (expression)
  (flet
    ((constant-p (x) (qk::extnump x))
     (sum-p (x) (and (listp x) (eq '+ (car x)))))
    (let ((arg-list (rest expression)))
      (if arg-list
        (let* ((first-arg (first arg-list))
               (first-simplified-arg (simplify first-arg))
               (remaining-args (rest arg-list)))
          (if remaining-args
            (let ((remaining-result 
                   (append (list '+)
                           (loop for arg in remaining-args
                                 collect (simplify arg)))))
              (cond
               ((or
                 (equal first-simplified-arg remaining-result)
                 (equal first-arg remaining-result)
                 (and
                  (constant-p first-simplified-arg)
                  (or
                   (and (= (length remaining-args) 1)
                        (or (and (constant-p (second remaining-result))
                                 (= first-simplified-arg (second remaining-result)))
                            (and (constant-p (first remaining-args))
                                 (= first-simplified-arg (first remaining-args))))))
                  )
                 (and (= (length remaining-args) 1)
                      (or
                       (equal first-arg (second remaining-result))
                       (equal first-arg (first remaining-args))
                       (equal first-simplified-arg (second remaining-result))
                       (equal first-simplified-arg (first remaining-args)))
                      ))
                0)
               ((and (sum-p remaining-result)
                     (member first-simplified-arg
                             remaining-result
                             :test
                             #'(lambda (x y)
                                 (or (and (constant-p x)
                                          (constant-p y)
                                          (= x y))
                                     (equal x y)))))
                (let ((pos (position first-simplified-arg
                                     remaining-result
                                     :test
                                     #'(lambda (x y)
                                         (or (and (constant-p x)
                                                  (constant-p y)
                                                  (= x y))
                                             (equal x y))))
                           )
                      )
                  (setf remaining-result
                        (simplify
                         (loop for i from 0 to (- (length remaining-result) 1)
                               when (not (= pos i))
                               collect (elt remaining-result i))))
                  (simplify (list '-  remaining-result))
                  )
                )
               (T
                (setf remaining-result
                      (simplify remaining-result))
                (cond
                 ((and (constant-p first-simplified-arg)
                       (constant-p remaining-result))
                  (- first-simplified-arg remaining-result))
                 
                 ((constant-p remaining-result)
                  (if (= 0 remaining-result)
                    first-simplified-arg
                    (simplify
                     (list '+ first-simplified-arg
                           (- remaining-result))))
                  )
                 
                 ((constant-p first-simplified-arg)
                  (cond
                   ((= 0 first-simplified-arg)
                    (list '- remaining-result))
                   ((and (sum-p remaining-result)
                         (member first-simplified-arg remaining-result
                            :test #'(lambda (x y)
                                      (and (constant-p x)
                                           (constant-p y)
                                           (= x y))))
                         )
                    (let ((pos (position first-simplified-arg
                                         remaining-result
                                         :test
                                         #'(lambda (x y)
                                             (and (constant-p x)
                                                  (constant-p y)
                                                  (= x y))))
                               )
                          )
                      (setf remaining-result
                            (simplify
                             (loop for i from 0 to (- (length remaining-result) 1)
                                   when (not (= pos i))
                                   collect (elt remaining-result i))))
                      (simplify (list '-   remaining-result))
                      )
                    )
                   (T (list '- first-simplified-arg remaining-result))))
                 
                 ((equal first-simplified-arg remaining-result)
                  0)
                 ((and (sum-p remaining-result)
                       (member first-simplified-arg
                          remaining-result
                          :test
                          #'(lambda (x y)
                              (or (and (constant-p x)
                                       (constant-p y)
                                       (= x y))
                                  (equal x y)))))
                  (let ((pos (position first-simplified-arg
                                       remaining-result
                                       :test
                                       #'(lambda (x y)
                                           (or (and (constant-p x)
                                                    (constant-p y)
                                                    (= x y))
                                               (equal x y))))
                             )
                        )
                    (setf remaining-result
                          (simplify
                           (loop for i from 0 to (- (length remaining-result) 1)
                                 when (not (= pos i))
                                 collect (elt remaining-result i))))
                    (simplify (list '-  remaining-result))
                    )
                  )
                 
                 (T
                  (list '- first-simplified-arg remaining-result))))))
            (if (constant-p first-simplified-arg)
              (- first-simplified-arg)
              (list '- first-simplified-arg))
            )
          )
        0)
      )
    ))

(defun flatten-sum (expression)
  (flet
    (;(constant-p (x) (qk::extnump x))
     (sum-p (x) (and (listp x) (eq '+ (car x)))))
    (let ((arg-list (rest expression)))
      (if arg-list
        (let (flat-args result)
          (setf
           flat-args
           (do ((i 0 (+ i 1))
                arg
                flat-list
                )
               ((= i (length arg-list)) flat-list)
             (setf arg (simplify (elt arg-list i)))
             (cond
              ((sum-p arg)
               (setf arg (flatten-sum arg))
               (setf flat-list (append flat-list (rest arg)))
               )
              (T
               (setf flat-list (append flat-list (list arg)))
               )
              )
             ))
          (setf result
                (if (listp flat-args)
                  (if (= (length flat-args) 1)
                    (first flat-args)
                    (cons '+ flat-args))
                  flat-args)
                )
          result
          )
        0))))

(defun flatten-product (expression)
  (flet
    (;(constant-p (x) (qk::extnump x))
     (product-p (x) (and (listp x) (eq '* (car x)))))
    (let ((arg-list (rest expression)))
      (if arg-list
        (let (flat-args result)
          (setf
           flat-args
           (do ((i 0 (+ i 1))
                arg
                flat-list
                )
               ((= i (length arg-list)) flat-list)
             (setf arg (simplify (elt arg-list i)))
             (cond
              ((product-p arg)
               (setf arg (flatten-product arg))
               (setf flat-list (append flat-list (rest arg)))
               )
              (T
               (setf flat-list (append flat-list (list arg)))
               )
              )
             ))
          (setf result
                (if (listp flat-args)
                  (if (= (length flat-args) 1)
                    (first flat-args)
                    (cons '* flat-args))
                  flat-args)
                )
          result
          )
        0))))


(defun simplify-sum (expression)
  (flet
    ((constant-p (x) (qk::extnump x))
     (sum-p (x) (and (listp x) (eq '+ (car x)))))
    (setf expression (flatten-sum expression))
    (if (sum-p expression)
      (let ((arg-list (rest expression)))
        (if arg-list
          (let* ((simple-pieces
                  (loop for piece in arg-list
                        collect (simplify piece)))
                 (non-nums (remove-if #'constant-p simple-pieces))
                 (extended-nums (remove-if #'(lambda (x) (not (constant-p x)))
                                           simple-pieces))
                 result)
            (when non-nums
              (setf non-nums (flatten-sum (cons '+ non-nums)))
              (setf non-nums 
                    (if (sum-p non-nums)
                      (rest non-nums)
                      (list non-nums)))
              )
            (when extended-nums
              (setf extended-nums (apply #'+ extended-nums))
              )
            (cond
             ((and non-nums extended-nums)
              (let
                ((singletons (remove-duplicates non-nums :test  #'equal))
                 count)
                (do*
                  ((i 0 (+ i 1))
                   s
                   )
                  ((= i (length singletons)) NIL)
                  (setf s (elt singletons i))
                  (setf count (count s non-nums :test #'equal))
                  (push
                   (if (> count 1)
                     (simplify (list '* count s))
                     s)
                   result)
                  )
                (if (= extended-nums 0)
                  (if (> (length result) 1)
                    (setf result (append (list '+ ) (reverse result)))
                    (setf result (first result)))
                  (if (> (length result) 1)
                    (setf result (append (list '+ extended-nums) (reverse result)))
                    (setf result (list '+ extended-nums (first result)))))
                )
              )
             (extended-nums
              (setf result extended-nums))
             (non-nums
              (let
                ((singletons (remove-duplicates non-nums :test  #'equal))
                 count)
                (do*
                  ((i 0 (+ i 1))
                   s
                   )
                  ((= i (length singletons)) NIL)
                  (setf s (elt singletons i))
                  (setf count (count s non-nums :test #'equal))
                  (push
                   (if (> count 1)
                     (simplify (list '* count s))
                     s)
                   result)
                  )
                (if (> (length result) 1)
                  (setf result (cons '+ (reverse result)))
                  (setf result (first result)))
                )
              )
             (T (quail-error "~&In simplify-addition: expression = ~s." expression)))
            (cond
             ((constant-p result) result)
             ((and (sum-p result) (= (length result) 2))
              (second result))
             (T result))
            )
          0)
        )
      expression))
  )



(defun simplify-product (expression)
  (flet
    ((constant-p (x) (qk::extnump x))
     (product-p (x) (and (listp x) (eq '* (car x)))))
    (setf expression (flatten-product expression))
    (if (product-p expression)
      (let ((arg-list (rest expression)))
        (if arg-list
          (let* ((simple-pieces
                  (loop for piece in arg-list
                        collect (simplify piece)))
                 (non-nums (remove-if #'constant-p simple-pieces))
                 (extended-nums (remove-if #'(lambda (x) (not (constant-p x)))
                                           simple-pieces))
                 result)
            (when non-nums
              (setf non-nums
                    (flatten-product (cons '* non-nums)))
              (setf non-nums 
                    (if (product-p non-nums)
                      (rest non-nums)
                      (list non-nums)))
              )
            (when extended-nums
              (setf extended-nums (apply #'* extended-nums))
              )
            (cond
             ((and non-nums extended-nums)
              (cond
               ((= extended-nums 0)  0)
               ((= extended-nums 1)
                (setf result (append (list '*) non-nums)))
               (T (setf result 
                        (append (list '* extended-nums)
                                non-nums)))
               )
              )
             (extended-nums
              (setf result extended-nums))
             (non-nums
              (let
                ((singletons (remove-duplicates non-nums :test  #'equal))
                 count)
                (do*
                  ((i 0 (+ i 1))
                   s
                   )
                  ((= i (length singletons)) NIL)
                  (setf s (elt singletons i))
                  (setf count (count s non-nums :test #'equal))
                  (push
                   (if (> count 1)
                     (simplify (list 'expt s count))
                     s)
                   result)
                  )
                (if (> (length result) 1)
                  (setf result (cons '* (reverse result)))
                  (setf result (first result)))
                )
              )
             (T result))
            (cond
             ((constant-p result) result)
             ((and (product-p result) (= (length result) 2))
              (second result))
             (T result))
            )
          1)
        )
      expression))
  )

(defun simplify-division (expression)
  ;(flet
    ;(;(constant-p (x) (qk::extnump x))
     ;(quotient-p (x) (and (listp x) (eq '/ (car x))))
  ;   )
    (let ((arg-list (rest expression)))
      (if arg-list
        (if (= 1 (length arg-list))
          expression
          (list '/ (first arg-list)
                (simplify (cons '* (rest arg-list)))))
        expression)
      )
    ;)
  )

(defun simplify (expression)
  (labels
    (;(constant-p (x) (qk::extnump x))
     ;(variable-p (x) (symbolp x))
     ;(same-variable-p (x y)
     ;  (and (variable-p x) (variable-p y) (eq x y)))
     (sum-p (x) (and (listp x) (eq '+ (car x))))
     (difference-p (x) (and (listp x) (eq '- (car x))))
     (product-p (x) (and (listp x) (eq '* (car x))))
     (quotient-p (x) (and (listp x) (eq '/ (car x))))
     )
    (cond
     ((sum-p expression)                  (simplify-sum expression))
     ((difference-p expression)           (simplify-difference expression))
     ((product-p expression)              (simplify-product expression))
     ((quotient-p expression)             (simplify-division expression))
     ((and expression (listp expression)) (loop for thing in expression
                                                collect (simplify thing))
      )
     (T                                   expression)
     )
    )
  )
