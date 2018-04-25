
(defun make-data (n)
  (loop for i from 1 to n 
        collect (list (princ-to-string i) i (* i i) )))

(setq d (make-data 2))

(dataset-name d)

(list-variates d)
(setq c (second (list-cases d)))
(identifier-of c)
(value-of c 0)

(value-of c '(sqrt 0))
(value-of c '(+ 0 1))
(values-of c '(0 1))
(values-of c (list (list #'sqrt 0) (list #'sqrt 0)))

(values-of c '( (sqrt 0) (sqrt 0)))

(setq vars (list "a" "b"))
(push vars d)

(value-of c "a" :vars vars)
(value-of c 1 :vars vars)
(value-of c '(+ "a" 1) :vars vars)

(value-of c '(sqrt "a") :vars vars)

(value-of c '(+ "a" "b") :vars vars)
(value-of c '(+ (log "a") "b") :vars vars)
(value-of c '(+ (log a)  b) '(a b))

(values-of c '("a" "b") :vars vars)


(values-of c '( (sqr "a") (sqr "a")) :vars vars)

(value-of (second d) '(log "a") :vars vars)
(value-of (cdr d) '(log "a") :vars vars)
(value-of d "a" :vars vars)
