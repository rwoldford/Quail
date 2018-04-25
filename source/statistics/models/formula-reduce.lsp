(in-package :quail)

(defvar *f-remove-p* nil)
(defvar *f<* nil)
(defvar *intercept* "1")
(defvar *negative-intercept* "-1")

(defun f-reduce (formula &key (f-remove-p nil) (f< nil))
  (declare (special *f-remove-p* *f<*))
  (if (not *f-remove-p*)
    (setf *f-remove-p* #'(lambda (x) (declare (ignore x)) t)))
  (if (not *f<*)
    (setf *f<* #'string<))
  ;; properly, this should be set up with unwind-protect
  (let ((old-f-remove-p *f-remove-p*)
        (old-f< *f<*))
    (if f-remove-p
      (setf *f-remove-p* f-remove-p))
    (if f<
      (setf *f<* f<))
    (prog1
      (f-sort (f-reduce-1 formula))
      (setf *f-remove-p* old-f-remove-p)
      (setf *f<* old-f<))))

;;;  I'm trying to use the following naming conventions:
;;;     "fterm" or "fterms"  looks like (f-op arg1 arg2 arg3 ..) or an atom
;;;     "term" or "terms"    looks like (arg1 arg2 arg3 ...)
;;;  where f-op is like f+, f*, f., etc, and each arg is an fterm.
;;;     "reduced-fterms"     like fterm except satisfies f-reduced-p
;;;     "reduced-terms"      like (rest x), x is a non-atom reduced-fterm.

(defun simple-dotted-fterm-p (fterm)
  "Returns non-nil if fterm is an atom or is of form (f. atom1 atom2 ...)"
  (or (atom fterm)
      (and (eq (first fterm) 'f.)
           (> (length fterm) 2)
           (let ((all-atoms t))
             (loop for a in (rest fterm)
                   until (not all-atoms)
                   do (setf all-atoms (atom a)))
             all-atoms))))

(defun dotted-term-p (term)
  (and (listp term) (simple-dotted-fterm-p term)))

(defun f-reduced-p (fterms)
  "Returns non-nil if fterms satistfies simple-dotted-fterm-p or is ~%~
of form (f+ simple-dotted-fterm-1 simple-dotted-fterm-2 ...)"
  (or (simple-dotted-fterm-p fterms)
      (and (eq (first fterms) 'f+)
           (let ((all-simple t))
             (loop for fterm in (rest fterms)
                   until (not all-simple)
                   do (setf all-simple (simple-dotted-fterm-p fterm)))
             all-simple))))

(defun f-reduce-1 (fterms)
  (if (f-reduced-p fterms)
    (f-cancel fterms)
    (f-reduce-2 (first fterms) (rest fterms))))

(defun f-sort (reduced-fterms)
  (cond ((atom reduced-fterms) reduced-fterms)
        ((eq (first reduced-fterms) 'f.) 
         (list* 'f. (f.-sort (rest reduced-fterms))))
        ((eq (first reduced-fterms) 'f+)
         (list* 'f+ (f+-sort (rest reduced-fterms))))))

(defun f.-sort (reduced-terms)
  (declare (special *f<*))
  (sort (copy-list reduced-terms) *f<*))

(defun f+-sort (reduced-terms)
  (let ((r (mapcar #'f-sort reduced-terms)))
    (sort (copy-list r) #'simple-dotted<)))

(defun f-cancel (fterms)
  (cond ((atom fterms) fterms)
        ((eq (first fterms) 'f.) 
         (let ((c (f.-cancel (rest fterms))))
           (if c
             (list* 'f. c)
             nil)))
        ((eq (first fterms) 'f+) 
         (let ((c (f+-cancel (rest fterms))))
           (if c
             (list* 'f+ c)
             nil)))))

#|
(defun f.-cancel (terms)
  (declare (special *f-remove-p*))
  (remove-duplicates terms
                     :test #'(lambda (x y)
                               (and (equal x y)
                                    (funcall *f-remove-p* x)))))
|#

;; add the f.-sort here to help f+-cancel with 
;; e.g. (f+ (f. "x1" "x2") (f. "x2" "x1"))
;; as early as possible
(defun f.-cancel (terms)
  (declare (special *f-remove-p*))
  (remove-duplicates (f.-sort terms)
                     :test #'(lambda (x y)
                               (and (equal x y)
                                    (funcall *f-remove-p* x)))))

(defun simple-dotted< (x y)
  (declare (special *f<*))
  (cond ((and (atom x) (atom y))
         (funcall *f<* x y))
        ((atom x) t)
        ((atom y) nil)
        ((< (length x) (length y)) t)
        ((> (length x) (length y)) nil)
        (t (let (done answer)
             ;; from 1, not 0 !!
             (loop for xx in (rest x)
                   as yy in (rest y)
                   until done
                   do (when (string/= xx yy)
                        (setf done t)
                        (setf answer (funcall *f<* xx yy))))
             answer))))

(defun f+-cancel (terms)
  (declare (special *intercept* *negative-intercept*))
  (setf terms (remove nil terms))
  (let ((new-terms (remove-duplicates (mapcar #'f-cancel terms) 
                                      :test #'equal)))
    ;; should work OK with :test #'eql
    (if (and (member *intercept* new-terms)
             (member *negative-intercept* new-terms))
      (remove *intercept* 
              (remove *negative-intercept* new-terms))
      new-terms)))

(defun f-variables (fterms)
  "Returns all the atoms involved at any depth in fterms."
  (cond ((stringp fterms) (list fterms))
        (t 
         (f+-cancel
          (loop for tt in fterms
                append (cond ((symbolp tt) nil)
                             ((stringp tt) (list tt))
                             (t (f-variables tt))))))))

(defun f-remove-ops (reduced-fterms)
  (if (atom reduced-fterms) 
    (f-variables reduced-fterms)
    (mapcar #'f-variables (rest reduced-fterms))))

(defmethod f-reduce-2 ((kind (eql 'f+)) args)
  (let* ((new-args
         (loop for arg in args
               append (cond ((atom arg) (list arg))
                            ((eq (first arg) 'f+)
                             (rest arg))
                            (t (list (f-reduce-1 arg))))))
         (new-f (list* 'f+ new-args)))
    (f-reduce-1 new-f)))

(defun first-if-list (x)
  (if (listp x)
    (first x)
    x))
    
(defmethod f-reduce-2 ((kind (eql 'f.)) args)
  (let* ((new-args
          (loop for arg in args
                append (cond ((atom arg) (list arg))
                             ((eq (first arg) 'f.)
                              (rest arg))
                             (t (list (f-reduce-1 arg))))))
         (first-f+ (position-if #'(lambda (x) (eq x 'f+)) new-args
                                :key #'first-if-list :from-end t))
         new-f)
    (if first-f+
      (let ((the-f+ (elt new-args first-f+)))
        (setf args (remove the-f+ new-args :test #'equal :count 1))
        (setf new-f
              (list* 'f+
                     (loop for item in (rest the-f+)
                           collect (list* 'f. item args)))))
      (setf new-f (if (eq 1 (length new-args))
                    (first new-args)
                    (list* 'f. new-args))) )  
    (f-reduce-1 new-f)))

(defmethod f-reduce-2 ((kind (eql 'f*)) args)
  (f-reduce-1
   (case (length args)
     ;; this case can only happen as x**1
     (1 (first args))
     ;; this is the usual case
     (2 (list 'f+ (first args) (second args) (list* 'f. args)))
     ;; this case is rare, occurring only as a result of x**n, n>2.
     (t (list 'f* (first args) (list* 'f* (rest args)))))))

(defmethod f-reduce-2 ((kind (eql 'f**)) args)
  ;; args of length 2, (second args) an integer >0
  (f-reduce-1 (list* 'f* (make-sequence 'list (second args)
                                      :initial-element (first args)))))

(defmethod f-reduce-2 ((kind (eql 'f/)) args)
  ;;  We assume args is of length 2
  (let ((left (f-reduce-1 (first args)))
        (right (second args)))
    (f-reduce-1
     (list 'f+
           left
           (cond ((simple-dotted-fterm-p left) (list 'f. right left))
                 (t (list* 'f. right (f-variables left))))))))

(defun f-remove (args test)
  ;;  We assume args is of length 2
  (let ((left (f-reduce-1 (first args)))
        (right (f-reduce-1 (second args))))
    (f-reduce-1
     (cond ((simple-dotted-fterm-p left)
            (cond ((equal left right) nil)
                  (t (error "Can't remove ~S from ~S." right left))))
           ((simple-dotted-fterm-p right)
            (cond ((member right left :test test)
                   (remove right left :test test))
                  (t (error "Can't remove ~S from ~S." right left))))
           (t
            (let ((reduced left))
               (loop for right-arg in (rest right)
                     do (setf reduced
                              (cond ((member right-arg left :test test)
                                     (remove right-arg reduced :test test))
                                    (t 
                                     (error "Can't remove ~S from ~S." right left)))))
               reduced))))))

(defmethod f-reduce-2 ((kind (eql 'f-)) args)
  (declare (special *intercept* *negative-intercept*))
  (cond ((and (eq (length args) 1)
              (equal (first args) *intercept*))
         *negative-intercept*)
        ((equal (second args) *intercept*)
         (list 'f+ (first args) *negative-intercept*))
        (t
         (f-remove args #'equal))))

(defun simple-dotted-subfactor (x y)
  (cond ((atom y) (equal x y))
        ((atom x) (member x (rest y) :test #'equal))
        (t (let ((all-there t)
                 (remaining (rest y)))
             (loop for xx in (rest x)
                   while all-there
                   do (progn
                        (setf all-there (member xx remaining :test #'equal))
                        (setf remaining (remove xx remaining :test #'equal :count 1))))
             all-there))))

(defmethod f-reduce-2 ((kind (eql 'f-*)) args)
  (f-remove args #'simple-dotted-subfactor))

(defmethod f-reduce-2 ((kind (eql 'f-/)) args)
  ;;  We assume args is of length 2
  (let ((left (f-reduce-1 (first args)))
        (right (f-reduce-1 (second args))))
    (f-reduce-1 (list 'f+ (list 'f-* left right) right))))

           
   
    
  
    
      
      
    
