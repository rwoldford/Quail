;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               utility.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1991.
;;;     R.W. Oldford 1991
;;;     M.E. Lewis 1991
;;;
;;;--------------------------------------------------------------------------------
;;;
;;;  Includes:
;;;          fixnump mappend flatten mklist flatten-alist
;;;          missing-method iseq string-to-symbol string-to-key 
;;;          interpret interpret-keys interpret-keys-only nuke-null-keys
;;;          split-at-nth split-n-ways gather-into-groups-of-n
;;;          collapse-list all-equal pad-list expand-list
;;;          rm-seq-to-list list-complement
;;;          column-major-next-subscript row-major-next-subscript
;;;          list-if-t conform all-non-nil-p some-non-nil-p
;;;          dump-hash-table  merge-pathnames-to-translated-namestring
;;;

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(fixnump
          missing-method
          flet*
          mlet
          <-
          ;;cond-each
          ;;lambda
          flatten
          flatten-alist
          mappend
          merge-pathnames-to-translated-namestring
          )))

;;;---------------------------------------------------------------------------------

(defun merge-pathnames-to-translated-namestring (pathname &rest defaults)
  (namestring
   (translate-logical-pathname
    (if defaults
      (apply #'merge-pathnames pathname defaults)
      pathname))))

;------------------------------------------------------------------------------

(defmacro <- (accessor new-value &rest args)
  "Assignment operator.  Identical to setf except that ~
   no value is returned."
  `(progn
     ,(if args
        (append `(setf , accessor , new-value) args)
        `(setf , accessor , new-value))
     (values)))

(defmacro flet* (&rest args)
  "Identical to Common Lisp's labels function."
  (cons 'labels args))

(defmacro mlet (&rest args)
  "Identical to Common Lisp's macrolet function."
  (cons 'macrolet args))

(defmacro fixnump (x)
  "Test whether the argument is a fixnum."
  `(and (integerp ,x)
        (<= ,x most-positive-fixnum)
        (>= ,x most-negative-fixnum)))

(defun missing-method (method &rest objects)
  "Identifies that the method named is missing and must be ~
   specialized for the classes of the remaining arguments.  ~
   Produces an error when called advising the user of the need ~
   for specialization."
  (let* ((numbers (iseq 1 (length objects)))
         (object-class-names 
           (mapcar #'(lambda (object) (class-name (class-of object)))
                   objects))
         (out-list (mapcar #'list numbers object-class-names objects)))
    (quail-error "The method ~S must be specialized ~%~
            for the supplied parameter classes:~2%~
            ~:{~&Parm ~S~10,4T Class    ~S~%~10,4T Instance ~S~2%~}"
           method
           out-list)))
            
(defun string-to-symbol (the-string)
  "Turns the given string into a symbol in the current package."
  (with-input-from-string (s the-string)
    (read s)))

(defun string-to-key (the-string)
  "Turns the given string into a symbol in the keyword package."
  (with-input-from-string (s (concatenate 'string ":" the-string))
    (read s)))

;;;
;
;  (interpret args-list list-of-keys &optional allow-other-keys) 
;
;  is best explained by example:
;
;  (interpret '(1 2 NaN 3 NaN :a 7 :c 13) '(:a (:b 100) :c :d))
;
;  yields multiple values
;
;  (1 2 NaN 3 NaN)     ; the portion of args-list before any keywords
;                        ;         from list-of-keys
;  7                     ; value of first keyword (in list-of-keys)
;  100                   ; value of second keyword, 
;                        ;         not found -> default value given
;  13                    ; value of third keyword
;  nil                   ; value of fourth keyword,
;                                  not found -> no default given -> nil
; 
;  The portion of args-list including and following the first keyword 
;  from list-of-keys to appear in args-list must be of even length and 
;  consist of a sequence of keys each followed immediately by its value,
;  as in the above example.
; 
;  An error is signalled if a keyword appears in said portion of args-list
;  which is not present in list-of-keys, unless allow-other-keys is non-nil.
;  Note that the  2 NaN arguments in the above example are legitimate since
;  they appear before the first located keyword from list-of-keys.
;

(defun interpret (args-list list-of-keys &optional allow-other-keys)
  (let* ((split (find-split args-list list-of-keys))
         (leader (if split (subseq args-list 0 split) args-list))
         (keys (if split (subseq args-list split) nil)))
    (apply #'values leader (interpret-keys keys
                                           list-of-keys
                                           allow-other-keys)))) ; apply req'd !!

;;  This next function is a special case of interpret which forces the
;;  args-list to always consist entirely of keys and values, and screams
;;  otherwise.

(defun interpret-keys-only (args-list list-of-keys
                                      &optional calling-routine
                                                allow-other-keys)
  (let* ((split (position-if #'keywordp args-list))
         (leader (if split (subseq args-list 0 split) args-list))
         (keys (if split (subseq args-list split) nil)))
    (if leader
      (if calling-routine
        (quail-error "Extraneous parameters to ~S: ~{~S ~}." calling-routine leader)
        (quail-error "Extraneous parameters: ~{~S ~}." leader)))
    (apply #'values (interpret-keys keys
                                    list-of-keys
                                    allow-other-keys)))) ; apply req'd !!

(defun find-split (args-list list-of-keys)
  (let* ((split-locs
          (remove 'nil (loop for key-spec
                             in list-of-keys
                             collect (position (if (listp key-spec)
                                                 (first key-spec)
                                                 key-spec)
                                               args-list)))))
    (if split-locs
      (apply #'min split-locs)
      nil)))

(defun interpret-keys (keys list-of-keys allow-other-keys)
  (let* ((len-keys (length keys))
         (keys-alist '())
         (remain (rem len-keys 2)))
    (if (/= 0 remain)
      (quail-error "The list of keys ~S is not of even length."
             keys))
    (loop for i from 0 to (- len-keys 1) by 2 do
          (setf keys-alist (acons (elt keys i) (elt keys (+ i 1)) keys-alist)))
    (let ((key-values
           (loop for key-spec
                 in list-of-keys
                 collect (let* ((key (if (listp key-spec)
                                       (first key-spec)
                                       key-spec))
                                (key-default (if (listp key-spec)
                                               (second key-spec)
                                               nil))
                                (key-val (assoc key keys-alist)))
                           (if key-val
                             (progn (setf keys-alist (delete key-val keys-alist))
                                    (cdr key-val))
                             key-default)))))
        (if (and keys-alist (not allow-other-keys))
          (quail-error "Provided keys ~S are not permitted."
                 (maplist #'car keys-alist))
          (append key-values
                  (let ((remainder nil))
                    (loop for akey in keys-alist
                        do (progn
                             (setf remainder (list* (cdr akey) remainder))
                             (setf remainder (list* (car akey) remainder))))
                    (list remainder)))))))

(defun nuke-null-keys (keys)
  (let* ((len-keys (length keys))
         (key-list '())
         (remain (rem len-keys 2)))
    (if (/= 0 remain)
      (quail-error "The list of keys ~S is not of even length."
             keys))
    (loop for i 
          from 0
          to (- len-keys 1)
          by 2
          do (setf key-list (append key-list
                                    (let ((key (elt keys i))
                                          (val (elt keys (+ i 1))))
                                      (if (not (keywordp key))
                                        (quail-error "Invalid keyword ~S." key))
                                      (if val
                                        (list key val)
                                        nil)))))
    key-list))

(defun split-at-nth (n alist)            ; list length must be at least n
  (list (butlast alist (- (length alist) n)) (nthcdr n alist)))

(defun split-n-ways (n n-div-list)        ; list length must be divisible by n
  (let* ((len (length n-div-list))
         (m (floor len n))
         (r (rem len n)))
    (labels 
      ((split-recurs (alist)
         (if (null (cadr alist))
           (list (car alist))  
           (cons (car alist)
                 (split-recurs (split-at-nth m (cadr alist)))))))
      (if n-div-list
        (if (equal 0 r)
          (split-recurs (split-at-nth m n-div-list))
          (quail-error "List not evenly divisible into ~D pieces." n))
        '()))))

(defun gather-into-groups-of-n (n alist)
  (let* ((len (length alist))
         (r (rem len n)))
    (if alist
      (if (eq 0 r)
        (loop for i
              from 0
              to (- len 1)
              by n
              collect (subseq alist i (+ i n)))
        (quail-error "List not evenly divisible into groups of ~S." n))
      '())))

(defun collapse-list (listed-array)
  (if (null listed-array)
    '()
    (if (typep (car listed-array) 'atom)
      listed-array
      (collapse-list (apply #'append listed-array)))))

(defun all-equal (the-list)
  (if the-list
    (let ((proto (first the-list)))
      (every #'(lambda (x) (equal proto x)) (rest the-list)))
    t))

; pad-list:
;    pads list with pad-with out to length len.
;    WARNING: if the-list is longer than len, pad-with truncates to length len !!!
;

(defun pad-list (the-list len pad-with)
  (let ((the-len (length the-list)))
    (if (> len the-len)
      (append the-list
              (make-sequence 'list (- len the-len) :initial-element pad-with))
      (subseq the-list 0 len))))

;
;  (expand-list the-list mask filler)
;
;         mask is a list of t and nil of length >= length the-list.  There must be
;         as many t's in mask as the length of the-list.  Wherever mask is nil, filler
;         is inserted into the-list.  If mask is nil, the-list is unaffected.
;         There is no error checking for mask; use with caution.
;
; e.g     Now if current is a list of indices specified by (expand-list specs mask '(0)), then
;
;             (apply #'eref-mask specs current :mask mask)
;
;         yields a legitimate reference to specs.  This helps deal with conformability.
;

(defun expand-list (the-list mask filler)                     
    (if mask
      (let* ((new-list (make-sequence 'list (length mask) :initial-element t))
             (j 0))
        (loop for i
              from 0
              to (- (length mask) 1)
              do (if (elt mask i)
                   (progn
                     (setf (elt new-list i) (elt the-list j))
                     (setf j (+ 1 j)))
                   (setf (elt new-list i) filler)))
        new-list)
      the-list))

(defun rm-seq-to-list (rm-seq dim)
  (let ((len (length dim)))
    (case len
      (0 (if (eq (length rm-seq) 1)
           (first rm-seq)
           (quail-error "~&Can't convert row-major sequence ~S to list with dimensions ~S"
                  rm-seq
                  dim)))
      (t (let ((rm-list rm-seq))
           (loop for i
                 from (- len 1)
                 downto 0
                 do (setf rm-list (gather-into-groups-of-n (elt dim i) rm-list)))
           (first rm-list))))))

(defun list-complement (subset set)
  (let ((complement (copy-list set)))
    (loop for i in subset do
          (setf complement (delete i complement)))
    complement))

(defun base-transpose (listed-array)
  (if (and listed-array (listp listed-array))
    (if (listp (car listed-array))
      (apply #'mapcar (append (list 'list) listed-array))
      (quail-error "Input must be a list of lists. eg. '((1 2 3))."))
    listed-array))

(defun gen-transpose (dimensions listed-array)
  (labels ((t-split (n x) (base-transpose (split-n-ways n x))))
    (let ((new-array (collapse-list listed-array))
          (len (length dimensions)))
      (cond ((equal len 0) '())
            ((equal len 1) (base-transpose (list new-array)))
            (t (loop for i from 0 to (- len 2) do
                 (setf new-array 
                         (t-split (nth i dimensions) new-array)))
               new-array)))))

(defun column-major-next-subscript (current dim)
  (let ((got-next-subscript (null dim)))  
    (loop for i 
          from 0 
          to (- (length dim) 1) 
          until got-next-subscript
          do (progn
               (setf (elt current i) (1+ (elt current i)))
               (if (< (elt current i) (elt dim i))
                 (setf got-next-subscript t)
                 (progn (setf (elt current i) 0)
                        (setf got-next-subscript nil)))))
    current))

(defun row-major-next-subscript (current dim)
  (let ((got-next-subscript (null dim)))
    (loop for i 
          from (- (length dim) 1) 
          downto 0 
          until got-next-subscript
          do (progn
               (setf (elt current i) (1+ (elt current i)))
               (if (< (elt current i) (elt dim i))
                 (setf got-next-subscript t)
                 (progn (setf (elt current i) 0)
                        (setf got-next-subscript nil)))))
    current))

(defun list-if-t (any-list boolean-list &key pad-with)
  (cond ((null any-list) nil)
        ((null boolean-list) (cons (first any-list)
                                   (list-if-t (rest any-list)
                                              pad-with
                                              :pad-with pad-with)))
        ((first boolean-list)  (cons (first any-list)
                                   (list-if-t (rest any-list) 
                                              (rest boolean-list)
                                              :pad-with pad-with)))
        (t (list-if-t (rest any-list) 
                      (rest boolean-list)
                      :pad-with pad-with))))

(defun conform (a b)
  (let ((aa (remove 1 a))
        (bb (remove 1 b)))
    (if (equal aa bb)
      (multiple-value-bind (conform-list a-mask b-mask)
                           (conform-recurs a b)
        (values t conform-list a-mask b-mask))
      (values nil nil nil nil))))

(defun conform-recurs (a b)
  (cond ((and (null a) (null b))
           (values nil nil nil))
        ((null a)
           (let ((len (length b)))
             (values (pad-list '() len 1)
                     (pad-list '() len nil)
                     (pad-list '() len t))))
        ((null b)
           (let ((len (length a)))
             (values (pad-list '() len 1)
                     (pad-list '() len t)
                     (pad-list '() len nil))))
        ((and (eq (first a) (first b)))
           (conform-recurs-call a b 1 1 (first a) t t))
        ((eq 1 (first a))
           (conform-recurs-call a b 1 0 1 t nil))
        ((eq 1 (first b))
           (conform-recurs-call a b 0 1 1 nil t))
        (t (quail-error "This should never, never happen ... "))))

(defun conform-recurs-call (a b a-remove b-remove ab-front a-front b-front)
  (let ((new-a (subseq a a-remove))
        (new-b (subseq b b-remove)))
    (multiple-value-bind (ab-back a-back b-back)
                         (conform-recurs new-a new-b)
      (values (cons ab-front ab-back)
              (cons a-front a-back)
              (cons b-front b-back)))))

(defun all-non-nil-p (any-list)
  (notany #'null any-list))

(defun some-non-nil-p (any-list)
  (notevery #'null any-list))

(defun dump-hash-table (hash-table &key (key-print #'identity)
                                   (value-print #'identity))
  (maphash #'(lambda (key val)
               (declare (special *quail-terminal-io*))
               (format *quail-terminal-io* 
                       "~&key: ~S ~%value: ~S~%~%"
                       (funcall key-print key)
                       (funcall value-print val)))
           hash-table)
  (values))

#|
(defmacro cond-each (forms)
  "Like cond, but evaluates every form whose test is true."
  `(progn ,@(mapcar #'(lambda (x)
                        `(if ,(first x) ,(second x) ,(third x)))
                    forms)))

(defmacro lambda (args &body body)
  "Allow (lambda (x) ...) instead of #'(lambda (x) ...)"
  `#'(lambda ,args .,body))

|#

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun mklist (x) 
  "If x is a list return it, otherwise return the list of x"
  (if (listp x) x (list x)))

(defun flatten (exp)
  "Get rid of imbedded lists (to one level only)."
  (mappend #'mklist exp))

(defun flatten-alist (alist)
  (cond ((null alist) nil)
        ((atom alist) (list alist))
        (t
         (append (flatten-alist (car alist))
                 (flatten-alist (cdr alist))))))

