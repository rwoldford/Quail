(in-package :quail)

;;; these are macros so that we can setf them.
(defmacro kind-of (pt) `(first ,pt))
(defmacro node-of (pt) `(second ,pt))
(defmacro start-of (pt) `(third ,pt))
(defmacro end-of (pt) `(fourth ,pt))
(defmacro subtrees-of (pt) `(fifth ,pt))

(defun make-parse (kind node start end &optional (subtrees nil))
  (list kind node start end subtrees))

(defun build-tree (kind super-node subtrees)
  (let ((start (apply #'min (mapcar #'third subtrees)))
        (end (apply #'max (mapcar #'fourth subtrees))))
  (list kind (subseq super-node start end) start end subtrees)))

(defun find-next (char string &optional (start 0) (end (length string)))
  (let (found)
    ; (loop for i from start to (cl:- end 1)
    ;     until found do
    (do ((i start (incf i)))
        ((or (= i end) found))
      (if (eq char (char string i))
        (setf found i)))
    (if found
      found
      nil)))

(defun find-end (str pred &optional (start 0) (max-end (length str)))
  (let (end)
    ; (loop for i from start to max-end
    ;     until end do
    (do ((i start (incf i)))
        ((or (> i max-end) end))
      (if (< i max-end)
        (if (not (funcall pred (char str i)))
          (setf end i))
        (setf end i)))
    end))

(defun whitespace-p (x)
  (member x (list #\Space #\Newline)))

(defun skip-whitespace (str &optional (start 0))
  (find-end str #'whitespace-p start))

(defmethod parse ((kind (eql :whole-number)) parse-string
                  start &optional (end (length parse-string)))
  (let (local-end)
    (setf local-end (find-end parse-string #'digit-char-p start end))
    (if (eq local-end start)
      nil
      (make-parse :whole-number 
                  (read-from-string
                   (subseq parse-string start local-end))
                  start
                  local-end))))

(defvar *special-chars* (list #\( #\) #\~ #\[ #\] #\{ #\} #\Space #\Newline))

(defun special-char-p (x)
  (declare (special *special-chars*))
  (member x *special-chars*))

(defun variable-char-p (x)
  (declare (special *special-chars*))
  (not (member x *special-chars*)))

(defmethod parse ((kind (eql :variable)) parse-string
                  start &optional (end (length parse-string)))
  (if (eq start end)
    (make-parse :null "" start start)
    (let (local-end
          variable
          kind)
      (if (eq (char parse-string start) #\")
        (progn
          (setf local-end (find-next #\" parse-string (cl:+ start 1)))
          (if local-end
            (incf local-end)
            (error "Unmatched '\"' in ~S." parse-string)))
        (setf local-end (find-end parse-string #'variable-char-p start end)))
      (setf kind :variable)
      (setf variable (subseq parse-string start local-end))
      (let ((vble-read (read-from-string variable)))   
        (when (numberp vble-read)
          (setf kind :number)
          (setf variable vble-read))
        (when (keywordp vble-read)
          (setf kind :keyword)
          (setf variable vble-read))
        (when (stringp vble-read)
          (setf variable vble-read)))
      (make-parse kind
                  variable
                  start
                  local-end))))

(defmethod parse ((kind (eql :selector-left)) masked-parse-string
                  start &optional (end (length masked-parse-string)))
  (declare (ignore end))
  (if (and (> (length masked-parse-string) start)
           (eq (char masked-parse-string start) #\[))
    (make-parse :selector-left "[" start (cl:+ start 1))
    nil))

(defmethod parse ((kind (eql :selector-right)) masked-parse-string
                  start &optional (end (length masked-parse-string)))
  (declare (ignore end))
  (let ((close (find-next #\] masked-parse-string start)))
    (if (not close)
      (error "Unmatched '[' in ~S." masked-parse-string))
    (make-parse :selector-right "]" close (cl:+ close 1))))

(defmethod parse ((kind (eql :selector)) parse-string
                  start &optional (end (length parse-string)))
  (if (eq start end)
    nil
    (let* ((masked-parse-string (mask-quotes parse-string start end))
           (tree0 (parse :selector-left masked-parse-string start end)))
      (if tree0
        (let* ((tree1 (parse :selector-right
                             masked-parse-string 
                             (end-of tree0)
                             end))
               (selector (parse :select-lisp-args
                                parse-string
                                (end-of tree0)
                                (start-of tree1))))
          ;;  don't need tree0 or tree1 after this ...
          (setf (start-of selector) (start-of tree0))
          (setf (end-of selector) (end-of tree1))
          selector)
        nil))))

(defmethod parse ((kind (eql :vble-select)) parse-string
                  start &optional (end (length parse-string)))
  (setf start (skip-whitespace parse-string start))
  (let ((tree0 (parse :variable parse-string start end)))
    (if tree0
      (let* ((start1 (end-of tree0))
             (tree1 (parse :selector parse-string start1 end)))
        (if (and (eq (kind-of tree0) :variable) tree1)
          (build-tree :vble-select parse-string (list tree0 tree1))
          tree0))
      nil)))

(defvar *infix-table* (make-hash-table :test #'equal))

(defun put-infix (infix type &optional (precedence 0) (action nil))
  (setf (gethash infix *infix-table*) (list type precedence action)))

(defun infix-type (infix)
  (first (gethash infix *infix-table*)))

(defmacro infix-op (infix)
  `(first (infix-type ,infix)))

(defmacro infix-left (infix)
`(second (infix-type ,infix)))

(defmacro infix-right (infix)
  `(third (infix-type ,infix)))

(defun infix-precedence (infix)
  (second (gethash infix *infix-table*)))

(progn
  (put-infix "**" '(:expstar :infix :whole-number) 4)
  (put-infix "^" '(:expstar :infix :whole-number) 4)
  (put-infix "*" '(:star :infix :infix) 3)
  (put-infix "/" '(:slash :infix :infix) 2.5)
  (put-infix "." '(:dot :infix :infix) 2)
  (put-infix "+" '(:terms :infix :infix) 1)
  (put-infix "-" '(:remove :infix :infix) 1)
  (put-infix "-/" '(:remove-slash :infix :infix) 1)
  (put-infix "-*" '(:remove-star :infix :infix) 1)
  (put-infix "~" '(:formula :flv :infix) 0)
  t)

(defvar *max-infix-length*)

(defun remaximize-infix ()
  (declare (special *max-infix-length* *infix-table*))
  (setf *max-infix-length* -1)
  (maphash #'(lambda (key value)
            (declare (ignore value) (special *max-infix-length*))
            (if (> (length key) *max-infix-length*)
              (setf *max-infix-length* (length key))))
         *infix-table*)
  *max-infix-length*)

(remaximize-infix)

(defun infix-here (str pos)
  (declare (special *max-infix-length*))
  (let ((len (length str))
        infix
        done)
    ; (loop for local-end
    ;      from (min len (cl:+ pos *max-infix-length*))
    ;      downto (cl:+ pos 1)
    ;      until done
    ;      do
    (do ((local-end (min len (cl:+ pos *max-infix-length*)) (decf local-end)))
        ((or (= local-end pos) done))
      (progn
        (setf infix (subseq str pos local-end))
        (setf done (infix-type infix))))
    (if done
      infix
      nil)))

(defun mask-quotes (str &optional (start 0) (end (length str)))
  (mask-quotes-recurs (subseq str 0) start end))

(defun mask-quotes-recurs (str start end)
  (let* ((begin (find-next #\" str start))
         (local-end (if begin (find-next #\" str (cl:+ begin 1)))))
    (cond ((and begin local-end)
           (progn
             ; (loop for i 
             ;      from (cl:+ begin 1)
             ;      to (cl:- local-end 1) do
             (do ((i (cl:+ begin 1) (incf i)))
                 ((= i local-end))
               (setf (char str i) #\"))
             (mask-quotes-recurs str 
                                 (cl:+ local-end 1)
                                 end)))
          (begin (error "Unmatched '\"' in ~S." str))
          (t str))))

(defun find-close-paren (str &optional (start 0))
  "Finds closing paren for open paren in str appearing at position start."
  (let ((open (find-next #\( str (cl:+ start 1)))
        (close (find-next #\) str (cl:+ start 1))))
    (if (not open)
      (setf open (length str)))
    (cond ((not close)
           (error "Unmatched '(' in ~S." str))
          ((< open close)
           (find-close-paren str
                             (find-close-paren str open)))
          (t close))))

(defun find-close (open-char close-char str &optional (start 0))
  "Finds close-char for open-char in str appearing at position start.
Assumes there is no nesting of open-char/close-char."
  (let ((close (find-next close-char str (cl:+ start 1))))
    (cond ((not close)
           (error "Unmatched '~A' in ~S." open-char str))
          (t close))))

(defun find-infixes (str start end)
  (setf str (mask-quotes str start))
  (let* (infixes)
    (setf infixes
          (loop for i from start to (cl:- end 1)
                collect (cond
                         ((eq (char str i) #\()
                          (setf i (find-close-paren str i))
                          nil)
                         ((eq (char str i) #\{)
                          (setf i (find-close #\{ #\} str i))
                          nil)
                         ((eq (char str i) #\[)
                          (setf i (find-close #\[ #\] str i))
                          nil)
                         (t
                          (let ((infix (infix-here str i)))
                            (when infix
                              (prog1 (list i 
                                           (infix-precedence infix)
                                           (length infix))
                                (setf i (min
                                         (cl:+ i (length infix) -1)
                                         (cl:- end 1))))))))))
    (setf infixes (delete nil infixes))
    (sort infixes #'(lambda (x y)
                      (or (< (second x) (second y))
                          (and (= (second x) (second y))
                                  (> (first x) (first y))))))))

(defmacro infix-position (x) `(first ,x))

(defun infix-parse-recurs (parse-string infixes start end)
  (if infixes
    (let* ((infix-info (first infixes))
           (rest-infixes (rest infixes))
           (end0 (infix-position infix-info))
           (start1 (cl:+ end0 (third infix-info)))
           (infix (subseq parse-string end0 start1))
           (tree0 (infix-subtree infix rest-infixes
                                 :left parse-string start end0 end0))
           (tree1 (infix-subtree infix rest-infixes
                                 :right parse-string start1 start1 end)))
      (build-tree (infix-op infix)
                   parse-string
                   (list tree0
                         tree1)))
    (parse :flv parse-string start end)))

(defun infix-subtree (infix rest-infixes side str start pivot end)
  (let* ((pred (case side
                 (:left #'(lambda (x)
                            (>= (infix-position x) pivot)))
                 (:right #'(lambda (x)
                             (< (infix-position x) pivot)))))
         (subtree (remove-if pred rest-infixes))
         (infix-type (case side
                       (:left (infix-left infix))
                       (:right (infix-right infix)))))
    (case infix-type
      (:infix
       (if subtree
         (infix-parse-recurs str subtree start end)
         (parse :flv str start end)))
      (t 
       (if subtree
         (error "~S does not parse to a component of type ~S"
                (subseq str start end)
                infix-type)
         (parse infix-type str start end))))))

(defmethod parse ((kind (eql :infix)) parse-string
                  start &optional (end (length parse-string)))
  (setf start (skip-whitespace parse-string start))
  (infix-parse-recurs parse-string
                      (find-infixes parse-string start end)
                      start
                      end))

(defmethod parse ((kind (eql :function-left)) masked-parse-string
                  start &optional (end (length masked-parse-string)))
  (declare (ignore end))
  (let ((open (find-next #\( masked-parse-string start)))
    (if open
      (make-parse :function-left "(" open (cl:+ 1 open))
      nil)))

(defmethod parse ((kind (eql :function-right)) masked-parse-string
                  start &optional (end (length masked-parse-string)))
  (declare (ignore end))
  ;; Here start is known to be on the open paren.
  ;; find-close-paren will signal an error if there's a problem.
  (let ((close (find-close-paren masked-parse-string start)))
    (make-parse :function-right ")" close (cl:+ 1 close))))

(defmethod parse ((kind (eql :function-name)) parse-string
                  start &optional (end (length parse-string)))
  (let* ((tree (parse :variable parse-string start end))
         (null-name (eq :null (kind-of tree))))
    (cond ((and tree null-name) nil)
          ;;
          ;; read-from-string is used instead of intern so that one can
          ;; use, say, my-package::foo(x) in a formula
          ;;
          ((and tree (eq :variable (kind-of tree)))
           (setf (kind-of tree) :function-name))
          (t (error "Semantic error during formula parsing. ~
                     ~&~S(...) is not a legitimate functional operator."
                    (node-of tree))))
    tree))

(defmethod parse ((kind (eql :lisp-op)) parse-string
                  start &optional (end (length parse-string)))
  (setf start (skip-whitespace parse-string start))
  (let ((masked-parse-string (mask-quotes parse-string start end))
        (first-char (char parse-string start)))
    (cond ((eq start end)
           nil)
          ((eq first-char #\()
           (let* ((close (find-close-paren masked-parse-string
                                           start))
                  (op (parse :lisp-read parse-string (cl:+ start 1) end)))
             (if (eq (node-of op) 'quote)
               (parse :lisp-read parse-string start end)
               (let ((args (parse :lisp-args
                                  parse-string
                                  (end-of op)
                                  close)))
                 (make-parse :lisp-op
                             (subseq parse-string start (cl:+ close 1))
                             start
                             (cl:+ close 1)
                             (list op args))))))
          ((eq first-char #\')
           (parse :lisp-read parse-string start end))
          ((eq first-char #\`)
           (error "This parser can't handle backquotes."))
          (t
           (parse :vble-select parse-string start end)))))

(defun parse-args (arg-kind parse-string
                            start &optional (end (length parse-string)))
  (if (eq start end)
    nil
  (let* (arg
         args)
    (setf args
          (loop for i from start to (cl:- end 1)
                collect (prog1 
                          (setf arg (parse arg-kind parse-string i end))
                          (setf i (- (end-of arg) 1)))))
    (build-tree :args
                parse-string
                args))))

(defmethod parse ((kind (eql :lisp-args)) parse-string
                  start &optional (end (length parse-string)))
  (parse-args :lisp-op parse-string start end))

(defmethod parse ((kind (eql :select-lisp-args)) parse-string
                  start &optional (end (length parse-string)))
  (parse-args :lisp-read parse-string start end))

(defmethod parse ((kind (eql :args)) parse-string
                  start &optional (end (length parse-string)))
  (parse-args :flv parse-string start end))

(defmethod parse ((kind (eql :lisp-op-left)) masked-parse-string
                  start &optional (end (length masked-parse-string)))
  (declare (ignore end))
  (let ((open (find-next #\{ masked-parse-string start)))
    (if open
      (make-parse :lisp-op-left "{" open (cl:+ 1 open))
      nil)))

(defmethod parse ((kind (eql :lisp-op-right)) masked-parse-string
                  start &optional (end (length masked-parse-string)))
  (declare (ignore end))
  (let ((close (find-next #\} masked-parse-string start)))
    (if (not close)
      (error "Unmatched '{' in ~S." masked-parse-string))
    (make-parse :lisp-op-right "}" close (cl:+ close 1))))

(defmethod parse ((kind (eql :flv)) parse-string
                  start &optional (end (length parse-string)))
  (setf start (skip-whitespace parse-string start))
  (let* ((mask-parse-string (mask-quotes parse-string start end))
         (critical (position-if #'special-char-p mask-parse-string :start start)))
    (if critical
      (case (char parse-string critical)
        (#\( (parse :function parse-string start end))
        (#\[ (parse :vble-select parse-string start end))
        (#\{ (parse :lisp parse-string start end))
        (t (parse :variable parse-string start end)))
      (parse :variable parse-string start end))))

(defmethod parse ((kind (eql :function)) parse-string
                  start &optional (end (length parse-string)))
  (setf start (skip-whitespace parse-string start))
  (let ((mask-parse-string (mask-quotes parse-string start end))
        tree0 tree1 tree2 tree3)
    (setf tree1 (parse :function-left mask-parse-string start end))
    ;; we use (start-of tree1) here to help find-close-paren
    (setf tree3 (parse :function-right 
                       mask-parse-string
                       (start-of tree1)
                       end))
    (setf tree0 (parse :function-name
                       parse-string
                       start
                       (start-of tree1)))
    (if (eq (kind-of tree0) :null)
      ;; this is the case when the parens mark an infix subexpression
      (progn
        (setf tree2 (parse :infix parse-string (end-of tree1) (start-of tree3)))
        (decf (start-of tree2))
        (incf (end-of tree2))
        tree2)
      ;; this is the usual function case
      (progn
        (setf tree2 (parse :args
                           parse-string
                           (end-of tree1)
                           (start-of tree3)))
        (if (not tree2)
          (error "Null argument list to ~A(..)" (node-of tree0)))
        ;; don't need tree1 and tree3 after this ...
        (make-parse :function
                    (subseq parse-string (start-of tree0) (end-of tree3))
                    (start-of tree0)
                    (end-of tree3)
                    (list tree0 tree2))))))

(defmethod parse ((kind (eql :lisp)) parse-string
                  start &optional (end (length parse-string)))
  (setf start (skip-whitespace parse-string start))
  (let ((mask-parse-string (mask-quotes parse-string start end))
        tree0 tree1 tree2 tree3)
    (setf tree1 (parse :lisp-op-left mask-parse-string start end))
           (setf tree3 (parse :lisp-op-right mask-parse-string (end-of tree1) end))
           (setf tree0 (parse :variable 
                              parse-string
                              start
                              (start-of tree1)))
           (setf tree2 (parse :lisp-op parse-string (end-of tree1) end))
           (make-parse :lisp
                       (subseq parse-string (start-of tree0) (end-of tree3))
                       (start-of tree0)
                       (end-of tree3)
                       (list tree0 tree2))))

(defmethod parse ((kind (eql :lisp-read)) parse-string
                  start &optional (end (length parse-string)))
  (let ((lisp-string (subseq parse-string start end)))
    ;;
    ;; read-from-string is used instead of intern so that one can
    ;; use, say, {(my-package::foo x)} in a formula
    ;;
    (multiple-value-bind (lisp-object local-end)
                         (read-from-string lisp-string)
      (make-parse :lisp-read
                  lisp-object
                  start
                  (+ start local-end)))))
      
#|
;;; this is conceptually what's going on, but it's slow so use it only for
;;; development and debugging.
;;; node contains current parse-string when subtrees is non-nil, else
;;; the value for that terminal tree.

(defclass parse-tree ()
  ((kind :accessor kind-of :initarg :kind)
   (node :accessor node-of :initarg :node)
   ;; start of this node in parent's node
   (start :accessor start-of :initarg :start)  
   ;; end (points to first char beyond)
   (end :accessor end-of :initarg :end)
   (subtrees :accessor subtrees-of :initarg :subtrees :initform nil)))

(defmethod print-object ((pt parse-tree) stream)
  (if (slot-boundp pt 'node)
            (format stream "#<~S ~S ~S>"
                    (class-name (class-of pt))
                    (kind-of pt)
                    (node-of pt))
            (format stream "#<~S ~S>"
                    (class-name (class-of pt))
                    (qk::system-get-pointer pt)))
  pt)

(defmethod print-object ((pt parse-tree) stream)
  (cond 
    ((eq stream *terminal-io*)               (inspect pt))
    ((eq stream *standard-output*)           (inspect pt))
    ((eq stream *query-io*)                  (inspect pt))
    ((eq stream *debug-io*)                  nil)
    ((eq stream *error-output*)              nil)
    ((eq stream *trace-output*)              nil)
    ((eq stream *quail-terminal-io*)         (inspect pt))
    ((eq stream *quail-standard-output*)     (inspect pt))
    ((eq stream *quail-query-io*)            (inspect pt))
    ((eq stream *quail-debug-io*)            nil)
    ((eq stream *quail-error-output*)        nil)
    ((eq stream *quail-trace-output*)        nil)
    (T                                       nil))
  (if (slot-boundp pt 'node)
            (format stream "#<~S ~S ~S>"
                    (class-name (class-of pt))
                    (kind-of pt)
                    (node-of pt))
            (format stream "#<~S ~S>"
                    (class-name (class-of pt))
                    (qk::system-get-pointer pt)))
  pt)

(defun make-parse (kind node start end &optional (subtrees nil))
  (make-instance 'parse-tree
                 :kind kind
                 :node node
                 :start start
                 :end end
                 :subtrees subtrees))

(defun build-tree (kind super-node subtrees)
  (let ((start (apply #'min (mapcar #'start-of subtrees)))
        (end (apply #'max (mapcar #'end-of subtrees))))
  (make-instance 'parse-tree
                 :kind kind
                 :node (subseq super-node start end)
                 :start start
                 :end end
                 :subtrees subtrees)))
|#

     
          
    
         
    
   
