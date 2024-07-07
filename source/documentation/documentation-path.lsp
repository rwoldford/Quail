;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          test-documentation-path.lsp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1991 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1991...1993
;;;
;;;
;;;----------------------------------------------------------------------------

;(in-package :make)
(in-package :quail-kernel) ;;; until we decide about packages

(defun maximum-file-name-length () 
  31)

(defun maximum-doc-ext-length () 
  5)

(defun doc-type-extension (doc-type)
  (let ((doc-ext (case doc-type
                   (:function           "fn")
                   (:generic-function   "gf")
                   (:macro              "mc")
                   (:arguments          "arg")
                   (:built-in-class     "bic")
                   (:class              "cla")
                   (:dataset            "ds")
                   (:constant           "cst")
                   (:variable           "var")
                   (:structure          "strc")
                   (:topic              "tpc"))))
    (concatenate 'string 
                 "-"
                 (string-downcase doc-ext))))

;; from quail-path.lsp
(defun system-name-convert (string)
"Removes spaces which some file systems cannot handle in a file name."
#+:apple string
#-:apple (substitute #\_ #\Space string))

;;; And 
;;; START gwb functions for appending pathnames
;;; The possibilities for the two inputs are
;;; real-abs-path, log-abs-path, real-rel-path, log-rel-path
;;; Any abs,rel pair is to return a LOGICAL pathname
;;; Write code to deal with each and then a wrapper to
;;; combine the results depending on the pairing.

(defun one-real-abs (pn)
  "convert one real/ordinary pathname to the start of  
  a logical-pathname."
  (let ((work NIL))
    ; replace all the / with ;
    (setf work (substitute #\; #\/ (namestring pn)))
    ; if the position of the first ; is 0, remove it
    (if (eql 0 (position #\; work))
        (setf work (remove-if #'(lambda (x) (eql x #\;)) work :count 1))
        work)
    ; replace the (now) first one with :
    (setf work (substitute #\: #\; work :count 1))
    work
    )
  )

(defun one-real-rel (pn)
  "convert one real relative pathname to the end of  
  a logical-pathname."
  (let ((work pn))
    (setf work (substitute #\; #\/ (namestring work)))
    work
    ))

(defun one-log-abs (pn)
  "we are done as we start"
  pn ;(pathname (namestring pn))
  )

(defun one-log-rel (pn)
  "we are done as we start"
  pn;(pathname (namestring pn))
  )

(defun append-directories (dir1 dir2)
  "Generates a logical-pathname for a DIRECTORY from dir1 and dir2
  dir1 is assumed to be an absolute name - either ordinary or logical,
  dir2 is asssumed to be a relative name - either ordinary or logical"
  (cond ((and (position #\/ dir1) (position #\/ dir2))
         ;(pathname 
           (concatenate 'string (one-real-abs dir1) (one-real-rel dir2)))
         ;)
        ((and (position #\/ dir1) (position #\; dir2))
         ;(pathname 
           (concatenate 'string (one-real-abs dir1) (one-log-rel dir2)))
        ;)
        ((and (position #\: dir1) (position #\/ dir2))
         ;(pathname 
           (concatenate 'string (one-log-abs dir1) (one-real-rel dir2)))
         ;)
        ((and (position #\: dir1) (position #\; dir2))
         ;(pathname 
           (concatenate 'string (one-log-abs dir1) (one-log-rel dir2)))
         ;)
        (t (format t "~%At least one of ~s and ~s is not either an absolute or a relative path specifier" dir1 dir2))
        )
  )

;;; Seems to do what it required using
;;; real-absolute "q/r/s/" real-relative "t/u/v"
;;; logical absolute "q:r;s" logical-relative "t;u;v;"
;;; all 4 possibiities yield #P"Q:R;S;T;U;V;"

;;;

(defun force-to-pathname (pathname)
  "Ensures a pathname that is a legal argument for merge-pathnames, say, ~
   is returned.  Required by edit-file."
   ;; Possibly an egregious hack.  ... rwo
  #+:aclpc (cond
             ((typep pathname 'cl::pathname) pathname)
             (T (translate-logical-pathname pathname)))
   #-:aclpc pathname
   )

(defun doc-merge-pathnames (p1 p2)
   ;; Possibly an egregious hack.  ... rwo
   ;; It's all ACL's fault.
   (cond 
         ((and (stringp p1) (stringp p2))
          (concatenate 'string p1 p2))
         (T 
   (merge-pathnames (force-to-pathname p1) p2))))

(defun path-documentation ()
  (system-name-convert "documentation;"))

(defun path-doc-polished ()
  (append-directories
         (path-quail)
         (system-name-convert "doc;polished;"))
  )

(defun path-doc-indices ()
  (append-directories
         (path-quail)
         (system-name-convert "doc;indices;"))
  )

(defun path-doc-out (&optional symbol &aux dir)
  (setf dir (path-doc-polished))
  (if symbol
    (setf dir
          (append-directories
           dir
           (concatenate 'string
                        (string-downcase
                         (package-name (symbol-package symbol)))
                        ";"))))
  dir)

(defun path-auto-doc-out (&optional symbol &aux dir)
  (setf dir
        (append-directories
         (path-quail)
         (system-name-convert "doc;auto;")))
  (if symbol
    (setf dir
          (append-directories
           dir
           (concatenate 'string
                        (string-downcase
                         (package-name (symbol-package symbol)))
                        ";"))))
  dir)

(defun path-general-topics-out ()
  (append-directories
    (path-doc-polished)
    (concatenate 'string "topics"
                        ";")))

(defun path-doc-tex-out (&optional symbol &aux dir)
  (setf dir
        (append-directories
         (path-quail)
         (system-name-convert "doc;tex;")))
  (if symbol
    (setf dir
          (append-directories
           dir
           (concatenate 'string
                        (string-downcase
                         (package-name (symbol-package symbol)))
                        ";"))))
  dir)

(defun path-doc-current-symbols-out (&optional directory &aux dir)
  (setf dir
        (append-directories
         (path-quail)
         (system-name-convert "doc;current-symbols;")))
  (if directory
    (setf dir
          (append-directories
           dir
           (concatenate 'string
                        (string-downcase directory)
                        ";"))))
  dir)

;;;--------------------------------------------------------------------
;;;
;;;  Need to be able to remove * from file-names since these
;;;  are typically wildcard characters in a filing system.
;;;
;;;--------------------------------------------------------------------

(defun string-replace-* (string)
  "Returns a copy of the input string with every occurrence of ~
   the character * by the string ``star''."
  (setf string (if (stringp string)
                 string
                 (format NIL "~s" string)))
  (let ((replaced-string ""))
    (with-input-from-string (ifile string)
      (do ((next-char (read-char ifile nil nil)
                      (read-char ifile nil nil)))
          
          ((null next-char))
        (setf replaced-string 
              (concatenate 'string replaced-string
                           (if (char= next-char #\*)
                             "star"
                             (string next-char))))))
    replaced-string))


(defun doc-file-name (sym doc-type)
  (let* ((file-name (string-replace-* (string-downcase (string sym))))
         (dt-ext (doc-type-extension doc-type))
         (max-len (- (maximum-file-name-length)
                     (length dt-ext)
                     (maximum-doc-ext-length))))
    (if (> (length file-name) max-len)
      (concatenate 'string (subseq file-name 0 max-len) dt-ext)
      (concatenate 'string file-name dt-ext))))


(defun doc-full-file-name (sym doc-type &optional (extension "lsp"))
  (concatenate 'string
               (doc-file-name sym doc-type)
               "."
               extension))

       
(defun doc-path-name (sym doc-type &optional (extension "lsp"))
  (doc-merge-pathnames (path-doc-out sym)
                   (doc-full-file-name sym doc-type extension)))

(defun doc-general-topics-path-name (sym doc-type &optional (extension "lsp"))
  (doc-merge-pathnames
   (path-general-topics-out)
   (doc-full-file-name sym doc-type extension)))

       
(defun doc-auto-path-name (sym doc-type &optional (extension "lsp"))
  (doc-merge-pathnames (path-auto-doc-out sym)
                   (doc-full-file-name sym doc-type extension)))

       
(defun doc-tex-path-name (sym doc-type &optional (extension "tex"))
  (doc-merge-pathnames 
   (path-doc-tex-out sym)
   (doc-full-file-name sym doc-type extension)))

(defun doc-current-symbols-path-name (name &key directory (extension "lsp"))
  (doc-merge-pathnames
   (path-doc-current-symbols-out directory)
   (if extension
     (concatenate 'string (string-downcase name) "." extension)
     (string-downcase name))))

(defun doc-index-filename (&key package)
  (doc-merge-pathnames
   (append-directories
         (path-doc-indices)
         (system-name-convert
          (concatenate 'string
                (string-downcase (package-name package))
                ";")))
   "doc-index.lsp"
   ))

(defun doc-auto-topics-path-name (filename &optional (extension "lsp"))
  (doc-merge-pathnames (path-auto-doc-out)
                   (concatenate 'string
                                (string filename)
                                "."
                extension)))
