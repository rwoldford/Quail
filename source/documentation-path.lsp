;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          documentation-path.lsp                               
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


(in-package :make)

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
  (system-name-convert "Documentation;"))

(defun path-doc-polished ()
  (append-directories
         (path-quail)
         (system-name-convert "Doc;polished;"))
  )

(defun path-doc-indices ()
  (append-directories
         (path-quail)
         (system-name-convert "Doc;indices;"))
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
         (system-name-convert "Doc;auto;")))
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
         (system-name-convert "Doc;tex;")))
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
         (system-name-convert "Doc;current-symbols;")))
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
