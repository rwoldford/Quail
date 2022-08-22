;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                quail-make.lsp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1997 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1997-2000
;;;
;;;
;;;----------------------------------------------------------------------------
;;;
;;;
;;;   This file contains the parameters which need modification for
;;;   each local system.  These need to be modified for each installation
;;;   NOT for each user.
;;;
;;;   After the end of the modifiable parameters, the "make" begins
;;;   by loading appropriate files.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;   In this file various features are used to distinguish which Common Lisp
;;;   system is being used.  Some examples are
;;;
;;;   :ccl, :mcl, :ccl-4.0, etc.  ... Macintosh Common Lisp sometimes by version
;;;   :aclunix                    ... Franz Allecro CL for Unix
;;;   :aclpc                      ... Franz Allegro as implemented for Windows
;;;   :aclpc-mswins                   ... running on Windows
;;;   :aclpc-linux                    ... running on Linux
;;;   :aclpc-mac                      ... running on Mac
;;;   :cl-2                       ... A Common Lisp: The Language 2nd Edition
;;;                                   (by Steele) so CLOS is included for example.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;;;
;;;   Begin by figuring out what Common Lisp system is running and 
;;;   putting appropriate info on the *features* for us.
;;;
;;;
;;;  Because the correct name for the Common Lisp's user package
;;;  depends on the edition of Steele's book (Common Lisp: The Language)
;;;  whether the implementation corresponds to the 2nd edition or not.
;;;  DO NOT CHANGE THIS.
;;;

#+(and :allegro :unix)(pushnew :aclunix *features*)

#+(and :allegro :mswindows)(pushnew :aclpc-mswin *features*)
#+(and :linux  :allegro)(pushnew :aclpc-linux *features*)
#+(and :target-mac :allegro)(pushnew :aclpc-mac *features*)
#+(and (or :aclpc-linux :aclpc-mac :aclpc-mswin) :allegro)(pushnew :aclpc *features*)


#+(and :sbcl :unix) (pushnew :sbcl-linux *features*)
#+(or :ccl-2 :aclunix :cltl2 :aclpc :sbcl-linux :ccl-1.11)(pushnew :cl-2 *features*)
(format t "Running  ~s "(or #+:linux (format nil "LINUX") #+:windows (format nil "WINDOWS")))
(format t "~%Using ~s version ~s  "
  (format nil (lisp-implementation-type))
  (format nil (lisp-implementation-version)))
(format t "~%and ASDF version ~s ~%" (asdf:asdf-version))
(format t "~%At the top of quail-make")

;;; We require this package to find lambda lists etc.
;;; Seemed to be automatically in Linux but not Mac.
#+:sbcl (require "sb-introspect")

;;;
;;;  Now get the correct name for the Common Lisp's user package
;;;  to be used for this file
;;;  DO NOT CHANGE THIS.
;;;

#+:cl-2 (in-package :cl-user) 
#-:cl-2(in-package "USER")

;
;;;   start with the directory from which quail-make.lsp (this file) was loaded:
#+:cl-2(setf *quail-make-load-directory* (make-pathname :directory
    (pathname-directory *load-truename*)))


;;; Setup logical pathname for quail

(defun set-up-quail (&key (base *quail-make-load-directory*))
  (let* ((location (merge-pathnames  "quail-init.lsp"  base ))
        (dir-list (pathname-directory base))
         (string-qmld "/"))
    (loop for dir in (cdr dir-list)
          do (setf string-qmld (concatenate 'string string-qmld dir "/")))
    (setf string-qmld (concatenate 'string string-qmld "**/*.*"))
     (cond ((probe-file location)
      #+(or :sbcl-linux :aclpc-linux :ccl)
      (setf location
          (list (append (list "**;*.*.*")
                       (list string-qmld)                       
                         )))
      #+:aclpc-mswin(setf location
         (list (append (list "**;*.*")
                        (list  string-qmld "**\\*.*")))))
     (t
      (warn  (format NIL "~%Prompt cancelled~%path to quail not set. ~
                                 ~%Edit or reload quail-make.lsp to try again."))
             (return-from set-up-quail))
      )))

(setf (logical-pathname-translations "q")
    (set-up-quail))

(pushnew :quail *features*)




;;;  With Quail located, "q:" is a logical-directory that
;;;  now refers to the Quail directory in Common Lisp,
;;;
;;;  For example, a file called bar.lsp can now be referred to as
;;;
;;;     "q:bar.lsp"
;;;
;;;  and files (using the "*" wildcard) in a (non-existent) sub-directory 
;;;  of Quail called foo can now be referred to as
;;;
;;;     "q:foo;*"
;;;
;;;  and all ".lsp" files nested (arbitrarly deep)
;;;  within all directories within the Quail foo directory as
;;;     "q:foo;**;*.lsp"
;;;
;;;  (Aside: certain important Quail sub-directories will be given their
;;;   own logical directory name defined via "q:", namely
;;;
;;;        Data directory    ...  "Data:"
;;;        Examples directory ... "Eg:"
;;;
;;;   This is the recommended way to access the directories in Quail.)
;;;
;;;;;;;;;;


;;; Need this from quail-make-1
(defun lisp-ext (filename-string)
  (concatenate 'string filename-string "." "lsp"))
;;;;;;;;;;
;;;
;;; What follows should not need modification UNLESS
;;; the Quail Examples and Data directories have been moved out of
;;; the Quail directory system.
;;;
;;; OR unless the lisp downcases names in logical-pathname-translations
;;; as sbcl does
;;;;;;;;;;
;;;
;;;  Examples  directory
;;;


(let (translation)
  ;;  The following should just work for everything.
  ;;  Do not change this.
  (if (probe-file (translate-logical-pathname (lisp-ext "q:examples;welcome")))
    (setf translation "q:examples;**;*.*"))
  
  ;;  If that failed, then the Examples directory has been
  ;;  moved elsewhere and you better say where here.
  ;;  For example
  
  (if translation
    (setf (logical-pathname-translations "eg")
          (list (list "**;*.*" translation)
                #+:aclpc (list "*.*" "q:examples;*.*")
                )
         )
    (warn "Can't find Examples."))
  )

;;;  That done, "eg" now refers to the Quail Examples directory in Common Lisp.
;;;
;;;;;;;;;;

;;;;;;;;;;
;;;
;;;  Data
;;;


  (let ((translation
       ;;  The following should just work for everything.
       ;;  Do not change this.
       (if (probe-file (translate-logical-pathname (lisp-ext "q:data;welcome")))
            "q:data;**;*.*")))
  
  ;;  If that failed, then the Data directory has been
  ;;  moved elsewhere and you better say where here.
  ;;  For example
  
  (if translation
    (setf (logical-pathname-translations "data")
          (list (list "**;*.*" translation)
                #+:aclpc (list "*.*" "q:data;*.*")))
    (warn "Can't find Data."))
  )

;;;  That done, "data" now refers to the Quail Data directory in Common Lisp.
;;;
;;;;;;;;;;
;;;;;;;;;;
;;;
;;;  Documentation
;;;


(let ((translation
       ;;  The following should just work for everything.
       ;;  Do not change this.
       (if (probe-file (translate-logical-pathname (lisp-ext "q:doc;welcome"))) 
                       "q:doc;**;*.*")))
  
  ;;  If that failed, then the Doc directory has been
  ;;  moved elsewhere and you better say where here.
  ;;  For example
  (if translation
    (setf (logical-pathname-translations "doc")
          (list (list "**;*.*" translation)
                #+:aclpc (list "*.*" "q:doc;*.*")))
    (warn "Can't find Documentation."))
  )

;;;  That done, "doc" now refers to the Quail Doc directory in Common Lisp.
;;;

;;;;;;;;;;
;;;
;;;    Define the lookup for the Quail-init file.
;;;

(defvar *quail-init-directory* *quail-make-load-directory* 
  "This is where user-defined initialization instructions would be.")

(defun quail-init-file ()
   "Returns the pathname for the quail-init file."
   (merge-pathnames *quail-make-load-directory* "Quail-init.lsp"))

;;;  A variable to contain the Quail subsystems.  Set in quail.qmk.

(defvar *quail-systems* NIL
  "Collection of Quail systems loaded into the current Quail image.~%~
   Defined in the file q:quail.qmk")

;;;  Define the systems which make up Quail

(setf *quail-systems* (list "quail-user"
                            "initialization"
                            ;; "analysis-map"
                            ;;"browser" 15F2018
                            "statistics"
                            "probability"
                            "mathematics"
                            "linear"
                            ;;"top-level"
                            ;;"documentation"
                            ;; systems above this line use Quail package.
                            "quail"
                            ;;"views"
                            ;;"window-basics"
                            "new-math"
                            "quail-kernel"
                            ))


;;;  Do the make
(format t "~%Starting the make")
(loop for system in (reverse *quail-systems*)
  do (when (or (string-equal system "quail-kernel") (string-equal system "initialization")) 
  #+:sbcl(sb-ext:unlock-package :sb-mop)
  #+:sbcl(sb-ext:unlock-package :common-lisp))
  (asdf:clear-source-registry)
      (format t "~%Loading ~s " system)
      (asdf:load-system system)
  (when (or (string-equal system "quail-kernel")  (string-equal system "initialization"))
  #+:sbcl(sb-ext:lock-package :sb-mop)
  #+:sbcl(sb-ext:lock-package :common-lisp))    
      (format t "~% ~s loaded" system)
    )
(format t "~%Quail Loaded")

;;;;;;;;;;;;;;;;;;;;;;;;;;;; End of File ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

