;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               restore-pc.lsp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W.Oldford 2000
;;;     G.W.Bennett 2000
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)
;;;
;;; The single acl dependency here.  Due to the fact that 
;;; Work-around to define (logical-pathname-translations "q")
;;; until Franz deals with the rescuing of single-letter logical-pathnames
;;; in images created by dumplisp.
;;; In fact this is probably a better way to do the job 
;;; and is to be a permanent part of Quail .. 16FEB00
;;; A directory dialog is popped up and the choice parsed
;;; If the directory is a top-level one then its namestring
;;; ends in \\, otherwise it seems it does not..
;;; in any case we need to check so that only one set of \\
;;; at a time appears in the result

;;;

(defun find-quail ()
  (let (location)
    (cond
     ((probe-file "sys:quail-location.lsp")
      (with-open-file (s "sys:quail-location.lsp" :direction :input)
        (setf location (read s))
        )
      )
     (T
      (setf location (cg::name-string (cg::ask-user-for-directory
                                       :prompt
                                       "Quail directory ...")
                                      )
            )
      (with-open-file (s "sys:quail-location.lsp" :direction :output
                         :if-exists :supersede)
        (prin1 location s))
      ))
     (if (string-equal "\\" (subseq location (1- (length location))))
       (setf (logical-pathname-translations "q")
             (list (append (list "**;*.*")
                           (list (concatenate 'string location "**\\*.*")))))
       (setf (logical-pathname-translations "q")
             (list (append (list "**;*.*")
                           (list (concatenate 'string location "\\**\\*.*")))))
     )
       (setf (logical-pathname-translations "eg")
              (list (list "**;*.*" "q:examples;**;*.*")
                    (list "*.*" "q:examples;*.*")
              )
             )
       (setf (logical-pathname-translations "data")
              (list (list "**;*.*" "q:data;**;*.*")
                    (list "*.*" "q:data;*.*")
              )
             )
       (setf (logical-pathname-translations "doc")
              (list (list "**;*.*" "q:doc;**;*.*")
                    (list "*.*" "q:doc;*.*")
              )
             )
     )
    )
