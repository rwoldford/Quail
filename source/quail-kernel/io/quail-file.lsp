;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-file.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989-93.
;;;
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(*quail-data-dir-default*
          *quail-data-dir*
          set-quail-data-directory
          quail-file
          quail-file-pathname
          quail-file-cleanup)))

;--------------------------------------------------------------------------------

(defvar *quail-data-dir-default*)
;; this is a CLtL2 logical pathname
;#-:sbcl-linux
(setf *quail-data-dir-default* "Home:Quail Data;")
;#+:sbcl-linux(setf *quail-data-dir-default* cl-user::*data-dir*)

(defvar *quail-data-dir*)
(setf *quail-data-dir* nil)

(defvar *quail-files*)
(setf *quail-files* nil)

;--------------------------------------------------------------------------------

(defclass quail-file (quail-open-state-mixin quail-object)
  ((pathname :accessor pathname-of
             :initarg :pathname
             :initform :NONE)
   (stream :accessor stream-of
           :initarg :stream
           :initform :NONE)))

(push-extension-class 'quail-file)

(defmethod initialize-instance :after ((self quail-file) &rest args)
  (declare (ignore args))
  (pushnew self *quail-files* :test #'equal))

(defun quail-file-cleanup (&optional (destroy nil))
  (mapcar (if destroy
            #'quail-close-destroy-object
            #'quail-close-physically-object)
          *quail-files*)
  nil)

(defmethod quail-open-object ((self quail-file))
  (with-slots (pathname stream) self
    (let ((open (quail-open-p-object self)))
      (if (not open)
        (setf stream (open-existing-io-file pathname))))))

(defmethod quail-close-preserve-object ((self quail-file))
  (let ((open (quail-open-p-object self))
        (originally-open (originally-open-p-object self)))
    (if (and open
             (not originally-open))
      (quail-file-close self))))

(defmethod quail-close-destroy-object ((self quail-file))
  (quail-file-destroy self))

(defmethod quail-close-physically-object ((self quail-file))
  (let ((open (quail-open-p-object self)))
    (if open
      (quail-file-close self))))

;;;
;  quail-file operations
;

(defmethod quail-file-initial-open ((self quail-file))
  (with-slots (pathname stream open-state) self
    (setf stream (open-io-file pathname))
    (setf open-state 1)
    #+:open-mixin-debug
    (format *quail-terminal-io*
            "~&Doing quail-file-initial-open of ~S, open-state ~S --> ~S"
            self
            (1- open-state)
            open-state)
    ))

(defmethod quail-file-open ((self quail-file))
  (with-slots (stream pathname) self
    (setf stream (open-existing-io-file pathname))))

(defmethod quail-file-close ((self quail-file))
  (with-slots (stream) self
    (if (streamp stream)
      (close stream))))

(defmethod quail-file-destroy ((self quail-file))
  (quail-file-close-and-delete self)
  (with-slots (pathname stream) self
    (setf pathname :none)
    (setf stream :none))
  (setf *quail-files* (remove self *quail-files*)))

(defmethod quail-file-close-and-delete ((self quail-file))
  (with-slots (pathname stream) self
    (if (streamp stream)
      (close stream))
    (if (and (pathnamep pathname)
             (probe-file pathname))
      (delete-file pathname))))

(defmethod quail-file-read ((self quail-file))
  (with-open-quail-objects (self) quail-file
    (with-slots (stream) self
      (read stream))))

(defmethod quail-file-read-at-index ((self quail-file) index)
  (with-open-quail-objects (self) quail-file
    (with-slots (stream) self
      (file-position stream index)
      (read stream))))

(defmethod quail-file-write ((self quail-file) format &rest args)
  (with-open-quail-objects (self) quail-file
    (with-slots (stream) self
        (apply #'format stream format args))))
    
(defmethod quail-file-write-at-index ((self quail-file) index format &rest args)
  (with-open-quail-objects (self) quail-file
    (with-slots (stream) self
      (file-position stream index)
      (apply #'format stream format args))))

;;  quail-file-length needs to be more explicit since using open-file-for-inspection.
;;  Note that it cheats by never modifying open-state.

(defmethod quail-file-length ((self quail-file))
  (with-slots (pathname stream) self
    (let ((open (quail-open-p-object self)))
      (if (not open)
        (setf stream (open-file-for-inspection pathname)))
      (prog1
        (file-length stream)
        (if (not open)
          (close stream))))))

;--------------------------------------------------------------------------------

;;;
;  File utility routines
;

(defun set-quail-data-directory (&optional dir &key reset)
  (declare (special *quail-data-dir* *quail-data-dir-default*))
  (setf *quail-data-dir*
        (pathname
         (or dir
             (let ((default (if reset
                              *quail-data-dir-default*
                              (or *quail-data-dir* *quail-data-dir-default*))))
               (funcall
                ;; A way of making this wb function accessible despite the fact
                ;; that package wb does not exist at this point when building quail.
                (symbol-function (find-symbol "PROMPT-USER" "WINDOW-BASICS"))                    
                :prompt-string (format nil "Please provide the name of a directory to reference for Quail data")
                :initial-string (namestring 
                                 (translate-logical-pathname default))
                :type 'string)))))
  (if (pathname-name *quail-data-dir*)
    ;; then the user forgot to specify the ending : or ;
    (setf *quail-data-dir* 
          (pathname
           (concatenate 'string 
                        (namestring *quail-data-dir*)
                        (if (typep *quail-data-dir* 'logical-pathname)
                          ";"
                          ":")))))
  *quail-data-dir*)

(defun quail-file-pathname (file &key 
                                 (prefix "quail-file-")
                                 (suffix nil))
  (declare (special *quail-data-dir* *quail-data-dir-default*))
  (let* ((file (if file (pathname file) ""))
         (name (pathname-name file))
         (dir (pathname-directory file)))
    (when (and (not dir) (not *quail-data-dir*))
      (set-quail-data-directory))
    (setf file (cond ((and (null name) (null dir))
                      (merge-pathnames *quail-data-dir*
                                       (get-unique-filename prefix
                                                            *quail-data-dir*)))
                     ((null dir)
                      (merge-pathnames *quail-data-dir* file))
                     ((null name)
                      (merge-pathnames file 
                                       (get-unique-filename prefix
                                                            dir)))
                     (t
                      file)))
    (if (pathname-type file)
      file
      (merge-pathnames file (make-pathname :type suffix)))))

(defun get-unique-filename (leader dir)
  (let (filename
        (there t))
    (loop while there
          do (progn
               (setf filename (string (gensym leader)))
               (setf there (probe-file (merge-pathnames dir filename)))))
    filename))

(defun open-file-for-inspection (file)
  (open file :direction :input
             :if-does-not-exist :create))

(defun open-existing-io-file (file)
  (open file :direction :io
             :if-exists :overwrite
             :if-does-not-exist :error))

(defun open-io-file (file)
  (open file :direction :io
             :if-exists :overwrite
             :if-does-not-exist :create))

(defun open-io-file-safely (file)
  (let (stream)
    ;; open :probe to determine if it exists
    (setf stream (open file :direction :probe))
    (if stream
      (quail-cerror "Overwrite ~S." "File ~S ~
                               ~&already exists."
              (truename file)))
    ;; open :direction :io for :overwrite
    (setf stream (open file :direction :io
                       :if-exists :overwrite
                       :if-does-not-exist :create))
    stream))

(defun open-io-file-with-no-mercy (file)
  (let (stream)
    ;; open :rename-and-delete then close to truncate
    (setf stream (open file :direction :output
                            :if-exists :rename-and-delete
                            :if-does-not-exist :create))
    (close stream)
    ;; now open :append (to a zero-length file)
    (setf stream (open file :direction :io
                            :if-exists :append
                            :if-does-not-exist :create))))



