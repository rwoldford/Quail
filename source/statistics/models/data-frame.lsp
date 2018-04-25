;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              data-frame.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1992 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     D.G. Anglin 1992-93
;;;     R.W. Oldford 1993
;;;
;;;--------------------------------------------------------------------------------

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(data-frame)))

(defclass data-frame (data-object ref-object)
  ((qk::ref-obj
    :initarg :ref-obj
    :initform "ref-obj slot is not used in direct-ref objects of class data-frame")
   (data :accessor data-of :initarg :data)
   (data-names :accessor data-names-of :initarg :data-names)
   (data-types :accessor data-types-of :initarg :data-types)))

;;; This function was destructive to data-names!  ... rwo
;;;
(defun data-frame-merge-nicknames (data-names data-nicknames)
  (let (n
        nn
        (data-names-copy (copy-list data-names)))
    (if (<= (length data-nicknames) (length data-names))
      (progn
        ; (loop for i from 0 to (- (length data-nicknames) 1) do 
        (do ((i 0 (incf i)))
            ((= i (length data-nicknames)))
          (setf n (elt data-names-copy i))
          (setf nn (elt data-nicknames i))
          (setf (elt data-names-copy i) (list*
                                         n
                                         (if (stringp nn)
                                           (list nn)
                                           nn))))
        ; (loop for i from (length data-nicknames) to (- (length data-names-copy) 1) do
        (do ((i (length data-nicknames) (incf i)))
            ((= i (length data-names-copy)))
          (setf n (elt data-names-copy i))
          (setf (elt data-names-copy i) (list n))))
      (quail-error "~&Nicknames ~S provided do not match names ~S."
                   data-nicknames
                   data-names))
    data-names-copy))


(defmethod initialize-instance :after ((self data-frame) &rest initargs)
  (declare (ignore initargs))
  (with-slots (data data-names data-types) self
    (let* ((numcols (length data))
           (difftypes (- numcols (length data-types))))
      (if (not (eq numcols (length data-names)))
        (quail-error "~&Names ~S provided do not match data ~S."
                     data-names
                     data))
      (if (>= difftypes 0)
        (setf data-types 
              (let* ((types (append data-types (make-list difftypes
                                                          :initial-element nil))))
                (loop as type in types
                      as d in data
                      collect (if type
                                (if (symbolp type) (find-class type) type)
                                (class-of d)))))
        (quail-error "~&Types ~S provided do not match data ~S."
                     data-types
                     data))
      )))
    
(defmethod data-frame ((data list)
                       (names list)
                       &key types nicknames)
    (make-instance 'data-frame
                   :data data
                   :data-names (data-frame-merge-nicknames names nicknames)
                   :data-types types))

(defmethod data-frame ((data matrix) (names string) &key types nicknames)
  (data-frame (list data) (list names) :types types :nicknames nicknames))

(defmethod data-frame ((data matrix) (names symbol) &key types nicknames)
  (data-frame data (string-downcase names) :types types :nicknames nicknames))

(defmethod data-frame ((data matrix) (names list) &key types nicknames)
  (let* ((dim (matrix-dimensions-of data))
         (num-columns (second dim))
         (num-names (length names)))
    (if (= num-names num-columns)
      (data-frame (loop for j from 0 to (1- num-columns)
                        collect (ref data t j))
                  names
                  :types types
                  :nicknames nicknames)
      (if (= num-names 1)
        (data-frame data 
                    (first names)
                    :types types
                    :nicknames nicknames)
        (quail-error "~&Can't determine proper assignment of names ~S to ~
                      data ~S with types ~S."
                     names
                     data
                     types)))))

(defmethod name-position (name (data-frame data-frame))
  (with-slots (data-names) data-frame
    (name-position name data-names)))

(defmethod name-position (name (data-names t))
  (let ((len (first (dimensions-of data-names)))
        (pos nil))
    (loop for i from 0 to (- len 1)
          until pos
          do (if (member name (eref data-names i) :test #'string-equal)
               (setf pos i)))
    pos))

(defmethod name-position (name (data-names null))
  (declare (ignore name))
  nil)

(defun name-type (name data-frame)
  (with-slots (data-types data-names) data-frame
    (eref data-types (name-position name data-names))))

(defmethod ref ((r data-frame) &rest args)
  (let ((ref-r (ref-instantiate r)))
    (ref-kernel ref-r r args)))

(defmethod ref-instantiate ((r data-frame))
  (make-instance (class-name (class-of r)) :ref-obj r))

(defmethod ref-kernel ((ref-r data-frame) r args)
  (with-slots (data
               data-names
               data-types) r
    (let ((ref-args (loop for name in args
                          collect (name-position name data-names))))
      (with-slots ((ref-data data)
                   (ref-data-names data-names)
                   (ref-data-types data-types)) ref-r
        (setf ref-data 
              (apply #'ref data (cons ref-args (list :shape t))))
        (setf ref-data-names
              (apply #'ref data-names (cons ref-args (list :shape t))))
        (setf ref-data-types
              (apply #'ref data-types (cons ref-args (list :shape t)))))))
  ref-r)

;;; eref returns two values, the data and its type.
;;; returns :none if there is nothing there.

(defmethod eref ((r data-frame) &rest args)
  (if (> (length args) 1)
    (error "Too many args to eref of a data-frame: (eref ~S ~{~S~})"
           r args)
    (with-slots (data-names data data-types) r
      (let ((pos (name-position (first args) data-names)))
        (if pos 
          (values (elt data pos) (elt data-types pos))
          :none)))))

(defun add-name (data-frame new-name previous-name)
  (with-slots (data-names) data-frame
    (let* ((pos (name-position previous-name data-names))
           (names (elt data-names pos)))
      (if (member new-name names)
        names
        (setf (elt data-names pos)
              (append names (list new-name)))))))

(defun add-data (data-frame new-data name &key type nicknames)
  ;;  We append here so that any previous refs to the data-frame are
  ;;  unaffected.
  (with-slots (data data-names data-types) data-frame
    (setf data (append data (list new-data)))
    (setf data-names (append data-names (list (list* name nicknames))))
    (setf data-types (append data-types 
                             (list (or type (class-of new-data))))))
  new-data)

(defmethod data-size ((df data-frame))
  ;; this'll need work later.
  (first (dimensions-of (first (data-of df)))))
      
(defmethod print-object ((df data-frame) stream)
  (let ((len (if (slot-boundp df 'data-names)
               (first (dimensions-of (data-names-of df)))
               :unbound)))
    (cond ((or (null len)
               (eq len :unbound))
           (format stream "#<~S ~S>"
                   (class-name (class-of df))
                   (qk::system-get-pointer df)))
          (t   (with-slots (data-names) df    
                 (format stream "#<~S" (class-name (class-of df)))
                 ; (loop for i from 0 to (- len 1) do
                 (do ((i 0 (incf i)))
                     ((= i len))
                   (format stream " ~S" (first (eref data-names i))))
                 (format stream ">"))))))
               
