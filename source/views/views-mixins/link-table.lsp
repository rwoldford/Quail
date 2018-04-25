;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               link-table.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1992 George Washington University
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(link-table asymmetric-link-table link-table-add-view set-link-table-test use-link-table
           link-table-remove-view link-table-clear show-links
           make-link-table *default-link-table* default-link-table
           foreground-link-table background-link-table
           choose-link-table delete-link-table delete-link-tables)))
;;;----------------------------------------------------------------------------------
(defvar *default-link-table* nil)
(setq *default-link-table* nil)


(defgeneric link-table-add-view(link-table view)
  (:documentation "Adds view to the link-table."))

(defgeneric link-table-remove-view(link-table view)
 (:documentation "Removes view from the link-table."))

(defvar *foreground-link-tables* nil)

(defvar *background-link-tables* nil)

(defclass link-table() 
  ((test :initarg :test
         :accessor link-table-test-of)
   (view-test :initarg :view-test
         :accessor link-table-view-test-of)
   (saved-links :initform nil
         :accessor saved-links-of)
   (name :initform nil :initarg :name
         :accessor link-table-name-of)
   (views :initform nil
          :accessor link-table-views-of)))

(defclass asymmetric-link-table(link-table) 
  ())

(defmethod link-table-test-of :before ((self link-table))
  (if (null (slot-value self 'test))
    (setf (slot-value self 'test) #'(lambda(a b) (declare (ignore a b)) t))))

(defmethod link-table-name-of ((self link-table))
  (or (slot-value self 'name)
      (string (qk::function-name (link-table-test-of self)))))


(defun set-link-table-test (new-test &key (link-table (default-link-table))
                                     (rebuild-links? t) (draw? rebuild-links?))
   "Sets the test of link-table to new-test."
   (setf (link-table-test-of link-table) new-test)
  (when rebuild-links?
  (let  ((views (link-table-views-of link-table)))
    (link-table-clear link-table)
    (loop for v in views do
          (link-view v :link-table link-table :draw? draw?))) ))

(defun set-link-table-view-test (new-test &key (link-table (default-link-table)))
   "Sets the test of link-table to new-test."
   (setf (link-table-view-test-of link-table) new-test)
   (loop for v in (link-table-views-of link-table) do
         (setf (linked-views-of v)
         (loop for s in (linked-views-of v)
               when (funcall new-test v s)
               collect s))))
 
 

(defun make-double-links (v1 v2) 
  (push v1 (linked-views-of v2))
  (push v2 (linked-views-of v1)))

(defun make-the-link (v1 v2) 
  (push v2 (linked-views-of v1)))




;;----------------------------------------------------------------------------------------


  
(defun show-links( &key (test #'identity) (sleep 1) 
                        (link-table (default-link-table)))
  "Selects links, one at a time"
  (loop for v in (link-table-views-of link-table) 
        when (funcall test v)
        do (select-one v)
        (sleep sleep)))


(defmethod link-table-add-view((self link-table) view)
  (setf (link-table-of view) self)
  (push view (link-table-views-of self))
  view)

(defmethod link-table-remove-view((self link-table) view)
   (loop for link in (linked-views-of view)
        do (setf (linked-views-of link) 
                 (delete view (linked-views-of link))))
  (setf (linked-views-of view) nil)
 
  (setf (link-table-of view) nil)
  (setf (link-table-views-of self)
        (remove view (link-table-views-of self))))

(defmethod link-table-remove-view((self asymmetric-link-table) view)
 (loop for link in (link-table-views-of self)
        do (setf (linked-views-of link) 
                 (delete view (linked-views-of link))))
  (setf (linked-views-of view) nil)
 
  (setf (link-table-of view) nil)
  (setf (link-table-views-of self)
        (remove view (link-table-views-of self))))


(defun link-table-clear (&optional (link-table (default-link-table)))
  "Clears the link-table. ~
   If a link-table is not specified, the ~
   default link-table is cleared."
  (loop for v in (link-table-views-of link-table) do
        (setf (linked-views-of v) nil)
        (setf (link-table-of v) nil))
  (setf (link-table-views-of link-table) nil))


(defun delete-link-table (&optional (link-table (default-link-table)))
  "Deletes the link-table. ~
   If a link-table is not specified, the ~
   default link-table is deleted."
  (declare (special  *foreground-link-tables* *background-link-tables*))
  (loop for v in (link-table-views-of link-table) do
        (setf (linked-views-of v) nil)
        (setf (link-table-of v) nil))
  (setf (link-table-views-of link-table) nil)
  (setq *foreground-link-tables* (remove link-table *foreground-link-tables*))
  (setq *background-link-tables* (remove link-table *background-link-tables*)))


(defun delete-link-tables ()
  "Deletes all link-tables. "
    (declare (special *default-link-table* *foreground-link-tables* *background-link-tables* ))
    (let ((all-tables (append *foreground-link-tables* *background-link-tables*)))
      (deselect-all)
    (setq *foreground-link-tables* nil)
    (setq *background-link-tables* nil)
    
    (mapcar #'delete-link-table all-tables)
    (setq *default-link-table* nil)))
   


(defun make-link-table (&key view-test (test *default-data-eq*) (symmetric? t) name)
  (declare (special *foreground-link-tables*))
  (let ((lt 
         (if symmetric? 
           (make-instance 'link-table :test test :name name :view-test view-test)
           (make-instance 'asymmetric-link-table :test test :name name :view-test view-test))))
    (push lt *foreground-link-tables*)
    lt))







(defmethod link-table-link-view((self link-table) view &optional linked-views)
   (let* ((list-vo (list-viewed-elements view))
         (nlist-vo (length list-vo))
         (view-test (link-table-view-test-of self))
         (test (link-table-test-of self))
         other-view)
    (if (eq linked-views :all) (setq linked-views (link-table-views-of self)))
    
     (flet ((same-dataset?(v bview)
               (declare (ignore v))
               (let ((list-vob  (list-viewed-elements bview)))
                 (or (eql list-vob list-vo)
                     (and (= nlist-vo (length list-vob))
                          (every #'eq list-vo list-vob))))))
        (if (and (null view-test)
                 list-vo
                 (setq other-view 
                       (find  view linked-views :test #'same-dataset?)))
          (loop for v in (linked-views-of other-view) do
                (make-double-links v view)
                finally 
                (make-double-links view other-view))
          
          
          (flet ((common-dataset?(bview)
                   (loop for  b in (list-viewed-elements bview) thereis
                         (loop for a in list-vo 
                               thereis (funcall test a b) ))))
            (if view-test
              (loop for v in linked-views 
                  when (and (funcall view-test view v) (common-dataset? v)) do
                  (make-double-links view v))
            (loop for v in linked-views 
                  when (common-dataset? v) do
                  (make-double-links view v)))))))
          
          (link-table-add-view self view))

(defmethod link-table-link-view((self asymmetric-link-table) view &optional linked-views) 
  
  (let* ((view-list-vo (list-viewed-elements view))
          (test (link-table-test-of self))
          (view-test (link-table-view-test-of self))
         )
    (if (eq linked-views :all) (setq linked-views (link-table-views-of self)))
    (flet ((common-dataset?(alist blist)
                 (loop for  b in blist thereis
                       (loop for a in alist
                             thereis (funcall test a b) ))))
      (if view-test
          (loop for v in linked-views 
                for v-list-vo = (list-viewed-elements v)
                do
                (if (and (funcall view-test v view)
                         (common-dataset? v-list-vo view-list-vo) )
                  (make-the-link  v view))
                (if (and (funcall view-test  view v) (common-dataset?  view-list-vo v-list-vo))
                  (make-the-link view v ))
                )
          (loop for v in linked-views 
                for v-list-vo = (list-viewed-elements v)
                do
                (if (common-dataset? v-list-vo view-list-vo) 
                  (make-the-link  v view))
                (if (common-dataset?  view-list-vo v-list-vo) 
                  (make-the-link view v ))
                ))))
          
          (link-table-add-view self view))






(defun background-link-table(&optional (link-table (default-link-table)))
  (declare (special *background-link-tables* *foreground-link-tables*))
  (unless (or (null link-table) (member link-table *background-link-tables*))
  
  (setf (saved-links-of link-table)
        (loop for v in (link-table-views-of link-table) 
              collect (linked-views-of v)))
  (loop for v in (link-table-views-of link-table) 
        do
        (setf (linked-views-of v) nil)
        (setf (link-table-of v) nil))
  (push link-table *background-link-tables*)
  (setq *foreground-link-tables* (remove link-table *foreground-link-tables*)))
  link-table)

(defun foreground-link-table(&optional (link-table (default-link-table)))
  (declare (special *background-link-tables* *foreground-link-tables*))
  (unless (or (null link-table) (member link-table *foreground-link-tables*))
   (loop for v in (link-table-views-of link-table) 
        for sv in (saved-links-of link-table)
       do
       
        (setf (linked-views-of v) sv)
        (setf (link-table-of v) link-table))
  (setf (saved-links-of link-table) nil)
  (push link-table *foreground-link-tables*)
  (setf *background-link-tables* (remove link-table *background-link-tables*)))
  link-table)



(defun default-link-table(&optional lt)
  (declare (special *default-link-table*))
  (setq *default-link-table* 
        (cond ((typep lt  'link-table) lt)
              ((eql lt :choose) (choose-link-table))
              ((and *default-link-table* (member *default-link-table* *foreground-link-tables*))
               *default-link-table*)

          (t (choose-link-table))))
  )

(defun use-link-table(&optional lt)
  (declare (special *default-link-table*))
        (cond 
              ((eql lt :choose) (choose-link-table))
              ((and *default-link-table* (member *default-link-table* *foreground-link-tables*))
               *default-link-table*)
              ((eq lt :default) (setq *default-link-table*  (choose-link-table)))

          (t (choose-link-table)))
  )



(defun choose-link-table(&optional table-list)
  (declare (special *background-link-tables* *foreground-link-tables*))

  (setq table-list
        (or table-list (append *foreground-link-tables* *background-link-tables*)))
  
  (cond ((and table-list (= 1 (length table-list)))
         (car table-list))
        ((null table-list)
         (make-link-table :name "Default"))
        
    (t (first (wb:prompt-for-items table-list
                                     :item-function
                                     #'link-table-name-of)))))

(defun choose-link-tables(table-list)
  (cond ((and table-list (= 1 (length table-list)))
         table-list)
        (table-list
         (wb:prompt-for-items table-list
                                     :item-function
                                     #'link-table-name-of))
        (t nil)))
    

  
        
