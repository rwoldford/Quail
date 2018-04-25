;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                                tex-ext.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1991.
;;;     R.W. Oldford 1991, 1994.
;;;
;;;
;;;----------------------------------------------------------------------------



(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '()))


(defvar tex-doc-style "quail"
  "The name of the TeX style file containing the TeX style extensions ~
   for the documentation system.")
;;;--------------------------------------------------------------------------
;;;
;;;  A header box taking three arguments.
;;;
;;;--------------------------------------------------------------------------

(defun write-tex-header-box (destination
                             &key (left nil left?)
                             (middle nil middle?)
                             (right nil right?))
  "Writes the TeX commands which produce a boxed header ~
   with left left-justified horizontally, middle centred, and right ~
   right-justified."
  
  (format destination "~&\\headerbox{")
  (if left?    (write-tex-value destination left))
  (format destination "}{")
  (if middle?  (write-tex-value destination middle))
  (format destination "}{")
  (if right?   (write-tex-value destination right))
  (format destination "}"))

;;;--------------------------------------------------------------------------
;;;
;;;  Write a doc-paragraph
;;;
;;;--------------------------------------------------------------------------

(defun write-tex-doc-paragraph (destination
                                &key (title nil title?)
                                (body nil body?))
  "Writes the TeX commands which produce a document paragraph ~
   having title title and contents body."
  
  (format destination "~& ~%~
                       {\\bf ")
  (if title?    (write-tex-value destination title))
  (format destination "}~%~
                       ~%")
  (if body?  (write-tex-value destination
                              (if (stringp body) (format NIL body) body)))
  (format destination "~&~%"))


;;;-------------------------------------------------------------------------
;;;
;;;  Hanging paragraphs
;;;
;;;-------------------------------------------------------------------------

(defun write-tex-begin-hang-par (destination
                                 &key (title nil title?))
  "Writes the TeX commands to destination that are necessary to begin ~
   a hanging paragraph.  If title is given this is emphasised ~
   as the first word in the paragraph."
  
  (format destination "~&\\beginhang")
  (when title?
    (format destination "~&{\\bf ")
    (write-tex-value destination title)
    (format destination "}~
                         \\hspace{2em}")))

(defun write-tex-end-hang-par (destination)
  "Writes the TeX commands to destination that are necessary to end ~
   a hanging paragraph."
  (format destination "~& ~%~
                       \\endhang"))


;;;------------------------------------------------------------------------------
;;;
;;;  Our titled lists
;;;
;;;-----------------------------------------------------------------------------

(defun write-tex-begin-titled-list (destination &key (title nil title?))
  "Writes the TeX commands necessary to begin a titled list."
  (if title?
    (write-tex-begin-hang-par destination :title title)
    (write-tex-begin-hang-par destination))
  )



(defun write-tex-end-titled-list (destination)
  "Writes the TeX commands necessary to end a titled list."
  (write-tex-end-hang-par destination)
  )



(defun write-tex-titled-list (destination
                              &key (title nil title?)
                              (items nil items?))
  "Writes the TeX commands necessary to produce a titled list ~
   of items."
  (if title?
    (write-tex-begin-titled-list destination :title title)
    (write-tex-begin-titled-list destination))
  
  (if items?
    (loop for i in items do
          (format destination "\\hspace{2em}")
          (write-tex destination i)))
  
  (write-tex-end-titled-list destination))


(defun write-tex-titled-alist (destination
                               &key (title nil title?)
                               (items nil items?))
  "Writes the TeX commands necessary to produce a titled list ~
   of items."
  (if title?
    (write-tex-begin-titled-list destination :title title)
    (write-tex-begin-titled-list destination))
  
  (if items?
    (loop for i in items do
          (if (cdr i)
            (write-tex-doc-paragraph destination
                                     :title (car i)
                                     :body (cdr i))
            (write-tex-doc-paragraph destination
                                     :title (car i)))))
  
  (write-tex-end-titled-list destination))


(defun write-tex-arg-element (destination
                              &key
                              (title nil title?)
                              (items nil items?))
  "Writes the TeX commands necessary to produce a titled list ~
   of items."
  (if title?
    (write-tex-begin-titled-list destination :title title)
    (write-tex-begin-titled-list destination))
  (when items?
    (loop
      for i in items do
      (cond
       ((cdr i)
        
        (write-tex-doc-paragraph
         destination
         :title (car i)
         :body
         (if (> (length i) 2)
           (format NIL "Default is ~a.  ~a" (second i) (third i))
           (second i))
         )
        )
       (T
        (write-tex-doc-paragraph destination
                                 :title (car i))))))
  
  (write-tex-end-titled-list destination))

;;;--------------------------------------------------------------------------
;;;
;;;  Write out examples
;;;
;;;--------------------------------------------------------------------------

(defun write-tex-doc-examples (destination
                            &key (title "Examples")
                            (examples nil))
  "Produce a help document string for examples ~
   having title title and examples examples."
  (cond
   ((and (assoc :root examples) (assoc :text examples))
    (write-tex-doc-paragraph destination
                          :title title
                          :body 
                          (cdr (assoc :root examples)))
    (write-tex-doc-paragraph destination
                          :body (cdr (assoc :text examples)))
    (when (assoc :files examples)
      (write-tex-doc-paragraph destination
                            :body 
                            "See also the following files.")
      (write-tex-titled-alist destination
       :items (loop for info in
                  (cdr (assoc :files examples))
                  collect
                  (cons (car info) (cadr info)))
       ))
    )
   ((assoc :root examples)
    (write-tex-doc-paragraph destination
                          :title title
                          :body (cdr (assoc :root examples)))
    (when (assoc :files examples)
      (write-tex-doc-paragraph destination
                            :body 
                            "See also the following files.")
      (write-tex-titled-alist destination
       :items (loop for info in
                  (cdr (assoc :files examples))
                  collect
                  (cons (car info) (cadr info)))
       ))
    )
   ((assoc :text examples)
    (write-tex-doc-paragraph destination
                          :title title
                          :body (cdr (assoc :text examples)))
    (when (assoc :files examples)
      (write-tex-doc-paragraph destination
                            :body 
                            "See also the following files.")
      (write-tex-titled-alist destination
       :items (loop for info in
                  (cdr (assoc :files examples))
                  collect
                  (cons (car info) (cadr info)))
       
       ))
    )
   ((assoc :files examples)
    (write-tex-titled-alist destination
     :title title
     :items (loop for info in
                  (cdr (assoc :files examples))
                  collect
                  (cons (cadr info) (car info))))
    )
   )
  )
