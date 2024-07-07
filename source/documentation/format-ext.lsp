;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                             format-ext.lisp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1990 Statistical Computing Laboratory, University Of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1991.
;;;
;;;
;;;----------------------------------------------------------------------------



(in-package :quail-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '()))

(defun format-list (destination items)
  "Formats a list of items to the designated destination."
  (if (= (length items) 1)
    (format destination "(~s)" (first items))
    (format destination "(~s~{   ~s~})"
            (first items) (rest items))))

(defun format-list-1-item-per-line (destination items)
  "Formats a list of items to the designated destination.  ~
   Each item in the list appears on a new line."
  (if (= (length items) 1)
    (format destination "(~s)" (first items))
    (format destination "(~s~{~%~s~})"
            (first items) (rest items))))
    

;;;--------------------------------------------------------------------------
;;;
;;;  A header box taking three arguments.
;;;
;;;--------------------------------------------------------------------------

(defun format-header-box (destination
                          &key (left nil left?)
                          (middle nil middle?)
                          (right nil right?))
  "Produce a boxed header ~
   with left left-justified horizontally, middle centred, and right ~
   right-justified."
  (let ((line (loop for i from 1 to 80 collect "_")))
    (flet ((print-line ()
             (format destination "~%~{~a~}" line)))
      (cond
       ((and left? middle? right?)
        (print-line)
        (format destination "~%~%~80,1<~a~;~^~a~;~^~a~>" left middle right)
        (print-line))
       ((and left? right?)
        (print-line)
        (format destination "~%~%~80,1<~a~;~^~a~;~^~a~>" left right)
        (print-line))
       ((and left? middle?)
        (print-line)
        (format destination "~%~%~80,1<~a~;~^~a~;~^~a~>" left middle "")
        (print-line))
       (left?
        (print-line)
        (format destination "~%~%~80,1<~a~;~^~a~;~^~a~>" left "" "")
        (print-line))
       ((and right? middle?)
        (print-line)
        (format destination "~%~%~80,1<~a~;~^~a~;~^~a~>"  "" middle right)
        (print-line))
       (middle?
        (print-line)
        (format destination "~%~%~80,1<~a~;~^~a~;~^~a~>"  "" middle "")
        (print-line))
       (right?
        (print-line)
        (format destination "~%~%~80,1<~a~;~^~a~;~^~a~>"  "" "" right)
        (print-line)))))
  (format destination "~%~%"))

;;;--------------------------------------------------------------------------
;;;
;;;  Write a doc-paragraph
;;;
;;;--------------------------------------------------------------------------

(defun format-doc-paragraph (destination
                             &key (title nil title?)
                             (body nil body?)
                             (line-size 80)
                             (indent 10))
  "Produce a help document paragraph ~
   having title title and contents body.  ~
   Number of characters per line is given by the line-size.  ~
   Each line in the body will be indented by indent characters."
  (when title?
    (format destination
            (concatenate 'string
                         "~&~a:~%~"
                         (format NIL "~s" indent)
                         "T")
            (string-upcase (string title)))
    )
  (when body?
    (let ((string (if (stringp body)
                    (concatenate 'string
                                 (string-capitalize
                                  (subseq body 0 1))
                                 (format NIL (subseq body 1)))
                    (string-capitalize (format NIL "~s" body))))
          (start 0)
          )
    (flet
      ((read-word (string)
         (If (> start (length string))
           nil
           (let* ((pos-space (position #\Space string :test #'char= :start start))
                  (pos-line (position #\Newline string :test #'char= :start start))
                  result
                  to-newline?)
             (cond
              ((and pos-line pos-space)
               (cond
                ((< pos-line pos-space)
                      (setq result (subseq string start pos-line))
                      (setq start (+  1 pos-line))
                      (setq to-newline? T))
                (T (setq result (subseq string start (+ 1 pos-space)))
                   (setq start (+  1 pos-space)))))
              (pos-space
               (setq result (subseq string start (+ 1 pos-space)))
               (setq start (+  1 pos-space)))
              (pos-line
               (setq result (subseq string start pos-line))
               (setq start (+  1 pos-line))
                      (setq to-newline? T))
              (T  (setq result (subseq string start))
                  (setq start (+ 1 (length string)))))
             (cons result to-newline?)))))

      (loop for (word . newline) = (read-word string)
            with i = indent
            until (null word)
            do
            (cond
             ((> (length word) (- line-size i))
              (format destination
                      (concatenate 'string
                                   "~&~"
                                   (format NIL "~s" indent)
                                   "T~a ")
                      word)
              (setf i (+ indent (length word) 1)))
             (T
              (format destination "~a " word)
              (setf i (+ i 1 (length word)))))
            (when newline
              (format destination
                      (concatenate 'string
                                   "~%~"
                                   (format NIL "~s" indent)
                                   "T"))
              (setf i indent))
              )
            )))
  (format destination "~&~%"))


;;;-------------------------------------------------------------------------
;;;
;;;  Hanging paragraphs
;;;
;;;-------------------------------------------------------------------------

(defun format-begin-hang-par (destination
                                        &key (title nil title?))
  "Begin ~
   a hanging paragraph.  If title is given this is emphasised ~
   as the first word in the paragraph."
    
    (if title?
      (format-doc-paragraph destination :title title)
      (format-doc-paragraph destination)))

(defun format-end-hang-par (destination)
  "Produces the help output to destination that are necessary to end ~
   a hanging paragraph."
  ;;(declare (ignore destination))
    (format destination "~&")
  )
;;;------------------------------------------------------------------------------
;;;
;;;  Our titled lists
;;;
;;;-----------------------------------------------------------------------------

(defun format-begin-titled-list (destination &key (title nil title?))
  "Produces the help output necessary to begin a titled list."
  (if title?
    (format-begin-hang-par destination :title title)
    (format-begin-hang-par destination))
  )



(defun format-end-titled-list (destination)
  "Produces the help output necessary to end a titled list."
  (format-end-hang-par destination)
  )



(defun format-titled-list (destination
                           &key (title nil title?)
                           (items nil items?))
  "Produces the help output necessary to produce a titled list ~
   of items."
  (if title?
    (format-begin-titled-list destination :title title)
    (format-begin-titled-list destination))
  
  (if items?
    (loop for i in items do
          (format destination "~&~10T")
          (format-doc destination i)
          ))
  (format destination "~%")
  
  (format-end-titled-list destination)
  )


;;;------------------------------------------------------------------------------
;;;
;;;  Our titled association lists
;;;
;;;-----------------------------------------------------------------------------

(defun format-begin-titled-alist (destination &key (title nil title?))
  "Produces the help output necessary to begin a titled list."
  (if title?
    (format-begin-hang-par destination :title title)
    (format-begin-hang-par destination))
  )



(defun format-end-titled-alist (destination)
  "Produces the help output necessary to end a titled list."
  (format-end-hang-par destination)
  )

(defun format-titled-alist (destination
                            &key (title nil title?)
                            (items nil items?))
  "Produces the help output necessary to produce a titled list ~
   of items."
  (if title?
    (format-begin-titled-alist destination :title title)
    (format-begin-titled-alist destination))
  
  (if items?
    (loop for i in items do
          (format destination "~&~10T")
          (format destination "~a" (car i))
          (when (cdr i)
            
            (format destination "~&~15T~a" (cdr i)))))
  (format destination "~%")
  
  (format-end-titled-alist destination)
  )

#| duplicate de below
(defun format-arg-element (destination
                           &key (title NIL title?)
                                (items NIL items?)
                                (indent 5)
                                (item-indent 10))
  (if title?
    (format destination "~5T~a - ~T" title)
    )
  (when items?
    (loop for i in items do
          (format destination "~&~10T")
          (format destination "~a"
                  (string-downcase (format NIL "~a" (car i))))
          (if (cdr i)
            (if (> (length i) 2)
              (format destination
                      "~&~15TDefault is ~a.  ~a"
                      (second i)
                      (third i))
              (format destination "~&~15T~a" (second i))
              )
            )
          )
    (format destination "~%")
    ))
    
    |#

(defun format-arg-element (destination
                           &key (title NIL title?)
                                (items NIL items?)
                                (line-size 80)
                                (indent 5)
                                (arg-indent 10)
                                (arg-desc-indent 15))
  "Produce a help document paragraph ~
   having title title and contents body.  ~
   Number of characters per line is given by the line-size.  ~
   Each line in the body will be indented by indent characters."
  (when title?
    (format destination
            (concatenate 'string
                         "~&~"
                         (format NIL "~s" indent)
                         "T~a - ~%")
            (string-upcase (string title)))
    )
  (when items?
    (let (string start arg-name-string)
      (flet
        ((read-word (string)
           (If (> start (length string))
             nil
             (let* ((pos-space (position #\Space string :test #'char= :start start))
                    (pos-line (position #\Newline string :test #'char= :start start))
                    result
                    to-newline?)
               (cond
                ((and pos-line pos-space)
                 (cond
                  ((< pos-line pos-space)
                   (setq result (subseq string start pos-line))
                   (setq start (+  1 pos-line))
                   (setq to-newline? T))
                  (T (setq result (subseq string start (+ 1 pos-space)))
                     (setq start (+  1 pos-space)))))
                (pos-space
                 (setq result (subseq string start (+ 1 pos-space)))
                 (setq start (+  1 pos-space)))
                (pos-line
                 (setq result (subseq string start pos-line))
                 (setq start (+  1 pos-line))
                 (setq to-newline? T))
                (T  (setq result (subseq string start))
                    (setq start (+ 1 (length string)))))
               (cons result to-newline?)))))
        
        (loop
          for i in items do
          (setf arg-name-string
                (string-downcase (format NIL "~a - " (car i))))
          (format destination
            (concatenate 'string
                         "~&~"
                         (format NIL "~s" arg-indent)
                         "T~a")
            arg-name-string)
          (when (cdr i)
            (if (> (length i) 2)
              (setf string
                    (format NIL
                      "Default is ~a.  ~a"
                      (second i)
                      (third i)))
              (setf string
                    (format NIL "~a" (second i)))
              )
            (setf string (if (stringp string)
                           (concatenate 'string
                                        (string-capitalize
                                         (subseq string 0 1))
                                        (format NIL (subseq string 1)))
                           (string-capitalize (format NIL "~s" string))))
              
            ;;(format destination
            ;;        (concatenate 'string
            ;;                     "~&~"
            ;;                     (format NIL "~s" arg-desc-indent)
            ;;                     "T "))
            (setf start 0)
            (loop for (word . newline) = (read-word string)
                  with i = (max arg-desc-indent
                                (+ arg-indent (length arg-name-string)))
                  until (null word)
                  do
                  (cond
                   ((> (length word) (- line-size i))
                    (format destination
                            (concatenate 'string
                                         "~&~"
                                         (format NIL "~s" arg-desc-indent)
                                         "T~a ")
                            word)
                    (setf i (+ arg-desc-indent (length word) 1)))
                   (T
                    (format destination "~a " word)
                    (setf i (+ i 1 (length word)))))
                  (when newline
                    (format destination
                            (concatenate 'string
                                         "~%~"
                                         (format NIL "~s" arg-desc-indent)
                                         "T"))
                    (setf i arg-desc-indent))
                  )
          ))))
  (format destination "~&~%"))
  )


;;;--------------------------------------------------------------------------
;;;
;;;  Write out examples
;;;
;;;--------------------------------------------------------------------------

(defun format-doc-examples (destination
                            &key (title "Examples")
                            (examples nil))
  "Produce a help document string for examples ~
   having title title and examples examples."
  (cond
   ((and (assoc :root examples) (assoc :text examples))
    (format-doc-paragraph destination
                          :title title
                          :body 
                          (cdr (assoc :root examples)))
    (format-doc-paragraph destination
                          :body (cdr (assoc :text examples)))
    (when (assoc :files examples)
      (format-doc-paragraph destination
                            :body 
                            "See also the following files.")
      (format-titled-alist destination
       :items (loop for info in
                  (cdr (assoc :files examples))
                  collect
                  (cons (car info) (cadr info)))
       ))
    )
   ((assoc :root examples)
    (format-doc-paragraph destination
                          :title title
                          :body (cdr (assoc :root examples)))
    (when (assoc :files examples)
      (format-doc-paragraph destination
                            :body 
                            "See also the following files.")
      (format-titled-alist destination
       :items (loop for info in
                  (cdr (assoc :files examples))
                  collect
                  (cons (car info) (cadr info)))
       ))
    )
   ((assoc :text examples)
    (format-doc-paragraph destination
                          :title title
                          :body (cdr (assoc :text examples)))
    (when (assoc :files examples)
      (format-doc-paragraph destination
                            :body 
                            "See also the following files.")
      (format-titled-alist destination
       :items (loop for info in
                  (cdr (assoc :files examples))
                  collect
                  (cons (car info) (cadr info)))
       
       ))
    )
   ((assoc :files examples)
    (format-titled-alist destination
     :title title
     :items (loop for info in
                  (cdr (assoc :files examples))
                  collect
                  (cons (cadr info) (car info))))
    )
   )
  )
