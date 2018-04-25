;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          doc-display-lists.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;
;;;  Authors:
;;;     C.B. Hurley 1992 George Washington University
;;;     R.W. Oldford 1992-4
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(make-examples-display-list make-titled-display-list
          make-methods-display-list make-accessors-display-list)))


(defun make-examples-display-list (&key (title "Examples")
                                        (examples NIL)
                                        (title-font wb:*help-key-font*)
                                        (body-font wb:*help-small-text-font*))
  (let*
    ((title-label
      (label :draw? NIL
             :justification '(:left :top)
             :font title-font
             :color q::*help-foreground-color*))
     (display-list
      (if (assoc :files examples)
        (make-display-list
         :items (cdr (assoc :files examples))
         :item-display-fn
         #'(lambda (x) (first x))
         :item-action-fn
         #'(lambda (x) (q::edit-file (first (last x))))
         )
        :none))
     (text (if (assoc :text examples)
             (text-view :draw? NIL
                        :justification '(:left :top)
                        :font body-font
                        :color q::*help-foreground-color*)
             :none))
     (root-text (if (assoc :root examples)
                  (text-view :draw? NIL
                             :justification '(:left :top)
                             :font body-font
                             :color q::*help-foreground-color*)
                  :none))
     (positions (loop for pos in '((0 3 4 5) (0.5 2.5 1 4) (0 3 0 1))
                      as view in (list root-text display-list text)
                      when (not (eq view :none))
                      collect pos))
     (display-views (loop for view in (list root-text display-list text)
                          when (not (eq view :none))
                          collect view)))
    
    (setf (text-of title-label) (format NIL (format NIL "~a" title)))
    (if (not (eq text :none))
      (setf (text-of text)
            (format NIL (format NIL "~a" (cdr (assoc :text examples))))))
    (if (not (eq root-text :none))
      (setf (text-of root-text)
            (format NIL (format NIL "~a" (cdr (assoc :root examples))))))
    (if (and (eq display-list :none)
             (not (eq root-text :none))
             (not (eq text :none)))
      (setf positions '((0 1 1 2) (0 1 0 1))))
    (list title-label
          (view-layout 
           :subviews display-views
           :positions positions
           ))
    )
  )
#|
  (cond
   ((assoc :files examples)
    (make-titled-display-list
               :title title
               :items (cdr (assoc :files examples))
               :item-display-fn
               #'(lambda (x) (first x))
               :item-action-fn
               #'(lambda (x) (ed (first (last x))))
               ))
   ((assoc :text examples)
    (make-view-paragraph :title title :body (cdr (assoc :text examples)) 
                         :title-font title-font 
                         :body-font body-font))
   (T
    (make-view-paragraph :title title :body (cdr (assoc :root examples)) 
                         :title-font title-font 
                         :body-font body-font))))
        
|#
(defun make-display-list (&key (items NIL)
                               (items-font wb:*help-small-text-font*)
                               (item-width 200)
                               (item-display-fn NIL)
                               (item-action-fn NIL))
  "Produce a help document titled display list ~
   having title title and contents the list given by items."
  (let* ((display (make-instance 'display-list
                    :draw? NIL
                    :font items-font
                    :viewed-object items
                    :items items
                    :color q::*help-foreground-color*
                    :labels
                    (if (functionp item-display-fn)
                      item-display-fn
                      #'(lambda (x)
                          (flet
                            ((display-string (name-string type-string)
                               (let* ((space (max 0
                                                  (- item-width
                                                     (wb::canvas-string-width
                                                      NIL
                                                      name-string
                                                      :font items-font)
                                                     (wb::canvas-string-width
                                                      NIL
                                                      type-string
                                                      :font items-font))))
                                      (n-blanks (/ space
                                                   (wb::canvas-string-width
                                                    NIL
                                                    " "
                                                    :font
                                                    items-font)))
                                      
                                      (separater
                                       (if (> n-blanks 2)
                                         (make-sequence 'string (round n-blanks)
                                                        :initial-element #\Space)
                                         "  ")))
                                 (concatenate 'string name-string separater type-string))))
                            
                            (cond
                             ((typep x 'qk::documentation-object)
                              (display-string (format NIL "~a" (qk::name  x ))
                                              (format NIL "~a" (qk::doc-type  x )))
                              )
                             ((listp x)
                              (display-string (format NIL "~a" (first x))
                                              (format NIL "~a" (second x)))
                              
                              )
                             (t (format NIL "~s" x))))))))
         (doit (if (functionp item-action-fn)
                 item-action-fn
                 #'q::help-display))
         (display-fn
          #'(lambda (label)
              (let ((doc-obj (viewed-object-of label)))
                (highlight-view label)
                (funcall doit doc-obj)
                (downlight-view label))))
         (scroll-display
          (scrolling-display :display display :bottom-scroller NIL)))
    (loop 
      for sv in (sub-views-of display)
      as i in items
      do
      (setf (viewed-object-of sv) i)
      (setf (left-fn-of sv) display-fn))
    
    scroll-display))

(defun make-titled-display-list (&key
                                 (title "")
                                 (items NIL)
                                 (title-font wb:*help-key-font*)
                                 (items-font wb:*help-small-text-font*)
                                 (item-width 200)
                                 (item-display-fn NIL)
                                 (item-action-fn NIL))
  "Produce a help document titled display list ~
   having title title and contents the list given by items."
  (let* ((title-label (label :draw? NIL
                             :justification '(:left :top) :font title-font
                             :color q::*help-foreground-color*))
         (display (make-instance 'display-list
                    :draw? NIL
                    :font items-font
                    :viewed-object items
                    :items items
                    :color q::*help-foreground-color*
                    :labels
                    (if (functionp item-display-fn)
                      item-display-fn
                      #'(lambda (x)
                          (flet
                            ((display-string (name-string type-string)
                               (let* ((space (max 0
                                                  (- item-width
                                                     (wb::canvas-string-width
                                                      NIL
                                                      name-string
                                                      :font items-font)
                                                     (wb::canvas-string-width
                                                      NIL
                                                      type-string
                                                      :font items-font))))
                                      (n-blanks (/ space
                                                   (wb::canvas-string-width
                                                    NIL
                                                    " "
                                                    :font
                                                    items-font)))
                                      
                                      (separater
                                       (if (> n-blanks 2)
                                         (make-sequence 'string (round n-blanks)
                                                        :initial-element #\Space)
                                         "  ")))
                                 (concatenate 'string name-string separater type-string))))
                            
                            (cond
                             ((typep x 'qk::documentation-object)
                              (display-string (format NIL "~a" (qk::name  x ))
                                              (format NIL "~a" (qk::doc-type  x )))
                              )
                             ((listp x)
                              (display-string (format NIL "~a" (first x))
                                              (format NIL "~a" (second x)))
                              
                              )
                             (t (format NIL "~s" x))))))))
         (doit (if (functionp item-action-fn)
                 item-action-fn
                 #'q::help-display))
         (display-fn
          #'(lambda (label)
              (let ((doc-obj (viewed-object-of label)))
                (highlight-view label)
                (funcall doit doc-obj)
                (downlight-view label))))
         (scroll-display
          (scrolling-display :display display :bottom-scroller NIL)))
   (loop 
      for sv in (sub-views-of display)
      as i in items
      do
      (setf (viewed-object-of sv) i)
      (setf (left-fn-of sv) display-fn))
    
    (setf (text-of title-label) (format NIL "~a" title))
    (list title-label scroll-display)))

#|
(defun make-titled-display-list (&key
                                 (title "")
                                 (items NIL)
                                 (title-font wb:*help-key-font*)
                                 (items-font wb:*help-small-text-font*)
                                 (item-width 200))
  "Produce a help document titled display list ~
   having title title and contents the list given by items."
  (let* ((title-label (label :draw? NIL
                             :justification '(:left :top)
                             :font title-font
                             :color q::*help-foreground-color*))
         (display (make-instance 'display-list
                    :draw? NIL
                    :font items-font
                    :viewed-object items
                    :items items
                    :color q::*help-foreground-color*
                    :labels
                    #'(lambda (x)
                        (flet
                          ((display-string (name-string type-string)
                             (let* ((space (max 0
                                                (- item-width
                                                   (wb::canvas-string-width
                                                    NIL
                                                    name-string
                                                    :font items-font)
                                                   (wb::canvas-string-width
                                                    NIL
                                                    type-string
                                                    :font items-font))))
                                    (n-blanks (/ space
                                                 (wb::canvas-string-width
                                                  NIL
                                                  " "
                                                  :font
                                                  items-font)))
                                    
                                    (separater
                                     (if (> n-blanks 2)
                                       (make-sequence 'string (round n-blanks)
                                                      :initial-element #\Space)
                                       "  ")))
                               (concatenate 'string name-string separater type-string))))

                          (cond
                           ((typep x 'qk::documentation-object)
                            (display-string (format NIL "~a" (qk::name  x ))
                                            (format NIL "~a" (qk::doc-type  x )))
                            )
                           ((listp x)
                            (display-string (format NIL "~a" (first x))
                                            (format NIL "~a" (second x)))
                            
                            )
                           (t (format NIL "~s" x)))))))
         (display-fn
          #'(lambda (label)
              (let ((doc-obj (viewed-object-of label)))
                (highlight-view label)
                (q::help-display doc-obj)
                (downlight-view label))))
         (scroll-display
          (scrolling-display :display display :bottom-scroller NIL)))
    (loop 
      for sv in (sub-views-of display)
      do
      (setf (left-fn-of sv) display-fn))
    
    (setf (text-of title-label) (format NIL "~a" title))
    (list title-label scroll-display)))

|#

(defun make-methods-display-list (&key (symbol NIL)
                                       (title "Methods")
                                       (methods NIL)
                                       (group? NIL)
                                       (title-font wb:*help-key-font*)
                                       (items-font wb:*help-small-text-font*))
  "Constructs and returns a Views display list of all the methods given in methods.  ~
   (:key ~
   (:arg symbol NIL The symbol of the generic function whose methods are to be ~
    displayed.) ~
   (:arg title \"Methods\" The title associated with the display list.) ~
   (:arg methods NIL The list of lists organizing method objects by their qualifiers.  ~
   Each element of methods must be a list whose first element is a list of qualifiers ~
   and whose second element is the list of all method objects of the generic-function ~
   \"name\" having that qualifier list.) ~
   (:arg group? NIL  If non-NIL the display lists only the qualifiers.  Each element ~
    in the display list has a pointer to the methods which have those qualifiers.) ~
   (:arg title-font wb:*help-key-font* Font for the title of the display list.) ~
   (:arg items-font wb:*help-small-text-font* Font for the items ~
   of the display list.) ~
   )"
  
  (if group?
    (make-titled-display-list
     :title title
     :items (cons
             (cons "All methods"
                   methods)
             methods)
     :title-font title-font
     :items-font items-font
     :item-display-fn
     #'(lambda (x) 
         (let ((key (car x)))
           (cond
            ((listp key)
             (format NIL "~{~s     ~}" key))
            ((stringp key)
             (format NIL "~a" key))
            ((symbolp key)
             (string-downcase (string key)))
            (T (format NIL "~s" key)))))
     :item-action-fn
     #'(lambda (x)
         (let ((key (car x)))
           (cond
            ((listp key)
             (qk::help
              (qk::make-doc (list symbol x) :methods))
             )
            ((stringp key)
             (qk::help
              (qk::make-doc (cons symbol (cdr x)) :methods)))
            )
           ))
     )
    (make-titled-display-list
     :title title
     :items methods
     :title-font title-font
     :items-font items-font
     :item-display-fn
     #'(lambda (x) 
         (string-downcase
          (format NIL "~{~s     ~}"
                 (loop for s in (qk::method-specializers x)
                  collect (cond
                           ((listp  s) s)
                           ((symbolp s) s)
                           (T (class-name s))))
                 )))
     :item-action-fn
     #'(lambda (x)
         (qk::help (qk::make-doc (list symbol x) :method))
         )
     )
    )
  )

(defun make-accessors-display-list (class
                                    &key
                                    (title "Accessors")
                                    (accessors NIL)
                                    (ungroup? NIL)
                                    (title-font wb:*help-key-font*)
                                    (items-font wb:*help-small-text-font*))
  "Constructs and returns a Views display list of all the accessor methods ~
   given in accessors.  ~
   (:required ~
   (:arg class NIL The class whose accessor methods are to be ~
   displayed.) ~
   )~
   (:key ~
   (:arg title \"Accessors\" The title associated with the display list.) ~
   (:arg accessors NIL The list of lists organizing accessor method objects by ~
   their type\:  \:reader or \:writer.  ~
   Each element of accessors must be a list whose first element is the type ~
   and whose remaining elements constitute a list of the selected accessor ~
   method objects associated with the class.) ~
   (:arg ungroup? NIL ~
   If NIL the display lists only the types or qualifiers.  Each element ~
   in the display list has a pointer to the accessor methods of that type.  ~
   If non-NIL, then types, or qualifiers, are ignored and all accessor methods are ~
   presented in a single display-list.) ~
   (:arg title-font wb:*help-key-font* Font for the title of the display list.) ~
   (:arg items-font wb:*help-small-text-font* Font for the items ~
   of the display list.) ~
   )"
  
  (if ungroup?
    (let ((ungrouped-accessors (rest (first accessors))))
      (loop for group in (rest accessors)
            do 
            (nconc ungrouped-accessors
                   (rest group)))
      
      (make-titled-display-list
       :title title
       :items ungrouped-accessors
       :title-font title-font
       :items-font items-font
       :item-display-fn
       
       #'(lambda (x)
           (string-downcase
            (format NIL "~s    ~{~s    ~}"
                    (qk::method-name x)
                    (mapcar #'class-name
                            (if (qk::writer-method-p x)
                              (reverse (qk::method-specializers x))
                              (qk::method-specializers x)))
                    )
            ))
       :item-action-fn
       #'(lambda (x)
           (qk::help (qk::make-doc (list class x) :accessor)
                     )
           )
       )
      )
    
    ;; else
    
    (make-titled-display-list
     :title title
     :items accessors
     :title-font title-font
     :items-font items-font
     :item-display-fn
     #'(lambda (x) 
         (let ((key (car x)))
           (cond
            ((stringp key)
             (format NIL "~a" key))
            ((listp key)
             (format NIL "~{~s     ~}" key))
            ((symbolp key)
             (string-downcase (string key)))
            (T (format NIL "~s" key)))))
     :item-action-fn
     #'(lambda (x)
         (let ((key (car x)))
           (cond
            ((and (stringp key) (string-equal key "All types"))
             (qk::help
              (qk::make-doc (cons class (cdr x)) :accessors)))
            ((or (keywordp key) (listp key))
             (qk::help
              (qk::make-doc (list class x) :accessors)
              )
             )
            )
           ))
     )
    )
  )
