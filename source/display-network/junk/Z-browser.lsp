;;; -*- Mode: LISP -*-


(in-package 'Z)

;;;
;;; class definition
;;;

(defclass Z-browser (title-bar-mixin named-object-mixin prompt-mixin 
                               browser::browser)
       
       ;; 
       ;; The order of inheritance is important : the mixins must be before
       ;; BROWSER in the list so that Z-BROWSER inherits methods and Class
       ;; variables of the mixins in priority on those of  BROWSER
       ;;
 
       ((local-commands :initform '(box-node name-this-item how-this-works 
                                          buttoning-behavior describe-contents)
               :allocation :class)
        (icon :initform view-icon :allocation :class)
                                               ; Default value of icon
        (mask :initform nil :allocation :class)
                                               ; default value of mask of icon
        (icon-font :initform nil :allocation :class)
                                               ; Font used in the Browser
        (icon-font-description :initform '(helvetica 7 medium)
               :allocation :class)
        (title-reg :initform '(0 31 77 10)
               :allocation :class)
                                               ; Default title region of Icon
        (invert :initform t :allocation :class)
                                               ; By default, print title of
                                               ; icon on Black background
        (sub-view-icon :initform view-icon :allocation :class)
                                               ; icon to appear inside a Map
        (cache-menu-p :initform t)
                                               ; menu is cached if T
        (title :initform "Z Browser")
                                               ; Title passed to GRAPHER
                                               ; package and displayed in the
                                               ; Icons
        ))

;;;
;;; function definitions
;;;

(defun format-for-eval (expression)
       

;;; 
;;; Format an expression containing objects so that it can be evaluated by EVAL
;;; The problem is that the objects must be replaced by (QUOTE object) inside
;;; the expression to eval if they are not symbols
;;; the evaluable expression is returned
;;; 

       (mapcar #'(lambda (x)
                        (cond ((symbolp x)
                               x)
                              ((listp x)
                               (format-for-eval x))
                              (t (list 'quote x))))
              expression))


(defun extract-objects (list &aux extracted)
 
;;; extract a list of objects from a list
   
   (dolist (element list)
      (if (listp element)
          (setf extracted (append extracted (extract-objects element)))
          (if (object-p element)
              (push element extracted))))
   extracted)


(defmacro with-link (expression-to-eval)

 `(let ((result (eval (format-for-eval ',expression-to-eval)))
        list-link)
       
    ;; Link with Obj and all other arguments

    (when (object-p result)
       (setf list-link (extract-objects (slot-value result 'spawning-expression)))
       (if (understands result 'causal-link t)
           (dolist (arg list-link)
               (when (understands arg 'causal-link result)
                     (causal-link arg result))))
       (if (understands result 'analysis-link t)
           (dolist (arg list-link)
               (when (understands arg 'analysis-link result)
                     (analysis-link arg result)))))
    result))


(defmacro from-map (expression)

;;; when a function is called by menu selection, its arguments are not
;;; bound because we only know the node it is applied from and the function
;;; may need other arguments. This macro ask the user values for the other 
;;; compulsory arguments. These compulsory arguments are those declared when
;;; the user ask a function to be callable by menu, using add-function-in-menu

   `(let ((classification (get-mc (first ,expression)))
          (length (length ,expression))
          arg-in-exp)
       (when classification
          (do ((arg (fourth classification) (cdr arg))
               (rank 1 (+ 1 rank)))
              ((null arg))
            (unless (nth rank ,expression)
               (if (>= rank length)
                   (setf ,expression 
                         (append ,expression 
                                 (list (requires-variable (first arg)))))                              
                   (setf (nth rank ,expression)
                         (requires-variable (first arg)))))))))


;;;
;;; method definitions
;;;

(defmethod new-item ((self Z-browser)
                     &optional new-item)
       

;;; 
;;; specialization
;;; 

       (if new-item
           ($! new-item)
           (set-name (prompt-eval self "Z Expression")
                  (prompt-read self "Name to be given to this Z Expression"
                         ))))

(defmethod name-this-item ((self Z-browser)
                           &optional object)
       

;;; 
;;; Name the Object and display the new name in the Browser or the browser
;;; itself if no object
;;; 

       (if object
           (progn (set-name object)
                  (let ((cached-label (assoc object (slot-value self
                                                           'label-cache))))
                       (setf (slot-value self 'label-cache)
                             (remove cached-label (slot-value self 
                                                         'label-cache)))
                       (get-display-label self object)))
           (set-name self))
       (recompute self))


(defmethod left-shift-select ((self Z-browser))
       

;;; 
;;; Specialization
;;; 

       (declare (special object))
       (unread self object))


(defmethod middle-shift-select ((self Z-browser))
       

;;; 
;;; Specialization
;;; 

       (declare (special object))
       (unread self object))



(defmethod title-left-shift-select ((self Z-browser))

;;;
;;; Specialization
;;;

       (unread self))


(defmethod title-middle-shift-select ((self Z-browser))

;;;
;;; Specialization
;;;

       (unread self))


(defmethod get-label ((self Z-browser)
                      object)
       

;;; 
;;; Get a label for an object to be displayed in the browser
;;; 

       (if (understands object 'descriptive-label)
           (descriptive-label object)
           (let ((name (get-name object)))
                (if name
                    (concatenate 'string (string name)
                           " "
                           (string (class-name (class-of object))))
                    (class-name (class-of object))))))


(defmethod get-links ((self Z-browser)
                     object &key (reverse? nil))
       

;;; 
;;; We will take care of links in class Network view
;;; 

       nil)


(defmethod show ((self Z-browser)
                 &optional browse-list window-or-title 
                 good-list)
       (call-next-method)
       

;;; 
;;; We must install our Shrink method 
;;; 

       (setf (icon-of (slot-value self 'window) #'Z-icon-fn)
       self)


(defmethod icon-title ((self Z-browser))
       

;;; 
;;; Returns the title for this View
;;; 

       (if (slot-value self 'name)
           (string (slot-value self 'name))
           (slot-value self 'title)))


(defmethod unread ((self Z-browser)
                   &optional object)
       

       (if object
           (push-in-buffer (access-expression object))
           (push-in-buffer (access-expression self))))


(defmethod set-name ((self Z-browser)
                     &optional name)
       

;;; 
;;; Specialization
;;; For Z-BROWSERS, we must update the slot-value TITLE when the name is
;;; changed
;;; 

       (call-next-method)
       (if (slot-value self 'name)
           (setf (slot-value self 'title)
                 (slot-value self 'name))))


(defmethod call-method-and-link ((self Z-browser) expression-to-eval)
       

;;; Given a selector obtained by selection in a menu, this method build the
;;; expression to call macro with-link in order to update Links and then call
;;; the macro . this method is Called by DO-SELECTED-COMMAND  
;;; Updates *Z-EXPRESSION* in order to construct the spawning expression
;;; of the result later
;;; 

       (declare (special *Z-expression*))
       (loop (if (first (last expression-to-eval)) ; remove the nils at the end
                 (return))                         ; of expression-to-eval
             (setf expression-to-eval 
                   (butlast expression-to-eval)))
       (from-map expression-to-eval)               ; complete the arguments
       (setf *Z-expression*
             (setf expression-to-eval (list 'with-link
                                            expression-to-eval)))
       (eval expression-to-eval))


(defmethod do-selected-command ((self Z-browser) command obj)
       
;;; 
;;; Does the selected command or forwards it to the object
;;; Differs from distributed version in that the value of the command is
;;; returned and the macros can be executed as well as the functions or
;;; methods
;;; 

 (prog (args)
    (if (null command)
        (return nil))
    (if (listp command)
        (progn (setf args (cdr command))
               (setf command (car command))))
    (if (listp obj)
                 
        ;; Take care of being passed in a dummy node from browser in
        ;; Lattice mode Dummy nodes are indicated by having the
        ;; object in a list

        (setf obj (car obj)))

    (push obj args)
    (unless (and (not (member command (slot-value self 'local-commands)))
                 (understands obj command))
            (push self args))
    (push command args)

    (return (call-method-and-link self args))))


