;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                              button-mixin.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system,
;;;  a toolkit for interactive statistical graphics.
;;;  
;;;
;;;  Authors:
;;;     C.B. Hurley 1988-1991 George Washington University
;;;     R.W. Oldford 1988-1991
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*selected-views*
           selected-view selected-views *selected-subviews-only*
           button-mixin left-fn-of left-menu middle-menu right-menu
           ;;(setf left-fn-of)
           remake-menu remake-menus
           left-button-fn middle-button-fn right-button-fn
           shift-left-button-fn shift-middle-button-fn shift-right-button-fn
           ctrl-left-button-fn ctrl-middle-button-fn ctrl-right-button-fn
           get-menu-items menu-properties  update-menu-items
           default-right-items style-menu-items
           )))
;;;----------------------------------------------------------------------------------

(defvar  *selected-subviews-only* t
  "when this is t menu operations for changing drawing styles ~
   operate on highlit subviews only")

(setq *selected-subviews-only* :if-highlit)


(defclass button-mixin ()
  ((left-fn 
    :accessor left-fn-of :initarg :left-fn
    :initform (list #'default-left-fn :viewport ))
   (left-menu :allocation :class :initform nil)
   (middle-menu :allocation :class :initform nil)
   (right-menu :allocation :class :initform nil)
   (ctrl-left-menu :allocation :class :initform nil)
   (ctrl-middle-menu :allocation :class :initform nil)
   (ctrl-right-menu :allocation :class :initform nil)
   ))

(defgeneric remake-menu (button-mixin slot-name &optional items)
  (:documentation "Remake the menu slot-name with items (if present) ~
                   or the default"))

(defgeneric remake-menus (button-mixin)
  (:documentation "Remake the menus"))


(defgeneric left-button-fn (button-mixin 
                            &key viewport &allow-other-keys )
  (:documentation "Invoked when left button is depressed on view"))


(defgeneric right-button-fn (button-mixin 
                             &key viewport &allow-other-keys )
  (:documentation "Invoked when right button is depressed on view"))

(defgeneric middle-button-fn (button-mixin 
                              &key viewport &allow-other-keys)
  (:documentation "Invoked when middle button is depressed on view"))

(defgeneric ctrl-left-button-fn (button-mixin 
                                 &key viewport &allow-other-keys )
  (:documentation "Invoked when ctrl-left button is depressed on view"))

(defgeneric ctrl-middle-button-fn (button-mixin 
                                   &key viewport &allow-other-keys)
  (:documentation "Invoked when ctrl-middle button is depressed on view"))

(defgeneric ctrl-right-button-fn (button-mixin 
                                  &key viewport &allow-other-keys)
  (:documentation "Invoked when ctrl-right button is depressed on view"))

(defgeneric shift-left-button-fn (button-mixin 
                                  &key viewport &allow-other-keys )
  (:documentation "Invoked when shift-left button is depressed on view"))

(defgeneric shift-middle-button-fn (button-mixin 
                                    &key viewport &allow-other-keys )
  (:documentation "Invoked when shift-middle button is depressed on view"))

(defgeneric shift-right-button-fn (button-mixin 
                                   &key viewport &allow-other-keys )
  (:documentation "Invoked when shift-right button is depressed on view"))

(defgeneric default-left-fn (self  &key   viewport &allow-other-keys)
  (:documentation "left button fn can be different from class to class."))


(defgeneric get-menu-items  (button-mixin menu-name)
  (:documentation "Return the menu items for menu-name"))

(defgeneric menu-properties (button-mixin menu-name)
  (:documentation "This method returns an alist to be placed as properties ~
                   on the menu associated with menu-name. ~
                   Whenever the associated menu is requested its properties are ~
                   checked with the result of menu-properties and a new menu is created ~
                   if the properties differ."))


(defgeneric update-menu-items (button-mixin menu-name)
  (:documentation "Updates the menu assiciated with menu-name. ~
                   Called before the menu is popped up"))

;;;----------------------------------------------------------------------------------

(defmethod set-color-style(self &rest keys &key highlit? color &allow-other-keys)
  (declare (ignore keys))
  (if (eq color :prompt)
    (setq color (wb::prompt-user-for-color)))
  (set-drawing-style self :color color :highlit? highlit?))


(defun color-menu-items ()
  (declare (special  *color-menu-list* *shade-menu-list*  *selected-subviews-only*))
  (loop for c in 
                (if (wb:color-device-p)
                  *color-menu-list*
                  *shade-menu-list*)
                collect 
                (list (car c) `(set-color-style :color ,(cadr c) :highlit?  *selected-subviews-only*))))

(defun font-menu-items ()
  (let ((font-name
         (loop for f in (wb:canvas-font-names)
               collect (list f `(set-view-font :name ,f))))
        (font-style
         (loop for f in (wb:canvas-font-styles)
               collect (list f `(set-view-font :style ,f))))
        (font-size 
         '(("smaller" (set-view-font :size :smaller ))
           ("larger" (set-view-font :size :bigger ))
           ("Prompt" (set-view-font :size :prompt)))))
    `(("FontSize" nil "" :sub-items ,font-size )
      ("FontName" nil "" :sub-items ,font-name )
      ("FontStyle" nil "" :sub-items ,font-style )
      )))



(defgeneric style-menu-items (self))

#|
(defun default-right-items ()
  (let ((layers (append
                 (loop for (string sym) in (view-menu-list 'view)
                       collect 
                       `(,string (layer-view ,sym :viewport)))
                 `(("Selected" (layer-selected-view :viewport ))))))
    `(;;("Link" (link-view ))
      ("Link" (link-selected-views ))
      ("Unlink" (unlink-selected-view))
      ("Cut"  (remove-view :viewport ))
      ("Paste" nil  "" :sub-items ,layers)
      ("Drag" (move-view :viewport ))
      ("DragCopy" (copy-image :viewport ))
      ("Recompute" (reposition-view :viewport))
      ("Clone" (copy-view :viewport :draw? t))
      ("CloneFn" (clone-view-fn-prompt :viewport))
      ("Inspect" (inspect))
      
      )))
|#





(defun default-right-items ()
  (let ((layers `(("Selected" (layer-selected-view :viewport )))))
    `(("Link" ,#'(lambda(v) (if (typep v 'linkable-mixin)
                              (setf (linkable-view-p v) t))
                  (link-selected-views v)))
      ("Unlink" (unlink-top-views))
      ("Cut"  (remove-view :viewport ))
      ("Paste" nil  "" :sub-items ,layers)
      ("Move" (move-view :viewport ))
      ("Copy image" (copy-image :viewport ))
      ("Recompute" (reposition-view :viewport))
      ("Copy" (copy-view :viewport :draw? t))
      ("CloneFn" (clone-view-fn-prompt :viewport))
      ("Inspect" (inspect))
      
      )))

(defvar *selected-views* nil)
(defvar *selected-view* nil)



(defun selected-views () 
   *selected-views*)


(defun set-selected-views(sel)
  (setq *selected-views* sel))


(defun selected-view () 
  (or *selected-view*
   (car *selected-views*)))

(defun highlighted-views()
  (let ((all 
         (loop for v in *selected-views*
               collect v append (linked-views-of v))))
    (loop for v in all when (any-highlight? v) collect v)))


      



(defmethod remake-menu ((self button-mixin) slot-name &optional items)
  (setf (slot-value self slot-name)
        (or items (get-menu-items self slot-name))))

(defmethod remake-menu :before ((self button-mixin) slot-name &optional items)
  "Releases the menu space."
  (declare (ignore items))
  (if (wb::menu-p (slot-value self slot-name))
    (wb::release-menu-space (slot-value self slot-name))))

(defmethod remake-menus ((self button-mixin)) 
  
  ;; for remaking the menus
  ;;(remake-left-menu self)
  (remake-menu self 'middle-menu)
  (remake-menu self 'right-menu)
  (remake-menu self 'ctrl-left-menu)
  (remake-menu self 'ctrl-middle-menu)
  (remake-menu self 'ctrl-right-menu))


(defun button-mixin-when-selected-fn (item menu mouse-button)
  (declare (ignore menu mouse-button))
 (second item ))



;;;
;;;  Added a cache for pop-up menus.  Machines are now fast enough that we don't
;;;  need to hold onto them all and this eliminates some storage (especially
;;;  important where there is an upper bound on the number of ``open-streams''
;;;  as in acl 3.02).   ... rwo May 1998.
;;;

(defvar *button-mixin-menu-cache* NIL
  "An association-list of cached pop-up menus for button-mixin."
  )

(defvar *button-mixin-menu-cache-size* 5
  "The number of pop-up menus that are to be cached.")

(defun button-mixin-cache-size ()
  "Returns the size of the cache of pop-up menus for button mixins."
  *button-mixin-menu-cache-size*)

(defun set-button-mixin-cache-size (size)
  "Sets the size of the cache of pop-up menus for button mixins."
  (setf *button-mixin-menu-cache-size* size))

(defun add-menu-to-button-mixin-cache (menu cache-key)
  (unless (assoc cache-key *button-mixin-menu-cache*)
    (if (= (length *button-mixin-menu-cache*) (button-mixin-cache-size))
      (let* ((last-cache (car (last *button-mixin-menu-cache*)))
             (last-key (car last-cache))
             (obj (first last-key))
             (slot-name (second last-key))
             )
        (setf *button-mixin-menu-cache* (butlast *button-mixin-menu-cache*))
        (setf (slot-value obj slot-name) NIL)
        (wb::release-menu-space (cdr last-cache))
        ))
    (push (cons cache-key menu) *button-mixin-menu-cache*)))


(defmethod menu-of ((self button-mixin) slot-name)
  
  (if (need-new-menu-p self slot-name)
    (remake-menu self slot-name))
  (let ((it (slot-value self slot-name))
        m)
    (when  (and it (not (wb:menu-p it)))
      (setq m (wb:make-menu :items it :when-selected-fn
                            #'button-mixin-when-selected-fn ))
      (loop for p in (menu-properties self slot-name) do
            (wb:put-menu-prop m (car p) (cdr p)))
      (add-menu-to-button-mixin-cache m (list self slot-name))
      (setf (slot-value self slot-name) m)))
  
  (update-menu-items self slot-name)
  (slot-value self slot-name))

#|
(defmethod menu-of ((self button-mixin) slot-name)
  
  (if (need-new-menu-p self slot-name)
    (remake-menu self slot-name))
  (let ((it (slot-value self slot-name))
        m)
    (when  (and it (not (wb:menu-p it)))
      (setq m (wb:make-menu :items it :when-selected-fn #'button-mixin-when-selected-fn ))
      (loop for p in (menu-properties self slot-name) do
            (wb:put-menu-prop m (car p) (cdr p)))
      (setf (slot-value self slot-name) m)))
  
  (update-menu-items self slot-name)
  (slot-value self slot-name))

|#


(defmethod menu-choose-item ((self button-mixin) slot-name
                             viewport )
  
  (if (typep self 'simple-view)
    (invert-view self :viewport viewport))
  
  (let* ((m (menu-of self slot-name))
         (choice (if m (wb:menu m))))
    
    (if (typep self 'simple-view)
    (invert-view self :viewport viewport))
  
  (if choice 
    (apply-choice self choice viewport ))))


(defmethod apply-choice ((self button-mixin)  choice &optional viewport )
  
  (if (listp choice)
    
    (if (not (or (eq  (first choice) 'setq)
                 (eq  (first choice) 'setf)))
      
      (let* ((f (car choice))   ; assume a function followed by args
             (args (mapcar #'(lambda(x)
                               (if (and (symbolp x) (boundp x))
                                 (eval x)
                                 x))
                           (cdr choice)))
             (vlist (member :viewport args))
             (target-fn (second (member :target args)))
             target)
        
        (unless (functionp f) (setq f (symbol-function f)))
        (when target-fn
          
          ;;(remf args :target)
          (setq args (delete :target args))
          (setq args (delete target-fn args))
          (unless (functionp target-fn) (setq target-fn (symbol-function target-fn)))
          (setq target (funcall target-fn self))
          (if vlist (setq viewport (select-viewport target viewport))))
        
        (if  vlist (push viewport (cdr  vlist)))
        
        (apply f (or target self) args))
      
      (eval choice))
    
    (funcall (if (functionp choice)
               choice
               (symbol-function choice))
             self )))



(defmethod default-left-fn ((self button-mixin) 
                            &key  &allow-other-keys)
  ;; left button fn can be different from instance to instance
  (select-one self)
  (short-description self)
  )



(defmethod left-button-fn ((self button-mixin) 
                           &key viewport )
  
  (let ((lbf (left-fn-of self)))
    (if lbf
      (apply-choice self lbf viewport )
      (menu-choose-item self 'left-menu  viewport ))))


(defmethod right-button-fn ((self button-mixin) 
                            &key viewport  )
  
  (menu-choose-item self 'right-menu  viewport ))


(defmethod middle-button-fn ((self button-mixin) 
                             &key viewport )
  
  (menu-choose-item self 'middle-menu viewport ))



(defmethod ctrl-left-button-fn ((self button-mixin) 
                                &key viewport )
  (menu-choose-item self 'ctrl-left-menu viewport ))
  

(defmethod ctrl-middle-button-fn ((self button-mixin) 
                                  &key viewport )
  (declare (ignore viewport ))
  (inspect-viewed-object self)
  )
(defvar *display-viewed-data-viewport* nil)

(defmethod ctrl-right-button-fn ((self button-mixin) 
                                 &key viewport )
  (declare (ignore viewport ) (special *display-viewed-data-viewport* ))
  (if (and *display-viewed-data-viewport* (active-viewport-p *display-viewed-data-viewport*))
    (let ((w (window-of *display-viewed-data-viewport*)))
      (wb:canvas-clear w)
      (remove-view-from-window w :viewport *display-viewed-data-viewport*)
      (display-viewed-object self :as-dataset? t :draw? t :signposts? nil
                        :viewport *display-viewed-data-viewport*))
  (let* ((r (view-region wb:*default-canvas-region*))
         (right (min (+ (wb:screen-mouse-x) (width-of r))
                                         (- (wb::window-max-width) 15) ))
         (top (min (+ (wb:screen-mouse-y) (height-of r))
                                      (- (wb::window-max-height) 30)))
          (w (make-view-window  :left (max 0 (- right (width-of r)))
                             :bottom (max 0 (- top (height-of r)))
                             :right right
                             :top top))
         (vp (make-viewport w)))
                             
 (display-viewed-object self :as-dataset? t :draw? t :signposts? nil
                        :viewport vp))))
  
(defmethod shift-left-button-fn ((self button-mixin) 
                                 &key viewport )
  (declare (ignore viewport ))
  ;;(toggle-select-view self)
  (select-view self)
  (short-description self))

 

(defmethod shift-middle-button-fn ((self button-mixin) 
                                   &key viewport)
  ;; intersection
  ;; combination
  ;; turns off the parts of highlit views which are not linked to self
  (declare (ignore viewport ))
  (short-description self)
  (when *selected-views*
    (let ((old-views (remove-duplicates (highlighted-views))))
    (with-update-style-cache
      (loop for v in old-views
              do (set-highlight-style v  nil :not-from self :draw-links? nil))))
        (set-selected-views (list self))))


       




(defmethod shift-right-button-fn ((self button-mixin) 
                                  &key viewport )
  ;; subtract
  (declare (ignore viewport ))
  (short-description self) 
 ;; (when (any-highlight? self)
  (with-update-style-cache 
    (set-highlight-style self nil)
    (set-selected-views (delete self *selected-views*))))


;; slight change 6-6-95
(defmethod temporary-left-fn ((self button-mixin) temp-fn current-viewport)
  
  ;; temp-fn is any acceptable 2nd argument to apply-choice
  ;; with click in viewport
  (declare (ignore current-viewport))
  (let ((old-fn (left-fn-of self))
        (current-time (get-universal-time))
        (wait-time 6) new-left-fn)
    (setf new-left-fn
          #'(lambda (v &key viewport  )
              (declare (ignore v))
                (if (<= (- (get-universal-time) current-time) wait-time)
                  (apply-choice self temp-fn  viewport )
               )
                (setf (left-fn-of self) old-fn)))
    (setf (left-fn-of self) (list new-left-fn :viewport ))))



(defmethod get-menu-items  ((self button-mixin) 
                           (slot-name (eql 'ctrl-left-menu)))
  `(("Inspect viewed object" (inspect-viewed-object))
    ("Display viewed-object" (display-viewed-object) ""
     :sub-items (("Display" (display-viewed-object :signposts? NIL))
                 ("Display with signposts" (display-viewed-object :signposts? T))))
    )
  )

(defmethod get-menu-items  ((self button-mixin) 
                           (slot-name (eql 'ctrl-middle-menu)))
  `(("Inspect viewed object" (inspect-viewed-object))
    ("Display viewed-object" (display-viewed-object) ""
     :sub-items (("Display" (display-viewed-object :signposts? NIL))
                 ("Display with signposts" (display-viewed-object :signposts? T))))
    )
  )

(defmethod get-menu-items  ((self button-mixin) 
                           (slot-name (eql 'ctrl-right-menu)))
  `(("Inspect viewed object" (inspect-viewed-object))
    ("Display viewed-object" (display-viewed-object) ""
     :sub-items (("Display" (display-viewed-object :signposts? NIL))
                 ("Display with signposts" (display-viewed-object :signposts? T))))
    )
  )


(defmethod get-menu-items ((self button-mixin) 
                           (slot-name (eql 'middle-menu)))
  )

(defmethod add-menu-items ((self button-mixin) add-items items)
  (if (eq add-items items)
    items
    (append (loop 
              for menu-item in add-items
              for label = (car menu-item) 
              unless (member label items :test #'string-equal :key #'car)
              collect menu-item)
            items)))

(defmethod get-menu-items :around ((self button-mixin) 
                                   (slot-name (eql 'middle-menu)))
  (add-menu-items self  (style-menu-items self) (call-next-method)))

(defmethod style-menu-items ((self button-mixin)))
 
(defmethod style-menu-items :around ((self button-mixin))
  (add-menu-items self   
                  `(("Highlight?" (toggle-select-view))
                    ("Color" nil "" :sub-items ,(color-menu-items))
                    ("Invisible?" (set-drawing-style :invisible? :toggle :highlit?  *selected-subviews-only* )))
                  (call-next-method)))




(defmethod get-menu-items ((self button-mixin) (slot-name (eql 'right-menu)))
  (default-right-items))

(defmethod get-menu-items :around ((self button-mixin) 
                                   (slot-name (eql 'right-menu)))
  (let ((result (call-next-method)))
    (add-menu-items self (default-right-items)  result)))

(defmethod menu-properties ((self button-mixin) slot-name)
  ;; this method can be specialized to return an alist
  ;; the items on the alist are placed as properties on the menu
  ;; accessed by slot-name
  
  ;; Whenever the associated menu is requested its properties are
  ;; checked with the result of menu-properties and a new menu is created
  ;; if the properties differ
  
  (declare (ignore slot-name))
  )  


(defmethod menu-properties :around ((self button-mixin) 
                            (slot-name (eql 'middle-menu)))
  ;; the items on the alist are placed as properties on the menu
  ;; accessed by slot-name
  ;; most middle menus use color/b+w depending on whether color is available
  ;; and being used
  
  (declare (ignore slot-name))
  (append (call-next-method) (list (cons :use-color (wb:color-device-p)))))


(defmethod need-new-menu-p ((self button-mixin) slot-name)
  (let* ((m (slot-value self slot-name)))
    (or (null m)
        (and (wb:menu-p m)
             (loop for p in (menu-properties self slot-name)
                   thereis 
                   (not (eql (cdr p)
                             (wb:get-menu-prop m (car p)))))))))


(defmethod update-menu-items ((self button-mixin)  slot-name)
  (declare (ignore slot-name)))



(defmethod update-menu-items :before ((self button-mixin) 
                                      (slot-name (eql 'middle-menu)))
  (let* ((m (slot-value self slot-name)))
    (wb:check-menu-item m "Highlight?" (draw-style self :highlight?))
    (wb:check-menu-item m "Invisible?" (draw-style self :invisible?))
    
    (if (and m (has-draw-style-p self :fill?))
      (wb:check-menu-item m "Fill?" (draw-style self :fill?))
      )))


