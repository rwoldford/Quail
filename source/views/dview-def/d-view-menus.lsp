;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               d-view-menus.lisp
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
;;;     C.B. Hurley 1988-1992 George Washington University
;;;     R.W. Oldford 1988-1992
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :views)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(menu-variates-of)))

(defgeneric menu-variates-of (d-view)
  (:documentation "Returns a list of the variates used in the variate menu."))

(defmethod menu-variates-of ((self d-view))
  (variates-of self))

(defmethod get-menu-items :around ((self 2d-view) 
                                   (slot-name (eql 'right-menu)))
  
  (let ((result (call-next-method))
        (layers (append
                 (loop for (string sym) in (view-menu-list '2d-view)
                       collect 
                       `(,string (layer-view ,sym :viewport)))
                 
                 `(("FunctionView" (layer-view function-view :viewport))
                   ("Line" (layer-view line :viewport :slope :prompt :intercept :prompt))
                   ("LineSegment" (layer-view line-segment :viewport ))
                   ("OrientedLine" nil "" :sub-items
                    (("Horizontal" (layer-view oriented-line  :viewport :orientation :horizontal
                                               :intercept :prompt ))
                    ("Vertical" (layer-view oriented-line  :viewport :orientation :vertical
                                            :intercept :prompt))))
                   ("Selected" (layer-selected-view :viewport ))))))
    
    (add-menu-items self   result `(("Paste" nil  "" :sub-items ,layers)))))



(defmethod get-menu-items :around ((self 1d-view) 
                                   (slot-name (eql 'right-menu)))
  
  (let ((result (call-next-method))
        (layers (append
                 (loop for (string sym) in (view-menu-list '1d-view)
                       collect 
                       (list string (list 'layer-view sym :viewport)))
                 
                 `(("FunctionView" (layer-view function-view :viewport))
                   ("Line" (layer-view line :viewport :slope :prompt :intercept :prompt))
                   ("OrientedLine" nil "" :sub-items
                    (("Horizontal" (layer-view oriented-line  :viewport :orientation :horizontal
                                               :intercept :prompt ))
                    ("Vertical" (layer-view oriented-line :viewport :orientation :vertical
                                            :intercept :prompt))))
                   ("Selected" (layer-selected-view :viewport ))))))
    (add-menu-items self   result `(("Paste" nil "" :sub-items ,layers)))))


(defmethod get-menu-items :around ((self d-view) (slot-name (eql 'middle-menu)))
  (let* ((vm (var-menu-items self))
         (cm (case-menu-items self))
         (var-menu-items (if vm
                           (append 
                            vm
                            `(("-" nil)
                              ("LinkVars?" (set-link-vars-p :toggle))))
                           `(("LinkVars?" (set-link-vars-p :toggle)))))
         (case-menu-items (if cm
                            (append cm
                                    `(("-" nil)
                                      ("LinkCases?" (set-link-cases-p :toggle))))
                            `(("LinkCases?" (set-link-cases-p :toggle))))))
    
    (add-menu-items self  (call-next-method) 
                    `(("-" nil)
                      ("Variables" nil "" :sub-items ,var-menu-items)
                      ("Cases" nil "" :sub-items ,case-menu-items)
                      ("-" nil)
                      ("Flip-x" (set-flip-x-p :toggle :draw? t))
                      ("Flip-y" (set-flip-y-p :toggle :draw? t))))))

(defmethod case-menu-items ((self d-view) &key target)
  `(
    ("DeactivateSelected" nil ""
     :sub-items 
     (("Rescale" (deactivate-cases :selected :rescale? t :target ,target))
      ("NoRescale" (deactivate-cases :selected :rescale? nil :target ,target))) )
    ("DeactivateUnSelected" nil ""
     :sub-items 
     (("Rescale" (deactivate-cases :unselected :rescale? t :target ,target))
      ("NoRescale" (deactivate-cases :unselected :rescale? nil :target ,target))) )
    ("ActivateSelected" nil ""
     :sub-items 
     (("Rescale" (activate-cases :selected :rescale? t :target ,target))
      ("NoRescale" (activate-cases :selected :rescale? nil :target ,target))))
    ("ActivateUnSelected" nil ""
     :sub-items 
     (("Rescale" (activate-cases :unselected :rescale? t :target ,target))
      ("NoRescale" (activate-cases :unselected :rescale? nil :target ,target))))
    ("ActivateAll" nil ""
     :sub-items 
     (("Rescale" (activate-all-cases  :rescale? t :target ,target))
      ("NoRescale" (activate-all-cases  :rescale? nil :target ,target))) ))
  )

(defmethod update-menu-items :after ((self d-view) 
                                     (slot-name (eql 'middle-menu)))
  (let* ((m-case (wb:get-menu-item (slot-value self slot-name) "Cases"))
         (m-var (wb:get-menu-item (slot-value self slot-name) "Variables"))
         )
    (if m-case (wb:check-menu-item m-case "LinkCases?" (link-cases-p self)))
   (if m-var  (wb:check-menu-item m-var "LinkVars?" (link-vars-p self)))))
    

(defmethod change-variable-when-vars ((self d-view) &rest args)
  (if (has-variates-p self)
    (apply #'change-variable self args) ))


(defun var-menu-item-list (vars key &key (names vars) target method)
       (let ((answer
              (loop for v in vars for vn in names
                    collect
                    `( ,(format nil "~(~A~)" vn)  
                       (,method   ,key ,v :draw? t :target ,target )))))
         (if (and answer
                  (member key (list :transform :x-transform :y-transform :z-transform)))
           (append answer
                   `(("Identity"  
                      (,method ,key nil :draw? t :target ,target))))
           answer)))




(defmethod var-menu-items ((self d-view) &key target 
                           (method 'change-variable-when-vars))
  (let* (( vars  (menu-variates-of self))
           (projs (loop with n = (ncases self)
                        for p in *projections*
                        when (= n (projector-dim p))
                        collect p))
           (var-list (var-menu-item-list  
                      vars  :var :names
                      (variate-names vars) 
                      :target target :method method ))
           
           (func-list (var-menu-item-list  
                       (variable-transform-list self)
                       :function
                       :target target :method method))
           (trans-list (var-menu-item-list 
                                projs :transform
                                :target target :method method
                                :names (mapcar #'projector-name projs))))
      
      `( 
        ,(if vars `( "Var" nil "" :sub-items ,var-list))
        ,(if func-list `( "Function" nil "" :sub-items ,func-list))
        ,(if projs `( "Matrix" nil "" :sub-items ,trans-list) nil) )))



       

(defmethod menu-properties :around ((self d-view) 
                                    (slot-name (eql 'middle-menu)))
  ;; middle menus for plots are remade when any of variates, projections
  ;; or functions change
  (let ((data (menu-variates-of  self)))
    (append
     (list (cons :ncases (ncases self))
           (cons :variates data)
           (cons :functions (variable-transform-list self))
           (cons :projections *projections*))
     (call-next-method))))

(defmethod variable-transform-list (self)
  (let ((f (x-func-of self)))
    (cond ((null f)
           *variable-transforms*)
          ((loop with f1 = (get-function f)
                 for v in *variable-summaries*
                 thereis (and (not (eql v :prompt) ) (equal f1 (get-function v))))
           *variable-summaries*)
          (t *variable-transforms*))))

(defmethod var-menu-items ((self 2d-view) 
                           &key target (method 'change-variable-when-vars))
  (let ((vars (menu-variates-of self))
        (projs (loop with n = (ncases self )
                     for p in *projections*
                     when (= n (projector-dim p))
                     collect p))
        x-list y-list func-list-x func-list-y
         p-list-x p-list-y)
    
    
    (multiple-value-setq (x-list y-list)
      (values-list
       (loop with names = (variate-names vars)
             for v in (list :x :y)
             collect 
             (var-menu-item-list vars  v :names names 
                                 :target target :method method))))
    (multiple-value-setq (func-list-x func-list-y)
      (values-list
       (loop with tlist = (variable-transform-list self)
             for v in (list :x-function :y-function)
             collect 
             (var-menu-item-list tlist  v  
                                 :target target :method method))))
    
    (multiple-value-setq (p-list-x p-list-y)
      (values-list
       (loop with names = (mapcar #'projector-name projs)
             for v in (list :x-transform :y-transform)
             collect 
             (var-menu-item-list projs v :names names 
                                 :target target :method method))))
      
      `( 
        ,(if vars `( "X Select" nil "" :sub-items ,x-list))
        ,(if func-list-x `( "X Function" nil "" :sub-items ,func-list-x))
         ,(if projs `("X Matrix" nil "" :sub-items ,p-list-x) nil)
        ,(if (or vars func-list-x projs)  `("-" nil))
        ,(if vars `( "Y Select" nil "" :sub-items ,y-list))
        ,(if func-list-y `( "Y Function" nil "" :sub-items ,func-list-y))
        ,(if projs `("Y Matrix" nil "" :sub-items ,p-list-y) nil))))
 

(defmethod var-menu-items ((self 3d-view) &key 
                           target (method 'change-variable-when-vars))
  (let ((vars (menu-variates-of self))
        (projs (loop with n = (ncases self )
                     for p in *projections*
                     when (= n (projector-dim p))
                     collect p))
        x-list y-list z-list func-list-x func-list-y
        func-list-z p-list-x p-list-y p-list-z)
    
    
    (multiple-value-setq (x-list y-list z-list)
      (values-list
       (loop with names = (variate-names vars)
             for v in (list :x :y :z)
             collect 
             (var-menu-item-list vars  v :names names 
                                 :target target :method method))))
    (multiple-value-setq (func-list-x func-list-y func-list-z)
      
      
      (values-list
       (loop with tlist = (variable-transform-list self)
             for v in (list :x-function :y-function :z-function)
             collect 
             (var-menu-item-list tlist  v  
                                 :target target 
                                 :method method))))
    
    (multiple-value-setq (p-list-x p-list-y p-list-z)
      (values-list
       (loop with names = (mapcar #'projector-name projs)
             for v in (list :x-transform :y-transform :z-transform)
             collect 
             (var-menu-item-list projs v :names names  :target target :method method))))
    `(,(if vars `( "X Select" nil "" :sub-items ,x-list))
      ,(if func-list-x `( "X Function" nil "" :sub-items ,func-list-x))
      ,(if projs `("X Matrix" nil "" :sub-items ,p-list-x) nil)
      ,(if (or vars func-list-x projs)  `("-" nil))
      ,(if vars `( "Y Select" nil "" :sub-items ,y-list))
      ,(if func-list-y `( "Y Function" nil "" :sub-items ,func-list-y))
      ,(if projs `("Y Matrix" nil "" :sub-items ,p-list-y) nil)
      ,(if (or vars func-list-y projs)  `("-" nil))
      ,(if vars `( "Z Select" nil "" :sub-items ,z-list))
      ,(if func-list-z `( "Z Function" nil "" :sub-items ,func-list-z))
      ,(if projs `("Z Matrix" nil "" :sub-items ,p-list-z) nil))))
          
