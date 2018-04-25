;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dialogs70.lsp
;;; with a copy of the example coed from acl62
;;; 14JAN2005
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; START OF ACL62 CODE
(defparameter *text-item* 
    (make-instance 'static-text
      :left 10 :top 50 :width 90 :height 20
      :value 0))

(defparameter *scroll-bar1* 
   (make-instance 'horizontal-scroll-bar
     :left 10 :top 10 :width 90 :height 16 
     :range '(0 100)
     :value 0
     :on-change 'set-text-item-too 
     ;; :increment is the amount stepped by a click in on
     ;; of the arrows
     :increment 10
     :delayed nil))

(defparameter *scroll-bar2* 
   (make-instance 'horizontal-scroll-bar
     :left 10 :top 30 :width 90 :height 16 
     :value 0
     :range '(0 10)
     :on-change 'set-text-item-too
     :delayed nil))

(defun set-text-item-too (item new-value old-value)
  (setf (value *text-item*)
        (+ (value *scroll-bar1*)
           (value *scroll-bar2*))))

;;;; now create the dialog 

(make-window :example-dialog
  :class 'dialog 
  :owner (development-main-window *system*) 
  :title "Example"
  :visible-box (make-box 0 0 110 80)
  :widgets  (list *scroll-bar1* *scroll-bar2* *text-item*))
;;; END OF ACL62 CODE
(defparameter *element4* 
   (make-instance 'multi-item-list
     :left 10 :top 10 :width 190 :height 160 
     :range (list "First" "Second" "Third")
     )) ;; ok
(defparameter *select4*
  (make-instance 'static-text
                 :left 10 :top 200 :width 150 :height 30
    :value "Select"
    :on-click #'(lambda (x y)
                   (format t  "Results! ~s" (selected-object *element4*))))
    ) ;; ok
;; create this
(make-window :example-dialog
  :class 'dialog
  :owner (development-main-window *system*)
  :title "ZOT"
  :visible-box (make-box 0 0 300 200)
  :widgets (list *element4* *select4*))

;; This returns ("Second") when Second is selected and Select is clicked!!