(in-package :q-user)

(defvar *all-menus*
  (list ccl:*apple-menu*
        ccl:*edit-menu*
        ccl:*eval-menu*
        ccl:*file-menu*
        ccl:*tools-menu*
        ccl:*windows-menu*
        (q::quail-menu)
        (q::quail-plot-menu)))
        
(defun re-enable-menubars (&rest whatever)
  (declare (ignore whatever))
  (loop
    for m in
    (union
     *all-menus*
     (union
      (ccl::menubar)
      (loop for vw in (ccl::windows :class 'view-window)
            append
            (loop for (key . m) in (slot-value vw 'wb::title-menus)
                  ;; using key here just suppresses a warning message
                  collect (progn key m))) 
      :test #'eq)
     :test #'eq)
    do (ccl::menu-enable m)))

(ccl::def-fred-command (:function #\c) re-enable-menubars)

;;; or set this up as a bug fix button.

(let* ((menubutton (control-button :text "Do it"))
       (tv (text-view :text
                      (format NIL
                              "Some buttons to fix bugs. ~%~
                               Evaluate the form or click on the button.")))
       (rm (text-view :text "(re-enable-menubars)   "))
       (vw (make-view-window
            :left 10 :top (- (wb::screen-height) 40)
            :bottom (- (wb::screen-height) 100)
            :right 300
            :title "Bug fixes"
            :background-color wb:*red-colour*
            :pen-color wb:*yellow-colour*))
       (vp (make-viewport vw))
       (vl (view-layout :subviews (list tv rm menubutton)
                        :positions '((0 10 5 10)
                                     (1  8 2 4)
                                     (8 10 2 4))))
       )
  (setf (left-fn-of menubutton)
        #'(lambda ()
            (highlight-view menubutton)
            (set-text menubutton "OK")
            (eval '(re-enable-menubars))
            (set-text menubutton "Done")
            (downlight-view menubutton)
            (set-text menubutton "Do it"))
        )
  (draw-view vl :viewport vp)
  )