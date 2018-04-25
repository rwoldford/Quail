;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               whats-new.lsp                            
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1995 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     R.W. Oldford 1995.
;;;
;;;
;;;--------------------------------------------------------------------------------
;;; 
;;;
;;;  This file sets up some a mouse-sensitive window that allows the
;;;  user access to information on known bugs and fixes.
;;;
(in-package :quail)

(export '(bugs whats-new?))

(eval-when (load eval)
  (load "q:comments;fixes.lsp"))

(defclass bugs-window (view-window)
  ()
  (:documentation "Just a place-holder class so that the bugs-window ~
                   isn't closed if all windows of some other class are closed.")
  )

(defun make-bugs-window (&rest canvas-keywords
                              &key (left 10)
                              (right 200)
                              (bottom (- (wb::screen-height) 200))
                              (top (- (wb::screen-height) 40))
                              (title "(bugs)")
                              (background-color wb:*red-colour*)
                              (pen-color wb:*white-colour*)
                              &allow-other-keys)
  (apply #'make-view-window
         :left left
         :top top
         :bottom bottom
         :right right
         :view-window-class 'bugs-window
         :title title
         :background-color background-color
         :pen-color pen-color
         canvas-keywords))

(defun bugs (&optional (draw? T))
  "Sets up a window which allows the user to access information ~
   on known bugs and fixes."
  
  (let* ((tv (text-view :text
                        (format NIL
                                "Some bug information. ~%~
                                 Click on the appropriate button.")
                        :draw? NIL
                        :color wb:*white-color*))
         (buttons
          #+:ccl
          (grid-layout
           :subviews
           (list (control-button
                  :text "General bugs" :draw? NIL
                  :left-fn
                  #'(lambda ()
                      (edit-file "q:comments;bugs.lsp")))
                 (control-button
                  :text "Mac specific bugs" :draw? NIL
                  :left-fn
                  #'(lambda ()
                      (edit-file "q:comments;bugs-mcl.lsp")))
                 )
           :draw? NIL
           :rows '(1.0 0.6 0.5 0.1)
           )
          #-:ccl
          (control-button
           :text "General bugs" :draw? NIL
           :left-fn
           #'(lambda ()
               (edit-file "q:comments;bugs.lsp")))
          )
         
         )
    (if draw?
      (let ((vl (view-layout :subviews (list tv buttons (text-view :text " "))
                             :positions '((0 10 8 10)
                                          (1  8 0.5 7)
                                          (0 10 0 0.1))
                             :draw? NIL))
            vw vp)
        (setf vw (make-bugs-window))
        (setf vp (make-viewport vw .1 1 0 1))
        (draw-view vl :viewport vp)
        vl)
      buttons)
    )
  )

(defun whats-new? ()
  "Sets up a window which allows the user to access various information ~
   on what is new, especially known bugs and fixes."
  
  (let* ((tv (text-view :text
                        (format NIL
                                "Some information. ~%~
                                 Click on the appropriate button.")
                        :draw? NIL
                        :color wb:*white-color*))
         (buttons (bugs NIL))
         (vl (view-layout :subviews (list tv buttons (text-view :text " "))
                          :positions '((0 10 8 10)
                                       (1  8 0.5 7)
                                       (1  8 0 0.1))
                          :draw? NIL))
         (vw (make-bugs-window))
         (vp (make-viewport vw .1 1 0 1))
         
         )
    (draw-view vl :viewport vp)
    )
  )

(eval-when (load eval)
  (bugs)
  (install-fixes))
