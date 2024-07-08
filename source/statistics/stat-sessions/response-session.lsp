;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;;                             response-sessions.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;  
;;;
;;;  Authors:
;;;     R.W. Oldford 1995
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(model-session glm-session lm-session)))


(defun model-session
       (&rest all-args
              &key data
              (draw? T)
              (color wb::*white-color*)
              (title NIL)
              (viewport NIL)
              &allow-other-keys)
  "Sets up a view from which a response model may be constructed."
  (declare (ignore color)) ; 31JUL2023
  (unless (dataset-p data) (setf data (choose-dataset)))
  (unless title (setf title "Model session"))
  (let*
    (;(data-name (text-view :text "Dataset: " ; 31JUL2023 never used 
     ;                      :draw? NIL
     ;                      :viewed-object data))
     (data-view (dataset-view :data data 
                              :draw? NIL
                              :viewed-object data
                              :signposts? NIL))
     (hline1    (line :slope 0 :intercept .5
                     :viewed-object data))
     (hline2    (line :slope 0 :intercept .5
                     :viewed-object data))
     (glm-button (control-button
                  :viewed-object data
                  :text
                  (format NIL "Generalized Linear Modelling")
                  :left-fn
                  #'(lambda ()
                      (apply #'glm-session :data data :draw? T all-args))
                  :draw? NIL))
     (lm-button (control-button
                 :viewed-object data
                 :text
                 (format NIL "Linear Regression")
                 :left-fn
                 #'(lambda ()
                     (apply #'lm-session :data data :draw? T all-args))))
     (button-panel
      (grid-layout :nsubviews 2
                   :ncols 2  :box-views? NIL
                   :gap-x 0.1 :gap-y 0.1
                   :subviews (list lm-button glm-button)
                   :cols '(0 .4 .6 1)
                   :draw? NIL
                   :viewed-object data
                   ))
     (result (view-layout
                   :viewed-object data
                   :subviews (list 
                              data-view
                              hline1 
                              button-panel
                              hline2 )
                   :positions '((0 100 60 100)
                                (0 100 59 60)
                                (10 90 5 20)
                                (0 100 0 2)
                                 )
                   :box-views? NIL
                   :gap-x 0.1 :gap-y 0.1
                   ))
     )
    (when draw?
      (unless (viewport-p viewport)
        (setf viewport 
               (make-viewport
                (make-view-window 
                 :left 10 :right 400
                 :bottom (- (wb::screen-height) 350)
                 :top (- (wb::screen-height) 100)
                 :title (if (dataset-name data)
                            (format NIL "Modelling ~a" (dataset-name data))
                            "Modelling Session")))))
      (draw-view result :viewport viewport))
    result))
  


(defun glm-session (&rest keyword-args
                          &key 
                          (data NIL)
                          ;;(model NIL)
                          (draw? NIL)
                          (viewport NIL)
                          &allow-other-keys
                          )
  (unless data
    (setf data (choose-dataset)))
  (let (;;(title (text-view :text "Model specification"
        ;;                  :justification :center
        ;;                  :draw? NIL))
        (name  (text-view :text (concatenate 'string "Modelling "
                                             (dataset-name data))
                          :draw? NIL))
        (data-view (dataset-view :data data 
                                 :draw? NIL
                                 :viewed-object data
                                 :signposts? NIL))
        (response-label
         (text-view :text "Response: "
                    :draw? NIL))
        (response
         (text-view :text ""
                    :draw? NIL))
        (predictor-label
         (text-view :text "Linear predictor: "
                    :draw? NIL))
        (linear-predictor
         (text-view :text ""
                    :draw? NIL))
        #|
        (de-activated-cases
         (view-layout 
          :draw? NIL
          :positions '((0 1 8.1 10) (0 1 0 8))
          :subviews
          (list 
           (text-view :draw? NIL
                      :text "Deactivate cases"
                      :justification :center)
           (case-display-list    
            :data data
            :draw? NIL
            :scrollable? T)))
         )
        |#
        (hline1 (line :slope 0 :intercept .5 :draw? NIL))
        (hline2 (line :slope 0 :intercept .5 :draw? NIL))
        (hline3 (line :slope 0 :intercept .5 :draw? NIL))
        (edit-response-btn (control-button :text "<- Edit"
                                           :draw? NIL))
        (edit-predictor-btn (control-button :text "<- Edit"
                                            :draw? NIL))
        (fit-button (control-button :text "Fit" :draw? NIL))
        ;;(save-model-button (control-button :text "Save model" :draw? NIL))
        model-spec-layout
        ;; cases-to-delete
        )
    
    ;; Entry for the linear predictor on a control button
    (setf  (left-fn-of edit-predictor-btn)
           #'(lambda () 
               (highlight-view linear-predictor)
               (erase-view linear-predictor)
               (set-text
                linear-predictor
                (wb::prompt-user
                 :prompt-string
                 (format NIL
                         "Enter linear predictor (Wilkinson-Rogers notation). ~%
                          EG. A + B + C*D ~%
                          Enter variate labels containing blanks as strings. ~%
                          Enclose lisp function calls in {} as in { (log x) }")
                 :initial-string (get-text linear-predictor))
                )
               (draw-view linear-predictor)))
    
    ;; Entry for the response on a control button
    (setf  (left-fn-of edit-response-btn)
           #'(lambda () 
               (highlight-view response)
               (erase-view response)
               (set-text
                response
                (wb::prompt-user
                 :prompt-string
                 (format NIL
                         "Enter response variate. ~%
                          If blanks are part of the variate name, ~%
                          enter the name as a string. ~%
                          Enclose lisp function calls in {} as in { (log y) }")
                 :initial-string (get-text response))
                )
               (draw-view response)))
    
    ;; Do the fit button
    (setf  (left-fn-of fit-button)
           #'(lambda ()
               (let
                 ((response-var (get-text response))
                  (lin-pred (get-text linear-predictor))
                  wilkinson-rogers-formula
                  fit-object)
                 (when (string-equal response-var "")
                   (set-text
                    response
                    (wb::prompt-user
                     :prompt-string
                     (format NIL
                             "Enter response variate. ~%
                              If blanks are part of the variate name, ~%
                              enter the name as a string. ~%
                              Enclose lisp function calls in {} as in { (log y) }")
                     :initial-string (get-text response))
                    )
                   (draw-view response)
                   (setf response-var (get-text response)))
                 (when (string-equal lin-pred "")
                   (set-text
                    linear-predictor
                    (wb::prompt-user
                     :prompt-string
                     (format NIL
                             "Enter linear predictor (Wilkinson-Rogers notation). ~%
                              EG. A + B + C*D ~%
                              Enter variate labels containing blanks as strings. ~%
                              Enclose lisp function calls in {} as in { (log x) }")
                     :initial-string (get-text linear-predictor))
                    )
                   (draw-view linear-predictor)
                   (setf lin-pred (get-text linear-predictor)))
                 (setf wilkinson-rogers-formula
                       (concatenate 'string
                                    response-var " ~ " lin-pred))
                 (setf fit-object (glm wilkinson-rogers-formula data))
                 (display fit-object :draw? T :signposts? T)
                 )
               )
           )
    
    #|
    ;; Do the save-model button
    (setf  (left-fn-of save-model-button)
           #'(lambda ()
               (let
                 ((response-var (get-text response))
                  (lin-pred (get-text linear-predictor))
                  wilkinson-rogers-formula)
                 (when (string-equal response-var "")
                   (set-text
                    response
                    (wb::prompt-user
                     :prompt-string
                     (format NIL
                             "Enter response variate. ~%~
                              If blanks are part of the variate name, ~
                              enter the name as a string. ~%~
                              Enclose lisp function calls in {} as in { (log y) }")
                     :initial-string (get-text response))
                    )
                   (draw-view response)
                   (setf response-var (get-text response)))
                 (when (string-equal lin-pred "")
                   (set-text
                    linear-predictor
                    (wb::prompt-user
                     :prompt-string
                     (format NIL
                             "Enter linear predictor (Wilkinson-Rogers notation). ~%~
                              EG. A + B + C*D ~%~
                              Enter variate labels containing blanks as strings. ~%~
                              Enclose lisp function calls in {} as in { (log x) }")
                     :initial-string (get-text linear-predictor))
                    )
                   (draw-view linear-predictor)
                   (setf lin-pred (get-text linear-predictor)))
                 (setf wilkinson-rogers-formula
                       (concatenate 'string
                                    response-var " ~ " lin-pred))
                 (setf model (model 'gaussian-linear-model
                                    wilkinson-rogers-formula))
                 )
               )
           )
      |#
    (setf model-spec-layout
          (apply #'view-layout
                 :viewed-object data
                 :draw? NIL
                 :subviews
                 (list 
                  ;;title
                  name
                  hline1
                  data-view
                  response-label
                  response
                  edit-response-btn
                  predictor-label
                  linear-predictor
                  edit-predictor-btn
                  ;;de-activated-cases
                  hline2
                  fit-button
                  ;;save-model-button
                  hline3
                  )
                 :positions           
                 '(;;(30  70 95 100)     ;; title
                   (10  40 92  95)     ;; name
                   ( 0 100 90  91)     ;; hline1
                   ( 0 100 60  89)     ;; data-view
                   ( 2  20 45  50)     ;; response-label
                   (21  75 45  50)     ;; response
                   (76  90 45  50)     ;; edit-response-btn
                   ( 2  20 40  45)     ;; predictor-label
                   (21  75 40  45)     ;; linear-predictor
                   (76  90 40  45)     ;; edit-predictor-btn
                   ;;(4.1 6 1.5 2.8)    ;; de-activated-cases
                   ( 0 100 38  40)     ;; hline2
                   (10  30 30  38)     ;; fit-button
                   ;;(4.5 5.9 0.3 .9)   ;; save-model-button
                   ( 0 100 28  30)     ;; bottom line
                   )
                 keyword-args
                 )
          )
    (if draw?
      (draw-view model-spec-layout
                 :viewport (if viewport
                             viewport
                             (make-viewport
                              (make-view-window 
                               :left 0
                               :bottom 0
                               :right (- (wb::screen-width) 50)
                               :top (- (wb::screen-height) 50)
                               :title (format NIL "Model session for ~s."
                                              (or (dataset-name data)
                                                  "data"))))))
      model-spec-layout
      )
    )
  )

    
(defun lm-session (&rest keyword-args
                          &key 
                          (data NIL)
                          ;;(model NIL)
                          (draw? NIL)
                          (viewport NIL)
                          &allow-other-keys
                          )
  (unless data (setf data (choose-dataset)))
  (let (;;(title (text-view :text "Regression session"
        ;;                  :justification :center
        ;;                  :draw? NIL))
        (name  (text-view :text (concatenate 'string "Linear modelling of "
                                             (dataset-name data))
                          :draw? NIL))
        (data-view (dataset-view :data data 
                                 :draw? NIL
                                 :viewed-object data
                                 :signposts? NIL))
        (response-label
         (text-view :text "Response: "
                    :draw? NIL))
        (response
         (text-view :text ""
                    :draw? NIL))
        (predictor-label
         (text-view :text "Linear predictor: "
                    :draw? NIL))
        (linear-predictor
         (text-view :text ""
                    :draw? NIL))
        #|
        (de-activated-cases
         (view-layout 
          :draw? NIL
          :positions '((0 1 8.1 10) (0 1 0 8))
          :subviews
          (list 
           (text-view :draw? NIL
                      :text "Deactivate cases"
                      :justification :center)
           (case-display-list    
            :data data
            :draw? NIL
            :scrollable? T)))
         )
        |#
        (hline1 (line :slope 0 :intercept .5 :draw? NIL))
        (hline2 (line :slope 0 :intercept .5 :draw? NIL))
        (hline3 (line :slope 0 :intercept .5 :draw? NIL))
        (edit-response-btn (control-button :text "<- Edit"
                                           :draw? NIL))
        (edit-predictor-btn (control-button :text "<- Edit"
                                            :draw? NIL))
        (fit-button (control-button :text "Fit" :draw? NIL))
        ;;(save-model-button (control-button :text "Save model" :draw? NIL))
        model-spec-layout
        ;; cases-to-delete
        )
    
    ;; Entry for the linear predictor on a control button
    (setf  (left-fn-of edit-predictor-btn)
           #'(lambda () 
               (highlight-view linear-predictor)
               (erase-view linear-predictor)
               (set-text
                linear-predictor
                (wb::prompt-user
                 :prompt-string
                 (format NIL
                         "Enter linear predictor (Wilkinson-Rogers notation). ~%
                          EG. A + B + C*D ~%
                          Enter variate labels containing blanks as strings. ~%
                          Enclose lisp function calls in {} as in { (log x) }")
                 :initial-string (get-text linear-predictor))
                )
               (draw-view linear-predictor)))
    
    ;; Entry for the response on a control button
    (setf  (left-fn-of edit-response-btn)
           #'(lambda () 
               (highlight-view response)
               (erase-view response)
               (set-text
                response
                (wb::prompt-user
                 :prompt-string
                 (format NIL
                         "Enter response variate. ~%
                          If blanks are part of the variate name, ~%
                          enter the name as a string. ~%
                          Enclose lisp function calls in {} as in { (log y) }")
                 :initial-string (get-text response))
                )
               (draw-view response)))
    
    ;; Do the fit button
    (setf  (left-fn-of fit-button)
           #'(lambda ()
               (let
                 ((response-var (get-text response))
                  (lin-pred (get-text linear-predictor))
                  wilkinson-rogers-formula
                  fit-object)
                 (when (string-equal response-var "")
                   (set-text
                    response
                    (wb::prompt-user
                     :prompt-string
                     (format NIL
                             "Enter response variate. ~%
                              If blanks are part of the variate name, ~%
                              enter the name as a string. ~%
                              Enclose lisp function calls in {} as in { (log y) }")
                     :initial-string (get-text response))
                    )
                   (draw-view response)
                   (setf response-var (get-text response)))
                 (when (string-equal lin-pred "")
                   (set-text
                    linear-predictor
                    (wb::prompt-user
                     :prompt-string
                     (format NIL
                             "Enter linear predictor (Wilkinson-Rogers notation). ~%
                              EG. A + B + C*D ~%
                              Enter variate labels containing blanks as strings. ~%
                              Enclose lisp function calls in {} as in { (log x) }")
                     :initial-string (get-text linear-predictor))
                    )
                   (draw-view linear-predictor)
                   (setf lin-pred (get-text linear-predictor)))
                 (setf wilkinson-rogers-formula
                       (concatenate 'string
                                    response-var " ~ " lin-pred))
                 (setf fit-object (glm wilkinson-rogers-formula data))
                 (display fit-object :draw? T :signposts? T)
                 )
               )
           )
    
    #|
    ;; Do the save-model button
    (setf  (left-fn-of save-model-button)
           #'(lambda ()
               (let
                 ((response-var (get-text response))
                  (lin-pred (get-text linear-predictor))
                  wilkinson-rogers-formula)
                 (when (string-equal response-var "")
                   (set-text
                    response
                    (wb::prompt-user
                     :prompt-string
                     (format NIL
                             "Enter response variate. ~%~
                              If blanks are part of the variate name,~
                              enter the name as a string.  ~%~
                              Enclose lisp function calls in {} as in { (log y) }")
                     :initial-string (get-text response))
                    )
                   (draw-view response)
                   (setf response-var (get-text response)))
                 (when (string-equal lin-pred "")
                   (set-text
                    linear-predictor
                    (wb::prompt-user
                     :prompt-string
                     (format NIL
                             "Enter linear predictor (Wilkinson-Rogers notation). ~%~
                              EG. A + B + C*D ~%~
                              Enter variate labels containing blanks as strings. ~%~
                          Enclose lisp function calls in {} as in { (log x) }")
                     :initial-string (get-text linear-predictor))
                    )
                   (draw-view linear-predictor)
                   (setf lin-pred (get-text linear-predictor)))
                 (setf wilkinson-rogers-formula
                       (concatenate 'string
                                    response-var " ~ " lin-pred))
                 (setf model (model 'gaussian-linear-model
                                    wilkinson-rogers-formula))
                 )
               )
           )
      |#
    (setf model-spec-layout
          (apply #'view-layout
                 :viewed-object data
                 :draw? NIL
                 :subviews
                 (list 
                  ;;title
                  name
                  hline1
                  data-view
                  response-label
                  response
                  edit-response-btn
                  predictor-label
                  linear-predictor
                  edit-predictor-btn
                  ;;de-activated-cases
                  hline2
                  fit-button
                  ;;save-model-button
                  hline3
                  )
                 :positions           
                 '(;;(30  70 95 100)     ;; title
                   (10  40 92  95)     ;; name
                   ( 0 100 90  91)     ;; hline1
                   ( 0 100 60  89)     ;; data-view
                   ( 2  20 45  50)     ;; response-label
                   (21  75 45  50)     ;; response
                   (76  90 45  50)     ;; edit-response-btn
                   ( 2  20 40  45)     ;; predictor-label
                   (21  75 40  45)     ;; linear-predictor
                   (76  90 40  45)     ;; edit-predictor-btn
                   ;;(4.1 6 1.5 2.8)    ;; de-activated-cases
                   ( 0 100 38  40)     ;; hline2
                   (10  30 30  38)     ;; fit-button
                   ;;(4.5 5.9 0.3 .9)   ;; save-model-button
                   ( 0 100 28  30)     ;; bottom line
                   )
                 keyword-args
                 )
          )
    (if draw?
      (draw-view model-spec-layout
                 :viewport (if viewport
                             viewport
                             (make-viewport
                              (make-view-window 
                               :left 0
                               :bottom 0
                               :right (- (wb::screen-width) 50)
                               :top (- (wb::screen-height) 50)
                               :title (format NIL "Linear model session for ~s."
                                              (or (dataset-name data)
                                                  "data"))))))
      model-spec-layout
      )
    )
  
  )



    
                        
