;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                        Box-Cox transformations on plots                             
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (C) 1994 Statistical Computing Laboratory, University of Waterloo
;;;
;;; 
;;;  Authors:
;;;     C.B. Hurley 1994.
;;;     R.W. Oldford 1994.
;;;

(in-package :quail-user)


;;; 
;;; In this file we are going to give a bunch of examples
;;; which construct Box-Cox transformations on plots.
;;;
;;; They begin with the simplest constructs and then increase in generality.
;;;

;;;
;;;  First the set of box-cox transformations are as follows:
;;;
;;;              p
;;;             y   - 1
;;;   y(p) =  -----------      p != 0
;;;               p
;;;
;;;
;;;   y(p) = ln(y)             p = 0
;;;
;;;  They are simple power transformations of *Non-negative* values y
;;;  Dividing by p ensures that the order of the observations is preserved
;;;  and together with subtraction of 1 ensures that the family is smoothly
;;;  changing and has the limit ln(y) as p -> 0.
;;;  In practice, we usually drop the -1/p since we would like p=1 to return
;;;  the original observations.  So our function will be the following.
;;;

(defun boxcox (y &optional (p 1))
  "Box-Cox transformation with parameter p."
  (cond 
   ((zerop p) (log y))
   ((= p 1) y)
   (T (/ (expt y p) p))))


;;;
;;; Now suppose we have a scatterplot in hand, say the famous brain and body
;;; weight data.

(load "q:Data;bbwgt.lsp")

(<- s (scatterplot :data brain-body-wts
                   :x "body weight" :y "brain weight"
                   :size 4 :fill? T
                   :link? T))

;;;
;;; And we would like to control the scatterplot's display by 
;;; applying box-cox transformation to x and y simultaneously.
;;; Then we might proceed as follows.
;;;

;;; First we will build the controller.  It will be a simple needle-slider
;;; with an axis alongside to indicate the power p.
;;;

;;; The needle-slider

(<- n (needle-slider
       :orientation :vertical
       :min -2 :max 3 :level 1
       :draw? NIL))

;;; and now draw it in a plot so that we can add the axis information.

(<- n-control
    (plot :title "Box-Cox both" :draw? T
          :right-view-size 0.5
          :interior-view n
          :right-view '(:type axis :orientation :vertical
                        :tic-list (-2 -1 (0 "log") (0.5 "sqrt") 1 2 3)
                        )
          :left-label "power"
          ))

;;;
;;; Fooling with the various plot parameters could produce different looking
;;; displays.  We won't explore these here.

;;;
;;; We will need a slider function that will do the box-cox transformation
;;; by picking up the power from the value of the needle-slider's level.
;;; NOTE that this is rather bad form at this level as it refers to the
;;; global variable n.  We will improve on this later.  Search forward
;;; for the string BETTER to find out how.
;;;

(defun slider-fn (x) (boxcox x (slider-level-of n)))

;;; Now we set the left button function of the needle slider to be
;;; an anonymous function of no arguments which calls change-variable on
;;; the scatterplot with x- and y-functions being the function #'slider-fn
;;;

(setf  (left-fn-of n)
       #'(lambda () 
             (change-variable s
                              :x-function #'slider-fn
                              :y-function #'slider-fn)))

;;;
;;;  Now moving the slider will cause the points to change positions.
;;;  Moving the needle to the "log" position on the scale will get a plot
;;;  that is roughly a straight line.  That is brain weight are roughly
;;;  linearly related on the log-scale.
;;;
;;;  For fun, you might paste a fitted line on the point cloud from its'
;;;  right button menu.
;;;
;;;  Or how about adding a display of the power?

(let* ((l (label :viewed-object n
                 :text #'(lambda(x)
                           (format nil "Box-Cox Power = ~4,3F"
                                   (slider-level-of n))))))
  ;; The following ensures that the power label is updated whenever
  ;; the interior view of s changes.
  (text-link (interior-view-of s) l)
  (setf (top-label-of s) l)
  (reposition-view s))


;;;
;;;  We might also have built two sliders, one for x and one for y.
;;;  The simplest way to achieve this is a variation on that above.
;;; 

;;;  First let's change our old needle-slider to affect only the x
;;;  values in the scatterplot.

(setf  (left-fn-of n)
       #'(lambda () (change-variable s
                                     :x-function #'slider-fn
                                     :y-function NIL)))
;;;
;;; And we better change the title while we are at it.
;;;

(set-text (title-of n-control) "Box-Cox X")


;;;
;;;  Now we construct the new needle-slider control in exactly the
;;;  same way as before
;;;

(<- n1 (needle-slider
        :orientation :vertical
        :min -2 :max 3 :level 1
        :draw? NIL))

;;; and now draw it in a plot so that we can add the axis information.

(<- n1-control
    (plot :title "Box-Cox Y" :draw? T
          :right-view-size 0.5
          :interior-view n1
          :right-view '(:type axis :orientation :vertical
                        :tic-list (-2 -1 (0 "log") (0.5 "sqrt") 1 2 3)
                        )
          :left-label "power"
          ))

(defun slider-fn-1 (x) (boxcox x (slider-level-of n1)))

(setf  (left-fn-of n1)
       #'(lambda () (change-variable s :y-function #'slider-fn-1)))
;;;
;;;  And fix the power label on the scatterplot.
;;;

(set-text (top-label-of s)
          #'(lambda(x)
              (format nil "Box-Cox Powers: x = ~4,3F, y = ~4,3F"
                      (slider-level-of n)
                      (slider-level-of n1))))

;;;
;;;  We could instead have chosen to put the two sliders in a single layout
;;;  as in
;;;

(view-layout :positions '((0 1 0 1) (1 2 0 1))
             :subviews (list n-control n1-control)
             :draw? T)

;;;
;;;  or even together in one window with the scatterplot as in
;;;

(view-layout :positions '((0 3 0 3) (3 4 0 3) (4 5 0 3))
             :subviews (list s n-control n1-control)
             :draw? T)

;;;
;;; Note of course that all other displays of s, n-control, and n1-control
;;; continue to work. They are after all truly the same views, just displayed
;;; in different places.
;;;

;;;
;;;  Another possibility could have been to add the slider-controls directly
;;;  on the original scatterplot.  For example,
;;;

(<- (right-view-of s) n1-control)

;;; And now to make it show up, we tell s to reposition its subviews now that
;;; it has one more.

(reposition-view s)

;;;  In this case we might have preferred a plot where
;;;  the slider for changing x was horizontal and the top-view of the plot
;;;  and that for y was vertical and the right-view of the plot.
;;;  For example,

(<- s2 (scatterplot :data brain-body-wts
                   :x "body weight" :y "brain weight"
                   :size 4 :fill? T
                   :link? T))
;;;
;;;  As before we'll construct a slider for x; this one however will be
;;;  horizontally oriented.

(<- n2 (needle-slider
        :orientation :horizontal
        :min -2 :max 3 :level 1
        :draw? NIL))

(<- n2-control
    (plot :draw? NIL
          :bottom-view-size 0.5
          :interior-view n2
          :bottom-view '(:type axis :orientation :horizontal
                        :tic-list (-2 -1 (0 "log") (0.5 "sqrt") 1 2 3)
                        )
          ))

(defun slider-fn-2 (x) (boxcox x (slider-level-of n2)))
(setf  (left-fn-of n2)
       #'(lambda () (change-variable s2
                                     :x-function #'slider-fn-2)))

(setf (top-view-of s2)  n2-control)
(reposition-view s2)

;;;
;;;  And just to be different, let's only allow a few the powers of Y
;;;  and so just use some buttons for that purpose. 

        
(<- bc-buttons
    (list
     (control-button
      :text "y^3"
      :left-fn
      #'(lambda ()
          (change-variable s2 :y-function
                           #'(lambda (x)
                               (boxcox x 3)))))
     (control-button
      :text "y^2"
      :left-fn
      #'(lambda ()
          (change-variable s2 :y-function
                           #'(lambda (x)
                               (boxcox x 2)))))
     (control-button
      :text "identity"
      :left-fn 
      #'(lambda ()
          (change-variable s2 :y-function #'identity)))
     (control-button
      :text "sqrt"
      :left-fn 
      #'(lambda ()
          (change-variable s2 :y-function
                           #'(lambda (x)
                               (boxcox x 1/2)))))
     (control-button
      :text "log"
      :left-fn 
      #'(lambda ()
          (change-variable s2 :y-function #'log)))
     (control-button
      :text "-1/y"
      :left-fn #'(lambda ()
                   (change-variable s2 :y-function
                                    #'(lambda (x)
                                        (boxcox x -1)))))
     (control-button
      :text "-0.5/y^2"
      :left-fn #'(lambda ()
                   (change-variable s2 :y-function
                                    #'(lambda (x)
                                        (boxcox x -2)))))))
(<- (right-view-of s2)
    (col-layout :subviews bc-buttons :draw? NIL :box-views? t))

(reposition-view s2)


;;;
;;; Now for the BETTER way of doing all these sorts of things.
;;; First, we want to avoid using variables like s s2 n1 etc. that are
;;; defined only in the top-level environment.  It is always dangerous and
;;; so nearly always a bad idea to write functions like our slider functions
;;; which depend on the value of a variable defined in the top-level environment.
;;; So while we will still make some use of named variables, this will be done
;;; only inside well protected lexical closures.
;;; Second, we might like our set of transforms to work more generally.
;;; So we will write a few more general functions. Note that a rich documentation
;;; string is also part of the functions (parsed by the help function).

(defun power-control (plot change-keys
                           &key
                           (orientation :vertical)
                           (draw? t))
  "Constructs a needle-slider to set the power to a value in [-2, 3].  ~
   Makes changes to the given plot by supplying a box-cox slider function ~
   as the value of each of the keywords in change-keys. ~
   (:required ~
   (:arg plot The plot object whose interior-view is to be connected with ~
   the Box-Cox needle-slider.) ~
   (:arg change-keys Either a single keyword as in :function or :x-function.  ~
   Or a list of keywords as in '(:x-function :y-function).)  ~
   )~
   (:key ~
   (:orientation :vertical Might also be :horizontal.) ~
   (:draw? T Should the slider be drawn or not?) ~
   ) ~
   "
  
  ;; First we locally define the needle-slider.
  (let*
    ((n
      (needle-slider
       :min -2 :max 3
       :level 1
       :orientation orientation
       :draw? NIL))
     (axis-info 
      (list :type 'axis :orientation orientation
            :tic-list '(-2 -1 (0 "log") (0.5 "sqrt") 1 2 3)))
     
     )
    ;;
    ;; Now create the local slider function.
    ;;
    (flet ((slider-fn (x) (boxcox x (slider-level-of n))))
      ;; make sure change-keys is a list.
      (when (not (listp change-keys))
        (setf change-keys (list change-keys)))
      ;;
      ;; The plot can be asked to use the slider-fn in a call to
      ;; change variable.
      ;; The left-fn of the slider will be assigned a function which
      ;; calls the change-variable function on the plot with the
      ;; slider-fn as the appropriate function.
      
      (setf
       (left-fn-of n)
       #'(lambda () 
           (apply #'change-variable plot
                  (loop for key in change-keys
                        collect key
                        collect #'slider-fn))))
      ;;
      ;; Now make the appropriate control depending on the desired orientation.
      ;;
      (ecase orientation
        (:vertical
         (plot :title nil :draw? draw?
               :right-view-size 0.5
               :interior-view n
               :right-view  axis-info))
        (:horizontal
         (plot :title nil :draw? draw?
               :bottom-view-size 0.5
               :interior-view n
               :bottom-view axis-info)))
      )))

(setq s (scatterplot :data brain-body-wts :x "body weight" :y "brain weight"
                     :size 4 :fill? t
                     :link? T))

(power-control s '(:x-function :y-function))
(power-control s :y-function)

;;;
;;;  Which now could also work with a boxplot, say.
;;;

(<- bp (boxplot :data brain-body-wts :var "body weight" ))

(power-control bp :function)

;;;
;;;  :function works with any 1d-plot.  For example
;;;

(<- h (histogram :data brain-body-wts :var "body weight" ))

(power-control h :function)

;;;
;;;  or
;;; 

(<- 1d-p (1d-plot :data brain-body-wts
          :interior-view 'fringe-view))

(power-control 1d-p :function)


;;;
;;;  And you can imagine the exotica you might produce.
;;;  For example,  (note here that control-buttons remain pressed to show
;;;  which transformation has been selected)
;;;

(defun box-cox-lesson (&optional (data NIL)
                                 (draw? T))
  "Produces a single view containing a scatterplot, two histograms ~
   and two ladders of Box-Cox power transformations.  ~
   (:optional ~
   (:arg data (choose-dataset) The dataset to be used.  If not supplied or NIL ~
   the user is prompted for one.) ~
   (:arg draw? T The draw? flag.  If non-NIL the view is drawn in its own window. ~
   If NIL, the view is simply constructed and returned.)~
   )"
  
  ;; First make sure we have some data.
  (unless data (setf data (choose-dataset)))

  (let* (
         ;; Here are all the pieces.  Note that none are yet drawn.
         ;; Note also the let* so that these pieces are defined in
         ;; sequence, not in parallel.
         
         (s (scatterplot :data data
                         :right-view 'fringe-view
                         :top-view 'fringe-view
                         :draw? NIL
                         :link? T))
         (point-cloud (interior-view-of s))
         (cases (case-display-list :data data
                                   :draw? NIL
                                   :link? T
                                   :scrollable? T))
         (x (x-variate-of point-cloud))
         (y (y-variate-of point-cloud))
         (hist-x (histogram :data data :var x :draw? NIL))
         (hist-y (histogram :data data :var y :draw? NIL))
         (x3 (control-button :text "(1/3) x^3" :toggle? NIL))
         (x2 (control-button :text "(1/2) x^2" :toggle? NIL))
         (x1 (control-button :text "x" :toggle? NIL))
         (x1/2 (control-button :text "sqrt(x)" :toggle? NIL))
         (logx (control-button :text "log(x)" :toggle? NIL))
         (x-1 (control-button :text "-1/x" :toggle? NIL))
         (x-2 (control-button :text "-(1/2)/x^2" :toggle? NIL))
         (selected-x x1)
         (buttons-x (list x3 x2 x1 x1/2 logx x-1 x-2))
         (y3 (control-button :text "(1/3) y^3" :toggle? NIL))
         (y2 (control-button :text "(1/2) y^2" :toggle? NIL))
         (y1 (control-button :text "y" :toggle? NIL))
         (y1/2 (control-button :text "sqrt(y)" :toggle? NIL))
         (logy (control-button :text "log(y)" :toggle? NIL))
         (y-1 (control-button :text "-1/y" :toggle? NIL))
         (y-2 (control-button :text "-(1/2)/y^2" :toggle? NIL))
         (selected-y y1)
         (buttons-y (list y3 y2 y1 y1/2 logy y-1 y-2))
         (x-button-layout 
          (col-layout :subviews buttons-x :draw? NIL :box-views? t))
         (y-button-layout 
          (col-layout :subviews buttons-y :draw? NIL :box-views? t))
         )
    
    ;; These are the buttons that are currently selected
    ;; so highlight them
    (control-start selected-x)
    (control-start selected-y)
    
    ;; Add the histograms to the linked views
    (link-view hist-x)
    (link-view hist-y)

    ;; Now set up the action functions
    ;; First for the y's
    (setf (left-fn-of y3)
          #'(lambda ()
              (let ((self y3))
                (when (not (eq selected-y self))
                  (control-done selected-y)
                  (setf selected-y self)
                  (control-start selected-y)
                  (change-variable s :y-function
                                   #'(lambda (x)
                                       (boxcox x 3)))
                  (change-variable hist-y :function
                                   #'(lambda (x)
                                       (boxcox x 3)))))))
    (setf (left-fn-of y2)
          #'(lambda ()
              (let ((self y2))
                (when (not (eq selected-y self))
                  (control-done selected-y)
                  (setf selected-y self)
                  (control-start selected-y)
                  (change-variable s :y-function
                                   #'(lambda (x)
                                       (boxcox x 2)))
                  (change-variable hist-y :function
                                   #'(lambda (x)
                                       (boxcox x 2)))))))
    (setf (left-fn-of y1)
          #'(lambda ()
              (let ((self y1))
                (when (not (eq selected-y self))
                  (control-done selected-y)
                  (setf selected-y self)
                  (control-start selected-y)
                  (change-variable s :y-function #'identity)
                  (change-variable hist-y :function #'identity)))))
    (setf (left-fn-of y1/2)
          #'(lambda ()
              (let ((self y1/2))
                (when (not (eq selected-y self))
                  (control-done selected-y)
                  (setf selected-y self)
                  (control-start selected-y)
                  (change-variable s :y-function
                                   #'(lambda (x) (boxcox x 1/2)))
                  (change-variable hist-y :function
                                   #'(lambda (x) (boxcox x 1/2)))))))
    (setf (left-fn-of logy)
          #'(lambda ()
              (let ((self logy))
                (when (not (eq selected-y self))
                  (control-done selected-y)
                  (setf selected-y self)
                  (control-start selected-y)
                  (change-variable s :y-function  #'log)
                  (change-variable hist-y :function #'log)))))
    (setf (left-fn-of y-1)
          #'(lambda ()
              (let ((self y-1))
                (when (not (eq selected-y self))
                  (control-done selected-y)
                  (setf selected-y self)
                  (control-start selected-y)
                  (change-variable s :y-function
                                   #'(lambda (x)
                                       (boxcox x -1)))
                  (change-variable hist-y :function
                                   #'(lambda (x)
                                       (boxcox x -1)))))))
    (setf (left-fn-of y-2)
          #'(lambda ()
              (let ((self y-2))
                (when (not (eq selected-y self))
                  (control-done selected-y)
                  (setf selected-y self)
                  (control-start selected-y)
                  (change-variable s :y-function
                                   #'(lambda (x)
                                       (boxcox x -2)))
                  (change-variable hist-y :function
                                   #'(lambda (x)
                                       (boxcox x -2)))))))
    
    ;; And now the same again for the x's

    (setf (left-fn-of x3)
          #'(lambda ()
              (let ((self x3))
                (when (not (eq selected-x self))
                  (control-done selected-x)
                  (setf selected-x self)
                  (control-start selected-x)
                  (change-variable s :x-function
                                   #'(lambda (x)
                                       (boxcox x 3)))
                  (change-variable hist-x :function
                                   #'(lambda (x)
                                       (boxcox x 3)))))))
    (setf (left-fn-of x2)
          #'(lambda ()
              (let ((self x2))
                (when (not (eq selected-x self))
                  (control-done selected-x)
                  (setf selected-x self)
                  (control-start selected-x)
                  (change-variable s :x-function
                                   #'(lambda (x)
                                       (boxcox x 2)))
                  (change-variable hist-x :function
                                   #'(lambda (x)
                                       (boxcox x 2)))))))
    (setf (left-fn-of x1)
          #'(lambda ()
              (let ((self x1))
                (when (not (eq selected-x self))
                  (control-done selected-x)
                  (setf selected-x self)
                  (control-start selected-x)
                  (change-variable s :x-function #'identity)
                  (change-variable hist-x :function #'identity)))))
    
    (setf (left-fn-of x1/2)
          #'(lambda ()
              (let ((self x1/2))
                (when (not (eq selected-x self))
                  (control-done selected-x)
                  (setf selected-x self)
                  (control-start selected-x)
                  (change-variable s :x-function
                                   #'(lambda (x) (boxcox x 1/2)))
                  (change-variable hist-x :function
                                   #'(lambda (x) (boxcox x 1/2)))))))

    (setf (left-fn-of logx)
          #'(lambda ()
              (let ((self logx))
                (when (not (eq selected-x self))
                  (control-done selected-x)
                  (setf selected-x self)
                  (control-start selected-x)
                  (change-variable s :x-function  #'log)
                  (change-variable hist-x :function #'log)))))

    (setf (left-fn-of x-1)
          #'(lambda ()
              (let ((self x-1))
                (when (not (eq selected-x self))
                  (control-done selected-x)
                  (setf selected-x self)
                  (control-start selected-x)
                  (change-variable s :x-function
                                   #'(lambda (x)
                                       (boxcox x -1)))
                  (change-variable hist-x :function
                                   #'(lambda (x)
                                       (boxcox x -1)))))))

    (setf (left-fn-of x-2)
          #'(lambda ()
              (let ((self x-2))
                (when (not (eq selected-x self))
                  (control-done selected-x)
                  (setf selected-x self)
                  (control-start selected-x)
                  (change-variable s :x-function
                                   #'(lambda (x)
                                       (boxcox x -2)))
                  (change-variable hist-x :function
                                   #'(lambda (x)
                                       (boxcox x -2)))))))

    
    (view-layout
     :subviews (list s hist-x hist-y
                     cases
                     x-button-layout y-button-layout
                     )
     :positions
     '((0 10 0 10) (11 16 6 10) (11 16 0 5)
       (17 22 0 10)
       (23 26 5 10) ( 27 30 5 10))
     :draw? draw?
     :viewed-object? data)))

(box-cox-lesson brain-body-wts)

         
        


