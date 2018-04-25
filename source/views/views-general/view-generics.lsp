;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               view-generics.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Views system, a toolkit for 
;;;  interactive statistical graphics.
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




(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(view-mixin 
           add-mixin-to-view remove-mixin-from-view
           view bounding-region-of 
           viewed-object-of 
           subviews-of sub-view-locns-of visible-subviews-p viewports-of
           select-sub-view-locn construct-sub-views 
           place-subview set-bounding-region compute-bounding-region 
           init-position-subviews
           map-to-viewport map-subviews-to-viewport subviews-of-type
           compute-sub-viewports 
           draw-view erase-view remove-view remove-subview
           invert-view highlight-view downlight-view selected-p
           reshape-viewport distance-to-location which-subview 
           remap-to-viewports remap-sub-to-viewports
           move-subview add-subview move-view copy-image draw-viewport 
           reposition-view describe-view 
           describe-viewed-object short-description description-string 
           inspect-view inspect-viewed-object
           change-bounding-region clip&draw print-viewed-object
           select-view deselect-view deselect-all toggle-select-view  
           set-aspect-ratio list-subviews
            set-view-font smallest-bounding-region
           reshape-sub-viewports constrain-bounds subview-position-region
           use-x-axis-p use-y-axis-p sub-views-of subview-locns-of 
           view-title-string
           )))

(defclass view-mixin (single-draw-style-mixin button-mixin linkable-bounds-mixin)
  ()
  (:documentation "This class gathers together mixins for the basic view."))



(defun add-mixin-to-view (super)
  "Add super to the front of the supers list of the ~
   class view-mixin.  The value of the argument super ~
   must be a symbol naming the class to be added."
  (let (supers)
    (setf supers
          (cons super
                (loop
                  for s in (qk::class-direct-superclasses (find-class 'view-mixin))
                  collect (class-name s))
                      ))
    (eval
     `(defclass view-mixin ,supers
        ()
        (:documentation
         "A class which gathers the many different mixins which together ~
          define a view-mixin.  It is the sole super class of view.")))
    )
  )
        
    
(defun remove-mixin-from-view (super)
  "Removes super from the supers list of the ~
   class view-mixin.  The value of the argument super ~
   must be a symbol naming the class to be removed."
  (let (supers)
    (setf supers
          (remove super
                (loop
                  for s in (qk::class-direct-superclasses (find-class 'view-mixin))
                  collect (class-name s))
                      ))
    (eval
     `(defclass view-mixin ,supers
        ()
        (:documentation
         "A class which gathers the many different mixins which together ~
          define a view-mixin.  It is the sole super class of view.")))
    )
  )

(defclass view (view-mixin)
  
  ((bounding-region 
    :initarg :bounding-region
    :initform nil 
    :accessor bounding-region-of 
    :documentation "A region in the local coordinates of view used to define its bounds")
   
   (viewed-object 
    :initarg :data :initarg :viewed-object
    :initform nil 
    :accessor viewed-object-of 
    :documentation "Object being viewed")
  
   (maps-to-viewports 
    :initform nil
    :accessor maps-to-viewports-of 
    :documentation "A list of viewing transforms, where the ith transform maps the local coordinates of VIEW to those of its ith VIEWPORT")
   
   (parent-viewports 
    :initform nil 
    :accessor parent-viewports-of
    :documentation "The VIEWPORTs of parent views of self")
   
   (viewports 
    :initform nil 
    :accessor viewports-of
    :documentation "The ith VIEWPORT defines the extent of VIEW in the ith  
    PARENT-VIEWPORT, i.e. it is obtained by mapping the BOUNDING-REGION of 
    VIEW onto VIEWPORT"))
  
  (:documentation "The basic view")
  (:default-initargs :initform-fn nil))


(defgeneric set-bounding-region (view &key region pretty? 
                                ignore-x? ignore-y? &allow-other-keys)
  (:documentation " Sets bounding-region to region , if non-nil, ~
                   or (compute-bounding-region self). ~
                  If pretty? is non-nil the view may expand the ~
                  bounding region to cover a 'pretty' region ~
                  If ignore-x? is non-nil the x bounds view remain unchanged, ~
                  similarly ignore-y?"))

(defgeneric compute-bounding-region (view )
  (:documentation "Computes the bounding region of view"))

(defgeneric construct-sub-views (view &key &allow-other-keys)
  (:documentation "Constructs sub-views of view"))

(defgeneric place-subview (view subview locn )
  (:documentation " Places subview at region locn in self. ~
                  Adds subview to self if not already present."))
                          
  
(defgeneric map-to-viewport (view &optional viewport)
  (:documentation "Maps self to viewport. ~
                  Viewport may be a new viewport for self. ~
                  If viewport is nil, map self to all viewports. "))

(defgeneric map-subviews-to-viewport (view &optional viewport subviews)
  (:documentation "Maps subviews  to viewport. ~
                  If subviews is nil, map all subviews of self. ~
                  If viewport is nil, map subviews to all viewports. "))

;;;----------------------------------------------------------------------------------

(defgeneric draw-view  (view &key viewport  &allow-other-keys)
  (:documentation "Draw the view. ~
                   If viewport is nil draw in all viewports"))

(defgeneric erase-view (view &key viewport &allow-other-keys)
  (:documentation "Erase the view ~
                   If viewport is nil erase in all viewports"))


(defgeneric remove-view (view &key viewport  &allow-other-keys)
  (:documentation "Removes the view (permanently) from viewport ~
                  If  viewport  is not provided, view is removed everywhere ~
                  Surgery is performed on the view hierarchy."))


(defgeneric remove-subview (view subview )
  (:documentation " Removes the subview (permanently) from self ~
                  redrawing self everywhere ~
                  Surgery is performed on the view hierarchy "))

(defgeneric invert-view (view &key viewport  &allow-other-keys)
  (:documentation "invert the view. ~
                   If viewport is nil invert in all viewports"))
                       
(defgeneric highlight-view (view &key viewport &allow-other-keys)
  (:documentation "highlight the view. ~
                   If viewport is nil highlight in all viewports"))

(defgeneric downlight-view (view &key viewport &allow-other-keys)
  (:documentation "downlight the view. ~
                   If viewport is nil downlight in all viewports"))
                           

(defgeneric selected-p (view viewport location)
   (:documentation "Is view selected with mouse at location? ~
                   default looks to see if location is in viewport "))


(defgeneric reshape-viewport (view viewport  
                             &key new-location transform draw?
                             &allow-other-keys)
   (:documentation "only one of transform or new-location should be provided"))

(defgeneric distance-to-location (view viewport location)
  (:documentation "Computes a distance from viewport of view to location")) 

(defgeneric which-subview (view viewport location)
   (:documentation "Returns the view, its viewport its parent and the ~
                   parent's viewport at location as multiple values. "))
   

(defgeneric remap-to-viewports (view &key erase? &allow-other-keys)
   (:documentation "Recomputes viewports and redraws"))
  

(defgeneric remap-sub-to-viewports (view subview &key erase? &allow-other-keys)
  (:documentation "Recomputes viewports of subviews and redraws"))

(defgeneric move-subview (view subview new-region)
   (:documentation " Moves subview to new-region (in local coords of self). ~
                   New-region must be contained in the bounding-region of self"))
   

(defgeneric add-subview (view subview new-region)
  (:documentation "Adds subview to self at position new-region (in coords of self).~
   Subview must be a legal subview of view to be added.~
   If subview is already in self, it is moved to new-region."))

(defgeneric move-view (view &key viewport  new-location &allow-other-keys)
  (:documentation "Moves self to new-location (in coords of viewport).~
                  New-location must be contained in the viewport of self's parent.~
                  Moves self within the viewport of parent."))  


(defgeneric copy-image (view  &key viewport new-location &allow-other-keys)
   (:documentation "Copies self to new-location (in screen coords).~
   If new-location is contained in the viewport ~
   of self's parent, this is the same as move-view."))

(defgeneric reposition-view (view &key default-positions &allow-other-keys)
  (:documentation "Performs initial-layout."))

(defgeneric describe-view (view ))

(defgeneric describe-viewed-object (view ))

(defgeneric short-description (view ))

(defgeneric description-string (view ))

(defgeneric inspect-view (view))

(defgeneric inspect-viewed-object (view))

(defgeneric print-viewed-object (view))

(defgeneric change-bounding-region (view region &key 
                             &allow-other-keys)
   (:documentation "Sets bounding-region of self to region.~
                   and redraws everywhere.~
                   If ignore-x? is non-nil the x bounds view remain unchanged, ~
                   similarly ignore-y?."))


(defgeneric select-view (view)
  (:documentation "Adds to *selected-views*"))

(defgeneric deselect-view (view)
  (:documentation "Removes from to *selected-views*"))

(defgeneric toggle-select-view (view))

(defgeneric set-aspect-ratio (view &key viewport ratio draw?
                                    &allow-other-keys)
  (:documentation "Sets the width/height ratio of viewport to ratio"))
                             

(defgeneric list-subviews (view &key test
                                &allow-other-keys)
   (:documentation "Lists subviews satisfying predicate test.~
                   Test could be a list, where entry i corresponds to subview i.~
                   Subviews with non-null test values are returned."))
  
(defgeneric init-position-subviews (view &key &allow-other-keys))





(defgeneric set-view-font (label &key name size style &allow-other-keys)
  (:documentation "Sets the font of the label to use name, size and style.~
                   Null values are ignored.~
                   (:see-also~
                   (wb:canvas-font-names :function)~
                   (wb:canvas-font-styles :function)~
                   (wb:canvas-make-font :function))~
                   "))


(defgeneric constrain-bounds (view  &key draw?  &allow-other-keys)
  (:documentation "Constrains the bounds of subviews of view.~
                   Redraws when draw? is non-nil"))



(defgeneric subview-position-region (view )
  (:documentation "Region in which subviews are positioned.~
                   Default is bounding region"))


(defgeneric use-x-axis-p (view)
  (:documentation "Does it make sense for this view to have an x axis?"))


(defgeneric use-y-axis-p (view)
  (:documentation "Does it make sense for this view to have a y axis?"))

(defgeneric view-title-string  (view)
  (:documentation "Returns a string used for a title"))
