;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               color.lsp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  This file is part of the Window-Basics package intended to provide a uniform
;;;  window environment for Common Lisp programming.
;;;  
;;;  For copyright information and history, see window-basics-copyright.lisp
;;;
;;;  Authors:
;;;     H.A. Chipman  1991
;;;     C.B. Hurley   1989-1991
;;;     J.A. McDonald 1988-89
;;;     R.W. Oldford  1989-1991
;;;     J.O. Pedersen 1988-89
;;;     N. Wiebe      1998
;;;     
;;;
;;;
;;;----------------------------------------------------------------------------------


(in-package :wb)

(eval-when (:compile-toplevel :load-toplevel :execute) (export  '(*light-grey-color* *grey-color* *dark-grey-color* *cyan-colour*
           *black-colour* *white-colour* *pink-colour* *red-colour* 
           *orange-colour* *yellow-colour* *green-colour* *dark-green-colour*
           *light-blue-colour* *blue-colour* *purple-colour* *brown-colour*
           *tan-colour* *light-gray-colour* *gray-colour* *dark-gray-colour*
           *light-grey-colour* *grey-colour* *dark-grey-colour*
           *bright-green-color* *magenta-color*
           *bright-green-colour* *magenta-colour*
           *colors*
           *bright-white-color*
           *default-canvas-background-color* *default-canvas-pen-color*
           shade-to-color color-to-shade
           optimize-color-record deoptimize-color-record
           rgb_to_lhs lhs_to_rgb
           hue-of lightness-of saturation-of
           lighten darken)))
;;;
;;; The following are typically shadowed from the host system
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
 (export '(*black-color* *white-color* *pink-color* *red-color*
          *orange-color* *yellow-color* *green-color* *dark-green-color*
          *light-blue-color* *blue-color* *purple-color* *brown-color*
          *tan-color* *light-gray-color* *gray-color* *dark-gray-color*)
        ))

;;;========================================================================
;;;                             Colors/Colours
;;;
;;;========================================================================
;;;
;;;  These follow a red green blue (RGB) model for representing colour.
;;;
;;;


(defparameter   *black-color*
  (make-color 0.0 0.0 0.0))
 
(defparameter   *white-color*
  (make-color 1.0 1.0 1.0))
 
(defparameter   *pink-color*
  (make-color 0.9490196078431372 0.03137254901960784 0.5176470588235295))
 
(defparameter *red-color*
  (make-color 0.8666666666666667 0.03137254901960784 0.023529411764705882))
(defparameter *orange-color*
  (make-color 1.0 0.39215686274509803 0.00784313725490196))
 
(defparameter *yellow-color*
  (make-color 0.9882352941176471 0.9529411764705882 0.0196078431372549))
 
(defparameter *green-color*
  (make-color 0.12156862745098039 0.7176470588235294 0.0784313725490196))
 
(defparameter *dark-green-color*
  (make-color 0.0 0.39215686274509803 0.06666666666666667))
 
(defparameter *light-blue-color*
  (make-color 0.00784313725490196 0.6705882352941176 0.9176470588235294))
 
(defparameter *blue-color*
  (make-color 0.0 0.0 0.8313725490196079))
 
(defparameter wb::*cyan-color*
  (wb::make-color 0.0 1.0 1.0))
 
(defparameter *purple-color*
  (make-color 0.27450980392156865 0.0 0.6470588235294118))
 
(defparameter *brown-color*
  (make-color 0.33725490196078434 0.17254901960784313 0.0196078431372549))
 
(defparameter *tan-color*
  (make-color 0.5647058823529412 0.44313725490196076 0.22745098039215686))
 
(defparameter *light-gray-color*
  (make-color 0.7529411764705882 0.7529411764705882 0.7529411764705882))
 
(defparameter *gray-color*
  (make-color 0.5019607843137255 0.5019607843137255 0.5019607843137255))
 
(defparameter *dark-gray-color*
  (make-color 0.25098039215686274 0.25098039215686274 0.25098039215686274))
 

;;;
;;; Accounting for different spellings of gray

(defparameter *grey-color* *gray-color*)
(defparameter *light-grey-color* *light-gray-color*)
(defparameter *dark-grey-color* *dark-gray-color*)

;;;
;;; Accounting for different spellings of colour


(defparameter  *black-colour*         *black-color*)
(defparameter  *white-colour*         *white-color*)
(defparameter  *pink-colour*          *pink-color*)
(defparameter  *red-colour*           *red-color*)
(defparameter  *orange-colour*        *orange-color*)
(defparameter  *yellow-colour*        *yellow-color*)
(defparameter  *green-colour*         *green-color*)
(defparameter  *dark-green-colour*    *dark-green-color*)
(defparameter  *light-blue-colour*    *light-blue-color*)
(defparameter  *blue-colour*          *blue-color*)
(defparameter  *cyan-colour*          *cyan-color*)
(defparameter  *purple-colour*        *purple-color*)
(defparameter  *brown-colour*         *brown-color*)
(defparameter  *tan-colour*           *tan-color*)
(defparameter  *light-gray-colour*    *light-gray-color*)
(defparameter  *gray-colour*          *gray-color*)
(defparameter  *dark-gray-colour*     *dark-gray-color*)
 
;(defparameter  *grey-colour*          *gray-color* ;;DUPLICATE of line 128 01SEP2021
;(defparameter  *light-grey-colour*    *light-gray-color* ) ;;DUPLICATE of line 127 01SEP2021
;(defparameter  *dark-grey-colour*     *dark-gray-color*) ;;DUPLICATE of line 129 01SEO2021

(defparameter  *bright-green-color*   (make-color 0 1.0 0))
(defparameter  *bright-green-colour*  *bright-green-color*)
(defparameter  *magenta-color*        (make-color 1.0 0 1.0))
(defparameter  *magenta-colour*       *magenta-color*)
 



(defvar *colors*
  (list *white-color* *pink-color* *red-color*
        *orange-color* *yellow-color* *green-color* *cyan-color* *blue-color*
        *purple-color* *brown-color* *tan-color* *gray-color*
        *black-color* *dark-green-color* *light-gray-color* *light-blue-color*
        *dark-gray-color*)
  "The list of predefined colors -- useful as an iteration sequence.")


(defparameter  *bright-white-color*   (make-color 1.0 1.0 1.0))

;;;
;;;  Some default canvas colours.
;;;

(defvar *default-canvas-background-color*
  *white-color*
  "The default colour for the background of a canvas. ~
   (:see-also *default-canvas-pen-color*)"
  )

(defvar *default-canvas-pen-color*
  *black-color*
  "The default colour for the pen of a canvas. ~
   (:see-also *default-canvas-background-color*)"
  )

;;;
;;; some utility functions
;;;

(defun shade-to-color (shade)
  "Translates a shade to a color."
  (declare (special *black-shade* *white-shade* *light-gray-shade*
                    *dark-gray-shade* *gray-shade*
                    *black-color* *white-color* *light-gray-color*
                    *dark-gray-color* *gray-color*))
  (cond ((eq *black-shade* shade)       *black-color*)
        ((eq *white-shade* shade)       *white-color*)
        ((eq *light-gray-shade* shade)  *light-gray-color*)
        ((eq *gray-shade* shade)        *gray-color*)
        ((eq *dark-gray-shade* shade)   *dark-gray-color*)
        (t                              *gray-color*)))

(defun color-to-shade (color)
  "Translates a color into a shade."
  (declare (special *black-shade* *white-shade* *light-gray-shade*
                    *dark-gray-shade*  *gray-shade*
                    *black-color* *white-color* *light-gray-color*
                    *dark-gray-color*))
  (cond ((eq-colors *black-color* color)       *black-shade*)
        ((eq-colors *white-color* color)       *white-shade*)
        ((eq-colors *light-gray-color* color)  *light-gray-shade*)
        ((eq-colors *gray-color* color)        *gray-shade*)
        ((eq-colors *dark-gray-color* color)   *dark-gray-shade*)
        (t                                     *gray-shade*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Transformative Colour Functions
;;;

;;; algorithms extracted from "Color Theory and Modeling for Computer Graphics, ;;; Visualization, and Multimedia Applications" by Haim Levkowitz, 1997, p 66 & 68.

;; values that are a little over 1? truncate

;;; compute rgb from lhs
(defun lhs_to_rgb (l h s &key (model :triangle))   ;;;lightness hue saturation
  (if (and (>= l 0) (<= l 1))
    (if (or (null h) (and (>= h 0) (< h 360))) ;;correction here
      (if (and (>= s 0) (<= s 1))
        (let (r g b)
          (if (or (zerop s) (not h)) ;;or h = nil
            ;;;achromatic
            (setq r l g l b l)        
            ;;;chromatic
            (let* ((M 1)
                   
                   (k (floor h 60)) ;sector-number
                   (f (- (/ h 60) k)) ;;hue within sector
                   (fprime (if (oddp k) (- 1 f) f))
                   latq min mid max wmin wmid wmax)
              
              (cond ((equal model :triangle) (setq wmin (/ 1 3) wmid (/ 1 3) wmax (/ 1 3))) 
                    ((equal model :hexcone) (setq wmin 0 wmid 0 wmax 1)) 
                    ((equal model :double-hexcone) (setq wmin (/ 1 2) wmid 0 wmax (/ 1 2))) 
                    (T (error "Don't know this model ~s ." model)))                  
              
              (setf latq (* (+ (* wmid fprime) wmax) M))
              
              (if (<= l latq)
                (progn (setf min (* (- 1 s) l))
                       (setf mid (/ (+ (* fprime l) (* min (- (* (- 1 fprime) wmax) (* fprime wmin))))
                                    (+ wmax (* fprime wmid))))
                       (setf max (/ (- l (* wmid mid) (* wmin min)) wmax)))
                (progn (setf max (+ (* s M) (* (- 1 s) l)))
                       (setf mid (/ (- (* (- 1 fprime) l) (* max (- (* (- 1 fprime) wmax) (* fprime wmin))))
                                    (+ (* (- 1 fprime) wmid) wmin)))
                       (setf min (if (> wmin 0)
                                   (/ (- l (* wmax max) (* wmid mid)) wmin)
                                   (/ (- mid (* fprime max)) (- 1 fprime))))))
              (case k
                (0 (setq r max g mid b min))
                (1 (setq r mid g max b min))
                (2 (setq r min g max b mid))
                (3 (setq r min g mid b max))
                (4 (setq r mid g min b max))
                (5 (setq r max g min b mid)))))
          (list r g b))
        (error "Saturation values range from 0 to 1: ~s " s))
      (error "Hue values range from 0 up to, but not including 360; ~%~
                    NIL for chromatic colours: ~s " h))
    (error "Lightness values range from 0 to 1: ~s " l)))
           
;;; compute lhs from rgb
;;; should be able to take just a colour as well
(defun rgb_to_lhs (r g b &key (model :triangle))  ;;;red green blue
  (let* ((cmax (max r g b))
         (cmin (min r g b))
         (cmid (elt (sort (list r g b) #'<=) 1))
         l h s)
    (if (equal cmax cmin)
      (setq l cmax h nil s 0)   ;;;achromatic
      (let* ((M 1)                     ;;;chromatic
             (k (cond
                 ((and (> r g) (>= g b)) 0)
                 ((and (>= g r) (> r b)) 1)
                 ((and (> g b) (>= b r)) 2)
                 ((and (>= b g) (> g r)) 3)
                 ((and (> b r) (>= r g)) 4)
                 ((and (>= r b) (> b g)) 5)))
             (f (if (evenp k)
                  (/ (- cmid cmin) (- cmax cmin))
                  (/ (- cmax cmid) (- cmax cmin))))
             wmin wmid wmax latq)
        (cond ((equal model :triangle) (setq wmin (/ 1 3) wmid (/ 1 3) wmax (/ 1 3))) 
              ((equal model :hexcone) (setq wmin 0 wmid 0 wmax 1)) 
              ((equal model :double-hexcone) (setq wmin (/ 1 2) wmid 0 wmax (/ 1 2))) 
              (T (error "Don't know this model ~s ." model)))                  
        (setf latq (* (+ (* wmid f) wmax) M))
        (progn (setf l (+ (* wmax cmax) (* wmid cmid) (* wmin cmin)))
               (setf h (* (+ k f) 60))
               (setf s (if (<= l latq)
                         (/ (- l cmin) l)
                         (/ (- cmax l) (- M l)))))))
    
    (list l h s)))  ;;a list or three separate parameters

(defun hue-of (colour &key (model :triangle))
    (second (rgb_to_lhs (red-of colour) (green-of colour)
                        (blue-of colour) :model model)))

(defun lightness-of (colour &key (model :triangle))
    (first (rgb_to_lhs (red-of colour) (green-of colour)
                        (blue-of colour) :model model)))

(defun saturation-of (colour &key (model :triangle))
    (third (rgb_to_lhs (red-of colour) (green-of colour)
                        (blue-of colour) :model model)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Shading Colour Functions
;;;
(defun lighten (colour &key (factor .2) (model :triangle))
  ;;factor is additive
  (let* ((h (wb::hue-of colour))
         (l (wb::lightness-of colour))
         (s (wb::saturation-of colour))
         (new-l (+ factor l))
         )
    
    (cond ((< new-l 0)
           (setf new-l 0))
          ((> new-l 1)
           (setf new-l 1)))
    
    (apply #'wb::make-color (wb::lhs_to_rgb new-l h s :model model))))

(defun darken (colour &key (factor .2) (model :triangle))
  (lighten colour :factor (* -1 factor) :model model))

#|
;alternative lighten function
(defun lighten (colour &optional (factor .2))
  ;; factor is additive
  (let* ((r (wb::red-of colour))
         (g (wb::green-of colour))
         (b (wb::blue-of colour))
         (zeror (zerop r))
         (zerog (zerop g))
         (zerob (zerop b))
         maxfactor)
    
    (if (and zeror zerog zerob)  ;;if black
      (if (> factor 0)       ;;then lighten
        (progn (if (<= factor 1)
                 (wb::make-color (+ r factor) (+ g factor) (+ b factor))
                 wb::*white-color*))
        wb::*black-color*)
      
      (if (and (<= (+ factor (max r g b)) 1)      ;;within bounds
               (> (+ factor (min (if zeror 1 r)   ;;nonzero minimum
                                 (if zerog  1 g)
                                 (if zerob 1 b))) 0))
        (wb::make-color (if zeror 0 (+ r factor))
                        (if zerog 0 (+ g factor))
                        (if zerob 0 (+ b factor)))

        ;;otherwise modify size of factor
        (cond ((> factor 0)
               (setf maxfactor (min (- 1 r) (- 1 g) (- 1 b)))
               (wb::make-color (if zeror 0 (+ r maxfactor))
                               (if zerog 0 (+ g maxfactor)) 
                               (if zerob 0 (+ b maxfactor))))
              (T
               (setf maxfactor (min (if zeror 1 r)
                                    (if zerog 1 g)
                                    (if zerob 1 b)))
               (wb::make-color (if zeror 0 (- r maxfactor))
                               (if zerog 0 (- g maxfactor))
                               (if zerob 0 (- b maxfactor)))))))))



(defun darken (colour &optional (factor .2))
  (lighten colour (* -1 factor)))
|#
