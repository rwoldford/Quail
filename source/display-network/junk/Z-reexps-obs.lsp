;;; -*- Mode: LISP -*-

(in-package 'Z)

;;;
;;; method definitions
;;;

(defmethod Z-cos ((self float-array))
       

;;; 
;;; Cosine
;;; 

       (reexpression self #'er.cos 'cos))



(defmethod Z-ceiling ((self float-array))
       

;;; 
;;; Least integer greater than or equal to  argument
;;; 

       (reexpression self #'er.ceiling 'ceiling))



(defmethod Z-atan ((self float-array))
       

;;; 
;;; Arc-tangent
;;; 

       (reexpression self #'er.atan))



(defmethod Z-asin ((self float-array))
       

;;; 
;;; Arc-sine
;;; 

       (reexpression self #'er.asin 'asin))



(defmethod Z-unary-minus ((self float-array))
       

;;; 
;;; -1 times the argument
;;; 

       (reexpression self #'er.unary-minus 'unary-minus))



(defmethod Z-trunc ((self float-array))
       

;;; 
;;; The integer part of the argument
;;; 

       (reexpression self #'er.trunc 'trunc))



(defmethod quail-sqrt ((self float-array))
       

;;; 
;;; Square root
;;; 

       (reexpression self #'er.sqrt 'sqrt))



(defmethod Z-sin ((self float-array))
       

;;; 
;;; Sine
;;; 

       (reexpression self #'er.sin 'sin))



(defmethod Z-log10 ((self float-array))
       

;;; 
;;; Base 10 logarithm
;;; 

       (reexpression self #'er.log10 'log10))



(defmethod Z-log ((self float-array))
       

;;; 
;;; Natural logarithm
;;; 

       (reexpression self #'er.log 'log))



(defmethod Z-lgamma ((self float-array))
       

;;; 
;;; The natural log of the Gamma function of the argument
;;; 

       (reexpression self #'er.lgamma 'lgamma))



(defmethod Z-gamma ((self float-array))
       

;;; 
;;; The Gamma function applied to the arguments
;;; 

       (reexpression self #'gamma 'gamma))



(defmethod Z-floor ((self float-array))
       

;;; 
;;; Greater integer less than or equal to the argument.
;;; 

       (reexpression self #'er.floor 'floor))



(defmethod Z-exp ((self float-array))
       

;;; 
;;; Exponentiate the arguments
;;; 

       (reexpression self #'er.exp 'exp))



(defmethod Z-acos ((self float-array))
       

;;; 
;;; Arc-Cosine
;;; 

       (reexpression self #'er.acos 'acos))



(defmethod Z-abs ((self float-array))
       

;;; 
;;; Absolute value
;;; 

       (reexpression self #'er.abs 'abs))



(defmethod Z-! ((self boolean-array))
       

;;; 
;;; Logical NOT
;;; 

       (reexpression self #'eb.not '!))