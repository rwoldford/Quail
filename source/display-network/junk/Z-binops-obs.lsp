;;; -*- Mode: LISP -*-

(in-package 'Z)

;;;
;;; method definitions
;;;

(defmethod Z-+ ((arg-left string-array)
                    &optional arg-right)
       

;;; 
;;; String concatenation
;;; 

        (conforming-binary-op #'es.concatenate arg-left (Z-coerce
                                                        arg-right
                                                        'as-any-string)))


(defmethod Z-~= ((arg-left string-array)
                     &optional arg-right)
       

;;; 
;;; String inequality
;;; 

       (conforming-binary-op #'es.~= arg-left (Z-coerce arg-right
                                                     'as-any-string)
              'boolean))


(defmethod Z-> ((arg-left string-array)
                    &optional arg-right)
       

;;; 
;;; String comparison
;;; 

       (conforming-binary-op #'es.> arg-left (Z-coerce arg-right
                                                    'as-any-string)
              'boolean))



(defmethod Z-== ((arg-left string-array)
                     &optional arg-right)
       

;;; 
;;; String equality
;;; 

       (conforming-binary-op #'es.= arg-left (Z-coerce arg-right
                                                    'as-any-string)
              'boolean))



(defmethod Z-< ((arg-left string-array)
                    &optional arg-right)
       

;;; 
;;; String Comparison
;;; 

       (conforming-binary-op #'es.< arg-left (Z-coerce arg-right
                                                    'as-any-string)
              'boolean))



(defmethod Z-~= ((arg-left float-array)
                     &optional arg-right)
       

;;; 
;;; Not Equal predicate
;;; 

       (conforming-binary-op #'er.~= arg-left (Z-coerce arg-right
                                                     'as-any-float)
              'boolean))



(defmethod Z-^ ((arg-left float-array)
                    &optional arg-right)
       

;;; 
;;; Exponentiation
;;; 

       (conforming-binary-op #'er.^ arg-left (Z-coerce arg-right
                                                    'as-any-float)))



(defmethod Z-rem ((arg-left float-array)
                      arg-right)
       

;;; 
;;; Modulus
;;; 

       (conforming-binary-op #'er.rem arg-left (Z-coerce arg-right
                                                      'as-any-float)))



(defmethod Z-mod ((arg-left float-array)
                      &optional arg-right)
       

;;; 
;;; Integer divide
;;; 

       (conforming-binary-op #'er.mod arg-left (Z-coerce arg-right
                                                      'as-any-float)))



(defmethod Z-> ((arg-left float-array)
                    &optional arg-right)
       

;;; 
;;; Greater than predicate
;;; 

       (conforming-binary-op #'er.> arg-left (Z-coerce arg-right
                                                    'as-any-float)
              'boolean))



(defmethod Z-== ((arg-left float-array)
                     &optional arg-right)
       

;;; 
;;; Equality
;;; 

       (conforming-binary-op #'er.= arg-left (Z-coerce arg-right
                                                    'as-any-float)
              'boolean))



(defmethod Z-< ((arg-left float-array)
                    &optional arg-right)
       

;;; 
;;; Less than predicate
;;; 

       (conforming-binary-op #'er.< arg-left (Z-coerce arg-right
                                                    'as-any-float)
              'boolean))



(defmethod Z-/ ((arg-left float-array)
                    &optional arg-right)
       

;;; 
;;; Division
;;; 

       (conforming-binary-op #'er./ arg-left (Z-coerce arg-right
                                                    'as-any-float)))



(defmethod Z-- ((arg-left float-array)
                    &optional arg-right)
       

;;; 
;;; Substraction
;;; 

       (conforming-binary-op #'er.- arg-left (Z-coerce arg-right
                                                    'as-any-float)))



(defmethod Z-+ ((arg-left float-array)
                    &optional arg-right)
       

;;; 
;;; Addition
;;; 

       (conforming-binary-op #'er.+ arg-left (Z-coerce arg-right
                                                    'as-any-float)))



(defmethod Z-* ((arg-left float-array)
                    &optional arg-right)
       

;;; 
;;; Multiplication
;;; 

       (conforming-binary-op #'er.* arg-left (Z-coerce arg-right
                                                    'as-any-float)))



(defmethod Z-xor ((arg-left boolean-array)
                      &optional arg-right)
       

;;; 
;;; Logical exclusif OR
;;; 

       (conforming-binary-op #'eb.xor arg-left (Z-coerce arg-right
                                                      'as-any-boolean)))



(defmethod Z-or ((arg-left boolean-array)
                     &optional arg-right)
       

;;; 
;;; Logical OR
;;; 

       (conforming-binary-op #'eb.or arg-left (Z-coerce arg-right
                                                     'as-any-boolean)))



(defmethod Z-and ((arg-left boolean-array)
                      &optional arg-right)
       

;;; 
;;; Logical AND
;;; 

       (conforming-binary-op #'eb.and arg-left (Z-coerce arg-right
                                                      'as-any-boolean)))