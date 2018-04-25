;;; -*- Mode: LISP -*-


(in-package 'Z)

;;;
;;; class definitions
;;;

(defclass Z-string () nil)

(defclass fixnum () nil)

(defclass flonum () nil)

(defclass string-array (ref-array) nil)

(defclass float-array (ref-array) nil)

(defclass boolean-array (ref-array) nil)

(defclass Z-matrix (ref-array) nil)

(defclass Z-scalar (ref-array) nil)

(defclass Z-vector (ref-array) nil)

(defclass boolean-matrix (boolean-array Z-matrix) nil)

(defclass boolean-scalar (boolean-array Z-scalar) nil)

(defclass boolean-vector (boolean-array Z-vector) nil)

(defclass float-matrix (float-array Z-matrix) nil)

(defclass float-scalar (float-array Z-scalar) nil)

(defclass float-vector (float-array Z-vector) nil)

(defclass string-matrix (string-array Z-matrix) nil)

(defclass string-scalar (string-array Z-scalar) nil)

(defclass string-vector (string-array Z-vector) nil)


;;;
;;; method definitions
;;;


(defmethod descriptive-label ((self ref-array))
       

;;; 
;;; Construct a label describing this array for display in a browser
;;; 

       (let ((my-name (if (get-name self)
                          (string (get-name self))
                          ""))
             (my-class (class-name (class-of self)))
             (my-shape (ref-array-dimensions self)))
            (if (typep self 'Z-scalar)
                (format nil "~A ~S" my-name (as-native-object self))
                (format nil "~A ~S ~S" my-name my-class my-shape))))


(defmethod Z-type ((self Z-vector))
       

;;; 

       'vector)


(defmethod mode ((self string-array))
       

;;; 

       'string)


(defmethod as-linearly-adressable-array ((self string-array))
       

;;; 

       (make-array (ref-array-total-size self)
              :element-type t :displaced-to (slot-value self 'the-cml-array)))


(defmethod as-any-string ((self string-array))
       

;;; 

       self)


(defmethod message-not-understood ((self Z-string)
                                   selector message-arguments super-flg)
       

;;; 

     (apply selector (cons self (cdr message-arguments))))


(defmethod as-any-string ((self Z-string))
       

;;; 

       self)


(defmethod Z-type ((self Z-scalar))
       

;;; 

       'scalar)


(defmethod as-native-object ((self Z-scalar))
       

;;; 

       (aref (slot-value self 'the-cml-array)
             0))


(defmethod Z-type ((self Z-matrix))
       

;;; 

       'matrix)


(defmethod message-not-understood ((self flonum)
                                   selector message-arguments super-flg)
       

;;; 

            (apply selector (cons self (cdr message-arguments))))


(defmethod as-any-float ((self flonum))
       

;;; 

       self)


(defmethod na-representation ((self float-array))
       

;;; 
;;; Return the representation for the Not Available value for objects of this
;;; class
;;; 

       (declare (special *ieee.not.available*))
       *ieee.not.available*)


(defmethod mode ((self float-array))
       

;;; 
;;; 

       'float)


(defmethod as-list-of-integers ((self float-array))
       

;;; 

       (let ((na (na-representation self))
             result)
            (dolist (x (as-list self))
                (unless (eql na x)
                    (push (round x)
                          result)))
            (reverse result)))


(defmethod as-linearly-adressable-array ((self float-array))
       

;;; 

       (make-array (ref-array-total-size self)
              :element-type
              'single-float :displaced-to (slot-value self 'the-cml-array)))


(defmethod message-not-understood ((self fixnum)
                                   selector message-arguments super-flg)
       

;;; 

         (apply selector (cons self (cdr message-arguments))))


(defmethod as-any-float ((self fixnum))
       

;;; 

       (float self))


(defmethod as-any-float ((self float-array))
       

;;; 

       self)


(defmethod na-representation ((self boolean-array))
       

;;; 
;;; Return the representation for the Not Available value for objects of this
;;; class
;;; 

       (declare (special *Z.boolean.not.available*))
       *Z.boolean.not.available*)


(defmethod mode ((self boolean-array))
       

;;;  

       'boolean)


(defmethod as-linearly-adressable-array ((self boolean-array))
       

;;; 

       (make-array (ref-array-total-size self)
              :element-type t :displaced-to (slot-value self 'the-cml-array)))


(defmethod as-any-boolean ((self boolean-array))
       

;;; 

       self)


(defmethod Z-type ((self ref-array))
       

;;; 
;;; Returns the type of self
;;; 

       'ref-array)


(defmethod reexpression ((self ref-array)
                         fcn fn-name)
       

;;; 
;;; Returns an array object having the same shape as self and with components
;;; obtained by applying the function fcn to the components of self
;;; 

       (let* ((result (clone self))
              (linear-result (as-linearly-adressable-array result)))
             (dotimes (i (ref-array-total-size result)
                         t)
                 (setf (aref linear-result i)
                       (funcall fcn (aref linear-result i))))
             result))


(defmethod conform-with ((self ref-array)
                         reference)
       

;;; 
;;; if self has higher dimensionality than reference then just return self
;;; else return an object having the same type and shape as reference and with
;;; self aproprietely replicated as its contents.
;;; 

       (let ((ref-shape (ref-array-dimensions reference))
             (self-shape (ref-array-dimensions self))
             (ref-rank (ref-array-rank reference))
             (self-rank (ref-array-rank self)))
            (if (equal self-shape ref-shape)
                self
                (if (> self-rank ref-rank)
                    self
                    (if (< self-rank ref-rank)
                        (conform-with-internal self self-shape self-rank 
                               reference ref-shape ref-rank)
                        (if (> (ref-array-total-size self)
                               (ref-array-total-size reference))
                            self
                            (if (eql (ref-array-total-size self)
                                     1)
                                (conform-with-internal self self-shape 
                                       self-rank reference ref-shape ref-rank)
                                (quail-error "Can't conform ~S with ~S !"
                                       (descriptive-label self)
                                       (descriptive-label reference)))))))))


(defmethod clone ((self ref-array)
                  &optional ignore-contents)
       

;;; 
;;; Returns a new array object shaped like self and, unless ignore-contents is
;;; not NIL with same contents.
;;; 

       (let ((the-new-object (make-instance (class-of self))))
            (if ignore-contents
                (setf (slot-value the-new-object 'the-cml-array)
                      (make-array (ref-array-dimensions self)
                             :element-type
                             (ref-array-element-type self)))
                (setf (slot-value the-new-object 'the-cml-array)
                      (make-array (ref-array-dimensions self)
                             :element-type
                             (ref-array-element-type self)
                             :initial-contents
                             (as-list self))))
            the-new-object))


(defmethod as-list ((self ref-array))
       

;;; 
;;; Returns contents of the-CML-array as a list
;;; 

       (let ((linear-self (as-linearly-adressable-array self))
             result)
            (dotimes (i (ref-array-total-size self))
                (push (aref linear-self i)
                      result))
            (reverse result)))


(defmethod ref-array-total-size ((self ref-array))
       

;;; 
;;; Returns the total number of elements in The-CML-Array calculated as the
;;; product of all dimensions. Note that the total size of a zero-dimensionnal
;;; array is 1.0
;;; 

       (array-total-size (slot-value self 'the-cml-array)))


(defmethod ref-array-rank ((self ref-array))
       

;;; 
;;; Returns the number of dimensionsd (axes) of the-CML-Array to parallel the
;;; indexing range. this is a zero-origin number.
;;; 

       (array-rank (slot-value self 'the-cml-array)))


(defmethod ref-array-element-type ((self ref-array))
       

;;; 
;;; Returns a type specifier for the set of objects that can be stored in
;;; The-CML-Array
;;; 

       (array-element-type (slot-value self 'the-cml-array)))


(defmethod ref-array-dimensions ((self ref-array))
       

;;; 
;;; Returns a list whose elements are the dimensions of The-CML-Array
;;; 

       (array-dimensions (slot-value self 'the-cml-array)))


(defmethod ref-array-dimension ((self ref-array)
                                  axis-number)
       

;;; 
;;; Returns the length of dimension number <axis-number> of the-CML-Array
;;; <axis-number>.
;;; Should be a non negative integer less than rank of The-CML-Array
;;; 

       (array-dimension (slot-value self 'the-cml-array)
              axis-number))


(defmethod na-representation ((self string-array))
       

;;; 
;;; Return the representation for the Not Available value for objects of this
;;; Class
;;; 

       (declare (special *Z.string.not.available*))
       *Z.string.not.available*)


(defmethod pcl::initialize-instance :after ((self float-array)
                                            &key shape init)
       
;;; initializes the array according to the init-list. the elements for the
;;; initialization must coerce with SINGLE-FLOAT

       (let (coerced-init)
            (if (listp init)
                (dolist (item init)
                    (setq coerced-init (append coerced-init
                                              (list (coerce item 'single-float)
                                                    )))))
            (setf (slot-value self 'the-cml-array)
                  (if (null init)
                      (make-array shape :element-type 'single-float)
                      (if (listp init)
                          (make-array shape :element-type 'single-float 
                                 :initial-contents coerced-init)
                          (make-array shape :element-type 'single-float 
                                 :initial-element (coerce init 'single-float))))
                  )
            self))


(defmethod pcl::initialize-instance :after ((self boolean-array)
                                            &key shape init)
       
;;; Initializing a new Boolean Array

       (setf (slot-value self 'the-cml-array)
             (if (null init)
                 (make-array shape :element-type t)
                 (if (symbolp init)
                     (make-array shape :element-type t :initial-element init)
                     (make-array shape :element-type t :initial-contents init))))
       self)


(defmethod pcl::initialize-instance :after ((self string-array)
                                            &key shape init)
       
       (setf (slot-value self 'the-cml-array)
             (if (null init)
                 (make-array shape :element-type t)
                 (if (stringp init)
                     (make-array shape :element-type t :initial-element init)
                     (make-array shape :element-type t :initial-contents init))))
       self)

;;;
;;; function definitions
;;;


(defun apply-binary-op (operation arg-left arg-right &optional mode-of-result)
       

;;; 
;;; Apply operation elementwise over the components of the conformed
;;; ref-array objects arg-left and arg-right returning a new object of mode
;;; Mode-of-result (Or Arg-Left if not supplied)
;;; 

       (let* ((result (make-instance (class-from-mode-type arg-left 
                                            mode-of-result nil)
                             :shape
                             (ref-array-dimensions arg-left)))
              (linear-left (as-linearly-adressable-array arg-left))
              (linear-right (as-linearly-adressable-array arg-right))
              (linear-result (as-linearly-adressable-array result)))
             (dotimes (i (ref-array-total-size result))
                 (setf (aref linear-result i)
                       (funcall operation (aref linear-left i)
                              (aref linear-right i))))
             result))


(defun apply-ternary-op (operation arg-left arg-right arg-far-right &optional 
                               mode-of-result)
       

;;; 
;;; Apply operation elementwise over the components of the conformed
;;; ref-array objects arg-left  arg-right and Arg-far-right returning a new
;;; object of mode Mode-of-result (Or Arg-Left if not supplied)
;;; 

       (let* ((result (make-instance (class-from-mode-type arg-left 
                                            mode-of-result nil)
                             :shape
                             (ref-array-dimensions arg-left)))
              (linear-left (as-linearly-adressable-array arg-left))
              (linear-right (as-linearly-adressable-array arg-right))
              (linear-far-right (as-linearly-adressable-array arg-far-right))
              (linear-result (as-linearly-adressable-array result)))
             (dotimes (i (ref-array-total-size result))
                 (setf (aref linear-result i)
                       (funcall operation (aref linear-left i)
                              (aref linear-right i)
                              (aref linear-far-right i))))
             result))


(defun c (the-list)
       

;;; 
;;; Transform The-list into a ref-array compatible with the data that
;;; contains The-List
;;; 

       (or (listp the-list)
           (setq the-list (list the-list)))
       (let ((common-mode (mode-of-list the-list))
             (common-type (if (= (length the-list)
                                 1)
                              'scalar
                              'vector)))
            
            ;; 
            
            ;; 
            (make-instance (class-from-mode-type nil common-mode common-type)
                   :shape
                   (list (length the-list))
                   :init the-list)))


(defun class-from-mode-type (ref-array-object &optional (mode nil)
                                   (type nil))
       

;;; 
;;; returns the class corresponding to MODE and TYPE taking the default value
;;; of ref-array-OBJECT if not given
;;; 

       (let ((the-mode (if mode
                           mode
                           (mode ref-array-object)))
             (the-type (if type
                           type
                           (Z-type ref-array-object))))
            (intern (concatenate 'string (symbol-name the-mode)
                           "-"
                           (symbol-name the-type))
                   'Z)))


(defun conform-with-internal (self self-shape self-rank reference ref-shape 
                                   ref-rank)
       

;;; 
;;; Called by method CONFORM-WITH when self has smaller rank than reference.
;;; The idea is to replicate self to produce an object  with the same shape as
;;; reference. Presently, only scalar self is handled (in the obvious way) .. 
;;; fill an object shaped like reference with the scalar
;;; 
      
       (declare (ignore ref-rank self-rank self-shape))
       (if (eql (Z-type self)
                'scalar)
           (make-instance (class-of reference)
                  :shape ref-shape :init (as-native-object self))
           (if (eql (ref-array-total-size self)
                    1)
               (make-instance (class-of reference)
                      :shape ref-shape :init (aref (
                                                   as-linearly-adressable-array
                                                    self)
                                                   0))
               (quail-error "Can't conform ~S with ~S" (descriptive-label self)
                      (descriptive-label reference)))))


(defun conforming-binary-op (operation arg-left arg-right &optional 
                                   mode-of-result)
       

;;; 
;;; Conform the ref-array objects Arg-left and Arg-right then apply
;;; operation Elementwise. The Conform-With operation is a bit subtle : If the
;;; self object is not "Smaller" than the argument object, then self is
;;; returned unchanged. So, typically, only one of the two Conform-with calls
;;; bellow does any work 
;;; 

       (apply-binary-op operation (conform-with arg-left arg-right)
              (conform-with arg-right arg-left)
              mode-of-result))


(defun Z-coerce (self as-mode)
       

;;; 
;;; Apply the method As-mode to self if it understands it
;;; 

       (if (understands ($! self t)
                  as-mode)
           (funcall as-mode ($! self t))
           (quail-error "Can't coerce ~S to ~S" (if (understands self 
                                                     'descriptive-label)
                                              (descriptive-label self)
                                              self)
                  as-mode)))