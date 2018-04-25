;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               stem.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) Statistical Computing Laboratory
;;;                University of Waterloo
;;;                Canada.
;;;
;;;  Authors:
;;;     J.W.H. Law 1995 University of Waterloo
;;;   
;;;
;;;
;;;----------------------------------------------------------------------------------


;;;(in-package :views)

(in-package :quail)

(eval-when (:compile-toplevel :load-toplevel :execute) (export '(stem stem-view  stem-plot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                               The stem-and-leaf display              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;;; Some data sets

(defconstant tempdata (array '(((53.07 70.2688 70.1) (73.1 84.38 55.3) (78.5 63.5 71.4)) 
                               ((53.4 82.5 67.3) (69.5 73.0 73.1) (73.2 73.2 73.5)) 
                               ((73.6 73.4 73.6) (73.9 73.9 55.7) (85.8 95.4 51.1)) 
                               ((74.4 54.1 77.8) (52.4 69.1 53.5) (64.3 82.7 0.5))
                               ((55.7 70.5 87.5) (50.7 59.5 59.1) (59.3 59.6 59.7))
                               ((59.7 59.4 0.1) (-0.7 -0.7 -0.5) (-0.6 -0.4 -7.8)))
                             :dimensions '(6 3 3)))

(defconstant tempdata1  (array '((3.25 3.54 3.23 3.76 3.12 2.87 2.54 2.45 2.08 2.06 1.02
                                  1.03 1.70 1.80 1.90 0.10 0.20 0.30 0.40 0.13 0.23 0.25
                                  0.44) 
                                 (0.76 0.88 0.01 -.01 -0.07 -.05 -0.10 -.09 -.30 -.20 
                                  -.70 -.69 -.50 -.12 -1.2 -.34 -.44 -.66 -.44 -1.3 -1.7
                                  -2.5 -3.6)) 
                               :dimensions '(2 23)))

(defconstant tempdata2 (array '(1.12 1.13 1.14 1.80 1.11 1.34 1.23 1.72 1.32 1.22 1.33
                                1.21 1.02 1.22 1.19 1.18 1.12 1.24 1.14 1.33 1.32 1.38
                                1.37 1.10 1.01 1.23 1.14 1.31 1.34 1.33 1.37 1.32 1.23
                                1.28 1.28)
                              :dimensions '(1 35)))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun stem_partition (data-list low high)
  (let* ((low-pos (slice-positions-if #'(lambda (x) (< x low)) data-list)) 
         (high-pos (slice-positions-if #'(lambda (x) (> x high)) data-list))
         (mid-pos (slice-positions-if #'(lambda (x) (and (> x low) (< x high)))
                                      data-list))
         (low-list nil)
         (high-list nil)
         (mid-list nil)) 
    (setf low-list (loop for i in low-pos collect
                         (row-major-eref data-list i)))  
    (setf high-list (loop for i in high-pos collect
                          (row-major-eref data-list i)))   
    (setf mid-list (loop for i in mid-pos collect
                         (row-major-eref data-list i)))
    (list low-list mid-list high-list)))

(defun stem_default (unit leafwidth spdata)
  (let* ((range (- (max spdata) (min spdata)))
         (width leafwidth)
         (a nil) (b nil) (u unit))
    (cond ((numberp unit)  
           (setf u u))
          (t
           (if (numberp width)
             (setf width width)
             (setf width 10))  
           (setf a (/ (* range 10) (* 20 width)))  
           (setf u (/ (expt 10 (ceiling (log a 10))) 10))))  
    (cond ((numberp leafwidth)
           (setf width width))
          (t
           (setf b (/ range u 10))
           (cond 
            ((> (* b 2) 20) (setf width 10))
            ((> (* b 5) 20) (setf width 5))
            (t (setf width 2)))))
    (list (float u) width)))

(defun stem_splitlist (dlist unit)
  (let* ((stem1 nil) 
         (leaf1 nil))
    (dolist (num dlist)
      (let* ((sign (signum num))
             (norm (* sign num))
             (multiplier (/ 0.1 unit))
             (num2 (* multiplier norm))
             (stemnorm (truncate num2))
             (leafnorm (- num2 stemnorm))
             (stem (truncate (* sign stemnorm)))
             (leaf (truncate (+ 0.001 (* 10 leafnorm)))))
        (cond 
         ((and (= 0 stem) (= -1 sign))
          (setf stem1 (push -0.01 stem1)))
         ((= 0 stem)
          (setf stem1 (push 0.01 stem1)))
         (T 
          (setf stem (push stem stem1))))
        (setf leaf1 (push leaf leaf1))))
    (list stem1 leaf1)))

(defun stem_pair (stem leaf initial)
  (cond 
   ((null stem) 
    initial)
   (t 
    (stem_pair (cdr stem) (cdr leaf) (cons (list (car stem) (car leaf)) initial)))))

(defun stem_group (initial pair-list)
  (flet ((exam (a i)
           (= a i)))
    (let* ((minimum (caar pair-list))
           (maximum (caar (last pair-list))) 
           (result nil))               
      (do ((i maximum (- i 1)))
          ((< i minimum) initial)      
        (cond
         ((= i 0)
          (setf i 0.01)
          (setf result (delete-if-not #'(lambda (x)
                                          (exam x i)) pair-list :key #'car))
          (if (null result)
            (setf result (cons (list i nil) nil))
            (setf result result))
          (setf initial (cons result initial))
          (setf i -0.01)
          (setf result (delete-if-not #'(lambda (x)
                                          (exam x i)) pair-list :key #'car))
          (if (null result)
            (setf result (cons (list i nil) nil))
            (setf result result))
          (setf initial (cons result initial))
          (setf i 0))
         (t 
          (setf result (delete-if-not #'(lambda (x)
                                          (exam x i)) pair-list :key #'car))
          (if (null result)
            (setf result (cons (list i nil) nil))
            (setf result result))
          (setf initial (cons result initial))))))))

(defun stem_again (group-list leafwidth)
  (let* ((finish nil) 
         (counter nil)
         (flag nil) 
         (part1 nil)
         (part2 nil))
    (cond  
     ((= leafwidth 10)
      (dolist (item group-list)    
        (setf finish (stem_leaf item leafwidth))
        (setf part1 (first finish))     
        (setf part2 (second finish))
        (setf counter (cons (car part1) counter))
        (setf flag (cons (car part2) flag)))
      (list (reverse counter) (reverse flag)))
     ((= leafwidth 5)
      (dolist (item group-list)    
        (setf finish (stem_leaf item leafwidth))   
        (setf part1 (first finish))    
        (setf part2 (second finish))      
        (setf counter (cons (cadr part1) (cons (car part1) counter)))  
        (setf flag (cons (cadr part2) (cons (car part2) flag))))   
      (list (reverse counter) (reverse flag)))
     ((= leafwidth 2)
      (dolist (item group-list)     
        (setf finish (stem_leaf item leafwidth))
        (setf part1 (first finish))   
        (setf part2 (second finish))
        (setf counter 
              (cons (car (cddddr part1))
                    (cons (cadddr part1) 
                          (cons (caddr part1) 
                                (cons (cadr part1)
                                      (cons (car part1) counter))))))          
        (setf flag 
              (cons (car (cddddr part2)) 
                    (cons (cadddr part2) 
                          (cons (caddr part2)  
                                (cons (cadr part2) 
                                      (cons (car part2) flag)))))))
      (list (reverse counter) (reverse flag))))))

(defun stem_leaf (datalist leafwidth)
  (flet ((exam (a i)
           (and (< a i) (>= a (- i leafwidth)))))
    (let* ((initial nil) (count nil)
           (result nil))                   
      (do ((i leafwidth  (+ i leafwidth)))    
          ((> i 10) initial)               
        (cond  
         ((numberp (second (car datalist)))                            
          (setf result (remove-if-not #'(lambda (x) 
                                          (exam x i)) datalist :key #'second))) 
         (t 
          (setf result nil)))  
        (if (null result)
          (setf result (cons (list (caar datalist) " ") nil))
          result)                                                
        (if (stringp (second (car  result)))
          (setf count (cons 0 count))
          (setf count (cons (length result) count)))             
        (setf initial (cons  result initial)))                   
      (if (> (caar datalist) 0)
        (setf initial (reverse initial))
        initial)
      (if (> (caar datalist) 0)
        (setf count (reverse count))
        count)
      (list count initial))))

(defun stem_marker (listlength stopper)
  (let* ((total 0)
         (flag (cons (car listlength) nil)))  
    (do ((i (cdr listlength) (cdr i)))     
        ((>= total stopper) flag) 
      (cond 
       ((= (car i) 0) 
        flag)
       (t 
        (setf total (+ (car flag) (car i)))   
        (setf flag (push total flag)))))))   

(defun stem_depth (newlist data-length)
  (let* ((stop 0) (list1 nil) (list2 nil))
    (if (oddp data-length)   
      (setf stop (/ (+ data-length 1) 2))
      (setf stop (/ data-length 2)))                      
    (setf list1 (stem_marker newlist stop))         
    (setf list2 (stem_marker (reverse newlist) stop))       
    (cond 
     ((oddp data-length)  
      (setf (car list1) (cons (- (first list1) (second list1)) nil)) 
      (list (reverse list1) (cdr list2)))
     ((= (car list1) (car list2)) 
      (setf (car list1) (cons (- (first list1) (second list1)) nil))
      (setf (car list2) (cons (- (first list2) (second list2)) nil))
      (list (reverse list1) list2))
     (t      
      (setf (car list1) (cons (- (first list1) (second list1)) nil))
      (list (reverse list1) (cdr list2))))))

(defun stem_print (output-stream depths lowlist highlist regroup leafwidth)
  (let* ((label nil) (flag (first depths)) (flag1 (cadr depths)) (letter nil))
    (cond 
     ((= leafwidth 5)
      (setf label '("*" ".")))
     ((= leafwidth 2)
      (setf label '("*" "s" "t" "u" ".")))
     (t 
      (setf label '("*"))))         
    (setf letter label)      
    (cond 
     ((= (car flag) 0)
      (setf flag (cdr flag))
      (format output-stream "~%"))
     (t 
      (format output-stream "~%~a~4t low ~,3t |~a" (car flag) lowlist)
      (setf flag (cdr flag))))        
    (dolist (item regroup)                     
      (cond 
       ((numberp (second (car item)))  
        (format output-stream "~%~a~5t" (car flag))
        (cond ((equalp (caar item) 0.01) 
               (format output-stream "+0"))
              ((equalp (caar item) -0.01)
               (format output-stream "-0"))
              (t 
               (format output-stream "~a" (caar item))))
        (format output-stream "~a~,3t |" (car letter))
        (dolist (subitem item)
          (format output-stream "~a" (second subitem)))
        (setf flag (cdr flag))) 
       (t
        (format output-stream "~%~5t")
        (cond 
         ((equalp (caar item) 0.01) 
          (format output-stream "+0"))
         ((equalp (caar item) -0.01)
          (format output-stream "-0"))
         (t
          (format output-stream "~a" (caar item))))
        (format output-stream "~a~,3t |" (car letter))))
      (if (null flag) 
        (setf flag flag1) flag)
      (if (equalp letter (last letter)) 
        (setf letter label) 
        (setf letter (cdr letter))))
    (cond 
     ((null highlist)
      (format output-stream "~%"))
     (t
      (format output-stream "~%~a~3t high ~,3t |~a" 
              (car (last flag1)) highlist)))))

;;;;The main function

(defun stem (data &key (unit nil) (leafwidth nil) (low -infinity) 
                  (high infinity) (output-stream *quail-terminal-io*))
  
  ;;;;The documentation string
  "(:capsule The function stem takes a batch of data values and creates a stem-and-leaf ~
   display.)~
   (:elaboration The stem-and-leaf display enables us to see how wide the range of ~
   values the data cover, where the values are concentrated, and so on.  ~%~
   A column of depths is located to the left of the stem column.  The depth is the ~
   cumulative number of leaves from the nearest end of the batch ~
   up to and including the current stem. ~
   For the stem containing the median, the depth is not uniquely defined so ~
   instead the number of leaves on that stem is recorded ~
   in parentheses.  Naturally, the depths increase from each end ~
   towards the middle of the batch.  
   Also, you can specify the range of the data to be displayed using the keyword arguments ~
   low and high.) ~
   (:required (:arg data The data to be displayed.)) ~
   (:returns A stem-and-leaf display with a depth column.) ~
   (:key (:arg unit 1 Determines the decimal place of the leaf.) ~
   (:arg leafwidth 10 This is the maximal number of different leaf values on each stem. ~
   It must be one of 2, 5, or 10.) ~
   (:arg low -infinity Values less than this are set aside from the main body ~
    of the display.) ~
   (:arg high +infinity Values greater than this are set aside from the main body ~
    of the display.) ~
   (:arg output *quail-terminal-io* A stream capable of receiving output.)
   )"
   
  
  (let* ((data-length (number-of-elements data))
         (sort-data (sort data #'<))
         (part-data (stem_partition sort-data low high))
         (lowlist (first part-data))
         (highlist (third part-data))
         (value (stem_default unit leafwidth (second part-data)))
         (newunit (first value))
         (newwidth (second value)) 
         (split-data (stem_splitlist (reverse (second part-data)) newunit))
         (pair-data (stem_pair (first split-data) (second split-data) nil))
         (group-data (stem_group nil (reverse pair-data)))
         (regroup-data (stem_again group-data newwidth))    
         (new1 (reverse (first regroup-data)))
         (new2 (push (length highlist) new1))
         (new3 (reverse new2))
         (new4 (push (length lowlist) new3))
         (depth-data (stem_depth new4 data-length)))  
    (format output-stream "~%(n= ~a)" data-length)
    (format output-stream "~%Depths      (unit= ~a)~%~%" newunit)
    (stem_print output-stream depth-data lowlist highlist 
                (second regroup-data) newwidth)))

;;;;The end of the program
(defun stem-view (data
                  &rest view-args
                  &key
                  (unit NIL) 
                  (leafwidth NIL)
                  (low -infinity) 
                  (high infinity)
                  (draw? NIL))

  "(:capsule The function stem takes a batch of data values and creates a stem-and-leaf ~
   display.)~
   (:elaboration The stem-and-leaf display enables us to see how wide the range of ~
   values the data cover, where the values are concentrated, and so on.  ~%~
   A column of depths is located to the left of the stem column.  The depth is the ~
   cumulative number of leaves from the nearest end of the batch ~
   up to and including the current stem. ~
   For the stem containing the median, the depth is not uniquely defined so ~
   instead the number of leaves on that stem is recorded ~
   in parentheses.  Naturally, the depths increase from each end ~
   towards the middle of the batch.  
   Also, you can specify the range of the data to be displayed using the keyword arguments ~
   low and high.) ~
   (:required (:arg data The data to be displayed.)) ~
   (:rest (:arg view-args Arguments such as style parameters to be past on to the ~
    view at creation time.) ) ~
   (:returns A stem-and-leaf display with a depth column.) ~
   (:key (:arg unit 1 Determines the decimal place of the leaf.) ~
   (:arg leafwidth 10 This is the maximal number of different leaf values on each stem. ~
   It must be one of  2, 5, or 10.) ~
   (:arg low -infinity Values less than this are set aside from the main body ~
    of the display.) ~
   (:arg high +infinity Values greater than this are set aside from the main body ~
    of the display.) ~
   (:arg draw? NIL Should the stem and leaf plot be drawn in its own view-window?) ~
   )"
  (let ((output (make-string-output-stream))
        result)
    (with-open-stream (s output)
      (stem data
            :unit unit
            :leafwidth leafwidth
            :low low
            :high high
            :output-stream s)
      (setf result (get-output-stream-string s)))
    (format *terminal-io* result)
    (apply #'text-view :text result :draw? draw? view-args)
    )
  )
