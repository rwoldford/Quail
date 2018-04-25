;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                      Extending the documentation system.                              
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1993 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     M.E. Lewis 1993.
;;;     R.W. Oldford 1994.
;;;
;;;
;;;  
;;; EXTENDING THE DOCUMENTATION SYSTEM 
;;; 
;;; The documentation system can itself be extended to handle new keywords.
;;; It can also be extended to handle new documentation objects.
;;; For the latter see the file "eg:Documentation;new-doc-object.lsp".
;;; Here we present only the means for adding new doc keywords.
;;;
;;; NOTE: THE INFORMATION BELOW IS INCOMPLETE WITHOUT ACCESS TO THE
;;;       DOCUMENTATION SOURCE CODE.
;;; 
;;; There are three steps to adding a new documentation keyword. 
;;; In the :quail-kernel package:
;;;  
;;;    1. Add your keyword to the special variable *doc-keywords* 
;;;    2. Add an entry to the hashtable called *node-value-formatter* 
;;;    3. Create a function to access the information. 
;;;  
;;; These three steps are described below.  
;;;  
;;; Beforehand, it is helpful to  
;;; know that there is a function in the :quail-kernel package  
;;; called 'interpret-quail-documentation' which actually reads 
;;; and interprets the documentation string.  The value returned 
;;; by 'interpret-quail-documentation' is an object of class 'key-tree'. 
;;; A 'key-tree' contains formatted information from the 
;;; documentation string.  The information is stored on the 'key-tree' 
;;; in the form of 'nodes', where each 'node' corresponds to one 
;;; of the documentation keywords encountered in the documentation 
;;; string of a Lisp object. 
;;;  
;;; STEP #1. Change the value of *doc-keywords*. 
;;;  
;;; Let's say that you want to add a new documentation keyword ":author". 
;;;  
;;; The special variable *doc-keywords* is arranged in a tree-list: 
;;;  
;;;                          ":root" 
;;;                             | 
;;;      --------------------------------------- 
;;;      |            |                        | 
;;; ":capsule" ":elaboration"  . . . . . . ":body" 
;;;                                            | 
;;;                                        ":arg" 
;;;  
;;; Note that the keywords are strings beginning with a colon. 
;;; Upper or lowercase does not matter.   
;;;  
;;; The list *doc-keywords* mimics this tree structure exactly: 
;;;  
;;; > *doc-keywords* 
;;;  
;;; > (":root" ":capsule" ":elaboration" (":examples" ":files" ":text") 
;;;    ":references" ":see-also" ":super-topics" ":topics" ":sub-topics" 
;;;    ":returns" ":side-effects" ":package" ":name" (":required" ":arg") 
;;;    (":optional" ":arg") (":rest" ":arg") (":key" ":arg") (":aux" ":arg") 
;;;    (":body" ":arg")) 
;;;  
;;; It should be pretty clear how to add the keyword ":author".  If 
;;; you want it to be keyword in its own right, then add it in place 
;;; right under ":root": 
;;;  
;;;   (":root" ":capsule" ":elaboration" (":examples" ":files" ":text") 
;;;    ":references" ":see-also" ":super-topics" ":topics" ":sub-topics" 
;;;    ":returns" ":side-effects" ":package" ":name" (":required" ":arg") 
;;;    (":optional" ":arg") (":rest" ":arg") (":key" ":arg") (":aux" ":arg") 
;;;    (":body" ":arg") ":author") 
;;;                     ^^^^^^^^^ 
;;;  
;;; If you want it to be a sub-key, say under the ":elaboration" keyword, then 
;;; add it there: 
;;;  
;;;   (":root" ":capsule" (":elaboration" ":author") (":examples" ":files" ":text") 
;;;                       ^^^^^^^^^^^^^^^^^^^^^^^^^^ 
;;;    ":references" ":see-also" ":super-topics" ":topics" ":sub-topics" 
;;;    ":returns" ":side-effects" ":package" ":name" (":required" ":arg") 
;;;    (":optional" ":arg") (":rest" ":arg") (":key" ":arg") (":aux" ":arg") 
;;;    (":body" ":arg")) 
;;;  
;;; Note that ":elaboration" now has to be parenthesized, because it has become the proud 
;;; 'parent' of a sub-key! 
;;;  
;;; STEP #2. Add an entry to the hashtable called *node-value-formatter* 
;;;  
;;; This step is actually optional.  It is only necessary if you don't like 
;;; the default Quail format.  Try the default first, you may like it! 
;;;  
;;; Otherwise, you have to add an entry to the hash table *node-value-formatter*. 
;;; This seems more difficult than it is.  The hash table *node-value-formatter* 
;;; contains lambda expressions of the following form: 
;;;  
;;; #'(lambda (node) 
;;;     #'(lambda () 
;;;         (with-accessors ((value value)) node 
;;;           ;; HERE GOES CODE TO FORMAT THE VALUE 
;;;           ))) 
;;;  
;;; The point of all this is: documentation information is 
;;; stored in the value slot of objects of class 'node.' They 
;;; are called nodes, because they are arranged in a tree structure 
;;; which mirror the tree structure of *doc-keywords*. 
;;;  
;;; A function like the one above creates another function. 
;;; Because the second function is 
;;; created within the lexical scope of the variable 'node', 
;;; it needs no argument: it will remember the value of the node, 
;;; and will keep track of changes in the value automatically. 
;;;  
;;; Of course, you have to actually put this in the hash table: 
;;;  
;;; (setf (gethash '(":author" ":root") *node-value-formatter*)  
;;;       #'(lambda (node) 
;;;           #'(lambda () 
;;;               (with-accessors ((value value)) node 
;;;                 ;; HERE GOES CODE TO FORMAT THE VALUE 
;;;                 )))) 
;;;  
;;; Note that the hash table key is a list of the names encounted when 
;;; travelling back up the *doc-keywords* tree from the keyword 
;;; that you are interested in back to ":root". 
;;;  
;;; STEP #3: Create a function to access the information. 
;;;  
;;;  
;;; There is a Quail function called 'find-and-format' which does this. 
;;; 'find-and-format' takes a key-tree (the repository of information 
;;; collected from the documentation string) and the same list 
;;; used as a key in the *node-value-formatter* hash table. 
;;;  
;;; For example,  
;;;  
;;; (find-and-format key-tree '(":author" ":root")) 
;;;  
;;; Will retrieve information from the key-tree.  It is usually more 
;;; convenient to create a function with a name like 'get-author': 
;;;  
;;; (defun get-author (key-tree) 
;;;   (find-and-format key-tree '(":author" ":root"))) 
;;; 
