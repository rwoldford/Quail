;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           Documenting code.                              
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
;;;--------------------------------------------------------------------------------
;;; 
;;; 
;;; ENHANCED DOCUMENTATION FACILITY
;;; 
;;; A documentation string is allowed for nearly every kind of defined
;;; Common Lisp structure and usually appears where that structure is defined
;;; (i.e. with the source code) --- e.g. in defun, defvar, defmethod, defgeneric,
;;; defclass, ...
;;;
;;; Quail allows the user to add structure to this documentation string.
;;; The Quail help facility understands the structure, and will produce fairly
;;; readable and mouse sensitive help windows automatically.
;;; 
;;; Quail allows the user to embed a number of special lists within
;;; a documentation string.  The lists are special because the first element
;;; is a documentation keyword.  The documentation keywords are understood by
;;; the Quail help facility.

(in-package :q-user)

;;; EXAMPLE

;;; If you evaluate the function 'foo', below, and then evaluate (help 'foo),
;;; the result should be very similar to the help message which follows the
;;; function.

;;; You should return to read the help message carefully after you have
;;; finished reading this file.  It contains useful information about
;;; the documentation keywords.

;;;;
;;;; foo
;;;;

(defun foo (x &key (message "Hello world!"))
  
  ;;
  ;;  The documentation string.
  ;;
  
  "The information in each of the following lists will be displayed in ~
   a separate section of the Quail help message for foo.  The heading ~
   of the section in the help message will be the documentation keyword ~
   which begins the list, or something close to it. ~
   
  (:capsule A short description of the function foo.) ~
   
  (:elaboration Additional information about the function foo. ~
   Note that format directives like ~%  ~
   can be used in the doc-string.  Indeed, the doc-string is passed through ~
   the Common Lisp format function and so the symbol tilde ~
   should never appear in a doc-string *unless* it indicates a valid ~
   format directive. ~
   
   Similarly, no word should appear prefaced with a colon as in :message. ~
   Even though this is the valid keyword to the function foo, it might ~
   also correspond to one of the documentation keywords like those ~
   below which begin with a colon.  ~
   If you feel it is necessary to have a colon word in the doc-string, ~
   then precede the colon with the escape character (backslash) as in ~
   \:package ~
   ) ~
   
  (:examples Such as (foo bar) ~
             (:files (Access to an example file ~
                      eg:Mathematics;extended-arithmetic.lsp ~
                      ) ~
                     (Another example file ~
                      eg:Mathematics;extended-arithmetic.lsp ~
                      ) ~
             ) ~
             (:text  This is just a sub text of secondary importance)) ~
   
  (:required (:arg x An argument required by the function foo.)) ~
   
  (:returns The thing itself.) ~
   
  (:side-effects Prints \"Message:\ \" and then the message.) ~
   
  (:key (:arg message 'Hello world!' A keyword argument of the function ~
   foo and its default value.))"
  
  ;;
  ;; A trivial program.
  ;;
  
  (format *terminal-io* "Message: ~a" message)
  x)

;;;
;;; (help 'foo)   when *help-in-windows* is NIL will interpret the doc-string
;;; and print some help on the terminal screen.
;;; If *help-in-windows* is non-NIL, the information is displayed in
;;; a mouse sensitive help window.
;;;

(setf *help-in-windows* NIL)
(help 'foo)

#|
________________________________________________________________________________

foo                                                                     FUNCTION
________________________________________________________________________________

DESCRIPTION:
          A  short  description  of  the  function  foo. 

LAMBDA LIST:
          (X  &Key  Message) 

ARGUMENTS:
          

     REQUIRED - 
          x - An  argument  required  by  the  function  foo. 

     &KEY - 
          message - Default  is  'Hello.    world!'  A  keyword  argument  of  
               the  function  foo  and  its  default  value. 


RETURNS:
          The  thing  itself. 

SIDE EFFECTS:
          Prints  "Message:  "  and  then  the  message. 

HOME PACKAGE:
          :quail-user 

EXAMPLES:
          Such  as  (  foo  bar  ) 

This  is  just  a  sub  text  of  secondary  importance 

See  also  the  following  files. 


          Access to an example file 
               eg:Mathematics;extended-arithmetic.lsp
          Another example file 
               eg:Mathematics;extended-arithmetic.lsp
ELABORATION:
          Additional  information  about  the  function  foo.  Note  that  
          format  directives  like  can  be  used  in  the  doc-string.  
          Indeed,  the  doc-string  is  passed  through  the  Common  Lisp  
          format  function  and  so  the  symbol  tilde  should  never  appear  
          in  a  doc-string  *unless*  it  indicates  a  valid  format  
          directive.  Similarly,  no  word  should  appear  prefaced  with  a  
          colon  as  in  :message.  Even  though  this  is  the  valid  keyword  
          to  the  function  foo,  it  might  also  correspond  to  one  of  
          the  documentation  keywords  like  those  below  which  begin  with  
          a  colon.  If  you  feel  it  is  necessary  to  have  a  colon  word  
          in  the  doc-string,  then  precede  the  colon  with  the  escape  
          character  (  backslash  )  as  in  :package 

|#

(setf *help-in-windows* T)
;;;;
;;;; end of the help message
;;;;


;;; DOCUMENTATION KEYWORDS

;;; Documentation keywords have been implemented for all kinds of Lisp objects.
;;; The keywords available vary according to the type of object.  A function,
;;; for example, may have side effects.  So, ':side-effects', is one of the
;;; documentation keywords for functions.  Side effects are not relevent for
;;; other objects.

;;; Here is a list of the keywords that have been implemented so far,
;;; and the types of object to which they apply:

;;; 1.  These keywords apply to all objects:

 :CAPSULE           
 :ELABORATION     
 :EXAMPLES           
 :REFERENCES     
 :SEE-ALSO       
 :SUPER-TOPICS 
 
;;; 2.  These keywords apply to functions, macros and generic functions:

 :RETURNS                                             
 :SIDE-EFFECTS                        
 :REQUIRED                            
 :OPTIONAL                            
 :REST                                
 :KEY                                 
 :AUX
 :BODY                                 

;;; 3. These keywords apply to topic-documentation objects

 :SUB-TOPICS

;;; The format for use of these keywords is, within the documentation string,
;;; a list with the appropriate keyword as the first element, for example:
;;; 
;;;        (:capsule This is a short description of the object.)
;;; 
;;; The remainder of the list is free form with the following exceptions: 
;;;  
;;;  (i) Procedure arguments.  If 'x' and 'y' are required arguments of some 
;;;      procedure (function, macro, method), they must be documented as follows: 
;;;  
;;;        (:required (:arg x A description of 'x'.) (:arg y A description of 'y')) 
;;;  
;;;      Within the ':required' list is a separate ':arg' list for each variable.  The 
;;;      second element of an ':arg' list is the variable name.  The rest of the 
;;;      ':arg' list is a free form description of the variable. 
;;;  
;;;      Procedure arguments other than required arguments may have default values. 
;;;      Therefore, the format of the ':arg' list is slightly different.  For example, 
;;;      if 'z' is a keyword argument, it must be documented in this format: 
;;;  
;;;        (:key (:arg z 99 The default value of z is 99)) 
;;;  
;;;      Similarly for optional, rest and aux arguments. 
;;;  
;;;      More information about the documentation keywords is contained in the 
;;;      documentation of the function foo above. 
;;;  
;;;  (ii) See-also.  The :see-also keyword is typically followed by symbols 
;;;      which name other relevant documentation.  As in 
;;;  
;;;        (:see-also bar baz snafu) 
;;;  
;;;      This indicates that additional information found on the symbols 
;;;      bar baz or snafu might be relevant given the user's interest in the 
;;;      present symbol. 
;;;      Seeing this the user might consider (help 'bar) then. 
;;;      This is one means of providing pointers from one help document to 
;;;      another. 
;;;      Sometimes a symbol has more than one help type available.  For 
;;;      instance, suppose that bar names both a function and a class. 
;;;      The above encoding on :see-also does not specify which is more 
;;;      relevant and so the user is likely to look at both. 
;;;      particular help-types can be specified as in 
;;;  
;;;        (:see-also (bar :class) baz snafu) 
;;;  
;;;      This indicates that it is the class definition which is most relevant. 
;;;      Or if there was also a variable called bar that was relevant, 
;;;  
;;;        (:see-also (bar :class) (bar :variable) baz snafu) 
;;;      
;;;      So the user would be encouraged to type (help 'bar :variable)
;;;      to access that information.
;;;
;;;      In fact this will occur automatically if the corresponding item is
;;;      selected under `See Also' in the original help window.
;;;  
;;;  (iii) Examples.  When followed by free form text, as in the function 
;;;      foo above, the :examples keyword reproduces that text as a single string. 
;;;      This can be made explicit by the sub-keyword :text as in 
;;;  
;;;        (:examples (:text You might use (foo bar).)) 
;;;  
;;;      Often it is more useful to have a collection of examples which the 
;;;      user can access and play with.  It is most convenient to have these 
;;;      stuck away in a file somewhere (like this one). 
;;;      To facilitate this, you use the sub-keyword :files followed by  
;;;      lists containing short pieces of text that describe the example file 
;;;      and the namestring of the logical pathname that identifies the file. 
;;;      As in 
;;;        
;;;        (:examples (:files (Michelson's speed of light data 
;;;                                     q:Data;michelson-1878.lsp 
;;;                                     ) 
;;;                           (Cigarette data 
;;;                                      q:Data;cigs.lsp 
;;;                                      ) 
;;;                           (The present file 
;;;                                      eg:Documentation;doc-example.lsp 
;;;                                      ))) 
;;;  
;;;      The user then knows that relevant examples can be found in three 
;;;      different files.  These can be edited with

          (edit-file "q:Data;michelson-1879.lsp")

;;;
;;;      As with see-also items, this will occur automatically if the 
;;;      "Michelson's speed of light data" item is selected under `Examples'
;;;      in the help-window.
;;;
;;;      NOTE: The logical pathname *cannot* contain blanks.  See CLtL2 for
;;;      more info.
;;;  
;;;  
;;; SCOPE AND LIMITATIONS 
;;;  
;;; Things in the documentation string outside of a documentation-keyword list 
;;; may or may not appear in the Quail help message for the object. 
;;; They will not appear in the help message if there is a ':capsule' section. 
;;; Otherwise, they will be collected and will appear 
;;; in the help message as the function's description. 
;;;  
;;; Documentation keywords, such as ':capsule', should be quoted if they are not 
;;; meant to function as documentation keywords.  Parentheses must be matched 
;;; within the documentation string, even if they are quoted. 
