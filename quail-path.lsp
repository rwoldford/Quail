;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                               quail-path.lsp                               
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Copyright (c) 1990 Statistical Computing Laboratory, University of Waterloo
;;;
;;;
;;;  Authors:
;;;     Greg Anglin 1989, 1990, 1991.
;;;     rwo 1991....
;;;
;;;--------------------------------------------------------------------------------

(in-package :MAKE)

(defun bin (directory-name) directory-name)

(defun system-name-convert (string)
  "Removes spaces which some file systems cannot handle in a file name."
	#+:apple string
	#-:apple (substitute #\_ #\Space string))

#-:sbcl-linux(defun path-source ()
  (append-directories
   (path-quail)
   (system-name-convert "Source;")))

#+:sbcl-linux(defun path-source ()
  (pathname-as-directory (merge-pathnames "Source" (path-quail))))

#-:sbcl-linux(defun path-binary ()
  (append-directories
   (path-quail)
   #|
   #+(and :apple :ccl-3 (not :PPC-TARGET)) "Binary 3.1 (M68000);"
   #+(and :apple :PPC-TARGET :ccl-4 (not (or :ccl-4.1 :ccl-4.2 :ccl-4.3)))  "Binary 4.0;"
   #+(and :apple :PPC-TARGET :ccl-4.1)  "Binary 4.1;"
   #+(and :apple :PPC-TARGET :ccl-4.2)  "Binary 4.2;"
   #+(and :apple :PPC-TARGET :ccl-4.3)  "Binary 4.3;"
   #+(and :aclpc (not (or :ALLEGRO-V6.0 :ALLEGRO-V6.1 :ALLEGRO-V6.2  
                          :ALLEGRO-V7.0 :ALLEGRO-V8.0 :ALLEGRO-V8.1 :ALLEGRO-V9.0))) "ACL5BIN;"
   #+(and :aclpc :ALLEGRO-V6.0 (not :ALLEGRO-V9.0)) "ACLBINV60;"
   #+(and :aclpc :ALLEGRO-V6.1 (not :ALLEGRO-V9.0)) "ACLBINV61;"
   #+(and :aclpc :ALLEGRO-V6.2 (not :ALLEGRO-V9.0)) "ACLBINV62;"
   #+(and :aclpc :ALLEGRO-V7.0 (not :ALLEGRO-V9.0)) "ACLBINV70;"
   #+(and :aclpc :ALLEGRO-V8.0 (not :ALLEGRO-V9.0)) "ACLBINV80;"
   #+(and :aclpc :ALLEGRO-V8.1 (not :ALLEGRO-V9.0)) "ACLBINV81;" ; 15JAN2009 "ACLBINV81B;"  
   #+(and :aclpc-mswin :ALLEGRO-V9.0) "ACLBINV90;" ; 10 Nov 2014
   #+(and :aclpc-linux :ALLEGRO-V9.0) "LX-ACLBINV90;" ; 10 Nov 2014
   #+(and :aclpc-mac :ALLEGRO-V9.0) "MAC-ACLBINV90;" ; 10 Nov 2014
   ;-31Dec14#+(and :aclpc :ALLEGRO-V9.0 (not :LINUX)) "ACLBINV90;" ; 10 Nov 2014
   ;-31Dec14 for CENTOS#+(and :aclpc :LINUX :ALLEGRO-CL-EXPRESS :ALLEGRO-v9.0) "32-LX-ACLBINV90;"  ;18 Nov 2014
   ;+#+( and :unix (not :aclpc)) "binaries;"
   ;=31Dec14 for 64bit #+ (and :aclpc :linux :allegro-v9.0 :64bit) "64-LX-ACLBINV90;" ;17 Dec 2014
   #+(and :aclpc-linux :ALLEGRO-V10.0) "LX-ACLBINV100;" ; 30 Jul 2016
   |#
   #+(and :aclpc-linux :allegro-v10.1) "LX-ACLBINV101;" ; 16 Jan 2016
   ;#+(and :aclpc-mswin :allegro-v10.1) "ACLBINV101" ;19 Feb 2018
   )
  )


;;;;;;;;;
;;;
;;;  Make sure that all those *.qmk files can be found
;;;

#-:sbcl-linux(push (path-source) *central-registry*)

;;; in sbcl thay are on *quail-make-registry* 
;;; which is a list of lists of system-name, pathname-to-qmk, pathname-to--path.lsp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; End of File ;;;;;;;;;;;;;;;;;;;;;;;
