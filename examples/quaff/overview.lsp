The Quail Foreign Functions Interface ("Quaff") is designed to be 
a portable interface from Quail to functions written in Fortran or 
C.

This document very briefly outlines the features of Quaff and 
states some restrictions.   The features outlined here are really 
the highest-level interface; when things stabilize at the lower 
level (ie. after porting the software to some other platforms) 
and when time permits, more features will be documented.

High-Level Functionality
-----------------------

1.  def-ffenv is a function which collects all the information 
about a group of files which will always be loaded together, as a 
so-called 'foreign environment'.   Relevant information includes 
the language of the foreign code, which entry points in the 
foreign code are to be accessible from Quail, and which C or 
Fortran libraries to use.

2.  defunf is a macro which defines the Quail interface function 
to an entry point.

The files fortran-example.lisp, etc demonstrate the syntax for 
these.

Restrictions
-----------

Here are some current restrictions imposed by Quaff.  We will 
remove as many of these as possible as time permits.

1.  Quaff currently works only with Macintosh Common Lisp 
(MCL), though it has been designed to be easily portable.

2.   The MCL foreign functions loader generates a large number 
of warnings when loading, and is slow.  Usually only warnings 
concerning entry points defined by the user are of any 
consequence.

3.  Since the MCL foreign functions interface does not support 
standard input and output from foreign code, Quaff doesn't 
either.

4.  Currently only some very basic data types are supported.  
These are 4-byte integers and 8-byte floats.  For C these can be 
referred to as "c-int" and "c-double".  For Fortran they are 
called "f-integer" and "f-real*8" (or "f-double-precision").

5.  Currently, all arguments to foreign functions are treated as 
passed by reference.   [ Thus in the examples, there is really no 
distinction between, say, f-real*8 and (array f-real*8) in a 
defunf statement. ]  This has no impact in the case of Fortran, 
but it does mean that all arguments to C routines attached to 
Quaff must be pointer types (see c-example.lisp).

6.  Different languages on different machines support different 
mechanisms for returning values from functions.  Accordingly, 
Quaff is designed for calling subroutines, which may modify the 
values pointed to by the arguments passed to the subroutines.    
To attach a function, write a cover subroutine in the same 
language which takes an argument for the return value (see 
c-example.lisp).

7.  Mixed-language foreign environments, while possible, can be 
flaky.  Proceed at your own risk.  In the def-ffenv statement 

:standard-libraries  (list :c :fortran)

will call in both sets of standard libraries.
