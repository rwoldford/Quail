From Greg Bennett running "Version 1.11-r16812  (LinuxX8664)" of Clozure
under Linux Mint 18.1 64bit.

I have two queries about draw-text* as I try to port code to mcclim,
and a comment

[C] The comment first. It seems that the towards-x and towards-y and towards-point
of the printed page should be, respectively, toward-x, toward-y, and toward-point.
The latter are accepted, the former produce in invalid keyword error.

I use the setup defined by p19 of the CLIM2.2.2 manual which gives mea
*test-frame* and *test-pane* together with a live repl.
I try this:
(draw-text* *test-pane* "Some Text" 25 25)
and receive, rather to my surprise, I confess:
Stream #<BASIC-FILE-BINARY-INPUT-STREAM ("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"/6 ISO-8859-1) #x302003C2055D> is private to #<PROCESS test(13) [Semaphore timed wait] #x302003BCF8AD>
   [Condition of type SIMPLE-ERROR]

[Q1] Does this mean that I have to specify the text-style explicitly every time I call draw-text* even if 
I want the value of *default-text-style*?

In an attempt to write vertically, I see that x,y is the start and  things are drawn on the line from it to
toward-x toward-y. Thus I try 25 25 25 500 to get things vertically setting :transform-glyphs to T just in case:

 (draw-text* *test-pane* "Some more text"  25 25 :text-style (clim:make-text-style :fix :bold :large) :toward-x 25 :toward-y 500 :transform-glyphs T)

and there is the text, still horizontally displayed.

[Q2] What have I failed to percieve ? 

Thanks for any and all advice and assistance
Cheers /Greg Bennett