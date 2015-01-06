massive-ironman
===============

How to use this?
----------------
1. Build yourself a nxt robot resembling mine, instructions coming
2. Pair your computer and nxt with bluetooth.
3. Bind the nxt to rfcomm1 with

        rfcomm bind <NXT addr> rfcomm0
    
4. Complie code
5. Run code

###Limitations###
1. The morse parses can only recognise morse with wpm of 5 and this was only tested with beep pitch
   of 700 hz.(recommend using [this](http://morsecode.scphillips.com/jtranslator.html) website to generate morse
2. The drawing is very crooked, this is mainly because I had no time to figure out how these motors
   really work...

