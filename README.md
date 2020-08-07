# TI84-qr-gen
 
This iteration of the z80 code is mostly worked out in Mimas and then run through 8xvtoasm, so the formatting is a little screwy, but I'll fix it later.

I was originally going to use AppVars for lookup tables to implement the GF(256) arithmetic operators, but as it turns out, the only operators that require LUTs are the ones only found in decoders, not encoders.

Commenting/uncommenting certain calls at the top of the program will demonstrate whether different parts of the program are working.

DO NOT RUN THIS CODE ON AN ACTUAL CALCULATOR. I haven't confirmed whether it's 100% safe yet. (I mean, as-is, it *should* be fine; it doesn't crash my 84+. But still, part of the debug code relies on some 84+-specific ports to slow the thing down.) There is next to zero error handling here: Garbage in, possibly bricked machine out.


The folder old_Basic_files has some old TI-Basic files from years ago. Some of them were attempts at the backend (made obsolete by the z80 backend I'm working on now), but there should also be some frontend code in there that'll be useful.
