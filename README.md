# Commodore-817
This is the collection of assembly code for the Commodore 817 - FT817 RS232 interface for the commodore 64

The code is designed to be compiled with 64Tass compiler or one of that style.  
Note that the initial file commodore-817.prg is written in such a way that the resulting prg will
record data between 0C00 through the areas starting at C000.  This will result in a large output file
that can easily be crunched. Exomizer was used during the development of this program.

./exomizer -s0xC000 -o ~/output/commodore-817.prg commodore-817.prg

Note that this CAT protocol will work with the FT817 and FT817NR style CAT and not FT450 or 950 models.
