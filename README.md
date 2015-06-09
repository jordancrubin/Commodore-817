# Commodore-817
This is the complete collection of assembly code for the Commodore 817 - FT817 RS232 interface for the commodore 64

The code is designed to be compiled with 64Tass compiler or one of that style.  
Note that the initial file commodore-817.prg is written in such a way that the resulting prg will
record data between 0C00 through the areas starting at C000.  This will result in a large output file
that can easily be crunched. Exomizer was used during the development of this program.

./exomizer -s0xC000 -o ~/output/commodore-817.prg commodore-817.prg
the resulting output should be no larger than 4 blocks in size


With the exception of commodore-817.prg, the resulting output prg files must all be capitalized. before or after moving to a disk image.

commodore-817.prg
MAIN.prg                                          
HELP.prg                                          
STARTUPCHK.PRG                                    
TCALCHK.PRG 

as such, only commodore-817.prg will be readable on a disk directory with load"$",8

The code also assumes that the serial interface is set at address DE00, with the radio set for CATBAUD at 9600


Note that this CAT protocol will work with the FT817 and FT817NR style CAT and not FT450 or 950 models.
