
Examples= \
   Examples/TclShell/Main.hs \

SRC = Scripting/HSTCL.hs

ExampleEXES = $(Examples:.hs=.exe)


all: lib Examples

lib: Scripting/HSTCL.o

Scripting/HSTCL.o: Scripting/HSTCL.hs
	ghc --make $< 

# Preprocessing step:
Scripting/HSTCL.hs: Scripting/HSTCL.chs 
	c2hs Scripting/HSTCL.chs 


Examples: $(ExampleEXES)


# How do I make this work for other tcls than 8.5
%.exe: %.hs 
	ghc -o $@ --make $< -ltcl8.5  


clean:
	rm -f *.o *.hi *.chi Scripting/HSTCL.hs Scripting/*.hi Scripting/*.o Scripting/*.chi Scripting/*.chs.h
	rm -f $(ExampleEXES) $(Examples:.hs=.hi) $(Examples:.hs=.o)
