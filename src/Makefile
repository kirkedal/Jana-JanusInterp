all: main

warn: Main.hs
	ghc -Wall -fno-warn-unused-do-bind --make Main.hs

main: Main.hs
	ghc --make Main.hs

optimized, opt: Main.hs
	ghc -O2 -rtsopts --make Main.hs

haddock, h: Main.hs
	haddock -h -o ../docs/ Main.hs

#bbpar: busyBeaver.hs TM.hs tape.hs 
#	ghc --make busyBeaver.hs -O2 -rtsopts -threaded -o bb

clean:
	-rm -f Main
	-rm -f *.hi *.o
	-rm -f Jana/*.hi Jana/*.o


