FILES=Main.hs RevTM.hs Jana/*.hs RevTM/*.hs

all: main

warn: $(FILES)
	ghc -Wall -fno-warn-unused-do-bind --make Main.hs

main: $(FILES)
	ghc --make Main.hs

optimized, opt: $(FILES)
	ghc -O2 -rtsopts --make Main.hs

haddock, h: $(FILES)
	haddock -h -o ../docs/ Main.hs

revTM: $(FILES)
	ghc --make -o RevTMcomp RevTM.hs 


#bbpar: busyBeaver.hs TM.hs tape.hs 
#	ghc --make busyBeaver.hs -O2 -rtsopts -threaded -o bb

clean:
	-rm -f Main
	-rm -f *.hi *.o
	-rm -f Jana/*.hi Jana/*.o
	-rm -f RevTM/*.hi RevTM/*.o


