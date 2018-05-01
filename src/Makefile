FILES=Main.hs Jana/*.hs

all: main

warn: $(FILES)
	ghc -Wall -fno-warn-orphans -fno-warn-unused-do-bind --make Main.hs

main: $(FILES)
	ghc --make Main.hs

optimized, opt: $(FILES)
	ghc -o ../bin/janus -O2 -rtsopts --make Main.hs

haddock, h: $(FILES)
	haddock -h -o ../docs/ Main.hs

# revTM: $(FILES)
# 	ghc --make -o RevTMcomp RevTM.hs 

clean:
	-rm -f Main
	-rm -f *.hi *.o
	-rm -f Jana/*.hi Jana/*.o
	# -rm -f RevTM/*.hi RevTM/*.o


