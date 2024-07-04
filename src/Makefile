FILES=Main.hs Jana/*.hs
GHC:=ghc

all: main

warn: $(FILE)
	$(GHC) -Wall -fno-warn-orphans -fno-warn-unused-do-bind --make Main.hs

main: $(FILES)
	$(GHC) --make Main.hs

optimized, opt: $(FILES)
	$(GHC) -o ../bin/janus -O2 -rtsopts --make Main.hs

haddock, h: $(FILES)
	haddock -h -o ../docs/ Main.hs

# revTM: $(FILES)
# 	$(GHC) --make -o RevTMcomp RevTM.hs 

clean:
	-rm -f Main
	-rm -f *.hi *.o
	-rm -f Jana/*.hi Jana/*.o
	# -rm -f RevTM/*.hi RevTM/*.o


