FILES=src/Main.hs src/Jana/*.hs
GHC:=ghc

all:
	(cd src; make opt GHC="$(GHC)")
