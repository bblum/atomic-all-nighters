HSFLAGS+= -fwarn-incomplete-patterns -O2 -funbox-strict-fields -fasm -optc-O3 -XGADTs
HSMAKE=ghc $(HSFLAGS) --make

EFILES=bin/aaa

.DEFAULT:all

all: $(EFILES)
aaa: bin/aaa

bin/aaa: Main.hs Check.hs Rules.hs
	mkdir -p bin
	$(HSMAKE) $< -o $@
