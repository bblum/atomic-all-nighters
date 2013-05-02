HSFLAGS+= -fwarn-incomplete-patterns -O2 -funbox-strict-fields -fasm -optc-O3 -XGADTs -XTupleSections
HSMAKE=ghc $(HSFLAGS) --make

EFILES=bin/aan

.DEFAULT:all

all: $(EFILES)
aan: bin/aan

bin/aan: Main.hs Check.hs Rules.hs Attributes.hs
	mkdir -p bin
	$(HSMAKE) $< -o $@

check: aan aan-wrap.sh aan-fail-wrap.sh
		./aan-wrap.sh tests/pass/*.c
		./aan-fail-wrap.sh tests/fail/*.c
