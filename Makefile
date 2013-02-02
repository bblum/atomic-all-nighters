HSFLAGS+= -fwarn-incomplete-patterns -O2 -funbox-strict-fields -fasm -optc-O3
HSMAKE=ghc $(HSFLAGS) --make

EFILES=bin/aan

.DEFAULT:all

all: $(EFILES)
aan: bin/aan

bin/aan: Main.hs Check.hs Rules.hs Constraints.hs
	mkdir -p bin
	$(HSMAKE) $< -o $@
