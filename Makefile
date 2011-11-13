HSFLAGS+= -fwarn-incomplete-patterns -O2 -funbox-strict-fields -fasm -optc-O3
HSMAKE=ghc $(HSFLAGS) --make

EFILES=bin/aaa

.DEFAULT:all

all: $(EFILES)
aaa: bin/aaa

bin/aaa: Main.hs
	mkdir -p bin
	$(HSMAKE) $< -o $@
