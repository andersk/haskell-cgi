PROGS = upload.cgi download.cgi printinput.cgi redirect.cgi hello.cgi

GHCFLAGS = -package cgi -fwarn-unused-imports

.PHONY: all clean

all: $(PROGS)

%.cgi: %.hs
	ghc $(GHCFLAGS) --make -o $@ $^ 

clean:
	-rm -f *.hi *.o
	-rm -f $(PROGS)