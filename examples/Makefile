PROGS = upload.cgi download.cgi printinput.cgi redirect.cgi multipart-extract \
        hello.cgi

.PHONY: all clean

all: $(PROGS)

%.cgi: %.hs
	ghc --make -o $@ $^ 

%: %.hs
	ghc --make -o $@ $^ 

clean:
	-rm -f *.hi *.o
	-rm -f $(PROGS)