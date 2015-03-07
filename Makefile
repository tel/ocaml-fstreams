OB = ocamlbuild -use-ocamlfind

all:
	$(OB) fstreams.cma
	$(OB) fstreams.cmxa

clean:
	$(OB) -clean

.PHONE: all clean
