TARGET=stockx
LIBS=-I,/usr/lib/ocaml
FLAGS=-pkg llvm -lib str
OCAMLBUILD=ocamlbuild
OPAM=opam config env

all: native
	mv stockx.native stockx


clean:
	$(OCAMLBUILD) -clean


native:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).native


byte:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).byte


depend:
	echo "Not needed."
