TARGET=stockx
LIBS=-I,/usr/lib/ocaml
FLAGS=-pkg llvm -lib str
OCAMLBUILD=ocamlbuild
OPAM=opam config env

all: clean native
	mv stockx.native stockx


compilei: clean
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml


clean:
	$(OCAMLBUILD) -clean
	rm -rf scanner.ml parser.ml parser.mli ast.cmo ast.cmi parser.cmi parser.cmo scanner.cmo scanner.cmi


native:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).native


byte:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).byte


depend:
	echo "Not needed."
