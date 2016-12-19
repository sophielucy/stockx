.PHONY : make
make :
	javac -cp ../YahooFinanceAPI-2.0.0.jar bin/*
	ocamlc -c ast.ml
	ocamlc -c sast.ml
	ocamlyacc -v parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex scanner.mll
	ocamlc -c scanner.ml
	ocamlc -c semantics.ml