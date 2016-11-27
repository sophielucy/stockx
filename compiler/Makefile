.PHONY : stockx.native

stockx.native :
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags \
		-w,+a-4 stockx.native

compilei :
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff stockx scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o
