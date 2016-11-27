.PHONY : stockx.native

stockx.native :
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags \
		-w,+a-4 stockx.native

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff stockx scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o
