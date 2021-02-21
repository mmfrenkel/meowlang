
.PHONY : all
all : meowlang.native

meowlang.native : parser.mly scanner.mll ast.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind meowlang.native

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
