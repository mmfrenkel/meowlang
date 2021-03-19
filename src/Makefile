# Run 'make' to build meowlang.native

.PHONY : all
all : meowlang.native

meowlang.native : parser.mly scanner.mll ast.ml semant.ml codegen.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis meowlang.native

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff parser.output
