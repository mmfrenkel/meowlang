# Run 'make' to build meowlang.native

.PHONY : all
all : meowlang.native custom_scanf.o

meowlang.native : parser.mly scanner.mll ast.ml semant.ml codegen.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind meowlang.native

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff parser.output custom_scanf.o

custom_scanf.o : custom_scanf.c
	cc -c custom_scanf.c
