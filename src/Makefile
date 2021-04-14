# Run 'make' to build meowlang.native

.PHONY : all
all : meowlang.native custom_scanf.o custom_casting.o custom_strcmp.o

meowlang.native : parser.mly scanner.mll ast.ml semant.ml codegen.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind meowlang.native

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff parser.output custom_scanf.o custom_casting.o custom_strcmp.o

custom_scanf.o : ./lib/custom_scanf.c
	cc -c ./lib/custom_scanf.c

custom_casting.o : ./lib/custom_casting.c
	cc -c ./lib/custom_casting.c

custom_strcmp.o : ./lib/custom_strcmp.c
	cc -c ./lib/custom_strcmp.c
