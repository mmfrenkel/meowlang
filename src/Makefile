
.PHONY : all
all : meowlang.native

meowlang.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind meowlang.native

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
