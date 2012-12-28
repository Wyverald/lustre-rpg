all: Main.native

Main.native: Main.ml Node.ml Signal.ml Type.ml Primitive.ml
	ocamlbuild Main.native

run: Main.native
	./Main.native

.PHONY: all run clean

clean:
	ocamlbuild -clean

