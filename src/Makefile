CC=gcc
CFLAGS= -g -Wall
LDFLAGS= -g

default: microc.native liblist.a

microc.native: microc.ml scanner.mll microcparse.mly irgen.ml semant.ml ast.ml sast.ml
	ocamlbuild -use-ocamlfind microc.native -pkgs llvm

liblist.a: list.o
	ar -crs liblist.a list.o
	ranlib liblist.a

list.o: list.c

.PHONY: clean
clean:
	ocamlbuild -clean 2>/dev/null
	rm -f scanner.native
	rm -rf _build
	rm -rf *.o *.s *.byte exe a.out *.a

.PHONY: all
all: clean default