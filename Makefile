CC=ocamlbuild
CCO=$(CC) -use-ocamlfind -use-menhir

all: info gen base

base: c/base.c
	gcc c/base.c -o base


info:  _tags enkindler/*
	$(CCO) $@.native && mv $@.native $@

gen:  _tags enkindler/*
	$(CCO) $@.native && mv $@.native $@

term: enkindler.cma

enkindler.cma: _tags enkindler/*
	$(CCO) $@

clean:
	$(CC) -clean
