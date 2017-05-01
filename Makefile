CC=ocamlbuild
CCO=$(CC) -use-ocamlfind -use-menhir

all: info gen base vk.cma baseml

base: c/base.c
	gcc c/base.c -o base

baseml: vk.cma examples/base.ml
	$(CCO) examples/base.native

info:  _tags enkindler/*
	$(CCO) $@.native && mv $@.native $@

gen:  _tags enkindler/*
	$(CCO) $@.native && mv $@.native $@

lib/vk.ml: _tags gen enkindler/*
	./gen specs/vk.xml lib/vk.ml

vk.cma: _tags lib/* lib/vk.ml
	$(CCO) $@

term: enkindler.cma

enkindler.cma: _tags enkindler/*
	$(CCO) $@

clean:
	$(CC) -clean
