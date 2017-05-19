CC=ocamlbuild
CCO=$(CC) -use-ocamlfind -use-menhir
SPIR=glslangValidator -V


all: info gen vk.cma base baseml libgen

base: c/base.c
	gcc -lvulkan c/base.c -o base

baseml: vk.cma examples/base.ml shaders
	$(CCO) examples/base.native

info:  _tags enkindler/*
	$(CCO) $@.native && mv $@.native $@

gen:  _tags enkindler/*
	$(CCO) $@.native && mv $@.native $@

libgen:  _tags enkindler/*
	$(CCO) $@.native && mv $@.native $@

lib/vk.ml: _tags libgen enkindler/*
	./libgen spec/vk.xml lib

vk.cma: _tags enkindler/*.ml lib/* lib/vk.ml libsdlvulkan.so
	$(CCO) $@

term: enkindler.cma

enkindler.cma: _tags enkindler/*
	$(CCO) $@

libsdlvulkan.so: sdl/vulkan_sdl.c
	gcc -shared -o libsdlvulkan.so -fPIC -lvulkan sdl/vulkan_sdl.c

shader: shaders/base.{frag,vert}
	$(SPIR) shaders/base.frag && $(SPIR) shaders/base.vert


clean:
	$(CC) -clean; rm lib/vk.ml
