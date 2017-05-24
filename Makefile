CC=ocamlbuild
CCO=$(CC) -use-ocamlfind -use-menhir
SPIR=glslangValidator -V


all: infolivine vk.cmxa triangle libgen

triangle: libsdlvulkan.so vk.cmxa shaders/frag.spv
	$(CCO) examples/$@.native && mv $@.native $@

infolivine:  _tags enkindler/*
	$(CCO) $@.native && mv $@.native $@

libgen:  _tags enkindler/*
	$(CCO) $@.native && mv $@.native $@

lib/vk.ml: _tags libgen enkindler/*
	./libgen spec/vk.xml lib

vk.cmxa: _tags spec/vk.xml enkindler/*.ml lib_aux/* lib/vk.ml libsdlvulkan.so
	$(CCO) $@

term: enkindler.cma

enkindler.cma: _tags enkindler/*
	$(CCO) $@

libsdlvulkan.so: sdl/vulkan_sdl.c 
	gcc -shared -o libsdlvulkan.so -fPIC -lvulkan sdl/vulkan_sdl.c

shaders/frag.spv shaders/vert.spv: shaders/triangle.frag shaders/triangle.vert
	cd shaders && $(SPIR) triangle.frag  && $(SPIR) triangle.vert

test-triangle: triangle
	VK_INSTANCE_LAYERS=VK_LAYER_LUNARG_standard_validation ./triangle

clean:
	$(CC) -clean; rm lib/vk.ml
