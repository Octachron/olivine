CC=ocamlbuild
CCO=$(CC) -use-ocamlfind -use-menhir
SPIR=glslangValidator -V


all: infolivine vk.cmxa triangle tesseract libgen

triangle: libsdlvulkan.so vk.cmxa shaders/triangle/vert.spv shaders/triangle/frag.spv
	$(CCO) examples/$@.native && mv $@.native $@

tesseract: libsdlvulkan.so vk.cmxa shaders/tesseract/vert.spv shaders/tesseract/frag.spv
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

shaders/%/frag.spv : shaders/%/base.frag
	cd shaders/$* && $(SPIR) base.frag

shaders/%/vert.spv : shaders/%/base.vert
	cd shaders/$* && $(SPIR) base.vert


test-triangle: triangle
	VK_INSTANCE_LAYERS=VK_LAYER_LUNARG_standard_validation ./triangle

test-tesseract: tesseract
	VK_INSTANCE_LAYERS=VK_LAYER_LUNARG_standard_validation ./tesseract

vkspec:
	mkdir -p spec\
	&& wget "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/1.0/src/spec/vk.xml" -o spec/vk.xml

clean:
	$(CC) -clean; rm lib/vk.ml
