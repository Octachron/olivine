CC=ocamlbuild
CCO=$(CC) -use-ocamlfind -use-menhir
SPIR=glslangValidator -V


all: bin/infolivine vk.cmxa bin/triangle bin/tesseract bin/libgen

bin/triangle: vk.cmxa shaders/triangle/vert.spv shaders/triangle/frag.spv
	$(CCO) examples/$(notdir $@).native && mv $(notdir $@).native $@

bin/tesseract: vk.cmxa shaders/tesseract/vert.spv shaders/tesseract/frag.spv
	$(CCO) examples/$(notdir $@).native && mv $(notdir $@).native $@

bin/infolivine:  _tags enkindler/*
	$(CCO) $(notdir $@).native && mv $(notdir $@).native $@

bin/libgen:  _tags enkindler/*
	$(CCO) $(notdir $@).native && mv $(notdir $@).native $@

lib/vk.ml: _tags bin/libgen enkindler/*
	./bin/libgen spec/vk.xml lib

vk.cmxa: _tags spec/vk.xml enkindler/*.ml lib_aux/* lib/vk.ml bin/libsdlvulkan.so
	$(CCO) $@

term: enkindler.cma

enkindler.cma: _tags enkindler/*
	$(CCO) $@

shaders/%/frag.spv : shaders/%/base.frag
	cd shaders/$* && $(SPIR) base.frag

shaders/%/vert.spv : shaders/%/base.vert
	cd shaders/$* && $(SPIR) base.vert


test-triangle: bin/triangle
	VK_INSTANCE_LAYERS=VK_LAYER_LUNARG_standard_validation ./bin/triangle

test-tesseract: bin/tesseract
	VK_INSTANCE_LAYERS=VK_LAYER_LUNARG_standard_validation ./bin/tesseract

vkspec:
	mkdir -p spec\
	&& cd spec && wget "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/1.0/src/spec/vk.xml"

clean:
	$(CC) -clean; rm lib/vk.ml
