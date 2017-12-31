CC=ocamlbuild
CCO=$(CC) -use-ocamlfind -use-menhir
DUNE=jbuilder build
SPIR=glslangValidator -V


all: bin/infolivine vk.cmxa bin/triangle bin/tesseract bin/libgen

bin/triangle: vk.cmxa shaders/triangle/vert.spv shaders/triangle/frag.spv
	$(CCO) examples/$(notdir $@).native && mv $(notdir $@).native $@

bin/tesseract: vk.cmxa shaders/tesseract/vert.spv shaders/tesseract/frag.spv
	$(CCO) examples/$(notdir $@).native && mv $(notdir $@).native $@

bin/infolivine:  info/*
	$(DUNE) info/infolivine.exe && $(CCO) \
	&& mv _build/default/info/$(notdir $@.exe) $@

bin/libgen:  aster/*.ml generator/*.ml info/*.ml
	$(DUNE) generator/libgen.exe && $(CCO) \
	&& mv _build/default/generator/$(notdir $@.exe) $@


lib/vk.ml: bin/libgen spec/vk.xml
	./bin/libgen spec/vk.xml lib

vk.cmxa: _tags spec/vk.xml info/*.ml aster/*.ml generator/*.ml\
	lib_aux/* lib/vk.ml
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
	$(CC) -clean; rm lib/vk.ml; rm bin/*
