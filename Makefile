CC=ocamlbuild
CCO=$(CC) -use-ocamlfind -use-menhir
DUNE=dune build
SPIR=glslangValidator -V


all: vk bin/infolivine bin/triangle bin/tesseract

bin/triangle: shaders/triangle/vert.spv shaders/triangle/frag.spv | vk
	$(DUNE) examples/$(notdir $@).exe
	mv _build/default/examples/$(notdir $@).exe $@

bin/tesseract: examples/tesseract.ml shaders/tesseract/vert.spv shaders/tesseract/frag.spv | vk
	$(DUNE) examples/$(notdir $@).exe
	mv _build/default/examples/$(notdir $@).exe $@

bin/infolivine:  info/*
	$(DUNE) info/infolivine.exe \
	&& mv _build/default/info/$(notdir $@.exe) $@

bin/libgen:  aster/*.ml generator/*.ml info/*.ml
	$(DUNE) generator/libgen.exe \
	&& mv _build/default/generator/$(notdir $@.exe) $@


lib/vk.ml: bin/libgen spec/vk.xml
	./bin/libgen spec/vk.xml lib

vk: _tags spec/vk.xml lib/vk.ml
	cp lib_aux/*.ml lib
	$(DUNE) @install

shaders/%/frag.spv : shaders/%/base.frag
	cd shaders/$* && $(SPIR) base.frag

shaders/%/vert.spv : shaders/%/base.vert
	cd shaders/$* && $(SPIR) base.vert


test-triangle: bin/triangle
	VK_INSTANCE_LAYERS=VK_LAYER_LUNARG_standard_validation ./bin/triangle

test-tesseract: bin/tesseract
	VK_INSTANCE_LAYERS=VK_LAYER_LUNARG_standard_validation ./bin/tesseract
.PHONY: vkspec
vkspec:
	mkdir -p spec \
	&& cd spec \
        && wget "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/master/xml/vk.xml"

clean:
	dune clean; rm lib/*.ml{,i}; rm bin/*
