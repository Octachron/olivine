DUNE=dune build
SPIR=glslangValidator -V


.PHONY: lib clean vkspec

lib:
	$(DUNE) @install @check

vkspec:
	mkdir -p spec \
	&& cd spec \
        && wget "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/master/xml/vk.xml"

clean:
	dune clean

# Examples

shaders/%/frag.spv : shaders/%/base.frag
	cd shaders/$* && $(SPIR) base.frag

shaders/%/vert.spv : shaders/%/base.vert
	cd shaders/$* && $(SPIR) base.vert


test-triangle: shaders/triangle/vert.spv shaders/triangle/frag.spv
	VK_INSTANCE_LAYERS=VK_LAYER_LUNARG_standard_validation dune exec -- ./examples/triangle.exe

test-tesseract: shaders/tesseract/vert.spv shaders/tesseract/frag.spv
	VK_INSTANCE_LAYERS=VK_LAYER_LUNARG_standard_validation dune exec -- ./examples/tesseract.exe
