#version 450
#extension GL_ARB_separate_shader_objects : enable

layout (binding = 0) uniform UniformBufferObject
{ mat4 rotation; } ubo ;
layout( location = 0 ) in vec3 pos;
layout( location = 1 ) in vec3 color;

layout(location=0) out vec3 fragColors;

void main() {
    gl_Position = vec4(0,0,0.5,1) + ubo.rotation * vec4(pos, 1.);
    fragColors =  color;
}
