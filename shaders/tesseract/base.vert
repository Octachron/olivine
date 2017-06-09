#version 450
#extension GL_ARB_separate_shader_objects : enable

layout (binding = 0) uniform UniformBufferObject
{ mat4 rotation; } ubo ;
layout( location = 0 ) in vec4 pos;
layout( location = 1 ) in vec2 texpos;

layout(location=0) out vec2 fragTexCoord;

void main() {
    vec4 newpos = ubo.rotation * pos;
    vec2 proj = vec2(newpos);
    float true_z = 1.5 + newpos.z;
    float z = true_z / 10;
    gl_Position = vec4(proj,z, true_z ) ;
    fragTexCoord =  texpos;
}
