#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) out vec4 outColor;

layout(location = 0) in vec2 fragTexCoord;

vec4 colorize(float x){

return vec4(pow(cos(5*x),2), pow(sin(2*x*x),2), pow(cos(17*x),2),1);
}

void main() {
    outColor = colorize(texture(texSampler,fragTexCoord).r);
}
