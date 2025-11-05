@vs vs
layout(binding=0) uniform vs_params {
    vec2 offset;
};

layout(location=0) in vec2 position;
layout(location=1) in vec3 color0;

out vec3 color;

void main() {
    gl_Position = vec4(position + offset, 0.0, 1.0);
    color = color0;
}
@end

@fs fs
in vec3 color;
out vec4 frag_color;

void main() {
    frag_color = vec4(color, 1.0);
}
@end

@program square vs fs
