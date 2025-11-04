// Helper functions to make sokol easier to call from Haskell

#include "sokol/sokol_gfx.h"
#include <string.h>

// Simple setup with default config
void sokol_setup_simple(int sample_count) {
    sg_desc desc = {0};
    sg_setup(&desc);
}

// Begin default pass with clear color (for GL swapchain)
void sokol_begin_default_pass(int width, int height, float r, float g, float b, float a) {
    sg_pass pass = {0};
    pass.action.colors[0].load_action = SG_LOADACTION_CLEAR;
    pass.action.colors[0].clear_value.r = r;
    pass.action.colors[0].clear_value.g = g;
    pass.action.colors[0].clear_value.b = b;
    pass.action.colors[0].clear_value.a = a;
    pass.swapchain.width = width;
    pass.swapchain.height = height;
    pass.swapchain.sample_count = 1;
    pass.swapchain.color_format = SG_PIXELFORMAT_RGBA8;
    pass.swapchain.depth_format = SG_PIXELFORMAT_DEPTH_STENCIL;
    pass.swapchain.gl.framebuffer = 0;  // Default framebuffer
    sg_begin_pass(&pass);
}

// Create a simple vertex buffer
uint32_t sokol_make_vertex_buffer(const void* data, size_t size) {
    sg_buffer_desc desc = {0};
    desc.type = SG_BUFFERTYPE_VERTEXBUFFER;
    desc.data.ptr = data;
    desc.data.size = size;
    desc.usage = SG_USAGE_IMMUTABLE;
    return sg_make_buffer(&desc).id;
}

// Create a simple shader from GLSL source
uint32_t sokol_make_shader_glsl(const char* vs_src, const char* fs_src) {
    sg_shader_desc desc = {0};
    desc.vs.source = vs_src;
    desc.fs.source = fs_src;
    return sg_make_shader(&desc).id;
}

// Create a simple pipeline (generic version)
uint32_t sokol_make_pipeline_simple(uint32_t shd_id, size_t stride, int attr_index, int format) {
    sg_pipeline_desc desc = {0};
    desc.shader.id = shd_id;
    desc.layout.attrs[attr_index].format = format;
    desc.primitive_type = SG_PRIMITIVETYPE_TRIANGLES;
    desc.layout.buffers[0].stride = stride;
    return sg_make_pipeline(&desc).id;
}

// Create a simple pipeline with position and color attributes
uint32_t sokol_make_pipeline_pos_color(uint32_t shd_id) {
    sg_pipeline_desc desc = {0};
    desc.shader.id = shd_id;

    // Attribute 0: position (vec2) at offset 0
    desc.layout.attrs[0].format = SG_VERTEXFORMAT_FLOAT2;
    desc.layout.attrs[0].offset = 0;

    // Attribute 1: color (vec3) at offset 8 (2 floats * 4 bytes)
    desc.layout.attrs[1].format = SG_VERTEXFORMAT_FLOAT3;
    desc.layout.attrs[1].offset = 8;

    desc.primitive_type = SG_PRIMITIVETYPE_TRIANGLES;
    desc.layout.buffers[0].stride = 20;  // 5 floats * 4 bytes

    return sg_make_pipeline(&desc).id;
}

// Apply pipeline (wrapper to convert from ID to struct)
void sokol_apply_pipeline_wrapper(uint32_t pip_id) {
    sg_pipeline pip = { .id = pip_id };
    sg_apply_pipeline(pip);
}

// Apply simple bindings with one vertex buffer
void sokol_apply_bindings_simple(uint32_t vbuf_id) {
    sg_bindings bindings = {0};
    bindings.vertex_buffers[0].id = vbuf_id;
    sg_apply_bindings(&bindings);
}
