#define WLR_USE_UNSTABLE
//wlr_godot_backend.c
//gles2_renderer:
#include <stdint.h>
//#include "drivers/gles2/rasterizer_gles2.h"
//#include "drivers/gles2/rasterizer_storage_gles2.h"
//#include "gles2_renderer.h"

//#define static //George: will not leave; not sure if we need
#include <wayland-server.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/render/interface.h>
#include <wlr/util/log.h>
//#undef static

//backend:
//#include "servers/visual/visual_server_globals.h"
//#include "wayland_display.h"
//#include "wlr_backend.h"
#include <assert.h>
#include <stdlib.h>
#include <wlr/backend.h>
#include <wlr/backend/interface.h>

#include <wlr_godot_backend.h>

//wlr_renderer:
static const enum wl_shm_format wl_formats[] = {
	WL_SHM_FORMAT_ARGB8888,
	WL_SHM_FORMAT_XRGB8888,
	WL_SHM_FORMAT_ABGR8888,
	WL_SHM_FORMAT_XBGR8888,
};

/* static const struct gles2_pixel_format formats[] = { */
/* 	{ */
/* 		.wl_format = WL_SHM_FORMAT_ARGB8888, */
/* 		.gl_format = GL_RGBA, */
/* 		.gl_type = GL_UNSIGNED_BYTE, */
/* 		.depth = 32, */
/* 		.bpp = 32, */
/* 		.has_alpha = true, */
/* 		.swizzle = true, */
/* 	}, */
/* 	{ */
/* 		.wl_format = WL_SHM_FORMAT_XRGB8888, */
/* 		.gl_format = GL_RGBA, */
/* 		.gl_type = GL_UNSIGNED_BYTE, */
/* 		.depth = 24, */
/* 		.bpp = 32, */
/* 		.has_alpha = false, */
/* 		.swizzle = true, */
/* 	}, */
/* 	{ */
/* 		.wl_format = WL_SHM_FORMAT_XBGR8888, */
/* 		.gl_format = GL_RGBA, */
/* 		.gl_type = GL_UNSIGNED_BYTE, */
/* 		.depth = 24, */
/* 		.bpp = 32, */
/* 		.has_alpha = false, */
/* 		.swizzle = false, */
/* 	}, */
/* 	{ */
/* 		.wl_format = WL_SHM_FORMAT_ABGR8888, */
/* 		.gl_format = GL_RGBA, */
/* 		.gl_type = GL_UNSIGNED_BYTE, */
/* 		.depth = 32, */
/* 		.bpp = 32, */
/* 		.has_alpha = true, */
/* 		.swizzle = false, */
/* 	}, */
/* }; */

/* const struct gles2_pixel_format *get_gles2_format_from_wl( */
/* 		enum wl_shm_format fmt) { */
/* 	for (size_t i = 0; i < sizeof(formats) / sizeof(*formats); ++i) { */
/* 		if (formats[i].wl_format == fmt) { */
/* 			return &formats[i]; */
/* 		} */
/* 	} */
/* 	return NULL; */
/* } */

static const enum wl_shm_format *renderer_formats(
		struct wlr_renderer *renderer, size_t *len) {
	*len = sizeof(wl_formats) / sizeof(wl_formats[0]);
	return wl_formats;
}

static bool renderer_format_supported(
		struct wlr_renderer *renderer, enum wl_shm_format fmt) {
	//return get_gles2_format_from_wl(fmt) != NULL;
  return false; //george
}

/* const char *gles2_strerror(GLenum err) { */
/* 	switch (err) { */
/* 	case GL_INVALID_ENUM: */
/* 		return "Invalid enum"; */
/* 	case GL_INVALID_VALUE: */
/* 		return "Invalid value"; */
/* 	case GL_INVALID_OPERATION: */
/* 		return "Invalid operation"; */
/* 	case GL_OUT_OF_MEMORY: */
/* 		return "Out of memory"; */
/* 	case GL_INVALID_FRAMEBUFFER_OPERATION: */
/* 		return "Invalid framebuffer operation"; */
/* 	default: */
/* 		return "Unknown error"; */
/* 	} */
/* } */

/* static bool gles2_flush_errors(const char *context) { */
/* 	GLenum err; */
/* 	bool failure = false; */
/* 	while ((err = glGetError()) != GL_NO_ERROR) { */
/* 		failure = true; */
/* 		if (!context) { */
/* 			continue; */
/* 		} */
/* 		if (err == GL_OUT_OF_MEMORY) { */
/* 			// The OpenGL context is now undefined */
/* 			wlr_log(WLR_ERROR, "%s: Fatal GL error: out of memory", context); */
/* 			exit(1); */
/* 		} else { */
/* 			wlr_log(WLR_ERROR, "%s: GL error %d %s", context, */
/* 					err, gles2_strerror(err)); */
/* 		} */
/* 	} */
/* 	return failure; */
/* } */

struct wlr_texture * texture_from_pixels(
		struct wlr_renderer *_renderer, enum wl_shm_format wl_fmt,
		uint32_t stride, uint32_t width, uint32_t height, const void *data) {
	/* struct WlrGLES2Renderer::renderer_state *state = */
	/* 	(struct WlrGLES2Renderer::renderer_state *)_renderer; */
	/* WlrGLES2Renderer *renderer = state->godot_renderer; */
	/* auto storage = */
	/* 	(RasterizerStorageGLES2 *)renderer->rasterizer->get_storage(); */
	/* gles2_flush_errors(NULL); */

	/* RID rid = storage->texture_create(); */
	/* gles2_flush_errors("texture_create"); */
	/* RasterizerStorageGLES2::Texture *texture = */
	/* 	storage->texture_owner.getornull(rid); */

	/* storage->texture_allocate(rid, width, height, 0, */
	/* 		Image::FORMAT_RGBA8, VS::TEXTURE_TYPE_2D, 0); */
	/* gles2_flush_errors("texture_allocate"); */

	/* const struct gles2_pixel_format *fmt = get_gles2_format_from_wl(wl_fmt); */
	/* if (fmt == NULL) { */
	/* 	wlr_log(WLR_ERROR, "Unsupported pixel format %" PRIu32, wl_fmt); */
	/* 	return NULL; */
	/* } */

	/* glActiveTexture(GL_TEXTURE0); */
	/* glBindTexture(texture->target, texture->tex_id); */

	/* glTexParameteri(texture->target, GL_TEXTURE_MIN_FILTER, GL_NEAREST); */
	/* glTexParameteri(texture->target, GL_TEXTURE_MAG_FILTER, GL_NEAREST); */
	/* glTexParameterf(texture->target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE); */
	/* glTexParameterf(texture->target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE); */
	/* gles2_flush_errors("glTexParameterf"); */
	/* glPixelStorei(GL_UNPACK_ALIGNMENT, 1); */
	/* gles2_flush_errors("glPixelStorei"); */

	/* if (fmt->swizzle) { */
	/* 	GLint swizzleMask[] = {GL_BLUE, GL_GREEN, GL_RED, GL_ALPHA}; */
	/* 	glTexParameteriv(texture->target, GL_TEXTURE_SWIZZLE_RGBA, swizzleMask); */
	/* } */

	/* glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, */
	/* 		GL_RGBA, fmt->gl_type, data); */
	/* gles2_flush_errors("glTexImage2D"); */

	/* WlrGLES2Texture *wlr_texture = new WlrGLES2Texture(rid, width, height, fmt); */
	/* wlr_texture->reference(); */
	/* return wlr_texture->get_wlr_texture(); */
  return NULL; //george
}

static void renderer_init_wl_display(struct wlr_renderer *renderer, struct wl_display *wl_display) {
	// TODO: bind EGL
}

static void renderer_begin(struct wlr_renderer *renderer,
		uint32_t width, uint32_t height) {
	/* This space deliberately left blank */
}

static void renderer_end(struct wlr_renderer *renderer) {
	/* This space deliberately left blank */
}

void renderer_clear(struct wlr_renderer *renderer, const float color[4]) {
	/* This space deliberately left blank */
}

void renderer_scissor(struct wlr_renderer *renderer, struct wlr_box *box) {
	/* This space deliberately left blank */
}

bool renderer_render_texture_with_matrix(struct wlr_renderer *renderer,
		struct wlr_texture *texture, const float matrix[9], float alpha) {
	/* This space deliberately left blank */
	return false;
}

void renderer_render_quad_with_matrix(struct wlr_renderer *renderer,
		const float color[4], const float matrix[9]) {
	/* This space deliberately left blank */
}

void renderer_render_ellipse_with_matrix(struct wlr_renderer *renderer,
		const float color[4], const float matrix[9]) {
	/* This space deliberately left blank */
}



static const struct wlr_renderer_impl renderer_impl = {
	/* We need to implement these, but we don't use them */
	/* TODO wlroots: we should consider separating the "allocate textures"
	 * interface from the "abstract drawing library" interface, at least */
	.begin = renderer_begin,
	.end = renderer_end,
	.clear = renderer_clear,
	.scissor = renderer_scissor,
	.render_texture_with_matrix = renderer_render_texture_with_matrix,
	.render_quad_with_matrix = renderer_render_quad_with_matrix,
	.render_ellipse_with_matrix = renderer_render_ellipse_with_matrix,
	/* We use these */
	.formats = renderer_formats,
	.format_supported = renderer_format_supported,
	.texture_from_pixels = texture_from_pixels,
	.init_wl_display = renderer_init_wl_display,
};

//wlr_backend:
bool backend_start(struct wlr_backend *backend) {
	/* This space deliberately left blank */
	return false;
}

void backend_destroy(struct wlr_backend *backend) {
	/* This space deliberately left blank */
}

//TODO: george: Change this
struct wlr_renderer *backend_get_renderer(struct wlr_backend *_backend) {
	/* WlrBackend *backend = (WlrBackend *)_backend; */
	/* return backend->get_renderer()->get_wlr_renderer(); */
	struct wlr_godot_backend *backend = (struct wlr_godot_backend *)_backend;
  return backend->renderer;
}

static const struct wlr_backend_impl backend_impl = {
	.start = backend_start,
	.destroy = backend_destroy,
	.get_renderer = backend_get_renderer,
};

struct wlr_backend *wlr_godot_backend_create(struct wl_display *display,
		wlr_renderer_create_func_t create_renderer_func) {
	wlr_log(WLR_INFO, "Creating godot backend");

	struct wlr_godot_backend *backend = calloc(1, sizeof(struct wlr_godot_backend));
	if (!backend) {
		wlr_log(WLR_ERROR, "Failed to allocate wlr_backend");
		return NULL;
	}

	wlr_backend_init(&backend->backend, &backend_impl);
	//backend->display = display;
	//wl_list_init(&backend->outputs);
	//wl_list_init(&backend->input_devices);

	/* static const EGLint config_attribs[] = { */
	/* 	EGL_SURFACE_TYPE, EGL_PBUFFER_BIT, */
	/* 	EGL_ALPHA_SIZE, 0, */
	/* 	EGL_BLUE_SIZE, 1, */
	/* 	EGL_GREEN_SIZE, 1, */
	/* 	EGL_RED_SIZE, 1, */
	/* 	EGL_NONE, */
	/* }; */

	/* if (!create_renderer_func) { */
	/* 	create_renderer_func = wlr_renderer_autocreate; */
	/* } */

	/* backend->renderer = create_renderer_func(&backend->egl, */
	/* 	EGL_PLATFORM_SURFACELESS_MESA, NULL, (EGLint*)config_attribs, 0); */
	/* if (!backend->renderer) { */
	/* 	wlr_log(WLR_ERROR, "Failed to create renderer"); */
	/* 	free(backend); */
	/* 	return NULL; */
	/* } */

	struct wlr_renderer *renderer = calloc(1, sizeof(struct wlr_renderer)); //TODO: Make sure this doesn't leak
  wlr_renderer_init(renderer, &renderer_impl);

  backend->renderer = renderer;

	/* backend->display_destroy.notify = handle_display_destroy; */
	/* wl_display_add_destroy_listener(display, &backend->display_destroy); */

	return &backend->backend;
}