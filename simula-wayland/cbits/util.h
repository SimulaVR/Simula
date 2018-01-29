#ifndef SIMULA_UTIL_H

#include "GL/gl.h"
#include "EGL/egl.h"
#include "compositor.h"
#include "libweston-desktop.h"

void setup_weston_log_handler();

int32_t weston_desktop_surface_get_position_x(struct weston_desktop_surface* surface);
int32_t weston_desktop_surface_get_position_y(struct weston_desktop_surface* surface);
int32_t weston_desktop_surface_get_width(struct weston_desktop_surface* surface);
int32_t weston_desktop_surface_get_height(struct weston_desktop_surface* surface);


int32_t weston_surface_get_position_x(struct weston_surface* surface);
int32_t weston_surface_get_position_y(struct weston_surface* surface);
int32_t weston_surface_get_width(struct weston_surface* surface);
int32_t weston_surface_get_height(struct weston_surface* surface);

struct weston_gl_surface_state {
  GLfloat color[4];
  void *shader;
  GLuint textures[3];
  int num_textures;
};

// part of the gl_renderer struct
struct weston_gl_renderer {
  struct weston_renderer base;
  int fragment_shader_debug;
  int fan_debug;
  struct weston_binding *fragment_binding;
  struct weston_binding *fan_binding;
  
  EGLDisplay egl_display;
  EGLContext egl_context;
  EGLConfig egl_config;
};


struct weston_gl_output_state {
  EGLSurface egl_surface;
};

struct wl_shm_buffer* weston_buffer_get_shm_buffer(struct weston_buffer* buffer);
void* weston_buffer_get_legacy_buffer(struct weston_buffer* buffer);

EGLContext makeContext(EGLDisplay dp, EGLContext conf);

int wet_load_xwayland(struct weston_compositor *comp);

void pointer_send_motion(struct weston_pointer *pointer, uint32_t time, wl_fixed_t sx, wl_fixed_t sy);
#endif
