#ifndef SIMULA_UTIL_H

#include "GL/gl.h"
#include "libweston-desktop.h"

void setup_weston_log_handler();

int32_t weston_desktop_surface_get_position_x(struct weston_desktop_surface* surface);
int32_t weston_desktop_surface_get_position_y(struct weston_desktop_surface* surface);
int32_t weston_desktop_surface_get_width(struct weston_desktop_surface* surface);
int32_t weston_desktop_surface_get_height(struct weston_desktop_surface* surface);

struct weston_gl_surface_state {
  GLfloat color[4];
  void *shader;
  GLuint textures[3];
};

#endif
