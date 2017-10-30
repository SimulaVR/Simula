
#include "GL/glew.h"
#include "util.h"
#include "compositor.h"
#include "libweston-desktop.h"

// from weston
static int
vlog(const char *fmt, va_list ap)
{
  return vprintf(fmt, ap);
}

static int
vlog_continue(const char *fmt, va_list argp)
{
  return vprintf(fmt, argp);
}

void setup_weston_log_handler() {
  weston_log_set_handler(vlog, vlog_continue);
}

int32_t weston_desktop_surface_get_position_x(struct weston_desktop_surface* surface) {
  return weston_desktop_surface_get_geometry(surface).x;
}

int32_t weston_desktop_surface_get_position_y(struct weston_desktop_surface* surface) {
  return weston_desktop_surface_get_geometry(surface).y;
}

int32_t weston_desktop_surface_get_width(struct weston_desktop_surface* surface) {
  return weston_desktop_surface_get_geometry(surface).width;
}

int32_t weston_desktop_surface_get_height(struct weston_desktop_surface* surface) {
  return weston_desktop_surface_get_geometry(surface).height;
}


int32_t weston_surface_get_position_x(struct weston_surface* surface) {
  return weston_surface_get_bounding_box(surface).x;
}

int32_t weston_surface_get_position_y(struct weston_surface* surface) {
  return weston_surface_get_bounding_box(surface).y;
}

int32_t weston_surface_get_width(struct weston_surface* surface) {
  return weston_surface_get_bounding_box(surface).width;
}

int32_t weston_surface_get_height(struct weston_surface* surface) {
  return weston_surface_get_bounding_box(surface).height;
}

struct wl_shm_buffer* weston_buffer_get_shm_buffer(struct weston_buffer* buffer) {
  return buffer->shm_buffer;
}

void* weston_buffer_get_legacy_buffer(struct weston_buffer* buffer) {
  return buffer->legacy_buffer;
}
 
GLenum glewInitExperimental() {
  glewExperimental = GL_TRUE;
  return glewInit();
}

EGLContext makeContext(EGLDisplay display, EGLContext shared) {
  EGLConfig config;
  EGLint num;
  const EGLint attribs[] = {EGL_SURFACE_TYPE, EGL_WINDOW_BIT, EGL_RED_SIZE, 1, EGL_GREEN_SIZE, 1, EGL_BLUE_SIZE, 1, EGL_ALPHA_SIZE, 1, EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT, EGL_NONE };
  if(!eglChooseConfig(display, attribs, &config, 1, &num))
    return NULL;

  const EGLint contextAttr[] = {EGL_CONTEXT_MAJOR_VERSION, 4, EGL_CONTEXT_MINOR_VERSION, 1, EGL_CONTEXT_OPENGL_PROFILE_MASK, EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT, EGL_NONE };
  return eglCreateContext(display, config, shared, contextAttr);

}
