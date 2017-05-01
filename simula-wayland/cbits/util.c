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
