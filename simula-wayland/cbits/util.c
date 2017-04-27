#include "compositor.h"


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
