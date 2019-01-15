#include "HsFFI.h"

static void flib_init() __attribute__((constructor));
static void flib_init() {
  static char *argv[] = { "libSimula.so", 0 }, **argv_ = argv;
  static int argc = 1;
  hs_init(&argc, &argv_);
}

static void flib_fini() __attribute__((destructor));
static void flib_fini() {
  hs_exit();
}
