#include "HsFFI.h"

static void flib_init() __attribute__((constructor));
static void flib_init() {
  static char *argv[] = { "libGodotHaskellPlugin.so", 0}, **argv_ = argv;
  static int argc = sizeof(argv)/sizeof(argv[0]) - 1;
  hs_init_with_rtsopts(&argc, &argv_);
}

static void flib_fini() __attribute__((destructor));
static void flib_fini() {
  hs_exit();
}
