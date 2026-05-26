#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "Rts.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <unistd.h>

//Used in profiling to get TID of godot threads to compare with perf output
long simula_gettid(void) {
  return (long)syscall(SYS_gettid);
}

// Resolve a symbol from a specific already-loaded shared library (e.g. libgodot_openxr.so).
// Used in Monado's Frame Timing HUD.
void *simula_dlsym_library(const char *library_path, const char *symbol) {
  void *handle = dlopen(library_path, RTLD_NOW | RTLD_NOLOAD);
  if (handle == NULL) {
    return NULL;
  }
  void *result = dlsym(handle, symbol);
  dlclose(handle);
  return result;
}

// Resolve a symbol from the process-wide dynamic linker scope. The Haskell
// Monado HUD uses this as a fallback for finding godot-openxr frame timing
// functions.
void *simula_dlsym_rtld_default(const char *symbol) {
  return dlsym(RTLD_DEFAULT, symbol);
}

// Helper function for GHC profiling flags
static int env_is_enabled(const char *env_name) {
  const char *value = getenv(env_name);
  if (value == NULL) {
    return 0;
  }

  return strcmp(value, "1") == 0
      || strcmp(value, "true") == 0
      || strcmp(value, "TRUE") == 0
      || strcmp(value, "yes") == 0
      || strcmp(value, "YES") == 0;
}

static void flib_init() __attribute__((constructor)); //forces flib_init to execute when the library is loaded

/* We build up a RTS command of form libGodotHaskellPlugin.so +RTS -N1 ... -RTS.
 *
 * 1. If SIMULA_HS_PROFILE_PREFIX is set/non-empty, we add `-lnu -s<prefix>.rts-stats -ol<prefix>.eventlog`
 * 2. If cost-centre profiling is also enabled (SIMULA_HS_PROFILE_COST_CENTER=1), we add `-lnu -s<prefix>.rts-stats -ol<prefix>.eventlog -p -po<prefix>.prof`
 * 3. What all this means:
 *
 *  - +RTS ... -RTS: delimit args for the Haskell runtime system (RTS).
 *  - -N4: run RTS with four capabilities (GHC speak for threads). Avoid setting
      higher to avoid potential higher synchronization costs.
 *  - --nonmoving-gc: use the concurrent nonmoving collector for old-generation
 *    collections to test whether it reduces frame-visible GC pauses.
 *  - --long-gc-sync=0.008: print an RTS warning if GC synchronization takes
 *    longer than 8ms.
 *  - -T: maintain RTS stats so GHC.Stats can report cumulative GC time to our
 *    Monado Frame Timing HUD without enabling full eventlog output.
 *  - -lnu: enable RTS eventlog generation with the default event classes
 *    (including scheduler/thread events and GC events), plus nonmoving-GC
 *    events (-n) and user trace events (-u). This is larger than a GC-only log,
 *    but it lets us inspect what the RTS scheduler was doing during no-GC
 *    profile spikes.
 *  - -ol<file>: set eventlog output file path.
 *  - -s<file>: write RTS summary stats to that file (a one-shot aggregate report at program end in *.rts-stats form)
 *  - -p: enable cost-center time/allocation profiling (s.t. "cost centers" = functions/regions; outputs *.prof file)
 *  - -po<file>: set .prof output file path.
 *
 */
static void flib_init() {
  const char *profile_prefix = getenv("SIMULA_HS_PROFILE_PREFIX");
  const int enable_profile = (profile_prefix != NULL) && (profile_prefix[0] != '\0');
  const int enable_cost_center = enable_profile && env_is_enabled("SIMULA_HS_PROFILE_COST_CENTER");

  static char rts_stats_arg[4096];
  static char eventlog_arg[4096];
  static char prof_arg[4096];
  static char *argv[20];
  char **argv_ = argv;
  int argc = 0;

  argv[argc++] = "libGodotHaskellPlugin.so";
  argv[argc++] = "+RTS";
  argv[argc++] = "-N4";
  argv[argc++] = "--nonmoving-gc";
  argv[argc++] = "--long-gc-sync=0.008";
  argv[argc++] = "-T";

  if (enable_profile) {
    argv[argc++] = "-lnu";

    snprintf(rts_stats_arg, sizeof(rts_stats_arg), "-s%s.rts-stats", profile_prefix);
    snprintf(eventlog_arg, sizeof(eventlog_arg), "-ol%s.eventlog", profile_prefix);
    argv[argc++] = rts_stats_arg;
    argv[argc++] = eventlog_arg;
  }

  if (enable_cost_center) {
    argv[argc++] = "-p";
    snprintf(prof_arg, sizeof(prof_arg), "-po%s.prof", profile_prefix);
    argv[argc++] = prof_arg;
  }

  argv[argc++] = "-RTS";
  argv[argc] = 0;

  RtsConfig config = defaultRtsConfig;
  config.rts_opts_enabled = RtsOptsAll;
  hs_init_ghc(&argc, &argv_, config);
}

static void flib_fini() __attribute__((destructor));
static void flib_fini() {
  hs_exit();
}
