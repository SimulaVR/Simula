#include "Rts.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Hedlper function for GHC profiling flags
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

/* We build up a RTS command of form libGodotHaskellPlugin.so +RTS -N ... -RTS.
 *
 * 1. If SIMULA_HS_PROFILE_PREFIX is set/non-empty, we add `-l -s<prefix>.rts-stats -ol<prefix>.eventlog`
 * 2. If cost-centre profiling is also enabled (SIMULA_HS_PROFILE_COST_CENTER=1), we add `-l -s<prefix>.rts-stats -ol<prefix>.eventlog -p -po<prefix>.prof`
 * 3. What all this means:
 *
 *  - +RTS ... -RTS: delimit args for the Haskell runtime system (RTS).
 *  - -N: run RTS with multiple cores. We run this no matter what.
 *  - -l: enable RTS eventlog generation (a timestamped timeline of runtime events including GC start/end, scheduler activities, thread wakes/blocks, etc in an *.eventlog file)
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
  static char *argv[12];
  char **argv_ = argv;
  int argc = 0;

  argv[argc++] = "libGodotHaskellPlugin.so";
  argv[argc++] = "+RTS";
  argv[argc++] = "-N";

  if (enable_profile) {
    argv[argc++] = "-l";

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
