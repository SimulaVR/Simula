#!${pkgs.stdenv.shell}
${xdgAndSimulaEnvVars}

pkill monado-service
export SIMULA_CONFIG_PATH=$SIMULA_NIX_DIR/opt/simula/config/simula_monado_config.json
export XR_RUNTIME_JSON=$SIMULA_NIX_DIR/opt/simula/config/active_runtime.json
export XRT_COMPOSITOR_LOG=debug
export XRT_COMPOSITOR_SCALE_PERCENTAGE=100

# If --local is passed, use the monado binary compiled in ./submodules/monado
if [[ "${1:-}" == "--local" ]]; then
  shift  # remove --local so $@ now contains only user args
  MONADO_BINARY="./submodules/monado/build/src/xrt/targets/service/monado-service"
else
  MONADO_BINARY="${monado}/bin/monado-service"
fi

$MONADO_BINARY 2>&1 | tee "$SIMULA_LOG_DIR/monado.log"
