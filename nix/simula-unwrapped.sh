#!/usr/bin/env sh

# Exit `simula` launcher early if anything weird happens
set -o errexit
set -o nounset
set -o pipefail

# Monado Env Vars
export SIMULA_CONFIG_PATH=./config/simula_monado_config.json
export XR_RUNTIME_JSON=./config/active_runtime.json
export XRT_COMPOSITOR_LOG=debug
export XRT_COMPOSITOR_SCALE_PERCENTAGE=100

# XDG and Simula Env Vars
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}

export SIMULA_NIX_DIR="$(dirname \"$(dirname \"$(readlink -f \"$0\")\")\")"
export SIMULA_LOG_DIR="$XDG_CACHE_HOME/Simula"
export SIMULA_DATA_DIR="$XDG_DATA_HOME/Simula"
export SIMULA_CONFIG_DIR="$XDG_CONFIG_HOME/Simula"

# Font Env Vars
export LOCALE_ARCHIVE=@glibcLocales@/lib/locale/locale-archive

# Copy over default config files if they don't already exist
if [ ! -f "$SIMULA_CONFIG_DIR/HUD.config" ]; then
  mkdir -p "$SIMULA_CONFIG_DIR"
  cp "$SIMULA_NIX_DIR/opt/simula/config/HUD.config" "$SIMULA_CONFIG_DIR/HUD.config"
fi

if [ ! -f "$SIMULA_CONFIG_DIR/config.dhall" ]; then
  mkdir -p "$SIMULA_CONFIG_DIR"
  cp "$SIMULA_NIX_DIR/opt/simula/config/config.dhall" "$SIMULA_CONFIG_DIR/config.dhall"
fi

if [ ! -d "$SIMULA_DATA_DIR/environments" ]; then
  mkdir -p "$SIMULA_DATA_DIR"
  cp -R "$SIMULA_NIX_DIR/opt/simula/environments" "$SIMULA_DATA_DIR/environments"
fi

# Launch Simula
# Use --local if you want to launch Simula locally (with godot binary from ./submodules/godot)
if [ "${1:-}" = "--local" ]; then
  GODOT_BINARY="./submodules/godot/bin/godot.x11.tools.64"
  PROJECT_PATH="./."
  shift  # remove --local so $@ now contains only user args
# Otherwise, use the nix store for everything
else
  GODOT_BINARY="@godot@/bin/godot"
  PROJECT_PATH="$SIMULA_NIX_DIR/opt/simula"

  cd "$PROJECT_PATH"
  PROJECT_PATH="./."
fi

# We `script` (+ stdbuf and ansi2text) to tee the output into the console (colorized) and into our log files (non-colorized)
if grep -qi NixOS /etc/os-release; then
  @util-linux@/bin/script -qfc "$GODOT_BINARY -m \"$PROJECT_PATH\" $@" >(@coreutils@/bin/stdbuf -oL -eL @colorized-logs@/bin/ansi2txt > "$SIMULA_DATA_DIR/log/output.file")
else
  echo "Detected non-NixOS distribution, so running Simula with nixGL"
  nix run --impure github:nix-community/nixGL -- @util-linux@/bin/script -qfc "$GODOT_BINARY -m \"$PROJECT_PATH\" $@" >(@coreutils@/bin/stdbuf -oL -eL @colorized-logs@/bin/ansi2txt > "$SIMULA_DATA_DIR/log/output.file")
fi
