# Use --local if you want to launch Simula locally (with godot binary from ./submodules/godot)
if [ "${1:-}" = "--local" ]; then
  GODOT_BINARY="./submodules/godot/bin/godot.x11.tools.64"
  PROJECT_PATH="./."
  shift  # remove --local so $@ now contains only user args
# Otherwise, use the nix store for everything
else
  GODOT_BINARY="${inputs.godot.packages."${system}".godot}/bin/godot"
  PROJECT_PATH="$SIMULA_NIX_DIR/opt/simula"

  # Ensure that we're in the right directory before launching so the relative res:// paths work correctly
  # (I tried using absolute paths instead of res://, but godot doesn't seem to play well so we use this hack)
  cd "$PROJECT_PATH"
  PROJECT_PATH="./."
fi

# We `script` (+ stdbuf and ansi2text) to tee the output into the console (colorized) and into our log files (non-colorized)
if grep -qi NixOS /etc/os-release; then
  ${pkgs.util-linux}/bin/script -qfc "$GODOT_BINARY -m \"$PROJECT_PATH\" $@" >(${pkgs.coreutils}/bin/stdbuf -oL -eL ${pkgs.colorized-logs}/bin/ansi2txt > "$SIMULA_DATA_DIR/log/output.file")
else
  echo "Detected non-NixOS distribution, so running Simula with nixGL"
  nix run --impure github:nix-community/nixGL -- ${pkgs.util-linux}/bin/script -qfc "$GODOT_BINARY -m \"$PROJECT_PATH\" $@" >(${pkgs.coreutils}/bin/stdbuf -oL -eL ${pkgs.colorized-logs}/bin/ansi2txt > "$SIMULA_DATA_DIR/log/output.file")
fi
