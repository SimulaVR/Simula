{
  description = "Flake for SimulaVR/Simula";

  nixConfig = {
    extra-substituters = [ "https://simula.cachix.org" ];
    extra-trusted-public-keys = [ "simula.cachix.org-1:Sr0SD5FIjc8cUVIeBHl8VJswQEJOBIE6u3wpmjslGBA=" ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    systems.url = "github:nix-systems/x86_64-linux";
    godot.url = "git+https://github.com/SimulaVR/godot?rev=1172b6b60efb77c58aa69d10dfad8b9a9565dc40&submodules=1";
    godot-haskell = {
      url = "git+https://github.com/SimulaVR/godot-haskell?rev=ae307f7de0c10f4e91930a156ee37bee8c79be5f&submodules=1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    monado.url = "git+https://github.com/SimulaVR/monado-xv?rev=47b52aa79efa6f39fc99502442223207ae067e6b&submodules=1";
    environments = {
      url = "git+https://github.com/SimulaVR/environments?rev=91bb3777d558e809be12bcc94f6c984487994765";
      flake = false;
    };
    i3status-fork.url = "git+https://github.com/SimulaVR/i3status?rev=f734c9fe2580b6a23bcb1d1081376ae7897bdbf2";
    godot-openxr = {
      url = "git+https://github.com/SimulaVR/godot-openxr?submodules=1";
      flake = false;
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (
      { moduleWithSystem, ... }:
      {
        systems = import inputs.systems;

        imports = [ inputs.treefmt-nix.flakeModule ];

        perSystem =
          {
            config,
            pkgs,
            lib,
            system,
            ...
          }:
          let
            godot-haskell = inputs.godot-haskell.packages."${system}".godot-haskell;
            godot-openxr = pkgs.callPackage "${inputs.godot-openxr}/default.nix" {
              godot = inputs.godot;
            };
            godot-haskell-plugin = pkgs.callPackage ./addons/godot-haskell-plugin { inherit godot-haskell; };

            # Needed to build godot-haskell-plugin locally inside a nix develop shell
            # TODO: Avoid code duplication with ./addons/godot-haskell-plugin/default.nix
            #
            # `haskell-dependencies` contains shared libraries
            # This attribute is needed to pick up `${any-package}/lib/ghc-9.6.5/lib/x86_64-linux-ghc-9.6.5/*.so` for `pkgs.autoPatchelfHook`
            haskell-dependencies = pkgs.stdenvNoCC.mkDerivation rec {
              name = "haskell-dependencies";
              dontUnpack = true;

              ghcVersion = pkgs.haskellPackages.ghc.version;
              hostSystem = pkgs.stdenv.system;

              buildInputs = [
                # godot-haskell-plugin dependencies
                godot-haskell
                pkgs.haskellPackages.QuickCheck
                pkgs.haskellPackages.base64-bytestring
                pkgs.haskellPackages.clock
                pkgs.haskellPackages.dhall
                pkgs.haskellPackages.extra
                pkgs.haskellPackages.hspec
                pkgs.haskellPackages.hspec-core
                pkgs.haskellPackages.http-client
                pkgs.haskellPackages.http-client-tls
                pkgs.haskellPackages.http-types
                pkgs.haskellPackages.inline-c
                pkgs.haskellPackages.io-streams
                pkgs.haskellPackages.iso8601-time
                pkgs.haskellPackages.ordered-containers
                pkgs.haskellPackages.path
                pkgs.haskellPackages.path-io
                pkgs.haskellPackages.process-extras
                pkgs.haskellPackages.raw-strings-qq
                pkgs.haskellPackages.safe-exceptions
                pkgs.haskellPackages.uuid
              ];

              installPhase = ''
                mkdir -p $out/lib
                for drv in $buildInputs; do
                  find "$drv" -name "*.so" -exec cp {} $out/lib \;
                done
              '';
            };

            # Wrapped packages with environment fixes
            xfce4-terminal-wrapped = pkgs.writeScriptBin "xfce4-terminal" ''
              #!${pkgs.stdenv.shell}
              export XDG_DATA_HOME=${pkgs.dejavu_fonts}/share
              cd $HOME
              exec ${pkgs.xfce.xfce4-terminal}/bin/xfce4-terminal "$@"
            '';

            # midori-wrapped = pkgs.writeScriptBin "midori" ''
            #   #!${pkgs.stdenv.shell}
            #   export XDG_DATA_HOME=${pkgs.dejavu_fonts}/share
            #   exec ${pkgs.midori}/bin/midori "$@"
            # '';

            i3status-forked = inputs.i3status-fork.packages.${system}.default;

            i3status-wrapped = pkgs.writeScriptBin "i3status" ''
              #!${pkgs.stdenv.shell}
              export LC_ALL=C
              exec ${i3status-forked}/bin/i3status "$@"
            '';

            monado = inputs.monado.packages.${system}.default;

            simulaMonadoServiceContent = ''
              #!${pkgs.stdenv.shell}
              ${xdgAndSimulaEnvVars}
              ${monadoEnvVars}

              ${pkgs.procps}/bin/pkill monado-service
              export XRT_COMPOSITOR_LOG=debug
              export XRT_COMPOSITOR_SCALE_PERCENTAGE=100
              export XR_RUNTIME_JSON=${monado}/share/openxr/1/openxr_monado.json

              # If --local is passed, use the monado binary compiled in ./submodules/monado
              if [[ "''${1:-}" == "--local" ]]; then
                shift  # remove --local so $@ now contains only user args
                export XR_RUNTIME_JSON="./config/active_runtime.json" # override nix store location
                MONADO_BINARY="./submodules/monado/build/src/xrt/targets/service/monado-service"
              else
                MONADO_BINARY="${monado}/bin/monado-service"
              fi

              $MONADO_BINARY 2>&1 | ${pkgs.coreutils}/bin/tee "$SIMULA_LOG_DIR/monado.log"
            '';

            # `rgp` is used for Vulkan profiling on AMD GPUs
            mkRgpWrapper =
              {
                targetLabel,
                targetBin,
                logFile,
                extraEnv ? "",
                extraEcho ? "",
                extraCleanup ? "",
              }:
              ''
                #!${pkgs.stdenv.shell}
                set -euo pipefail

                ts="$(${pkgs.coreutils}/bin/date -u +%Y%m%dT%H%M%SZ)"
                out_dir="''${SIMULA_RGP_OUTPUT_DIR:-$PWD/profiling/rgp/$ts}"
                trigger_file="''${MESA_VK_TRACE_TRIGGER:-$out_dir/rgp.trigger}"
                before_list="$out_dir/rgp-before.txt"
                after_list="$out_dir/rgp-after.txt"
                target_log="$out_dir/${logFile}"

                ${pkgs.coreutils}/bin/mkdir -p "$out_dir"
                ${pkgs.coreutils}/bin/rm -f "$trigger_file"
                ${pkgs.findutils}/bin/find /tmp -maxdepth 1 -type f -name '*.rgp' | ${pkgs.coreutils}/bin/sort > "$before_list"

                export MESA_VK_TRACE="''${MESA_VK_TRACE:-rgp}"
                export MESA_VK_TRACE_TRIGGER="$trigger_file"
                ${extraEnv}
                target_bin="${targetBin}"

                echo "RGP output directory: $out_dir"
                echo "Trigger capture with: touch $trigger_file"
                echo "Using ${targetLabel} binary: $target_bin"
                ${extraEcho}
                target_pid=""

                cleanup() {
                  set +e
                  if [ -n "$target_pid" ]; then
                    ${pkgs.coreutils}/bin/kill "$target_pid" 2>/dev/null || true
                    wait "$target_pid" 2>/dev/null || true
                    target_pid=""
                  fi

                  ${extraCleanup}

                  ${pkgs.findutils}/bin/find /tmp -maxdepth 1 -type f -name '*.rgp' | ${pkgs.coreutils}/bin/sort > "$after_list"
                  ${pkgs.coreutils}/bin/comm -13 "$before_list" "$after_list" | while read -r path; do
                    if [ -n "$path" ]; then
                      ${pkgs.coreutils}/bin/mv "$path" "$out_dir/"
                    fi
                  done

                  ${pkgs.coreutils}/bin/ls -1 "$out_dir"/*.rgp 2>/dev/null || echo "No new .rgp captures found."
                }
                trap cleanup EXIT

                set +e
                "$target_bin" "$@" > >(${pkgs.coreutils}/bin/tee -a "$target_log") 2>&1 &
                target_pid="$!"
                wait "$target_pid"
                status="$?"
                target_pid=""
                set -e

                exit $status
              '';

            simulaMonadoServiceRgpContent = mkRgpWrapper {
              targetLabel = "monado service";
              targetBin = "./result/bin/simula-monado-service";
              logFile = "monado-wrapper.log";
              extraEnv = ''
                export XRT_NO_STDIN="''${XRT_NO_STDIN:-1}"
              '';
              extraEcho = ''
                echo "XRT_COMPOSITOR_LOG=$XRT_COMPOSITOR_LOG"
              '';
              extraCleanup = ''
                # Ensure that rgp stops recording when the wrapper script terminates
                ${pkgs.procps}/bin/pkill monado-service 2>/dev/null || true
              '';
            };

            simulaRgpContent = mkRgpWrapper {
              targetLabel = "simula";
              targetBin = "./result/bin/simula";
              logFile = "simula-wrapper.log";
            };

            simulaMonadoServiceRgpTool = pkgs.writeShellScriptBin "simula-monado-service-rgp" simulaMonadoServiceRgpContent;
            simulaRgpTool = pkgs.writeShellScriptBin "simula-rgp" simulaRgpContent;
            awscli = if pkgs ? awscli2 then pkgs.awscli2 else pkgs.awscli;
            # Provide sources from certain nix supplied libs for penosco visiblity
            simulaPernoscoSourceTree = pkgs.stdenvNoCC.mkDerivation {
              pname = "simula-pernosco-source-tree";
              version = "0.0.0";
              dontUnpack = true;

              installPhase = ''
                mkdir -p $out/srcs/xwayland
                ${pkgs.gnutar}/bin/tar -xf ${pkgs.xwayland.src} --directory $out/srcs/xwayland --strip-components=1

                mkdir -p $out/srcs/libxcb
                ${pkgs.gnutar}/bin/tar -xf ${pkgs.xorg.libxcb.src} --directory $out/srcs/libxcb --strip-components=1

                mkdir -p $out/srcs/wayland
                ${pkgs.gnutar}/bin/tar -xf ${pkgs.wayland.src} --directory $out/srcs/wayland --strip-components=1
              '';
            };

            simulaPernoscoSubmit = pkgs.writeShellScript "simula-pernosco-submit" ''
              set -euo pipefail

              project_root="''${SIMULA_PROJECT_ROOT:-$PWD}"
              trace_dir=""
              global_args=()
              upload_args=()
              show_help=0
              extra_store_source_dirs=()

              # Support e.g. `./simula-pernosco-submit --title "Broken popup" ./rr/latest-trace`
              while [ "$#" -gt 0 ]; do
                case "$1" in
                  -x|--ignore-warnings)
                    global_args+=("$1")
                    ;;
                  --title|--url|--dry-run|--substitute)
                    opt="$1"
                    upload_args+=("$opt")
                    shift
                    if [ "$#" -eq 0 ]; then
                      echo "Missing value for $opt" >&2
                      exit 1
                    fi
                    upload_args+=("$1")
                    ;;
                  --consent-to-current-privacy-policy|--no-local-sources|--help|-h)
                    upload_args+=("$1")
                    if [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
                      show_help=1
                    fi
                    ;;
                  -*)
                    upload_args+=("$1")
                    ;;
                  *)
                    if [ -z "$trace_dir" ]; then
                      trace_dir="$1"
                    else
                      upload_args+=("$1")
                    fi
                    ;;
                esac
                shift
              done

              if [ "$show_help" -eq 1 ]; then
                exec ${pkgs.python3}/bin/python3 "$project_root/submodules/pernosco-submit/pernosco-submit" \
                  "''${global_args[@]}" \
                  upload \
                  "''${upload_args[@]}"
              fi

              if [ -z "$trace_dir" ]; then
                trace_dir="$project_root/rr/latest-trace"
              elif [[ "$trace_dir" != /* ]]; then
                trace_dir="$PWD/$trace_dir"
              fi

              if [ ! -e "$trace_dir" ]; then
                echo "Trace directory not found: $trace_dir" >&2
                exit 1
              fi

              pernosco_submit="$project_root/submodules/pernosco-submit/pernosco-submit"
              if [ ! -e "$pernosco_submit" ]; then
                echo "Couldn't find pernosco-submit at $pernosco_submit" >&2
                exit 1
              fi

              wlroots_lib="$project_root/submodules/wlroots/build/libwlroots.so.0"
              if [ ! -e "$wlroots_lib" ]; then
                echo "Expected wlroots build artifact at $wlroots_lib" >&2
                exit 1
              fi

              source_root="${simulaPernoscoSourceTree}/srcs"
              wlroots_so_name="$(${pkgs.coreutils}/bin/basename "$(${pkgs.coreutils}/bin/readlink -f "$wlroots_lib")")"
              wayland_client_so_name="$(${pkgs.coreutils}/bin/basename "$(${pkgs.coreutils}/bin/readlink -f ${pkgs.wayland}/lib/libwayland-client.so)")"
              wayland_egl_so_name="$(${pkgs.coreutils}/bin/basename "$(${pkgs.coreutils}/bin/readlink -f ${pkgs.wayland}/lib/libwayland-egl.so)")"
              wayland_server_so_name="$(${pkgs.coreutils}/bin/basename "$(${pkgs.coreutils}/bin/readlink -f ${pkgs.wayland}/lib/libwayland-server.so)")"
              libxcb_so_name="$(${pkgs.coreutils}/bin/basename "$(${pkgs.coreutils}/bin/readlink -f ${pkgs.xorg.libxcb}/lib/libxcb.so)")"

              shopt -s nullglob
              for pattern in /nix/store/*-wlroots-* /nix/store/*-wayland-* /nix/store/*-libxcb-*; do
                for path in $pattern; do
                  if [ -e "$path" ]; then
                    extra_store_source_dirs+=("$path")
                  fi
                done
              done
              shopt -u nullglob

              export PATH="${lib.makeBinPath [
                awscli
                pkgs.openssl
                pkgs.gnutar
                pkgs.zstd
                pkgs.git
                pkgs.mercurial
                pkgs.rr
                pkgs.coreutils
              ]}:$PATH"

              exec ${pkgs.python3}/bin/python3 "$pernosco_submit" \
                "''${global_args[@]}" \
                upload \
                --substitute="$wlroots_so_name=$project_root/submodules/wlroots/backend" \
                --substitute="$wayland_client_so_name=$source_root/wayland/src" \
                --substitute="$wayland_egl_so_name=$source_root/wayland/egl" \
                --substitute="$wayland_server_so_name=$source_root/wayland/src" \
                --substitute="$libxcb_so_name=$source_root/libxcb/src" \
                --substitute="Xwayland=$source_root/xwayland/doc" \
                "''${upload_args[@]}" \
                "$trace_dir" \
                "$project_root" \
                "$project_root/submodules/wlroots" \
                "$source_root" \
                "$source_root/wayland/src" \
                "$source_root/wayland/egl" \
                "$source_root/libxcb/src" \
                "$source_root/xwayland/doc" \
                "${pkgs.wayland}" \
                "${pkgs.xorg.libxcb}" \
                "${pkgs.wlroots}" \
                "''${extra_store_source_dirs[@]}"
            '';

            cleanSourceFilter =
              name: type:
              let
                baseName = baseNameOf (toString name);
              in
              !(
                (baseName == ".git")
                || lib.hasSuffix "~" baseName
                || builtins.match "^\\.sw[a-z]$" baseName != null
                || builtins.match "^\\..*\\.sw[a-z]$" baseName != null
                || lib.hasSuffix ".o" baseName
                || (baseName == "rr")
                #|| lib.hasSuffix ".so" baseName # ".so" cannot remove because dynamic libraries is used by Godot plugins
                || (type == "symlink" && lib.hasPrefix "result" baseName)
                || (type == "symlink" && baseName == "simula-pernosco-submit")
                || (type == "unknown")
              );

            copySimulaSourcesToNixStore = ''
              mkdir -p $out/opt/simula
              cp -r $src/* $src/.??* $out/opt/simula
              chmod -R u+w $out/opt/simula # Ensure things are writable so environments can be copied over next
            '';

            copyEnvironmentsToNixStore = ''
              mkdir -p $out/opt/simula/environments
              cp -r ${inputs.environments}/* $out/opt/simula/environments/
              chmod -R 755 $out/opt/simula/environments
            '';

            placeGodotHaskellPluginLib = ''
              chmod -R +w $out/opt/simula/addons/godot-haskell-plugin/bin/x11/
              cp ${godot-haskell-plugin}/lib/ghc-*/lib/libgodot-haskell-plugin.so $out/opt/simula/addons/godot-haskell-plugin/bin/x11/libgodot-haskell-plugin.so
              chmod 755 $out/opt/simula/addons/godot-haskell-plugin/bin/x11/libgodot-haskell-plugin.so

              # Replace pre-compiled libgodot_openxr.so with our source-built one
              chmod -R +w $out/opt/simula/addons/godot-openxr/bin/linux/
              cp ${godot-openxr}/bin/linux/libgodot_openxr.so $out/opt/simula/addons/godot-openxr/bin/linux/libgodot_openxr.so
              chmod 755 $out/opt/simula/addons/godot-openxr/bin/linux/libgodot_openxr.so
            '';

            initiateSimulaRunner = ''
              #!/usr/bin/env bash

              # Exit `simula` launcher early if anything weird happens
              set -o errexit
              set -o nounset
              set -o pipefail
            '';

            mkMonadoEnvVars =
              simulaRoot:
              ''
                export SIMULA_CONFIG_PATH=${simulaRoot}/opt/simula/config/simula_monado_config.json
                export XRT_COMPOSITOR_LOG=debug
                export XRT_COMPOSITOR_SCALE_PERCENTAGE=100
              '';

            monadoEnvVars = mkMonadoEnvVars "@out@";

            mkXdgAndSimulaEnvVars =
              simulaRoot:
              ''
                export XDG_CACHE_HOME=''${XDG_CACHE_HOME:-$HOME/.cache}
                export XDG_DATA_HOME=''${XDG_DATA_HOME:-$HOME/.local/share}
                export XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}

                export SIMULA_NIX_DIR="${simulaRoot}"
                export SIMULA_LOG_DIR="$XDG_CACHE_HOME/Simula"
                export SIMULA_DATA_DIR="$XDG_DATA_HOME/Simula"
                export SIMULA_CONFIG_DIR="$XDG_CONFIG_HOME/Simula"

                echo "XDG_CACHE_HOME: $XDG_CACHE_HOME"
                echo "XDG_DATA_HOME: $XDG_DATA_HOME"
                echo "XDG_CONFIG_HOME: $XDG_CONFIG_HOME"
                echo "SIMULA_NIX_DIR: $SIMULA_NIX_DIR"
                echo "SIMULA_LOG_DIR: $SIMULA_LOG_DIR"
                echo "SIMULA_DATA_DIR: $SIMULA_DATA_DIR"
                echo "SIMULA_CONFIG_DIR: $SIMULA_CONFIG_DIR"
              '';

            xdgAndSimulaEnvVars = mkXdgAndSimulaEnvVars ''$(dirname "$(dirname "$(readlink -f "$0")")")'';

            ensureRuntimePaths = binPath: libPath: ''
              # Ensure runtime tools/libs are visible to Godot and its child shells
              export PATH="${binPath}''${PATH:+:$PATH}"
              export LD_LIBRARY_PATH="${libPath}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
            '';

            # Needed to ensure fonts don't show up as blocks on certain non-NixOS distributions, IIRC
            fontEnvVars = ''
              export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
            '';

            copySimulaConfigFiles = ''
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
            '';

            parseSimulaLaunchArgs = ''
              # Support e.g. `./result/bin/simula --local --rr -- --godot --args --here`
              LAUNCH_LOCAL=0
              RUN_UNDER_RR=0
              simula_args=()

              while [ "$#" -gt 0 ]; do
                case "$1" in
                  -h|--help)
                    cat << _EOF_HELP_
Usage: simula [options] [-- [godot-args]]

Options:
  --local        Launch Simula using local binaries (useful for developers)
  --rr           Launch Simula under the rr debugger
  -h, --help     Show this help message

Debug Environment Variables:
- SIMULA_DEBUG_MEMORY=1: Shows per-second RSS memory deltas in the HUD so you can get an estimate of how various actions impact top level memory usage.
- SIMULA_DEBUG_SURFACE_BOUNDARIES=1: Shows wlr_surface (red), geometry boundaries with offset x/y offset (green), and geometry boundaries without x/y offset (blue) on all surfaces. Top-level XDG boundaries are colored in yellow (for wayland surfaces), while XWayland are colored black (though since most xwayland top levels don't have borders to begin with, you often won't see the black border).
- SIMULA_DEBUG_SURFACE_CREATIONS=1: Prints detailed surface information when they get created/mapped.
- SIMULA_DEBUG_MOUSE_EVENTS=1 Shows detailed tracing of mouse interaction logic.
_EOF_HELP_
                    exit 0
                    ;;
                  --local)
                    LAUNCH_LOCAL=1
                    ;;
                  --rr)
                    RUN_UNDER_RR=1
                    ;;
                  --)
                    shift
                    while [ "$#" -gt 0 ]; do
                      simula_args+=("$1")
                      shift
                    done
                    break
                    ;;
                  *)
                    simula_args+=("$1")
                    ;;
                esac
                shift
              done

              set -- "''${simula_args[@]}"
            '';

            resolveSimulaLaunchTarget = ''
              # Use --local if you want to launch Simula locally (with godot binary from ./submodules/godot)
              if [ "$LAUNCH_LOCAL" -eq 1 ]; then
                export IPC_IGNORE_VERSION=1
                export XR_RUNTIME_JSON="./config/active_runtime.json"
                GODOT_BINARY="./submodules/godot/bin/godot.x11.tools.64"
                PROJECT_PATH="./."
              # Otherwise, use the nix store for everything
              else
                export XR_RUNTIME_JSON="${monado}/share/openxr/1/openxr_monado.json"
                GODOT_BINARY="${inputs.godot.packages."${system}".godot}/bin/godot"
                PROJECT_PATH="$SIMULA_NIX_DIR/opt/simula"

                # Ensure that we're in the right directory before launching so the relative res:// paths work correctly
                # (I tried using absolute paths instead of res://, but godot doesn't seem to play well so we use this hack)
                cd "$PROJECT_PATH"
                PROJECT_PATH="./."
              fi
            '';

            launchResolvedSimula = ''
              # We `script` (+ stdbuf and ansi2text) to tee the output into the console (colorized) and into our log files (non-colorized)
              if grep -qi NixOS /etc/os-release; then
                ${pkgs.util-linux}/bin/script -qfc "$GODOT_BINARY -m \"$PROJECT_PATH\" $@" >(${pkgs.coreutils}/bin/stdbuf -oL -eL ${pkgs.colorized-logs}/bin/ansi2txt > "$SIMULA_DATA_DIR/log/output.file")
              else
                echo "Detected non-NixOS distribution, so running Simula with nixGL"
                nix run --impure github:nix-community/nixGL -- ${pkgs.util-linux}/bin/script -qfc "$GODOT_BINARY -m \"$PROJECT_PATH\" $@" >(${pkgs.coreutils}/bin/stdbuf -oL -eL ${pkgs.colorized-logs}/bin/ansi2txt > "$SIMULA_DATA_DIR/log/output.file")
              fi
            '';

            launchSimulaWithRr = ''
              trace_dir="''${SIMULA_RR_TRACE_DIR:-$PWD/rr}"
              mkdir -p "$trace_dir"
              export _RR_TRACE_DIR="$trace_dir"
              # Let godot/simula know we're running under rr (which skips VR init and enables debug printing)
              export RUNNING_UNDER_RR=1
              # Skip detect_prime() GLX forks, which uses MIT-SHM and cause rr replay issues
              export DRI_PRIME=0
              rr_record_args=(
                record
                # Tell rr to ignore SIGUSR1 so as to not get tripped up by XWayland emitting it during its normal startup
                -i
                SIGUSR1
              )
              # Prevent PipeWire from using shared memory (which causes rr replay issues)
              godot_rr_args=(--audio-driver Dummy)

              echo "rr trace directory: $trace_dir"
              echo "Using Godot binary: $GODOT_BINARY"
              echo "Using project path: $PROJECT_PATH"

              if grep -qi NixOS /etc/os-release; then
                exec ${pkgs.rr}/bin/rr "''${rr_record_args[@]}" "$GODOT_BINARY" --args -m "$PROJECT_PATH" "''${godot_rr_args[@]}" "$@"
              else
                echo "Detected non-NixOS distribution, so running Simula with nixGL"
                exec nix run --impure github:nix-community/nixGL -- ${pkgs.rr}/bin/rr "''${rr_record_args[@]}" "$GODOT_BINARY" --args -m "$PROJECT_PATH" "''${godot_rr_args[@]}" "$@"
              fi
            '';

            launchSimula = ''
              ${parseSimulaLaunchArgs}
              ${resolveSimulaLaunchTarget}

              if [ "$RUN_UNDER_RR" -eq 1 ]; then
                ${launchSimulaWithRr}
              else
                ${launchResolvedSimula}
              fi
            '';

            simula = pkgs.stdenv.mkDerivation rec {
              pname = "simula";
              version = "0.0.0";

              src = lib.cleanSourceWith {
                filter = cleanSourceFilter;
                src = ./.;
              };

              nativeBuildInputs = [
                pkgs.autoPatchelfHook
                pkgs.makeWrapper
              ];

              buildInputs = [
                pkgs.systemd
                pkgs.openxr-loader
              ];

              dontBuild = true;

              # Force certain nix programs that Simula needs at runtime to be in the front of the user's PATH
              # (We use this strategy instead of just adding the programs to bin/* to avoid
              # potential conflicts if the user has already installed them via nix)
              passthru.simulaRuntimePrograms = with pkgs; [
                xpra
                xorg.xrdb
                wmctrl
                ffmpeg
                synapse
                xsel
                mimic
                xclip
                xfce4-terminal-wrapped
                # midori-wrapped
                i3status-wrapped
                xorg.xkbcomp
                xwayland
              ];

              passthru.simulaRuntimeLibs = with pkgs; [
                openxr-loader
                libv4l
              ];

              installPhase = ''
                runHook preInstall

                ${copySimulaSourcesToNixStore}
                rm -f $out/opt/simula/addons/godot-haskell-plugin/godot-haskell-gdwlroots.tar.gz
                ${copyEnvironmentsToNixStore}
                ${placeGodotHaskellPluginLib}

                mkdir -p $out/bin
                cat > $out/bin/simula-unwrapped << 'EOF'
                ${initiateSimulaRunner}
                ${monadoEnvVars}
                ${xdgAndSimulaEnvVars}
                ${ensureRuntimePaths (lib.makeBinPath passthru.simulaRuntimePrograms) (
                  lib.makeLibraryPath passthru.simulaRuntimeLibs
                )}
                ${fontEnvVars}
                ${copySimulaConfigFiles}
                ${launchSimula} "$@"
                EOF
                chmod 755 $out/bin/simula-unwrapped

                # A wrapped `simula` includes various helper programs that Simula
                # needs guarantees are available at runtime.
                makeWrapper $out/bin/simula-unwrapped $out/bin/simula \
                --prefix PATH : ${lib.makeBinPath passthru.simulaRuntimePrograms} \
                --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath passthru.simulaRuntimeLibs}

                cat > $out/bin/simula-monado-service << 'EOF'
                ${simulaMonadoServiceContent}
                EOF
                chmod 755 $out/bin/simula-monado-service

                substituteInPlace $out/bin/simula-unwrapped --replace '@out@' "$out"
                substituteInPlace $out/bin/simula-monado-service --replace '@out@' "$out"

                runHook postInstall
              '';

              meta = {
                mainProgram = "simula";
                homepage = "https://github.com/SimulaVR/Simula";
                license = lib.licenses.mit;
                platforms = lib.platforms.linux;
              };
            };

          in
          {
            _module.args.pkgs = import inputs.nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };

            treefmt = {
              projectRootFile = "project.godot";
              programs.nixfmt.enable = true;
            };

            packages = {
              inherit
                simula
                godot-haskell
                godot-haskell-plugin
                godot-openxr
                ;
              simula-pernosco-submit = simulaPernoscoSubmit;
              simula-rgp = simulaRgpTool;
              simula-monado-service-rgp = simulaMonadoServiceRgpTool;
              default = simula;
              treefmt = config.treefmt.build.wrapper;
            };

            apps = {
              simula-pernosco-submit = {
                type = "app";
                program = "${simulaPernoscoSubmit}";
              };
              simula-rgp = {
                type = "app";
                program = "${simulaRgpTool}/bin/simula-rgp";
              };
              simula-monado-service-rgp = {
                type = "app";
                program = "${simulaMonadoServiceRgpTool}/bin/simula-monado-service-rgp";
              };
            };

            devShells.default = pkgs.haskellPackages.shellFor {
              packages = p: [ godot-haskell-plugin ];
              nativeBuildInputs = [
                inputs.godot.packages."${system}".godot

                # Tools for devs to build godot-haskell-plugin locally via `just build`
                pkgs.nil
                pkgs.just
                pkgs.inotify-tools
                pkgs.cabal-install
                pkgs.rgp
              ];

              buildInputs = [
                pkgs.zlib
              ];

              shellHook = ''
                export PS1="\n[nix-shell:\w]$ "
              '';
            };
          };
      }
    );
}
