{
  description = "SimulaVR's Nix flake for Simula";

  nixConfig = {
    extra-substituters = [
      "https://simula.cachix.org"
    ];
    extra-trusted-public-keys = [
      "simula.cachix.org-1:Sr0SD5FIjc8cUVIeBHl8VJswQEJOBIE6u3wpmjslGBA="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/63dacb46bf939521bdc93981b4cbb7ecb58427a0";
    systems.url = "github:nix-systems/x86_64-linux";
    godot.url = "git+https://github.com/SimulaVR/godot?rev=d4bfd13c124cae3393aacfdf97433bb1e8f79d92&submodules=1";
    godot-haskell.url = "git+https://github.com/SimulaVR/godot-haskell?rev=b06876dcd2add327778aea03ba81751a60849cc8&submodules=1";
    monado.url = "git+https://github.com/SimulaVR/monado-xv?rev=cc3eca140e762a990bf107f5282a2ae4853e3893&submodules=1";
    environments = {
      url = "git+https://github.com/SimulaVR/environments?rev=91bb3777d558e809be12bcc94f6c984487994765";
      flake = false;
    };
    i3status-fork.url = "git+https://github.com/SimulaVR/i3status?rev=f734c9fe2580b6a23bcb1d1081376ae7897bdbf2";
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
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      imports = [ inputs.treefmt-nix.flakeModule ];

      perSystem =
        {
          pkgs,
          lib,
          system,
          ...
        }:
        let
          godot-haskell = inputs.godot-haskell.packages."${system}".godot-haskell;
          godot-haskell-plugin = pkgs.callPackage ./addons/godot-haskell-plugin { inherit godot-haskell; };

          # Needed to build godot-haskell-plugin locally inside a nix develop shell
          # TODO: Avoid code duplication with ./addons/godot-haskell-plugin/default.nix
          # 
          # `haskell-dependencies` contains shared libraries
          # This attribute is needed to pick up `${any-package}/lib/ghc-9.6.5/lib/x86_64-linux-ghc-9.6.5/*.so` for `pkgs.autoPatchelfHook`
          haskell-dependencies = pkgs.stdenvNoCC.mkDerivation rec {
            name = "haskell-dependencies";
            dontUnpack = true;

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
              cp -r ${
                lib.strings.concatStringsSep " " (
                  builtins.map (
                    drv:
                    "${drv}/lib/ghc-${pkgs.haskellPackages.ghc.version}/lib/${pkgs.stdenv.system}-ghc-${pkgs.haskellPackages.ghc.version}/*.so"
                  ) buildInputs
                )
              } $out/lib
            '';
          };

          # Wrapped packages with environment fixes
          xfce4-terminal-wrapped = pkgs.writeScriptBin "xfce4-terminal" ''
            #!${pkgs.stdenv.shell}
            export XDG_DATA_HOME=${pkgs.dejavu_fonts}/share
            cd $HOME
            exec ${pkgs.xfce.xfce4-terminal}/bin/xfce4-terminal "$@"
          '';

          midori-wrapped = pkgs.writeScriptBin "midori" ''
            #!${pkgs.stdenv.shell}
            export XDG_DATA_HOME=${pkgs.dejavu_fonts}/share
            exec ${pkgs.midori}/bin/midori "$@"
          '';

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

            pkill monado-service
            export SIMULA_CONFIG_PATH=$SIMULA_NIX_DIR/opt/simula/config/simula_monado_config.json
            export XR_RUNTIME_JSON=$SIMULA_NIX_DIR/opt/simula/config/active_runtime.json
            export XRT_COMPOSITOR_LOG=debug
            export XRT_COMPOSITOR_SCALE_PERCENTAGE=100

            # If --local is passed, use the monado binary compiled in ./submodules/monado
            if [[ "''${1:-}" == "--local" ]]; then
              shift  # remove --local so $@ now contains only user args
              MONADO_BINARY="./submodules/monado/build/src/xrt/targets/service/monado-service"
            else
              MONADO_BINARY="${monado}/bin/monado-service"
            fi

            $MONADO_BINARY 2>&1 | tee "$SIMULA_LOG_DIR/monado.log"
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
              #|| lib.hasSuffix ".so" baseName # ".so" cannot remove because dynamic libraries is used by Godot plugins
              || (type == "symlink" && lib.hasPrefix "result" baseName)
              || (type == "unknown")
            );

          copySimulaSourcesToNixStore = ''
            mkdir -p $out/opt/simula
            cp -r $src/* $src/.??* $out/opt/simula
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
          '';

          initiateSimulaRunner = ''
            #!/usr/bin/env sh

            # Exit `simula` launcher early if anything weird happens
            set -o errexit
            set -o nounset
            set -o pipefail
          '';

          monadoEnvVars = ''
            export SIMULA_CONFIG_PATH=./config/simula_monado_config.json
            export XR_RUNTIME_JSON=./config/active_runtime.json
            export XRT_COMPOSITOR_LOG=debug
            export XRT_COMPOSITOR_SCALE_PERCENTAGE=100
          '';

          xdgAndSimulaEnvVars = ''
            export XDG_CACHE_HOME=''${XDG_CACHE_HOME:-$HOME/.cache}
            export XDG_DATA_HOME=''${XDG_DATA_HOME:-$HOME/.local/share}
            export XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}

            export SIMULA_NIX_DIR="$(dirname "$(dirname "$(readlink -f "$0")")")"
            export SIMULA_LOG_DIR="$XDG_CACHE_HOME/Simula"
            export SIMULA_DATA_DIR="$XDG_DATA_HOME/Simula"
            export SIMULA_CONFIG_DIR="$XDG_CONFIG_HOME/Simula"
            export SIMULA_APP_DIR="$SIMULA_NIX_DIR/bin"

            echo "XDG_CACHE_HOME: $XDG_CACHE_HOME"
            echo "XDG_DATA_HOME: $XDG_DATA_HOME"
            echo "XDG_CONFIG_HOME: $XDG_CONFIG_HOME"
            echo "SIMULA_NIX_DIR: $SIMULA_NIX_DIR"
            echo "SIMULA_LOG_DIR: $SIMULA_LOG_DIR"
            echo "SIMULA_DATA_DIR: $SIMULA_DATA_DIR"
            echo "SIMULA_CONFIG_DIR: $SIMULA_CONFIG_DIR"
          '';

          addSimulaProgramsToPaths = inputs_: buildInputs_: ''
            export PATH="${
              lib.makeBinPath [
                inputs_.godot.packages."${system}".godot
                pkgs.xpra
                pkgs.xorg.xrdb
                pkgs.wmctrl
                pkgs.ffmpeg
                pkgs.synapse
                pkgs.xsel
                pkgs.mimic
                pkgs.xclip
                pkgs.curl
                xfce4-terminal-wrapped
                midori-wrapped
                i3status-wrapped
                pkgs.xorg.xkbcomp # needed to avoid some edgecase errors, IIRC
                pkgs.libv4l       # needed for webcam support
                pkgs.xwayland
              ]
            }:$PATH"
          '';

          # Needed to ensure fonts don't show up as blocks on certain non-NixOS distributions, IIRC
          fontEnvVars = ''
            export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
          '';

          copySimulaConfigFiles = ''
            # Copy over default config files if they don't already exist
            if [ ! -f "$SIMULA_CONFIG_DIR/HUD.config" ]; then
              cp "$SIMULA_NIX_DIR/config/HUD.config" "$SIMULA_CONFIG_DIR/HUD.config"
            fi

            if [ ! -f "$SIMULA_CONFIG_DIR/config.dhall" ]; then
              cp "$SIMULA_NIX_DIR/config/config.dhall" "$SIMULA_CONFIG_DIR/config.dhall"
            fi

            if [ ! -d "$SIMULA_DATA_DIR/environments" ]; then
              cp -R "$SIMULA_NIX_DIR/environments" "$SIMULA_DATA_DIR/environments"
            fi
          '';

          launchSimula = ''
            # Use --local if you want to launch Simula locally (with godot binary from ./submodules/godot)
            if [ "''${1:-}" = "--local" ]; then
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
          '';

          symLinkSimulaHelperPrograms = ''
            ln -s ${pkgs.xpra}/bin/xpra $out/bin/xpra
            ln -s ${pkgs.xorg.xrdb}/bin/xrdb $out/bin/xrdb
            ln -s ${pkgs.wmctrl}/bin/wmctrl $out/bin/wmctrl
            ln -s ${pkgs.ffmpeg-full}/bin/ffplay $out/bin/ffplay
            ln -s ${pkgs.ffmpeg-full}/bin/ffmpeg $out/bin/ffmpeg
            ln -s ${pkgs.synapse}/bin/synapse $out/bin/synapse
            ln -s ${pkgs.xsel}/bin/xsel $out/bin/xsel
            ln -s ${pkgs.mimic}/bin/mimic $out/bin/mimic
            ln -s ${pkgs.xclip}/bin/xclip $out/bin/xclip
            ln -s ${pkgs.patchelf}/bin/patchelf $out/bin/patchelf
            ln -s ${pkgs.dialog}/bin/dialog $out/bin/dialog
            ln -s ${pkgs.curl}/bin/curl $out/bin/curl

            ln -s ${xfce4-terminal-wrapped}/bin/xfce4-terminal $out/bin/xfce4-terminal
            ln -s ${midori-wrapped}/bin/midori $out/bin/midori
            ln -s ${i3status-wrapped}/bin/i3status $out/bin/i3status

            # Hack to fix some bug
            # (I can't remember but think it had something to do with preserving SIMULA_* values from being defined correctly?)
            # For some reason if symlink to wrapper, it wouldn't work right.
            echo '${simulaMonadoServiceContent}' > $out/bin/simula-monado-service
            chmod +x $out/bin/simula-monado-service
          '';

          simula = pkgs.stdenv.mkDerivation rec {
            pname = "simula";
            version = "0.0.0";

            src = lib.cleanSourceWith {
              filter = cleanSourceFilter;
              src = ./.;
            };

            nativeBuildInputs = [ pkgs.autoPatchelfHook ];

            buildInputs = [
              pkgs.systemd
              pkgs.openxr-loader
            ];

            dontBuild = true;

            installPhase = ''
              runHook preInstall

              ${copySimulaSourcesToNixStore}
              ${copyEnvironmentsToNixStore}
              ${placeGodotHaskellPluginLib}

              mkdir -p $out/bin
              cat > $out/bin/simula << 'EOF'
                ${initiateSimulaRunner}
                ${monadoEnvVars}
                ${xdgAndSimulaEnvVars}
                ${addSimulaProgramsToPaths inputs buildInputs}
                ${fontEnvVars}
                ${copySimulaConfigFiles}
                ${launchSimula} "$@"
              EOF
              chmod 755 $out/bin/simula

              ${symLinkSimulaHelperPrograms}

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
              ;
            default = simula;
          };

          devShells.default = pkgs.mkShell rec {
            nativeBuildInputs = [
              inputs.godot.packages."${system}".godot

              # Tools for devs to build godot-haskell-plugin locally via `just build`
              pkgs.nil
              pkgs.just
              pkgs.inotify-tools
              pkgs.cabal-install
              pkgs.haskellPackages.ghc
            ];

            buildInputs = [
              haskell-dependencies
              pkgs.zlib
            ];

            LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;

            shellHook = ''
              export PS1="\n[nix-shell:\w]$ "
            '';
          };
        };
    };
}