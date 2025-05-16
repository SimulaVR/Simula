{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/63dacb46bf939521bdc93981b4cbb7ecb58427a0";
    systems.url = "github:nix-systems/x86_64-linux";
    godot.url = "git+https://github.com/haruki7049/godot?rev=df193d656aad69c0efe33fa9278907c2341d7ce9&submodules=1";
    godot-haskell.url = "git+https://github.com/haruki7049/godot-haskell?rev=b06876dcd2add327778aea03ba81751a60849cc8&submodules=1";
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

          # `haskell-dependencies` contains shared libraries
          # This attribute is needed to pick up `${any-package}/lib/ghc-9.6.5/lib/x86_64-linux-ghc-9.6.5/*.so` for `pkgs.autoPatchelfHook`
          haskell-dependencies = pkgs.stdenvNoCC.mkDerivation rec {
            name = "haskell-dependencies";
            dontUnpack = true;

            buildInputs = [
              # godot-haskell-plugin dependencies
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
              godot-haskell
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

          # A source filter for Simula
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

          # Simula package, with some tools:
          # | Package name             |
          # |--------------------------|
          # | pkgs.xpra                |
          # | pkgs.xfce.xfce4-terminal |
          # | pkgs.xorg.xrdb           |
          # | pkgs.wmctrl              |
          # | pkgs.ffmpeg              |
          # | pkgs.ffmpeg              |
          # | pkgs.midori              |
          # | pkgs.synapse             |
          # | pkgs.xsel                |
          # | pkgs.mimic               |
          # | pkgs.xclip               |
          # | pkgs.curl                |
          # | pkgs.i3status            |
          simula = pkgs.stdenv.mkDerivation rec {
            pname = "simula";
            version = "0.0.0-dev";

            # `lib.cleanSource` omits `.so` files such as `addons/gdleapmotion/bin/x11/libgdleapmotion.so`
            #src = lib.cleanSource ./.;
            src = lib.cleanSourceWith {
              filter = cleanSourceFilter;
              src = ./.;
            };

            nativeBuildInputs = [ pkgs.autoPatchelfHook ];

            buildInputs = [
              haskell-dependencies
              pkgs.systemd
              pkgs.openxr-loader
            ];

            dontBuild = true;

            installPhase = ''
              runHook preInstall

              mkdir -p $out/opt/simula
              cp -r $src/* $src/.??* $out/opt/simula

              # Install godot-haskell-plugin from the result by currently source code
              chmod 755 $out/opt/simula/addons/godot-haskell-plugin/bin/x11/libgodot-haskell-plugin.so
              find ${godot-haskell-plugin} -name libgodot-haskell-plugin.so | xargs -i cp {} $out/opt/simula/addons/godot-haskell-plugin/bin/x11/libgodot-haskell-plugin.so

              # Install Simula runner
              mkdir -p $out/bin
              echo '#!/usr/bin/env sh

              set -o errexit
              set -o nounset
              set -o pipefail

              export PATH="${
                lib.makeBinPath [
                  inputs.godot.packages."${system}".godot
                  pkgs.xpra
                  pkgs.xfce.xfce4-terminal
                  pkgs.xorg.xrdb
                  pkgs.wmctrl
                  pkgs.ffmpeg
                  pkgs.midori
                  pkgs.synapse
                  pkgs.xsel
                  pkgs.mimic
                  pkgs.xclip
                  pkgs.curl
                  pkgs.i3status
                ]
              }:$PATH"

              export LD_LIBRARY_PATH="${lib.makeLibraryPath buildInputs}"

              export XDG_CACHE_HOME=''${XDG_CACHE_HOME:-$HOME/.cache}
              export XDG_DATA_HOME=''${XDG_DATA_HOME:-$HOME/.local/share}
              export XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}

              export SIMULA_LOG_DIR="$XDG_CACHE_HOME/Simula"
              export SIMULA_DATA_DIR="$XDG_DATA_HOME/Simula"
              export SIMULA_CONFIG_DIR="$XDG_CONFIG_HOME/Simula"

              if grep -qi NixOS /etc/os-release; then
                godot -m ${simula.src}/project.godot
              else
                echo "Detects non-NixOS distribution. Running Simula with nixGL..."
                nix run --impure github:nix-community/nixGL -- godot -m ${simula.src}/project.godot
              fi' > $out/bin/simula
              chmod 766 $out/bin/simula

              # Install some tools
              ln -s ${pkgs.xpra}/bin/xpra $out/bin/xpra
              ln -s ${pkgs.xfce.xfce4-terminal}/bin/xfce4-terminal $out/bin/xfce4-terminal
              ln -s ${pkgs.xorg.xrdb}/bin/xrdb $out/bin/xrdb
              ln -s ${pkgs.wmctrl}/bin/wmctrl $out/bin/wmctrl
              ln -s ${pkgs.ffmpeg-full}/bin/ffplay $out/bin/ffplay
              ln -s ${pkgs.ffmpeg-full}/bin/ffmpeg $out/bin/ffmpeg
              ln -s ${pkgs.midori}/bin/midori $out/bin/midori
              ln -s ${pkgs.synapse}/bin/synapse $out/bin/synapse
              ln -s ${pkgs.xsel}/bin/xsel $out/bin/xsel
              ln -s ${pkgs.mimic}/bin/mimic $out/bin/mimic
              ln -s ${pkgs.xclip}/bin/xclip $out/bin/xclip
              ln -s ${pkgs.patchelf}/bin/patchelf $out/bin/patchelf
              ln -s ${pkgs.dialog}/bin/dialog $out/bin/dialog
              ln -s ${pkgs.curl}/bin/curl $out/bin/curl
              ln -s ${pkgs.i3status}/bin/i3status $out/bin/i3status

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
            inherit simula godot-haskell-plugin;
            default = simula;
          };

          devShells.default = pkgs.mkShell rec {
            nativeBuildInputs = [
              # A Simula runner, Godot engine forked by SimulaVR
              inputs.godot.packages."${system}".godot

              # Development tools
              pkgs.nil
              pkgs.just
              pkgs.inotify-tools
              pkgs.cabal-install
              pkgs.haskellPackages.ghc
            ];

            buildInputs = [
              # Add build dependencies you want to add LD_LIBRARY_PATH!!

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
