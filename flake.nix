{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/63dacb46bf939521bdc93981b4cbb7ecb58427a0";
    systems.url = "github:nix-systems/x86_64-linux";
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
          wlroots = pkgs.stdenv.mkDerivation {
            pname = "wlroots";
            version = "0.10.0-simula";

            src = pkgs.fetchFromGitHub {
              owner = "SimulaVR";
              repo = "wlroots";
              rev = "7d065df6f723bbb592fc50150feb213d323f37c6";
              hash = "sha256-1zjTWivMPoWM9tmAoWl5lhKnV1WgCY3wVMANDd9eAqM=";
              fetchSubmodules = true;
            };

            # $out for the library and $examples for the example programs (in examples):
            outputs = [
              "out"
              "examples"
            ];

            nativeBuildInputs = [
              pkgs.meson
              pkgs.cmake
              pkgs.ninja
              pkgs.pkg-config
              pkgs.wayland-scanner.bin
            ];

            buildInputs = [
              pkgs.wayland
              pkgs.libGL
              pkgs.wayland-protocols
              pkgs.libinput
              pkgs.libxkbcommon
              pkgs.pixman
              pkgs.xorg.xcbutilwm
              pkgs.libcap
              pkgs.xorg.xcbutilimage
              pkgs.xorg.xcbutilerrors
              pkgs.mesa
              pkgs.libpng
              pkgs.ffmpeg_4
              pkgs.xorg.libX11.dev
              pkgs.xorg.libxcb.dev
              pkgs.xorg.xinput

              libxcb-errors
            ];

            mesonFlags = [
              "-Dlibcap=enabled"
              "-Dlogind=enabled"
              "-Dxwayland=enabled"
              "-Dx11-backend=enabled"
              "-Dxcb-icccm=disabled"
              "-Dxcb-errors=enabled"
            ];

            LDFLAGS = [
              "-lX11-xcb"
              "-lxcb-xinput"
            ];

            postInstall = ''
              # Copy the library to $examples
              mkdir -p $examples/lib
              cp -Pr libwlroots* $examples/lib/
            '';

            postFixup = ''
              # Install ALL example programs to $examples:
              # screencopy dmabuf-capture input-inhibitor layer-shell idle-inhibit idle
              # screenshot output-layout multi-pointer rotation tablet touch pointer
              # simple
              mkdir -p $examples/bin
              cd ./examples
              for binary in $(find . -executable -type f -printf '%P\n' | grep -vE '\.so'); do
                cp "$binary" "$examples/bin/wlroots-$binary"
              done
            '';

            meta = with lib; {
              description = "More or less a pinned version of wlroots that Simula can use (with a few patches)";
              homepage = "https://github.com/SimulaVR/wlroots";
              license = licenses.mit;
              platforms = platforms.linux;
            };
          };
          libxcb-errors = pkgs.stdenv.mkDerivation {
            pname = "libxcb-errors";
            version = "0.0.0";
            src = pkgs.fetchFromGitHub {
              owner = "SimulaVR";
              repo = "libxcb-errors";
              rev = "cb26a7dc442b0bb37f8648986350291d3d45a47a";
              hash = "sha256-LPNRkLQSTPkCRIc9lhHYIvQs8ags3GpGehCWLY5qvJw=";
            };

            nativeBuildInputs = [
              pkgs.pkg-config
              pkgs.python310
              pkgs.autoreconfHook
            ];

            buildInputs = [
              pkgs.xorg.libxcb
              pkgs.xorg.libXau
              pkgs.xorg.libXdmcp
              pkgs.libbsd
              pkgs.xorg.utilmacros
              pkgs.xorg.xcbproto
            ];

            meta = {
              description = "Allow XCB errors to print less opaquely";
              homepage = "https://github.com/SimulaVR/libxcb-errors";
              license = lib.licenses.mit;
              platforms = lib.platforms.linux;
            };
          };
          godot = pkgs.stdenv.mkDerivation {
            pname = "godot";
            version = "3.x-simula";
            src = pkgs.fetchFromGitHub {
              owner = "haruki7049";
              repo = "godot";
              rev = "df193d656aad69c0efe33fa9278907c2341d7ce9";
              hash = "sha256-Df5ToAYrb5xAqqKQqngAVSVfz9v1l61ZTQqOu7Xvudc=";
              fetchSubmodules = true;
            };

            nativeBuildInputs = [
              pkgs.scons
              pkgs.pkg-config
              pkgs.autoPatchelfHook
            ];

            buildInputs = [
              pkgs.xorg.libX11
              pkgs.xorg.libXcursor
              pkgs.xorg.libXinerama
              pkgs.xorg.libXext
              pkgs.xorg.libXrandr
              pkgs.xorg.libXi
              pkgs.libGLU
              pkgs.zlib

              pkgs.alsa-lib
              pkgs.libpulseaudio
              pkgs.yasm
              pkgs.systemd
              pkgs.libxkbcommon
              pkgs.wayland
              pkgs.pixman
              pkgs.dbus-glib

              libxcb-errors
              wlroots
              leap-sdk
            ];

            outputs = [
              "out"
              "dev"
            ];

            configurePhase = ''
              echo 'Copying libLeap.so from .#leap-sdk'
              cd modules/gdleapmotionV2/LeapSDK/lib/x64/
              cp ${leap-sdk}/lib/* .
              cd -


              echo 'Generate xdg-shell-protocol.{h,c}'
              cd modules/gdwlroots
              ${pkgs.wayland-scanner.bin}/bin/wayland-scanner server-header ${pkgs.wayland-protocols}/share/wayland-protocols/stable/xdg-shell/xdg-shell.xml xdg-shell-protocol.h
              ${pkgs.wayland-scanner.bin}/bin/wayland-scanner private-code ${pkgs.wayland-protocols}/share/wayland-protocols/stable/xdg-shell/xdg-shell.xml xdg-shell-protocol.c
              cd -
            '';

            buildPhase = ''
              echo Building...
              scons platform=x11 tools=no target=release bits=64 -j $NIX_BUILD_CORES
            '';

            installPhase = ''
              # Install godot
              mkdir -p $out/bin
              cp bin/godot.x11.opt.64 $out/bin/godot

              # Install gdnative headers
              mkdir $dev
              cp -r modules/gdnative/include $dev

              # Install man
              mkdir -p $out/share/man/man6
              cp misc/dist/linux/godot.6 $out/share/man/man6

              mkdir -p $out/share/applications
              mkdir -p $out/share/icons/hicolor/scalable/apps
              cp misc/dist/linux/org.godotengine.Godot.desktop $out/share/applications
              cp icon.svg $out/share/icons/hicolor/scalable/apps/godot.svg
              cp icon.png $out/share/icons/godot.png
              substituteInPlace $out/share/applications/org.godotengine.Godot.desktop \
                --replace-warn "Exec=godot" "Exec=$out/bin/godot"
            '';

            meta = {
              homepage = "https://github.com/SimulaVR/godot";
              license = lib.licenses.mit;
              platforms = [ "x86_64-linux" ];
            };
          };
          leap-sdk = pkgs.stdenv.mkDerivation {
            name = "leap-sdk";
            src = pkgs.fetchFromGitHub {
              owner = "SimulaVR";
              repo = "gdleapmotionV2";
              rev = "e3917f9a45ad7899704c65a3120cec38f3e093bb";
              hash = "sha256-F9N4yC/Mw3LcW+LQCCfnRzOYPS8mEypnX7UodnZdY4U=";
            };

            nativeBuildInputs = [ pkgs.autoPatchelfHook ];

            buildInputs = [ pkgs.stdenv.cc.cc.lib ];

            dontBuild = true;

            outputs = [
              "out"
              "dev"
            ];

            installPhase = ''
              mkdir -p $dev/include
              mkdir -p $out/lib

              cp LeapSDK/include/* $dev/include
              cp LeapSDK/lib/x64/* $out/lib
            '';

            meta = {
              homepage = "https://github.com/SimulaVR/gdleapmotionV2";
              license = lib.licenses.unfree;
              platforms = [ "x86_64-linux" ];
            };
          };
          godot-haskell = pkgs.haskellPackages.mkDerivation {
            pname = "godot-haskell";
            version = "3.4.4.0-simula";

            src = pkgs.fetchFromGitHub {
              owner = "SimulaVR";
              repo = "godot-haskell";
              rev = "c4105239909af90758c585d35f2d03f71381fb57";
              hash = "sha256-iqXW6e+bL33AJAWPsrhdU8yJC4MJjrdBqGAGDA7Xupw=";
              fetchSubmodules = true;
            };

            libraryHaskellDepends = [
              pkgs.haskellPackages.aeson
              pkgs.haskellPackages.ansi-wl-pprint
              pkgs.haskellPackages.casing
              pkgs.haskellPackages.colour
              pkgs.haskellPackages.lens
              pkgs.haskellPackages.linear
              pkgs.haskellPackages.parsers
              pkgs.haskellPackages.unordered-containers
              pkgs.haskellPackages.vector
              pkgs.haskellPackages.prettyprinter
              pkgs.haskellPackages.prettyprinter-ansi-terminal
              pkgs.haskellPackages.extra
              pkgs.haskellPackages.fsnotify
              pkgs.haskellPackages.interpolate
            ];

            libraryToolDepends = [
              pkgs.haskellPackages.c2hs
              pkgs.haskellPackages.hpack
            ];

            preConfigure = ''
              hpack
            '';

            configureFlags = [ "--ghc-options=-fPIC -fexternal-dynamic-refs" ];

            homepage = "https://github.com/KaneTW/godot-haskell#readme";
            description = "Haskell bindings for the Godot game engine API";
            license = lib.licenses.bsd3;

            doCheck = false;
            doHaddock = false;
            enableLibraryProfiling = true;
          };
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
                  godot
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
            inherit
              simula
              haskell-dependencies
              godot
              godot-haskell
              godot-haskell-plugin
              ;
            default = simula;
          };

          devShells.default = pkgs.mkShell rec {
            nativeBuildInputs = [
              # A Simula runner, Godot engine forked by SimulaVR
              godot

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
