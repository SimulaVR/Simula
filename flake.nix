{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/63dacb46bf939521bdc93981b4cbb7ecb58427a0";
    systems.url = "github:nix-systems/default-linux";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      perSystem =
        {
          pkgs,
          lib,
          system,
          ...
        }:
        let
          devBuild-onNixOS = pkgs.callPackage ./. {
            devBuild = true;
            onNixOS = true;
          };
          releaseBuild-onNixOS = pkgs.callPackage ./. {
            devBuild = false;
            onNixOS = true;
          };
          devBuild-onNonNixOS = pkgs.callPackage ./. {
            devBuild = true;
            onNixOS = false;
          };
          releaseBuild-onNonNixOS = pkgs.callPackage ./. {
            devBuild = false;
            onNixOS = false;
          };

          wlroots = pkgs.callPackage ./submodules/wlroots { };
          libxcb-errors = pkgs.callPackage ./submodules/wlroots/libxcb-errors { };
          godot-haskell-classgen =
            pkgs.haskellPackages.callPackage ./submodules/godot-haskell-cabal/classgen/classgen.nix
              { };
          godot-haskell = pkgs.haskellPackages.callPackage ./submodules/godot-haskell/godot-haskell.nix {
            inherit godot-haskell-classgen;
          };
          xvsdk = pkgs.callPackage (
            (pkgs.fetchFromGitHub {
              owner = "SimulaVR";
              repo = "xvsdk";
              rev = "c58f6e022742841c8dc9a476ec80eb37416c0332";
              sha256 = "14lfh2m1zfpgqi5y6x1pkckr0gk9x9q1d33q04lgxkggm8ipprsb";
            })
            + "/xvsdk.nix"
          ) { };

          build-godot =
            let
              pkgconfig-libpath = [
                pkgs.xorg.libX11.dev
                pkgs.xorg.libXcursor.dev
                pkgs.xorg.libXinerama.dev
                pkgs.xorg.libXext.dev
                pkgs.xorg.libXrandr.dev
                pkgs.xorg.libXrender.dev
                pkgs.xorg.libXi.dev
                pkgs.xorg.libXfixes.dev
                pkgs.xorg.libxcb.dev
                pkgs.libGLU.dev
                pkgs.libglvnd.dev
                pkgs.zlib.dev
                pkgs.alsa-lib.dev
                pkgs.pulseaudio.dev
                pkgs.eudev
                pkgs.libxkbcommon.dev
                pkgs.wayland.dev
                pkgs.pixman
                pkgs.dbus.dev
                libxcb-errors
                wlroots
              ];
              pkgconfig-sharepath = [
                pkgs.xorg.xorgproto
              ];
            in
            pkgs.writeShellApplication {
              name = "build-godot";
              runtimeInputs = [
                pkgs.wayland-scanner
                pkgs.scons
                pkgs.inotify-tools
                pkgs.pkg-config
                pkgs.gcc
              ];
              text = ''
                export PKG_CONFIG_PATH="${lib.strings.makeSearchPath "lib/pkgconfig" pkgconfig-libpath}:${lib.strings.makeSearchPath "share/pkgconfig" pkgconfig-sharepath}"

                help_message () {
                  echo "Usage: $0 help|build|watch"
                }

                build_once () {
                  cd ./submodules/godot

                  wayland-scanner server-header ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.h
                  wayland-scanner private-code ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.c
                  scons -Q -j8 platform=x11 target=debug warnings=no

                  cd -
                }

                watch_build () {
                  cd ./submodules/godot

                  while inotifywait -qqre modify .
                  do
                    wayland-scanner server-header ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.h; wayland-scanner private-code ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.c; scons -Q -j8 platform=x11 target=debug warnings=no
                  done

                  cd -
                }

                while (( $# > 0 ))
                do
                  case $1 in
                    h | help)
                      help_message
                      exit 0
                      ;;
                    b | build)
                      build_once
                      exit 0
                      ;;
                    w | watch)
                      watch_build
                      exit 0
                      ;;
                    *)
                      echo "Unknown argument: $1"
                      exit 1
                      ;;
                  esac
                done

                help_message
                echo "No argument. exit with 2"
                exit 2
              '';
            };
        in
        {
          _module.args = {
            pkgs = import inputs.nixpkgs {
              inherit system;
              config.allowUnfree = true;
              overlays = [ inputs.nixgl.overlays.default ];
            };
          };

          packages = {
            inherit
              devBuild-onNixOS
              releaseBuild-onNixOS
              devBuild-onNonNixOS
              releaseBuild-onNonNixOS
              ;
          };

          apps = {
            build-godot = {
              type = "app";
              program = build-godot;
            };
          };

          devShells.default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              cachix
              git
              curl
              dialog
              scons
              meson
              ninja
              cmake
              wayland-scanner
              pkg-config
              inotify-tools
              python3
              patchelf
              godot-haskell-classgen
              glslang
              doxygen
              cabal-install
              ghc
            ];

            buildInputs = [
              pkgs.xorg.libX11.dev
              pkgs.xorg.libXcursor
              pkgs.xorg.libXinerama
              pkgs.xorg.libXext
              pkgs.xorg.libXrandr
              pkgs.xorg.libXi
              pkgs.libGLU
              pkgs.libxkbcommon
              wlroots
              pkgs.wayland-scanner.dev
              pkgs.pixman
              libxcb-errors
              pkgs.eudev
              pkgs.dbus.dev
              pkgs.alsa-lib
              pkgs.pulseaudio.dev
              pkgs.wayland-protocols
              pkgs.libdrm
              pkgs.mesa
              pkgs.wayland
              pkgs.libGL
              pkgs.libinput.dev
              pkgs.libxkbcommon
              pkgs.pixman
              pkgs.xorg.xcbutilwm
              pkgs.libcap
              pkgs.xorg.xcbutilimage
              pkgs.xorg.xcbutilerrors
              pkgs.mesa
              pkgs.libpng
              pkgs.ffmpeg_4
              pkgs.xorg.libxcb.dev
              pkgs.xorg.xinput
              pkgs.xorg.libxcb

              pkgs.bluez
              pkgs.cjson
              pkgs.eigen
              pkgs.elfutils
              pkgs.ffmpeg
              pkgs.gst_all_1.gst-plugins-base
              pkgs.gst_all_1.gstreamer
              pkgs.hidapi
              pkgs.libbsd
              pkgs.libdrm
              pkgs.libffi
              pkgs.libGL
              pkgs.libjpeg
              pkgs.librealsense
              pkgs.libsurvive
              pkgs.libunwind
              pkgs.libusb1
              pkgs.libuv
              pkgs.libuvc
              pkgs.libv4l
              pkgs.xorg.libXau
              pkgs.xorg.libXdmcp
              pkgs.onnxruntime
              pkgs.opencv4
              pkgs.openhmd
              pkgs.openvr
              pkgs.orc
              pkgs.pcre2
              pkgs.SDL2
              pkgs.shaderc
              pkgs.udev
              pkgs.vulkan-headers
              pkgs.vulkan-loader
              pkgs.wayland
              pkgs.wayland-protocols
              pkgs.wayland-scanner
              pkgs.zlib
              pkgs.zstd

              godot-haskell
              pkgs.haskellPackages.parsers

              xvsdk
            ];

            shellHook = ''
              export PS1="\n[nix-shell:\w]$ "
            '';
          };
        };
    };
}
