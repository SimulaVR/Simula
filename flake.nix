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
          simula-godot = pkgs.callPackage ./submodules/godot/godot.nix { };
          xvsdk = pkgs.callPackage (
            (pkgs.fetchFromGitHub {
              owner = "SimulaVR";
              repo = "xvsdk";
              rev = "c58f6e022742841c8dc9a476ec80eb37416c0332";
              sha256 = "14lfh2m1zfpgqi5y6x1pkckr0gk9x9q1d33q04lgxkggm8ipprsb";
            })
            + "/xvsdk.nix"
          ) { };

          # ShellScripts. Run with 'nix run .?submodules=1#clean-godot'
          clean-godot = pkgs.callPackage ./utils/nix/clean-godot.nix { };
          build-godot = pkgs.callPackage ./utils/nix/build-godot.nix { };
          build-wlroots = pkgs.callPackage ./utils/nix/build-wlroots.nix { };
          build-monado = pkgs.callPackage ./utils/nix/build-monado.nix { };
          clean-monado = pkgs.callPackage ./utils/nix/clean-monado.nix { };
          repl-godot-haskell-plugin = pkgs.callPackage ./utils/nix/repl-godot-haskell-plugin.nix { };
          build-godot-haskell-plugin = pkgs.callPackage ./utils/nix/build-godot-haskell-plugin.nix { };
          build-godot-haskell = pkgs.callPackage ./utils/nix/build-godot-haskell.nix {
            inherit godot-haskell-classgen;
            godot = simula-godot;
          };
          patch-godot-wlroots = pkgs.callPackage ./utils/nix/patch-godot-wlroots.nix { };
          switch-to-local = pkgs.callPackage ./utils/nix/switch-to-local.nix { };
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
              simula-godot
              ;
          };

          apps = {
            build-godot = {
              type = "app";
              program = build-godot;
            };
            clean-godot = {
              type = "app";
              program = clean-godot;
            };
            build-wlroots = {
              type = "app";
              program = build-wlroots;
            };
            build-monado = {
              type = "app";
              program = build-monado;
            };
            clean-monado = {
              type = "app";
              program = clean-monado;
            };
            repl-godot-haskell-plugin = {
              type = "app";
              program = repl-godot-haskell-plugin;
            };
            build-godot-haskell-plugin = {
              type = "app";
              program = build-godot-haskell-plugin;
            };
            build-godot-haskell = {
              type = "app";
              program = build-godot-haskell;
            };
            patch-godot-wlroots = {
              type = "app";
              program = patch-godot-wlroots;
            };
            switch-to-local = {
              type = "app";
              program = switch-to-local;
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

          devShells.wlroots-dev = pkgs.callPackage ./submodules/wlroots { };
          devShells.godot-dev = pkgs.callPackage ./submodules/godot/godot.nix { };
          devShells.monado-dev = pkgs.callPackage ./submodules/monado/shell.nix { };
          devShells.godot-haskell-plugin-dev = pkgs.callPackage ./addons/godot-haskell-plugin/shell.nix { };
          devShells.godot-haskell-cabal-dev = pkgs.haskellPackages.callPackage ./submodules/godot-haskell-cabal/godot-haskell.nix {
            inherit godot-haskell-classgen;
          };
        };
    };
}
