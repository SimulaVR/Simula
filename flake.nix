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
        { pkgs, system, ... }:
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

          devShells.default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              cachix
              git
              curl
              dialog
              scons
              ninja
              wayland-scanner
              pkg-config
              inotify-tools
            ];

            buildInputs = [
              pkgs.xorg.libX11
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
            ];

            shellHook = ''
              export PS1="\n[nix-shell:\w]$ "
            '';
          };
        };
    };
}
