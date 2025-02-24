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
          for-simula-beginner = pkgs.stdenv.mkDerivation rec {
            pname = "simula-for-beginner";
            version = "0.0.0-dev";
            src = lib.cleanSource ./.;

            nativeBuildInputs = [
              pkgs.godot3-headless
              pkgs.autoPatchelfHook
            ];

            buildInputs = [
              pkgs.xorg.libXcursor
              pkgs.xorg.libXinerama
              pkgs.xorg.libXext
              pkgs.xorg.libXrandr
              pkgs.xorg.libXi
              pkgs.libGL
            ];

            buildPhase = ''
              runHook preBuild

              # Cannot create file '/homeless-shelter/.config/godot/projects/...'
              export HOME=$TMPDIR

              # Link the export-templates to the expected location. The --export commands
              # expects the template-file at .../templates/{godot-version}.stable/linux_x11_64_release
              mkdir -p $HOME/.local/share/godot
              ln -s ${pkgs.godot3-export-templates}/share/godot/templates $HOME/.local/share/godot

              mkdir -p $out/share/simula

              godot3-headless --export "Linux/X11" $out/share/simula/out

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin
              ln -s $out/share/simula/out $out/bin/simula

              # Patch binaries.
              interpreter=$(cat $NIX_CC/nix-support/dynamic-linker)
              patchelf \
                --set-interpreter $interpreter \
                --set-rpath ${lib.makeLibraryPath buildInputs} \
                $out/share/simula/out

              # Install some tools' symlink
              ln -s ${pkgs.xpra}/bin/xpra $out/bin/xpra
              ln -s ${pkgs.xfce.xfce4-terminal}/bin/xfce4-terminal $out/bin/xfce4-terminal
              ln -s ${pkgs.xorg.xrdb}/bin/xrdb $out/bin/xrdb
              ln -s ${pkgs.wmctrl}/bin/wmctrl $out/bin/wmctrl
              ln -s ${pkgs.ffmpeg}/bin/ffplay $out/bin/ffplay
              ln -s ${pkgs.ffmpeg}/bin/ffmpeg $out/bin/ffmpeg
              ln -s ${pkgs.midori}/bin/midori $out/bin/midori
              ln -s ${pkgs.synapse}/bin/synapse $out/bin/synapse
              ln -s ${pkgs.xsel}/bin/xsel $out/bin/xsel
              ln -s ${pkgs.mimic}/bin/mimic $out/bin/mimic
              ln -s ${pkgs.xclip}/bin/xclip $out/bin/xclip
              ln -s ${pkgs.curl}/bin/curl $out/bin/curl
              ln -s ${pkgs.i3status}/bin/i3status $out/bin/i3status

              runHook postInstall
            '';

            meta = {
              platforms = lib.platforms.linux;
            };
          };
          simula = pkgs.stdenv.mkDerivation rec {
            pname = "simula";
            version = "0.0.0-dev";
            src = lib.cleanSource ./.;

            nativeBuildInputs = [
              pkgs.godot3-headless
              pkgs.autoPatchelfHook
            ];

            buildInputs = [
              pkgs.xorg.libXcursor
              pkgs.xorg.libXinerama
              pkgs.xorg.libXext
              pkgs.xorg.libXrandr
              pkgs.xorg.libXi
              pkgs.libGL
            ];

            buildPhase = ''
              runHook preBuild

              # Cannot create file '/homeless-shelter/.config/godot/projects/...'
              export HOME=$TMPDIR

              # Link the export-templates to the expected location. The --export commands
              # expects the template-file at .../templates/{godot-version}.stable/linux_x11_64_release
              mkdir -p $HOME/.local/share/godot
              ln -s ${pkgs.godot3-export-templates}/share/godot/templates $HOME/.local/share/godot

              mkdir -p $out/share/simula

              godot3-headless --export "Linux/X11" $out/share/simula/out

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin
              ln -s $out/share/simula/out $out/bin/simula

              # Patch binaries.
              interpreter=$(cat $NIX_CC/nix-support/dynamic-linker)
              patchelf \
                --set-interpreter $interpreter \
                --set-rpath ${lib.makeLibraryPath buildInputs} \
                $out/share/simula/out

              runHook postInstall
            '';

            meta = {
              platforms = lib.platforms.linux;
            };
          };
        in
        {
          packages = {
            inherit simula for-simula-beginner;
            default = simula;
          };

          devShells.default = pkgs.mkShell {
            nativeBuildInputs = [
              pkgs.godot3
            ];

            shellHook = ''
              export PS1="\n[nix-shell:\w]$ "
            '';
          };
        };
    };
}
