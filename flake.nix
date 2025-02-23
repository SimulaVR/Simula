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
          simula = pkgs.stdenv.mkDerivation rec {
            pname = "simula";
            version = "0.0.0-dev";
            src = lib.cleanSource ./.;

            nativeBuildInputs = [
              pkgs.godot3-headless
            ];

            buildInputs = [
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
              #patchelf \
              #  --set-interpreter $interpreter \
              #  --set-rpath ${lib.makeLibraryPath buildInputs} \
              #  $out/share/test-godot3-with-nix/out

              runHook postInstall
            '';

            meta = {
              platforms = lib.platforms.unix;
            };
          };
        in
        {
          packages = {
            inherit simula;
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
