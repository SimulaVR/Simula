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
          godot-haskell = pkgs.haskellPackages.mkDerivation {
            pname = "godot-haskell";
            version = "3.1.0.0-simula";

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
            ];

            libraryToolDepends = [
              pkgs.haskellPackages.c2hs
              pkgs.haskellPackages.hpack
            ];

            preConfigure = ''
              hpack
            '';

            configureFlags = [
              "--ghc-options=-fPIC -fexternal-dynamic-refs"
            ];

            homepage = "https://github.com/KaneTW/godot-haskell#readme";
            description = "Haskell bindings for the Godot game engine API";
            license = lib.licenses.bsd3;

            doCheck = false;
            doHaddock = false;
            enableLibraryProfiling = true;
          };
          haskell-dependencies = pkgs.stdenvNoCC.mkDerivation rec {
            name = "haskell-dependencies";
            dontUnpack = true;

            buildInputs = [
              pkgs.haskellPackages.ghc
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
              godot-haskell
            ];

            installPhase = ''
              mkdir -p $out/lib
              cp -r ${lib.strings.concatStringsSep " " (builtins.map (drv: "${drv}/lib/ghc-${pkgs.haskellPackages.ghc.version}/lib/${pkgs.stdenv.system}-ghc-${pkgs.haskellPackages.ghc.version}/*.so") buildInputs)} $out/lib
            '';
          };
          simula = pkgs.stdenv.mkDerivation rec {
            pname = "simula";
            version = "0.0.0-dev";

            # `lib.cleanSource` omits `.so` files such as `addons/gdleapmotion/bin/x11/libgdleapmotion.so`
            #src = lib.cleanSource ./.;
            src = ./.;

            nativeBuildInputs = [
              pkgs.godot3-headless
              pkgs.autoPatchelfHook
            ];

            buildInputs = [
              pkgs.xorg.libXcursor
              pkgs.xorg.libXinerama
              pkgs.xorg.libXext
              pkgs.xorg.libXrandr
              pkgs.xorg.libXrender
              pkgs.xorg.libX11
              pkgs.xorg.libXi
              pkgs.libGL
              pkgs.stdenv.cc.cc.lib
              pkgs.openxr-loader
              pkgs.systemd
              pkgs.gmp
              haskell-dependencies
            ];

            LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;

            buildPhase = ''
              runHook preBuild

              # Cannot create file '/homeless-shelter/.config/godot/projects/...'
              export HOME=$TMPDIR

              # Link the export-templates to the expected location. The --export commands
              # expects the template-file at .../templates/{godot-version}.stable/linux_x11_64_release
              mkdir -p $HOME/.local/share/godot
              ln -s ${pkgs.godot3-export-templates}/share/godot/templates $HOME/.local/share/godot

              mkdir -p $out/opt/simula

              godot3-headless --export "Linux/X11" $out/opt/simula/out

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin
              ln -s $out/opt/simula/out $out/bin/simula

              runHook postInstall
            '';

            meta = {
              platforms = lib.platforms.linux;
            };
          };
          for-simula-beginner = simula.overrideAttrs (prevAttrs: {
            pname = "simula-for-beginner";

            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin
              ln -s $out/share/simula/out $out/bin/simula

              # Patch binaries.
              interpreter=$(cat $NIX_CC/nix-support/dynamic-linker)
              patchelf \
                --set-interpreter $interpreter \
                --set-rpath ${lib.makeLibraryPath prevAttrs.buildInputs} \
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
          });
        in
        {
          packages = {
            inherit simula for-simula-beginner haskell-dependencies godot-haskell;
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
