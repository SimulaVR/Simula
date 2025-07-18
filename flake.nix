{
  description = "Flake for SimulaVR/Simula";

  nixConfig = {
    extra-substituters = [ "https://simula.cachix.org" ];
    extra-trusted-public-keys = [ "simula.cachix.org-1:Sr0SD5FIjc8cUVIeBHl8VJswQEJOBIE6u3wpmjslGBA=" ];
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
          '';







          # Needed to ensure fonts don't show up as blocks on certain non-NixOS distributions, IIRC




          simula = pkgs.stdenv.mkDerivation rec {
            pname = "simula";
            version = "0.0.0";

            installPhase = ''
              runHook preInstall

              ${copySimulaSourcesToNixStore}
              ${copyEnvironmentsToNixStore}
              ${placeGodotHaskellPluginLib}

              mkdir -p $out/bin

              simulaUnwrapped=${pkgs.substituteAll {
                src = ./nix/simula-unwrapped.sh;
                inherit (pkgs) util-linux coreutils colorized-logs;
                glibcLocales = pkgs.glibcLocales;
                godot = inputs.godot.packages."${system}".godot;
              }}

              cat > $out/bin/simula-unwrapped << EOF
              ''${simulaUnwrapped}
              EOF
              chmod 755 $out/bin/simula-unwrapped

              makeWrapper $out/bin/simula-unwrapped $out/bin/simula \
                --prefix PATH : ${lib.makeBinPath passthru.simulaRuntimePrograms} \
                --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath passthru.simulaRuntimeLibs}

              simulaMonadoService=${pkgs.substituteAll {
                src = ./nix/simula-monado-service.sh;
                inherit (pkgs) stdenv;
                inherit monado;
              }}

              cat > $out/bin/simula-monado-service << EOF
              ''${simulaMonadoService}
              EOF
              chmod 755 $out/bin/simula-monado-service

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
            inherit simula godot-haskell godot-haskell-plugin;
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
