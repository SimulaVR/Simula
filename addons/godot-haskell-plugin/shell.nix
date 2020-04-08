{ pkgs ? import ../../pinned-nixpkgs.nix { }, ghc ? pkgs.ghc }:

let
    godot = pkgs.callPackage ../../submodules/godot/godot.nix { devBuild = "true"; driverCheck = "nvidia 440.64 0xbm1dh95kz8h4d62pql2wmvw2gbgc7iif2bkixbnqijl4dryg71"; pkgs = import ../../pinned-nixpkgs.nix; };
    godot-api = "${godot}/bin/api.json";
    godot-haskell = pkgs.haskellPackages.callPackage ../../submodules/godot-haskell/godot-haskell.nix { api-json = godot-api; };

in

pkgs.haskell.lib.buildStackProject {
  name = "Simula";
  inherit ghc;
  buildInputs =  [
    godot-haskell # <- Doesn't work
    pkgs.zlib
  ];

  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
