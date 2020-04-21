{ pkgs ? import ../../pinned-nixpkgs.nix { }, ghc ? pkgs.ghc, onNixOS ? false }:

let
    godot = pkgs.callPackage ../../submodules/godot/godot.nix { devBuild = "true"; onNixOS = onNixOS; pkgs = import ../../pinned-nixpkgs.nix; };
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
