{ devBuild ? true, onNixOS ? false }:
let
  pkgs = import ../../pinned-nixpkgs.nix { };
in
  pkgs.haskellPackages.callPackage ./godot-haskell-plugin-dev.nix { devBuild = devBuild; onNixOS = onNixOS; }
