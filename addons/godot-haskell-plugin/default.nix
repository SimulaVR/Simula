{ devBuild ? true, driverCheck ? "" }:
let
  pkgs = import ../../pinned-nixpkgs.nix { };
in
  pkgs.haskellPackages.callPackage ./godot-haskell-plugin.nix { devBuild = devBuild; driverCheck = driverCheck; pkgs = import ../../pinned-nixpkgs.nix; }
