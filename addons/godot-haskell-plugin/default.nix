let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./godot-haskell-plugin.nix { }
