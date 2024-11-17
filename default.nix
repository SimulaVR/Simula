{ onNixOS, devBuild }:
let
    pkgs = import ./pinned-nixpkgs.nix { };
in
pkgs.callPackage ./Simula.nix { onNixOS = onNixOS; devBuild = devBuild; pkgs = pkgs; }
