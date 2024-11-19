{ devBuild, profileBuild ? false }:
let
    pkgs = if profileBuild then (import ./pinned-nixpkgs.nix { overlays = (import ./nix/profileOverlays.nix); }) else (import ./pinned-nixpkgs.nix { });
in
pkgs.callPackage ./Simula.nix { devBuild = devBuild; profileBuild = profileBuild; pkgs = pkgs; }
