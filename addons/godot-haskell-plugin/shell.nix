{ devBuild ? true, onNixOS ? false, profileBuild ? false }:
let
  pkgs = if profileBuild then (import ../../pinned-nixpkgs.nix { overlays = (import ../../nix/profileOverlays.nix); }) else (import ../../pinned-nixpkgs.nix { });
  haskellCallPkg = if profileBuild then (pkgs.haskellPackagesPIC.callPackage) else (pkgs.haskellPackages.callPackage);
in
  haskellCallPkg ./godot-haskell-plugin-dev.nix { devBuild = devBuild; onNixOS = onNixOS; profileBuild = profileBuild; }
