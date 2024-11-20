{ devBuild ? true, onNixOS ? false }:
let
  pkgs = import ../../pinned-nixpkgs.nix { };
  haskellCallPkg = pkgs.haskellPackages.callPackage;
  godot = pkgs.callPackage ../../submodules/godot/godot.nix { devBuild = devBuild; onNixOS = onNixOS; pkgs = import ../../pinned-nixpkgs.nix; };
  godot-api = "${godot}/bin/api.json";
  godot-haskell = haskellCallPkg ../../submodules/godot-haskell/godot-haskell.nix { api-json = godot-api; };

in
  haskellCallPkg ./godot-haskell-plugin.nix { devBuild = devBuild; onNixOS = onNixOS; godot = godot; godot-haskell = godot-haskell; }
