{ devBuild ? true, onNixOS ? false, profileBuild ? false, pkgs }:
let
  haskellCallPkg = if profileBuild then (pkgs.haskellPackagesPIC.callPackage) else (pkgs.haskellPackages.callPackage);
  godot = pkgs.callPackage ../../submodules/godot/godot.nix { devBuild = devBuild; onNixOS = onNixOS; inherit pkgs; };
  godot-api = "${godot}/bin/api.json";
  godot-haskell = haskellCallPkg ../../submodules/godot-haskell/godot-haskell.nix { api-json = godot-api; inherit profileBuild; };

in
  haskellCallPkg ./godot-haskell-plugin.nix { devBuild = devBuild; onNixOS = onNixOS; godot = godot; godot-haskell = godot-haskell; profileBuild = profileBuild; }
