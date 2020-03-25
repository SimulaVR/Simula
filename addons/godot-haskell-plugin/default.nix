{ devBuild ? true, driverCheck ? "" }:
let
  pkgs = import ../../pinned-nixpkgs.nix { };
  godot = pkgs.callPackage ../../submodules/godot/godot.nix { devBuild = devBuild; driverCheck = driverCheck; pkgs = import ../../pinned-nixpkgs.nix; };
  godot-api = "${godot}/bin/api.json";
  godot-haskell = pkgs.haskellPackages.callPackage ../../submodules/godot-haskell/godot-haskell.nix { api-json = godot-api; };
in
  pkgs.haskellPackages.callPackage ./godot-haskell-plugin.nix { devBuild = devBuild; driverCheck = driverCheck; godot = godot; godot-haskell = godot-haskell; }
