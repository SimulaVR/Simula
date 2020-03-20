{ driverCheck, devBuild }:
(import ./pinned-nixpkgs.nix { }).callPackage ./Simula.nix { driverCheck = driverCheck; devBuild = devBuild; }
