{ driverCheck ? "" }:
(import <nixpkgs> {}).callPackage ./Simula.nix { driverCheck = driverCheck; }
