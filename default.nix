{ onNixOS, devBuild }:
(import ./pinned-nixpkgs.nix { }).callPackage ./Simula.nix { onNixOS = onNixOS; devBuild = devBuild; }
