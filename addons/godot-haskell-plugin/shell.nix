{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "Simula";
  inherit ghc;
  buildInputs = with pkgs; [ 
                             libinput
                             (callPackage ./nix/wayland.nix { } )
                             (callPackage ./nix/wayland-protocols.nix { } )
                             (callPackage ./nix/wlroots.nix { } )
                             libGL
                             xorg.pixman
                             libxkbcommon
                             zlib
                             git
                             godot
                             xorg.libX11
                             udev
                             cabal-install
                          ];

  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
