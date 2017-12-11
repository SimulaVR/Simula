{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "simula-wayland";
  inherit ghc;
  buildInputs = with pkgs; [ mesa
                             xorg.pixman
                             wayland-protocols
                             wayland
                             xorg.libX11
                            (callPackage ./weston3.nix { })
                          ];
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
