{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "SimulaHS";
  inherit ghc;
  buildInputs = with pkgs; [ mesa
                             xorg.pixman
                             wayland-protocols
                             wayland
                             xorg.libX11
                             dbus
                             (callPackage simula-wayland/weston2.nix { })
                             (callPackage simula-osvr/OSVR-Core.nix { })
                          ];
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
