{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "SimulaHS";
  inherit ghc;
  buildInputs = with pkgs; [ mesa
                             gcc7
                             xorg.pixman
                             wayland-protocols
                             wayland
                             xorg.libX11
                             dbus
                             weston
                             (callPackage simula-openvr/openvr.nix { })
                             libxml2
                             libxkbcommon
                             vulkan-loader
                             steam
                          ];

  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
