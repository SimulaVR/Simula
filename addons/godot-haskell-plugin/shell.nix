{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "simula-godot";
  inherit ghc;
  buildInputs = with pkgs; [ 
                             libGL
                             xorg.pixman
                             (callPackage ./wayland.nix { } )
                             (callPackage ./wayland-protocols.nix { } )
                             (callPackage ./wlroots.nix { } )
                             libxkbcommon
                             zlib
                             git
                             godot
                             xorg.libX11
                             udev
                           # xdg_utils
                          ];

  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
