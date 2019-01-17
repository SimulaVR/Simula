{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "simula-godot";
  inherit ghc;
  buildInputs = with pkgs; [ 
                             libGL
                             xorg.pixman
                             wayland-protocols
                             wayland
                             weston
                             libxkbcommon
                             zlib
                             git
                             godot
                          ];

  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
