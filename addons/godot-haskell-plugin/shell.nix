{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "simula-godot";
  inherit ghc;
  buildInputs = with pkgs; [ 
                             mesa
                             pkgconfig
                             xorg.pixman
                             wayland-protocols
                             wayland
                             weston
                             xorg.libX11 # possibly not needed
                             libxkbcommon
                             git
                             zlib
                          ];

  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
