{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "simula-godot";
  inherit ghc;
  buildInputs = with pkgs; [ 
                             (callPackage ./nix/libinput/default.nix { } )
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
                           # libinput
                           # xdg_utils
                          ];

  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
