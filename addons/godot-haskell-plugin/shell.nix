{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "Simula";
  inherit ghc;
  buildInputs = with pkgs; [
                             (callPackage ./nix/godot/default.nix { } ) # SimulaVR/godot fork
                                                                        # Pseudo-depends on libpulseaudio, yasm, & libXi

                             (callPackage ./nix/wayland.nix { } )
                             (callPackage ./nix/wayland-protocols.nix { } )
                             (callPackage ./nix/wlroots.nix { } )
                             (callPackage ./nix/libdrm.nix { } )
                             libinput
                             pkgconfig
                             pixman
                             libGL
                             xorg.pixman
                             libxkbcommon
                             zlib
                             git
                             xorg.libX11
                             udev
                             cabal-install
                          ];

  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
