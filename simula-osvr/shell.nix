{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "simula-osvr";
  inherit ghc;
  buildInputs = with pkgs; [(callPackage ./OSVR-Core.nix { })]; # provides osvrClientKit
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
