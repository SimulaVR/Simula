{ callPackage, fetchFromGitHub, gcc9Stdenv }:

let
  rr = callPackage ./. { gcc9Stdenv = gcc9Stdenv; };
in

  rr.overrideAttrs (old: {
    version = "unstable-2020-10-10";

    src = fetchFromGitHub {
      owner = "mozilla";
      repo = "rr";

      rev = "1817b9d440a2725eeae7ffff2764f6a102989042";
      sha256 = "0m9pxhr539zarfx8kxkxa6k8xari05ylcbmrvkdbgshd5786rxpz";

    };
  })