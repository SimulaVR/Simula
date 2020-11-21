{ mkDerivation, aeson, base, base64-bytestring, bytestring, clock
, colour, containers, directory, extra, http-client
, http-client-tls, http-types, inline-c, iso8601-time, lens, linear
, process, raw-strings-qq, stdenv, stm, text, time, unix, uuid
, vector, fetchFromGitHub, callPackage, haskellPackages, devBuild ? false, onNixOS ? false, pkgs ? (import ../../pinned-nixpkgs.nix), godot, godot-haskell, ordered-containers, process-extras, dhall, hspec, QuickCheck
}:
mkDerivation {
  pname = "godot-haskell-plugin";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring clock colour containers
    directory extra http-client http-client-tls
    http-types inline-c iso8601-time lens linear process raw-strings-qq
    stm text time unix uuid vector godot-haskell ordered-containers process-extras dhall hspec QuickCheck
  ];
  homepage = "https://github.com/SimulaVR/Simula#readme";
  license = stdenv.lib.licenses.asl20;

  doHaddock = false;
  doCheck = false;
  enableLibraryProfiling = false;
}
