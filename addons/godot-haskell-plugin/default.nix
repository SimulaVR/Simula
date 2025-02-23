{
  lib,
  callPackage,
  haskellPackages,

  # Dependencies
  godot-haskell ? callPackage ../../submodules/godot-haskell { },

  # Arguments when you build this
  profileBuild ? false,
}:

haskellPackages.mkDerivation {
  pname = "godot-haskell-plugin";
  version = "0.1.0.0";
  src = lib.cleanSource ./.;

  libraryHaskellDepends = [
    godot-haskell

    haskellPackages.base
    haskellPackages.QuickCheck
    haskellPackages.aeson
    haskellPackages.base64-bytestring
    haskellPackages.clock
    haskellPackages.colour
    haskellPackages.dhall
    haskellPackages.extra
    haskellPackages.hspec
    haskellPackages.hspec-core
    haskellPackages.http-client
    haskellPackages.http-client-tls
    haskellPackages.http-types
    haskellPackages.inline-c
    haskellPackages.io-streams
    haskellPackages.iso8601-time
    haskellPackages.lens
    haskellPackages.linear
    haskellPackages.ordered-containers
    haskellPackages.path
    haskellPackages.path-io
    haskellPackages.process-extras
    haskellPackages.raw-strings-qq
    haskellPackages.safe-exceptions
    haskellPackages.uuid
    haskellPackages.vector
  ];

  homepage = "https://github.com/SimulaVR/Simula#readme";
  license = lib.licenses.asl20;

  doHaddock = false;
  doCheck = false;
  enableLibraryProfiling = profileBuild;
}
