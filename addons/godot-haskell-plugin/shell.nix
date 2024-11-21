{ devBuild ? true, onNixOS ? false, profileBuild ? false, pkgs }:
let
  haskellCallPkg = if profileBuild then (pkgs.haskellPackagesPIC.callPackage) else (pkgs.haskellPackages.callPackage);
in
   pkgs.mkShell {
     buildInputs = [
       pkgs.zlib
       pkgs.ghc
       pkgs.cabal-install
       pkgs.haskellPackages.aeson
       pkgs.haskellPackages.base
       pkgs.haskellPackages.base64-bytestring
       pkgs.haskellPackages.bytestring
       pkgs.haskellPackages.clock
       pkgs.haskellPackages.colour
       pkgs.haskellPackages.containers
       pkgs.haskellPackages.directory
       pkgs.haskellPackages.extra
       pkgs.haskellPackages.http-client
       pkgs.haskellPackages.http-client-tls
       pkgs.haskellPackages.http-types
       pkgs.haskellPackages.inline-c
       pkgs.haskellPackages.iso8601-time
       pkgs.haskellPackages.lens
       pkgs.haskellPackages.linear
       pkgs.haskellPackages.process
       pkgs.haskellPackages.raw-strings-qq
       pkgs.haskellPackages.stm
       pkgs.haskellPackages.text
       pkgs.haskellPackages.time
       pkgs.haskellPackages.unix
       pkgs.haskellPackages.uuid
       pkgs.haskellPackages.vector
       pkgs.haskellPackages.ordered-containers
       pkgs.haskellPackages.process-extras
       pkgs.haskellPackages.dhall
       pkgs.haskellPackages.hspec
       pkgs.haskellPackages.QuickCheck
       pkgs.haskellPackages.safe-exceptions
       pkgs.haskellPackages.io-streams
       pkgs.haskellPackages.zlib
     ];
   }
