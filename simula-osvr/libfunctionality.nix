{ stdenv, fetchFromGitHub, cmake }:

stdenv.mkDerivation {
  name = "libfunctionality";
  buildInputs = [ cmake ];
  src = fetchFromGitHub {
        owner  = "OSVR";
        repo   = "libfunctionality";
        rev    = "3c8b1e4d21283eed346ee17d95d270c9a1d97db9";
        sha256 = "04dz32nbgv2sij5gxy0a3szni12w7xs4wwzxi01knw7bppqggfd4";
  };
}
