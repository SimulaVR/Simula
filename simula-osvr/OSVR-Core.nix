{ pkgs, stdenv, fetchgit, cmake, jsoncpp, opencv, python27, libusb1, boost }:

stdenv.mkDerivation {
  name = "OSVR-Core";
  buildInputs = with pkgs; [ cmake
                             jsoncpp
                             opencv
                             python27
                             libusb1
                             boost
                             (callPackage ./libfunctionality.nix { })
                            ];
  src = fetchgit {
    url       = "https://github.com/OSVR/OSVR-Core.git";
    rev       = "95655d3174851670b85e9be8e8620ba28f9872f4";
    sha256    = "0y3lfagv3h2i9fd4py2fpfixcfpa3ad3gzdpj3wxcl7jlrxznvs4";
    deepClone = true; # git clone --recursive
  };
}
