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
    sha256    = "16sbfv4fxcvxqhm81in8lkvjpfbiz312kh7pm4vipj7dja1fchy8";
    deepClone = true; # git clone --recursive
  };
}
