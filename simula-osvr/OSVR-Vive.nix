    { pkgs, stdenv, fetchgit, cmake, eigen3_3, boost, jsoncpp }:

    stdenv.mkDerivation {
      name = "OSVR-Vive";
      buildInputs = with pkgs; [ cmake
                                (callPackage ./libfunctionality.nix { })
                                (callPackage ./OSVR-Core.nix { })
                                eigen3_3
                                boost
                                jsoncpp
                                ];
      src = fetchgit {
        url       = "https://github.com/OSVR/OSVR-Vive.git";
        rev       = "e0ebcdbe2d065448fcebacc2828712a946695004";
        sha256    = "1d10gp7xalqdclskxc804fp56gz3k1sqzzqbdm3y54iwshmahwfw";
        deepClone = true; # git clone --recursive
      };
    }
