{ stdenv, fetchurl, pkg-config, libpthreadstubs, libpciaccess, valgrind-light, lib }:

stdenv.mkDerivation rec {
  name = "libdrm-2.4.96";

  src = fetchurl {
    url = "https://dri.freedesktop.org/libdrm/${name}.tar.bz2";
    sha256 = "14xkip83qgljjaahzq40qgl60j54q7k00la1hbf5kk5lgg7ilmhd";
  };

  outputs = [ "out" "dev" "bin" ];

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ libpthreadstubs libpciaccess valgrind-light ];
    # libdrm as of 2.4.70 does not actually do anything with udev.

  patches = lib.optional stdenv.isDarwin ./libdrm-apple.patch;

  postPatch = ''
    for a in */*-symbol-check ; do
      patchShebangs $a
    done
  '';

  preConfigure = lib.optionalString stdenv.isDarwin
    "echo : \\\${ac_cv_func_clock_gettime=\'yes\'} > config.cache";

  configureFlags = [ "--enable-install-test-programs" ]
    ++ lib.optionals (stdenv.isAarch32 || stdenv.isAarch64)
      [ "--enable-tegra-experimental-api" "--enable-etnaviv-experimental-api" ]
    ++ lib.optional stdenv.isDarwin "-C"
    ++ lib.optional (stdenv.hostPlatform != stdenv.buildPlatform) "--disable-intel"
    ;

  meta = {
    homepage = https://dri.freedesktop.org/libdrm/;
    description = "Library for accessing the kernel's Direct Rendering Manager";
    license = "bsd";
    platforms = lib.platforms.unix;
  };
}
