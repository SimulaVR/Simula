{ stdenv, lib, fetchFromGitHub, cmake, python3, vulkan-headers, pkgconfig
, freetype, fontconfig, xorg, libxcb, libXrandr, libXext, wayland, addOpenGLRunpath, mesa_drivers }:

stdenv.mkDerivation rec {
  pname = "vulkan-loader";
  version = "1.2.162.0";

  src = fetchFromGitHub {
    owner = "KhronosGroup";
    repo = "Vulkan-Loader";
    rev = "sdk-${version}";
    sha256 = "0w9i2pliw4ccmjyfzff4i2f3hxwsfd54jg7ahv2v634qmx59bsbi";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ cmake python3 freetype fontconfig xorg.xorgproto xorg.libX11 xorg.libXt xorg.libXft xorg.libXext xorg.libSM xorg.libICE libxcb libXrandr libXext wayland ];
  enableParallelBuilding = true;

  preConfigure = ''
    substituteInPlace loader/vulkan.pc.in --replace 'includedir=''${prefix}/include' 'includedir=${vulkan-headers}/include'
  '';

  cmakeFlags = [
    "-DSYSCONFDIR=${mesa_drivers}/share" # Don't use addOpenGLRunpath.driverLink
    "-DVULKAN_HEADERS_INSTALL_DIR=${vulkan-headers}"
  ];

  outputs = [ "out" "dev" ];

  meta = with lib; {
    description = "LunarG Vulkan loader";
    homepage    = "https://www.lunarg.com";
    platforms   = platforms.linux;
    license     = licenses.asl20;
    maintainers = [ maintainers.ralith ];
  };
}
