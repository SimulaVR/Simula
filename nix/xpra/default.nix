{ stdenv, lib, fetchurl, callPackage, substituteAll, python3, pkg-config
, xorg, gtk3, glib, pango, cairo, gdk-pixbuf, atk
, wrapGAppsHook, xorgserver, getopt, xauth, utillinux, which
, ffmpeg_4, x264, libvpx, libwebp, x265
, libfakeXinerama
, gst_all_1, pulseaudio, gobject-introspection
, pam, python3Packages, pkgs }:

with lib;

let
  inherit (python3.pkgs) cython buildPythonApplication;

  xf86videodummy = callPackage ./xf86videodummy { };

  pyopengl-fixed = callPackage ./pyopengl.nix { fetchPypi = python3Packages.fetchPypi; buildPythonPackage = python3Packages.buildPythonPackage; pillow = python3Packages.pillow; };


in buildPythonApplication rec {
  pname = "xpra";
  version = "3.0.5";

  src = fetchurl {
    url = "https://xpra.org/src/${pname}-${version}.tar.xz";
    sha256 = "1zy4q8sq0j00ybxw3v8ylaj2aj10x2gb0a05aqbcnrwp3hf983vz";
  };

  patches = [
    (substituteAll {
      src = ./fix-paths.patch;
      inherit (xorg) xkeyboardconfig;
    })
  ];

  postPatch = ''
    substituteInPlace setup.py --replace '/usr/include/security' '${pam}/include/security'
  '';

  nativeBuildInputs = [ pkg-config wrapGAppsHook ];
  buildInputs = with xorg; [
    libX11 xorgproto libXrender libXi
    libXtst libXfixes libXcomposite libXdamage
    libXrandr libxkbfile
    ] ++ [
    cython

    pango cairo gdk-pixbuf atk.out gtk3 glib

    ffmpeg_4 libvpx x264 libwebp x265

    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-plugins-bad
    gst_all_1.gst-libav

    pam
    gobject-introspection
  ];
  propagatedBuildInputs = with python3.pkgs; [
    pillow rencode pycrypto cryptography pycups lz4 dbus-python
    netifaces numpy pygobject3 pycairo gst-python pam
    paramiko opencv4 python-uinput pyxdg
    ipaddress idna pyopengl-fixed
  ];

    # error: 'import_cairo' defined but not used
  NIX_CFLAGS_COMPILE = "-Wno-error=unused-function";

  setupPyBuildFlags = [
    "--without-Xdummy"
    "--without-Xdummy_wrapper"
    "--with-gtk3"
    "--without-gtk2"
    # Override these, setup.py checks for headers in /usr/* paths
    "--with-pam"
    "--with-vsock"
  ];

  preFixup = ''
    gappsWrapperArgs+=(
      --set XPRA_INSTALL_PREFIX "$out"
      --prefix LD_LIBRARY_PATH : ${libfakeXinerama}/lib
      --prefix PATH : ${lib.makeBinPath [ getopt xorgserver xauth which utillinux pulseaudio ]}
    )
  '';

  doCheck = false;

  enableParallelBuilding = true;

  # passthru = { inherit xf86videodummy; };

  meta = {
    homepage = http://xpra.org/;
    downloadPage = "https://xpra.org/src/";
    downloadURLRegexp = "xpra-.*[.]tar[.]xz$";
    description = "Persistent remote applications for X";
    platforms = platforms.linux;
    license = licenses.gpl2;
    maintainers = with maintainers; [ tstrobel offline numinit ];
  };
}
