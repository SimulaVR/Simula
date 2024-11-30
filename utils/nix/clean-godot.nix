{
  callPackage,
  writeShellApplication,
  lib,
  xorg,
  libGLU,
  libglvnd,
  zlib,
  alsa-lib,
  pulseaudio,
  eudev,
  libxkbcommon,
  wayland,
  pixman,
  dbus,

  # runtimeInputs
  wayland-scanner,
  scons,
  gcc,
  pkg-config,
  inotify-tools,
  
  # Imported from submodules
  wlroots ? callPackage ../../submodules/wlroots { },
  libxcb-errors ? callPackage ../../submodules/wlroots/libxcb-errors { },
}:

let
  pkgconfig-libpath = [
    xorg.libX11.dev
    xorg.libXcursor.dev
    xorg.libXinerama.dev
    xorg.libXext.dev
    xorg.libXrandr.dev
    xorg.libXrender.dev
    xorg.libXi.dev
    xorg.libXfixes.dev
    xorg.libxcb.dev

    libGLU.dev
    libglvnd.dev
    zlib.dev
    alsa-lib.dev
    pulseaudio.dev
    eudev
    libxkbcommon.dev
    wayland.dev
    pixman
    dbus.dev

    libxcb-errors
    wlroots
  ];
  pkgconfig-sharepath = [
    xorg.xorgproto
  ];
in

writeShellApplication {
  name = "clean-godot";
  runtimeInputs = [
    wayland-scanner
    scons
    pkg-config
    gcc
  ];
  text = ''
    export PKG_CONFIG_PATH="${lib.strings.makeSearchPath "lib/pkgconfig" pkgconfig-libpath}:${lib.strings.makeSearchPath "share/pkgconfig" pkgconfig-sharepath}"

    cd ./submodules/godot
    scons --clean
    cd -
  '';
}
