{
  callPackage,
  writeShellApplication,
  lib,

  xorg,
  wayland,
  libGL,
  libinput,
  libxkbcommon,
  pixman,
  libcap,
  libpng,
  libglvnd,
  libdrm,
  mesa,
  eudev,
  systemdLibs,
  ffmpeg,
  udev,
  eglexternalplatform,
  wayland-protocols,

  # runtimeInputs
  gcc,
  meson,
  cmake,
  ninja,
  pkg-config,
  wayland-scanner,

  # Imported from submodules
  libxcb-errors ? callPackage ../../submodules/wlroots/libxcb-errors { },
}:

let
  pkgconfig-libpath = [
    xorg.xcbutilwm
    xorg.xcbutilimage
    xorg.xcbutilerrors
    xorg.libX11.dev
    xorg.libxcb.dev
    xorg.xinput
    wayland.dev
    libGL
    libinput.dev
    libxkbcommon.dev
    pixman
    libcap.dev
    libpng.dev
    libglvnd.dev
    libdrm.dev
    mesa.dev
    eudev
    systemdLibs.dev
    ffmpeg.dev
    udev.dev
    eglexternalplatform

    libxcb-errors
  ];
  pkgconfig-sharepath = [
    wayland-protocols
    xorg.xorgproto
  ];
in

writeShellApplication {
  name = "build-wlroots";
  runtimeInputs = [
    gcc
    meson
    cmake
    ninja
    pkg-config
    wayland-scanner
  ];

  text = ''
    export PKG_CONFIG_PATH="${lib.strings.makeSearchPath "lib/pkgconfig" pkgconfig-libpath}:${lib.strings.makeSearchPath "share/pkgconfig" pkgconfig-sharepath}"

    cd ./submodules/wlroots

    if [ -d "./build" ]; then
      ninja -C build
    else
      meson build\
        -Dlibcap=enabled\
        -Dlogind=enabled\
        -Dxwayland=enabled\
        -Dx11-backend=enabled\
        -Dxcb-icccm=disabled\
        -Dxcb-errors=enabled
      ninja -C build
    fi

    cd -
  '';
}
