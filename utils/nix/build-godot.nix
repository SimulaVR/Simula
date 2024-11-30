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
  name = "build-godot";
  runtimeInputs = [
    wayland-scanner
    scons
    inotify-tools
    pkg-config
    gcc
  ];
  text = ''
    export PKG_CONFIG_PATH="${lib.strings.makeSearchPath "lib/pkgconfig" pkgconfig-libpath}:${lib.strings.makeSearchPath "share/pkgconfig" pkgconfig-sharepath}"

    help_message () {
      echo "Usage: $0 help|build|watch"
    }

    build_once () {
      cd ./submodules/godot

      wayland-scanner server-header ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.h
      wayland-scanner private-code ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.c
      scons -Q -j8 platform=x11 target=debug warnings=no

      cd -
    }

    watch_build () {
      cd ./submodules/godot

      while inotifywait -qqre modify .
      do
        wayland-scanner server-header ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.h; wayland-scanner private-code ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.c; scons -Q -j8 platform=x11 target=debug warnings=no
      done

      cd -
    }

    while (( $# > 0 ))
    do
      case $1 in
        h | help)
          help_message
          exit 0
          ;;
        b | build)
          build_once
          exit 0
          ;;
        w | watch)
          watch_build
          exit 0
          ;;
        *)
          echo "Unknown argument: $1"
          exit 1
          ;;
      esac
    done

    help_message
    echo "No argument. exit with 2"
    exit 2
  '';
}
