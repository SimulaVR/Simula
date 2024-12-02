{
  callPackage,
  writeShellApplication,
  lib,
  nix,
}:

writeShellApplication {
  name = "build-godot";
  runtimeInputs = [
    nix
  ];
  text = ''
    help_message () {
      echo "Usage: $0 help|build|watch"
    }

    build_once () {
      nix develop '.?submodules=1#godot-dev'\
        --command sh -c\
          "cd ./submodules/godot
           wayland-scanner server-header ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.h
           wayland-scanner private-code ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.c
           scons -Q -j8 platform=x11 target=debug warnings=no"
    }

    watch_build () {
      nix develop '.?submodules=1#godot-dev'\
        --command sh -c\
          "cd ./submodules/godot

           while inotifywait -qqre modify .
           do
             wayland-scanner server-header ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.h
             wayland-scanner private-code ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.c
             scons -Q -j8 platform=x11 target=debug warnings=no
           done"
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
