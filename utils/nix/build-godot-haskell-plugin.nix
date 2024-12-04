{
  writeShellApplication,
  nix,
  inotify-tools,
}:

writeShellApplication {
  name = "build-godot-haskell-plugin";
  runtimeInputs = [
    nix
    inotify-tools
  ];
  text = ''
    help_message () {
      echo "Usage: $0 help|build|profileBuild|watch"
    }

    build_once () {
      nix develop '.?submodules=1#godot-haskell-plugin-dev'\
        --command sh -c\
          'cd ./addons/godot-haskell-plugin
           cabal build'
    }

    profile_build () {
      nix develop '.?submodules=1#godot-haskell-plugin-dev'\
        --command sh -c\
          'cd ./addons/godot-haskell-plugin
           cabal --enable-profiling build --ghc-options="-fprof-auto -rtsopts -fPIC -fexternal-dynamic-refs"'
    }

    watch_build () {
      nix develop '.?submodules=1#godot-haskell-plugin-dev'\
        --command sh -c\
          'cd ./addons/godot-haskell-plugin
           while inotifywait -qqre modify .;
           do
             cabal build
           done'
    }

    # --- Main process

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
        p | profileBuild)
          profile_build
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
