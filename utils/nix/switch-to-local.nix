{
  writeShellApplication,
  nix,
}:

writeShellApplication {
  name = "switch-to-local";
  runtimeInputs = [
    nix
  ];
  text = ''
    nix develop '.?submodules=1#godot-haskell-plugin-dev'\
      --command sh -c\
        "cd ./addons/godot-haskell-plugin
        rm -f libgodot-haskell-plugin.so
        ln -s $(cabal list-bin flib:godot-haskell-plugin) libgodot-haskell-plugin.so"
  '';
}
