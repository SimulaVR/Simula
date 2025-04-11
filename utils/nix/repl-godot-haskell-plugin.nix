{ writeShellApplication, nix }:

writeShellApplication {
  name = "repl-godot-haskell-plugin";
  runtimeInputs = [ nix ];
  text = ''
    nix develop '.?submodules=1#godot-haskell-plugin-dev' --command sh -c "cd ./addons/godot-haskell-plugin; cabal repl"
  '';
}
