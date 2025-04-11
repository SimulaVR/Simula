{
  writeShellApplication,
  nix,
  cabal-install,
  cmd-sh ? ./command.sh,
}:

writeShellApplication {
  name = "switch-to-local";
  runtimeInputs = [ nix ];
  text = ''
    nix develop '.?submodules=1#godot-haskell-plugin-dev' --command sh -c ${cmd-sh}
  '';
}
