{
  writeShellApplication,
  nix,
}:

writeShellApplication {
  name = "build-godot";
  runtimeInputs = [
    nix
  ];
  text = ''
    nix develop '.?submodules=1#monado-dev' --command sh -c "cd ./submodules/monado; nsBuildMonadoIncremental"
  '';
}
