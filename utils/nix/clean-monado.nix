{
  writeShellApplication,
  nix,
}:

writeShellApplication {
  name = "clean-monado";
  runtimeInputs = [
    nix
  ];
  text = ''
    nix develop '.?submodules=1#monado-dev' --command sh -c "cd ./submodules/monado; rmBuilds"
  '';
}
