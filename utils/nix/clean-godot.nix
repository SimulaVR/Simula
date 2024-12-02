{
  callPackage,
  writeShellApplication,
  lib,

  # runtimeInputs
  nix,
}:

writeShellApplication {
  name = "clean-godot";
  runtimeInputs = [
    nix
  ];
  text = ''
    nix develop '.?submodules=1#godot-dev' --command sh -c "cd ./submodules/godot; scons --clean"
  '';
}
