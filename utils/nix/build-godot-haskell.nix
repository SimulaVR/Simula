{
  writeShellApplication,
  nix,
  godot,
  godot-haskell-classgen,
}:

writeShellApplication {
  name = "build-godot-haskell";
  runtimeInputs = [
    nix
    godot
    godot-haskell-classgen
  ];
  text = ''
    LD_LIBRARY_PATH=./submodules/godot/modules/gdleapmotionV2/LeapSDK/lib/x64 godot.x11.tools.64 --gdnative-generate-json-api ./api.json

    nix develop .?submodules=1#godot-haskell-cabal-dev --command sh -c "cd ./submodules/godot-haskell-cabal; ./updateApiJSON.sh"
  '';
}
