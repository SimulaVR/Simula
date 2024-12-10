{
  writeShellApplication,
  lib,
  nix,
  patchelf,
}:

writeShellApplication {
  name = "patch-godot-wlroots";
  runtimeInputs = [
    nix
    patchelf
  ];
  text = ''
    PATH_TO_SIMULA_WLROOTS="$(pwd)/submodules/wlroots/build/"
    OLD_RPATH=$(patchelf --print-rpath submodules/godot/bin/godot.x11.tools.64)

    # Check if the current RPATH contains our local simula wlroots build. If not, patchelf it to add it
    if [[ $OLD_RPATH != $PATH_TO_SIMULA_WLROOTS* ]]; then
      echo "Patching godot.x11.tools to point to local wlroots lib"
      echo "Changing path to: $PATH_TO_SIMULA_WLROOTS:$OLD_RPATH"
      patchelf --set-rpath "$PATH_TO_SIMULA_WLROOTS:$OLD_RPATH" submodules/godot/bin/godot.x11.tools.64
    else
      echo "Not patching godot.x11.tools, already patched."
    fi
  '';
}
