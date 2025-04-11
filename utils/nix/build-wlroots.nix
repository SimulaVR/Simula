{
  callPackage,
  writeShellApplication,
  lib,

  # runtimeInputs
  nix,
}:

writeShellApplication {
  name = "build-wlroots";
  runtimeInputs = [ nix ];

  text = ''
    if [ -d "./submodules/wlroots/build" ]; then
      nix develop '.?submodules=1#wlroots-dev' --command sh -c 'cd ./submodules/wlroots; ninja -C build'
    else
      nix develop '.?submodules=1#wlroots-dev' --command sh -c 'cd ./submodules/wlroots; meson build; ninja -C build'
    fi
  '';
}
