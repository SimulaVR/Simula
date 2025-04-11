{ writeShellApplication }:

writeShellApplication {
  name = "switch-to-nix";
  text = ''
    cd ./addons/godot-haskell-plugin
    rm -f libgodot-haskell-plugin.so
    ln -s ../../result/bin/libgodot-haskell-plugin.so libgodot-haskell-plugin.so
  '';
}
