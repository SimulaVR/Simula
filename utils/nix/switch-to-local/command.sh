# This shell script is loaded by ./default.nix

cd ./addons/godot-haskell-plugin
rm -f libgodot-haskell-plugin.so
libpath=$(cabal list-bin flib:godot-haskell-plugin)
ln -s ${ibpath} libgodot-haskell-plugin.so
