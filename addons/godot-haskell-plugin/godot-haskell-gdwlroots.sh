

# A helper function to
#   (i) Compile an instance of SimulaVR/godot with the gdwlroots bindings.
#  (ii) Use (i) to generate generate godot-haskell-gdwlroots-*.tar.gz
generateGodotHaskellGdwlroots() {
  # Remove files
  sudo rm -r godot
  sudo rm ./godot-haskell-gdwlroots-3.1.0.0.tar.gz

  # Get ./godot + ./godot/modules/gdwlroots + ./godot/godot-haskell-gdwlroots
    # 3.06-stable doesn't work (compilation issues)
    # 3.1 doesn't work (compilation issues) 
    # TODO: Use godot 3.1
  git clone https://github.com/SimulaVR/godot godot
  cd godot
  git clone --branch gdwlroots --recursive https://github.com/SimulaVR/godot-haskell godot-haskell-gdwlroots
  cd modules
  git clone --branch master --recursive https://github.com/SimulaVR/gdwlroots gdwlroots
  cd gdwlroots
  make all

  # Compile godot (w/ godot/modules/gdwlrots)
  cd ../..
  scons platform=x11 target=debug -j 8

  # Generate ./godot/godot-haskell-gdwlroots/api.json
  cd godot-haskell-gdwlroots
  rm api.json
  DISPLAY=:1 ../bin/godot.x11.tools.64 --gdnative-generate-json-api api.json

  # Morph ./godot/godot-haskell-gdwlroots source code to reflect updated api.json
  cd classgen
  stack build
  stack exec godot-haskell-classgen ../api.json
  cd ..
  cp -r src src.bak
  rsync -a classgen/src/ src/

  # Create godot-haskell-gdwlroots-*.tar.gz
  cabal sdist
  mv ./dist/godot-haskell-3.1.0.0.tar.gz ../../godot-haskell-gdwlroots-3.1.0.0.tar.gz
}