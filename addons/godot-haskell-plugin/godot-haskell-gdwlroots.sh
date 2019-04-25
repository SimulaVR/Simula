

# A helper function to
#   (i) Compile an instance of godot with the gdwlroots bindings.
#  (ii) Use (i) to generate generate godot-haskell-gdwlroots-*.tar.gz
generateGodotHaskellGdwlroots() {
    # Remove files
    rm -r godot
    rm ./godot-haskell-gdwlroots-3.1.0.0.tar.gz

    # Get ./godot + ./godot/modules/gdwlroots + ./godot/godot-haskell-gdwlroots
    git clone --branch "3.0.6-stable" https://github.com/godotengine/godot godot
    
  cd godot
  git clone --branch gdwlroots --recursive https://github.com/SimulaVR/godot-haskell godot-haskell-gdwlroots
  cd modules
  git clone --branch master --recursive https://github.com/SimulaVR/gdwlroots gdwlroots
  cd gdwlrots
  make all

  # Compile godot (w/ godot/modules/gdwlrots)
  cd ../..
  scons platform=x11 -target=debug -j 4

  # Generate ./godot/godot-haskell-gdwlroots/api.json
  cd godot-haskell-gdwlroots
  rm api.json
  ../bin/godot.x11.tools.64 --gdnative-generate-json-api api.json

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
