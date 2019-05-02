

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


generateGodotHaskellGdwlroots3.1() {
    # Remove old files from last time this command ran
    sudo rm -r godot
    sudo rm ./godot-haskell-gdwlroots-3.1.0.0.tar.gz
    sudo rm ./godot-3.1-gdwlroots.tar.gz

    # Get Godot source code
    wget -O godot-3.1-gdwlroots.tar.gz "https://github.com/lboklin/godot/archive/3.1-gdwlroots.tar.gz"
    tar -xvf godot-3.1-gdwlroots.tar.gz
    mv godot-3.1-gdwlroots godot

    # Put Godot binary inside new source code
    cd godot
    mkdir bin
    cd bin
    wget -O godot.x11.opt.tools.64 "https://github.com/lboklin/godot/releases/download/3.1-gdwlroots/godot.x11.opt.tools.64"
    cd ..

    # Get godot-haskell-gdwlroots
    git clone --recursive --branch gdwlroots-3.1 https://github.com/SimulaVR/godot-haskell.git godot-haskell-gdwlroots
    cd godot-haskell-gdwlroots

    # Create and place godot-haskell-gdwlroots-3.1.0.0.tar.gz
    cabal sdist
    mv ./dist/godot-haskell-3.1.0.0.tar.gz ../../godot-haskell-gdwlroots-3.1.0.0.tar.gz
    cd ../..
}