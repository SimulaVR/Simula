# The following functions assume they are called from project root.

installUbuntuDependencies() {
 sudo apt-get install build-essential \
                      haskell-stack \
                      libasound2-dev \
                      libcap-dev \
                      libdrm-dev \
                      libegl1-mesa-dev \
                      libelogind-dev \
                      libfreetype6-dev \
                      libgbm-dev \
                      libgl1-mesa-dev \
                      libgles2 \
                      libgles2-mesa-dev \
                      libgudev-1.0-dev \
                      libinput-dev \
                      libpixman-1-dev \
                      libpulse-dev \
                      libssl-dev \
                      libsystemd-dev \
                      libudev-dev \
                      libwayland-dev \
                      libx11-dev \
                      libx11-xcb-dev \
                      libxcb-composite0-dev \
                      libxcb-image0-dev \
                      libxcb-render-util0-dev \
                      libxcb-render0-dev \
                      libxcb-xfixes0-dev \
                      libxcb-xinput-dev \
                      libxcursor-dev \
                      libxkbcommon-dev \
                      libxi-dev \
                      libxinerama-dev \
                      libxkbcommon-x11-dev \
                      libxrandr-dev \
                      meson \
                      pkg-config \
                      scons \
                      steam \
                      steam-devices \
                      wayland-protocols \
                      yasm \
                      libwlroots-dev \
                      epiphany \
                      sakura \
                      cabal-install \
                      wmctrl \
                      xdotool \
                      bash
                      # libegl-dev       # Not provided in disco dango
                      # libxcb-iccm4-dev # Not provided in disco dango
                      # steam-runtime    # Not provided in disco dango


  # upgradeStack
}

installArchDependencies() {
 sudo pacman -S alsa-lib \
                freetype2 \
                glu \
                libcap \
                libdrm \
                libglvnd \
                libinput \
                libudev0-shim \
                libxcursor \
                libxi \
                libxinerama \
                libxkbcommon \
                libxkbcommon-x11 \
                libxrandr \
                mesa \
                meson \
                pixman \
                pulseaudio \
                scons \
                stack
                steam \
                systemd \
                wayland \
                wayland-protocols \
                yasm \
                wlroots \ # 0.5.0-1
                epiphany \
                sakura \
                cabal-install
 # yaourt -S elogind # optional dependency so we omit to avoid dealing with yaourt

  upgradeStack
}

installWlrootsInBuildFolder() {
    mkdir -p ./build
    cd ./build

    if [ -d wlroots ]; then
        echo "wlroots already built"
    else
        git clone --branch simula https://github.com/SimulaVR/wlroots
        cd wlroots
        meson --default-library static --buildtype release  build
        ninja -C build
        cd ..
    fi

    cd ..
}

# Only works on Ubuntu (or Debian based distros) and Arch
installNvidiaDrivers() {
    if hash apt 2>/dev/null; then
        sudo apt-get install nvidia-driver-418
    elif hash pacman 2>/dev/null; then
        sudo pacman -S nvidia
    else
        echo "Neither apt nor pacman is installed."
    fi
}

# Only works on Ubuntu (or Debian based distros) and Arch
installAMDDrivers() {
    if hash apt 2>/dev/null; then
        sudo add-apt-repository ppa:kisak/steamvr
        sudo apt update
        sudo apt dist-upgrade
        sudo apt-get install linux-generic-steamvr-18.04 \
                             xserver-xorg-hwe-18.04
                             mesa-vulkan-drivers
                             mesa-vulkan-drivers:i386
    elif hash pacman 2>/dev/null; then
        sudo pacman -S mesa vulkan-radeon libva-mesa-driver mesa-vdpau
    else
        echo "Neither apt nor pacman is installed."
    fi

}

upgradeStack() {
  stack upgrade
}

ensureGodotBinaryExists() {
    mkdir -p ./bin
    sudo rm ./bin/godot
    cp ./build/godot/bin/godot.x11.tools.64 ./bin/godot
    chmod +x ./bin/godot
}

# A helper function to
#   (i) Compile an instance of SimulaVR/godot with the gdwlroots bindings.
#  (ii) Use (i) to generate generate godot-haskell-gdwlroots-*.tar.gz
# The resulting tarball is playced in ./addons/godot-haskell-plugin/*.tar.gz
generateResourceTarballs() {
  if [ -e resources/godot.tar.gz -a -e resources/godot-haskell-gdwlroots.tar.gz ]; then
      echo "resources/*.tar.gz already exist"
  else
    local ROOTSIMULADIR=$(pwd)

    # Remove files and place ourselves in ./build
    mkdir -p resources
    cd ./resources
    sudo rm godot.tar.gz
    sudo rm godot-haskell-gdwlroots.tar.gz
    cd ..

    mkdir -p build
    cd ./build
    read -p "About to delete ./build/godot! If you are developing in that folder, press C-c to cancel script. Otherwise, press any key."
    sudo rm -r godot

    # Get ./godot + ./godot/modules/gdwlroots + ./godot/godot-haskell-gdwlroots
        # 3.06-stable doesn't work (compilation issues)
        # 3.1 doesn't work (compilation issues)
        # TODO: Use godot 3.1
    git clone --branch "3.2-simula" --recursive https://github.com/SimulaVR/godot godot
    cd godot
    git clone --branch simula --recursive https://github.com/SimulaVR/godot-haskell godot-haskell-gdwlroots
    cd modules
    git clone --branch "xwayland-3.2" --recursive https://github.com/SimulaVR/gdwlroots gdwlroots
    cd gdwlroots
    make all

    # Compile godot (w/ godot/modules/gdwlrots)
    cd ../..
    scons use_static_cpp=yes platform=x11 target=debug -j 8

    # Tarball godot and place in resources folder
    cd bin
    tar -cvzf ../../../resources/godot.tar.gz godot.x11.tools.64
    cd ..

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

    # Create godot-haskell-gdwlroots.tar.gz and place in resources folder
    cabal sdist
    mv ./dist/godot-haskell-3.1.0.0.tar.gz ../../../resources/godot-haskell-gdwlroots.tar.gz

    cd $ROOTSIMULADIR
  fi
}

ask() {
    # https://gist.github.com/davejamesmiller/1965569
    local prompt default reply

    if [ "${2:-}" = "Y" ]; then
        prompt="Y/n"
        default=Y
    elif [ "${2:-}" = "N" ]; then
        prompt="y/N"
        default=N
    else
        prompt="y/n"
        default=
    fi

    while true; do

        # Ask the question (not using "read -p" as it uses stderr not stdout)
        echo -n "$1 [$prompt] "

        # Read the answer (use /dev/tty in case stdin is redirected from somewhere else)
        read reply </dev/tty

        # Default?
        if [ -z "$reply" ]; then
            reply=$default
        fi

        # Check if the reply is valid
        case "$reply" in
            Y*|y*) return 0 ;;
            N*|n*) return 1 ;;
        esac

    done
}

# make godot-update
updateResourceGodot() {
    local ROOTSIMULADIR=$(pwd)

    sudo rm ./resources/godot.tar.gz
    cd ./build/godot
    scons use_static_cpp=yes platform=x11 target=debug -j 8

    # Tarball godot and place in resources folder
    cd bin
    tar -cvzf ../../../resources/godot.tar.gz godot.x11.tools.64
    cd "$ROOTSIMULADIR"
}

# make godot-haskell-gdwlroots-update
updateResourceGodotHaskellGdwlroots() {
    # Generate ./godot/godot-haskell-gdwlroots/api.json
    local ROOTSIMULADIR=$(pwd)

    sudo rm ./resources/godot-haskell-gdwlroots.tar.gz
    cd build/godot
    sudo rm -r godot-haskell-gdwlroots
    # git clone --branch gdwlroots --recursive https://github.com/SimulaVR/godot-haskell godot-haskell-gdwlroots
    git clone --branch simula --recursive https://github.com/SimulaVR/godot-haskell godot-haskell-gdwlroots

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

    # Create godot-haskell-gdwlroots.tar.gz and place in resources folder
    cabal sdist
    mv ./dist/godot-haskell-3.1.0.0.tar.gz ../../../resources/godot-haskell-gdwlroots.tar.gz

    cd $ROOTSIMULADIR
}

ubuntuAltTabReset() {
  gsettings reset org.gnome.desktop.wm.keybindings switch-applications
  gsettings get org.gnome.desktop.wm.keybindings switch-applications

  gsettings reset org.gnome.desktop.wm.keybindings switch-applications-backward
  gsettings get org.gnome.desktop.wm.keybindings switch-applications-backward
}

ubuntuAltTabDisable() {
  gsettings set org.gnome.desktop.wm.keybindings switch-applications "['']"
  gsettings set org.gnome.desktop.wm.keybindings switch-applications-backward "['']"
}

disableUbuntuSuperKey() {
  gsettings set org.gnome.mutter overlay-key ''
}

# Warning: installs to system!
compileWlroots() {
    cd ./submodules/wlroots

    meson build
    sudo ninja -C build
    sudo ninja -C build install

    cd -
}

# Requires a Godot launch to generate api.json
compileGodot() {
    cd ./submodules/godot

    cd ./modules/gdwlroots
    make all
    cd ../..

    scons platform=x11 target=debug -j 8

    if [ -e ./bin/godot.x11.tools.64 ]; then
        ./bin/godot.x11.tools.64 --gdnative-generate-json-api api.json
    fi

    cd ../..
}

compileGodotHaskell() {
    cd ./submodules/godot-haskell

    cd classgen
    if [ -e ../../godot/api.json ]; then
        stack build
        stack exec godot-haskell-classgen ../../godot/api.json
        cd ..
        cp -r src src.bak
        rsync -a classgen/src/ src/
    fi

    cd ../..
}

compileGodotHaskellPlugin() {
    cd ./addons/godot-haskell-plugin
    stack build
    cd -
}

switchToNix() {
    cd ./addons/godot-haskell-plugin
    rm libgodot-haskell-plugin.so
    ln -s ../../result/bin/libgodot-haskell-plugin.so libgodot-haskell-plugin.so
    cd -
}

switchToLocal() {
    cd ./addons/godot-haskell-plugin
    rm libgodot-haskell-plugin.so
    ln -s $(stack path --local-install-root)/lib/libgodot-haskell-plugin.so libgodot-haskell-plugin.so
    cd -

    mkdir -p ./result/bin
    cd ./result/bin

    sudo rm ./terminator
    sudo rm ./xpra
    sudo rm ./xrdb
    sudo rm ./wmctrl
    sudo ln -s $(which terminator) terminator
    sudo ln -s $(which xpra) xpra
    sudo ln -s $(which xrdb) xrdb
    sudo ln -s $(which wmctrl) wmctrl
    cd -
}

checkInstallCachix() {
    if [ ! command -v cachix ]; then
        nix-env -iA cachix -f https://cachix.org/api/v1/install
    fi
}

checkInstallNix() {
    if [ ! command -v nix ]; then
        curl https://nixos.org/nix/install | sh
        . $HOME/.nix-profile/etc/profile.d/nix.sh
    fi
}

installSimula() {
    checkInstallNix
    checkInstallCachix
    cachix use simula
    nix-build --option build-use-sandbox false default.nix --argstr driverCheck "$(./utils/DriverCheck.sh)"
}