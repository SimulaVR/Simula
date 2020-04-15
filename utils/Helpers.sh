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

    sudo rm ./xfce4-terminal
    sudo rm ./xpra
    sudo rm ./xrdb
    sudo rm ./wmctrl
    sudo rm ./ffplay
    sudo ln -s $(which xfce4-terminal) xfce4-terminal
    sudo ln -s $(which xpra) xpra
    sudo ln -s $(which xrdb) xrdb
    sudo ln -s $(which wmctrl) wmctrl
    sudo ln -s $(which ffplay) ffplay
    cd -
}

checkInstallCachix() {
    if command -v cachix; then
        echo "cachix already installed.."
    else
        nix-env -iA cachix -f https://cachix.org/api/v1/install
    fi
}

checkInstallNix() {
    if command -v nix; then
        echo "nix already installed.."
    else
        curl https://nixos.org/nix/install | sh
        . $HOME/.nix-profile/etc/profile.d/nix.sh
    fi
}

installSimula() {
    checkInstallNix
    checkInstallCachix
    cachix use simula
    curl https://www.wolframcloud.com/obj/george.w.singer/installMessage
    if [ -z $1 ]; then
      NIXPKGS_ALLOW_UNFREE=1 nix-build default.nix --argstr driverCheck "$(./utils/DriverCheck.sh)" --arg devBuild "false"
    else
      NIXPKGS_ALLOW_UNFREE=1 nix-build -K default.nix --argstr driverCheck "$(./utils/DriverCheck.sh)" --arg devBuild "true"
    fi
}

swapXpraNixToLocal() {
    sudo rm ./result/bin/xpra
    sudo ln -s $(which xpra) ./result/bin/xpra
}
