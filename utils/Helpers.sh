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
    if [ "command -v cachix" ]; then
        echo "cachix already installed.."
    else
        nix-env -iA cachix -f https://cachix.org/api/v1/install
    fi
}

checkInstallNix() {
    if [ "command -v nix" ]; then
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
    nix-build default.nix --argstr driverCheck "$(./utils/DriverCheck.sh)"
}

pushSimulaToCachix() {
  nix-build default.nix --argstr driverCheck "nvidia 430.26 1rnfxl4dxa3jjidfdvfjmg1a8nc787ss15cakrp2wwrn8jlr9av6" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 430.34 0c3x25gilibbgazvp20d5sfmmgcf0gfqf024nzzqryxg4m05h39b" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 430.40 1myzhy1mf27dcx0admm3pbbkfdd9p66lw0cq2mz1nwds92gqj07p" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 430.50 1i9x9lr6izfq6csgnh8dfg3sa7s3has20hdpi7wlbla7msa36s0c" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 430.64 1k5s05a7yvrms97nr3gd8cbvladn788qamsmwr6jsmdzv6yh5gvk" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 435.17 19p9v5km1kfw45ghqmzgawa2fdq559jj6h1dkbnkbbzhp2syq757" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 435.21 0v3pq677ab01qdmwl5dawk8hn39qlwj05p8s9qzh9irmrlnc1izs" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 440.26 0ay3c4vhl8cqhl57vjar4p6v1nkh5zpvya41ag2sibj30spyg62z" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 440.31 03w5v3079c35sz3nkdk28yc76jb5hv8dy99jjy7pkywvbhw2ynfd" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 440.36 0nbdldwizb802w4x0rqnyb1p7iqz5nqiahqr534n5ihz21a6422h" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 440.44 057wq9p2vl87gy61f079b6d7clw2vhw3kq7rj411brhrnvr7shmd" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 440.59 162gq6w44l8sgnn4qnl2rdlx8c008p04zv4c3i1ps20p21n1mjv1" | cachix push simula
  nix-build default.nix --argstr driverCheck "nvidia 440.64 0xbm1dh95kz8h4d62pql2wmvw2gbgc7iif2bkixbnqijl4dryg71" | cachix push simula

  nix-build default.nix --argstr driverCheck "nixos" | cachix push simula
  nix-build default.nix --argstr driverCheck "intel" | cachix push simula
}