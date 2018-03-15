if [ -z $SIM_ROOT ]; then
    echo "$SIM_ROOT is empty. Need to know project root path."
    exit 1
else
    echo "Project root: $SIM_ROOT"
fi

installUbuntuDependencies() {
  sudo apt install \
      g++ \
      automake \
      autoconf \
      autoconf-archive \
      make \
      cmake \
      libtool \
      pkg-config \
      binutils-dev \
      libegl1-mesa-dev \
      libgles2-mesa-dev \
      libxcb-composite0-dev \
      libxcursor-dev \
      libcairo2-dev \
      libpixman-1-dev \
      libgbm-dev \
      libmtdev-dev \
      libinput-dev \
      libxkbcommon-dev \
      libpam0g-dev \
      libgflags-dev \
      libgoogle-glog-dev \
      libssl-dev \
      libdouble-conversion-dev \
      libevent-dev \
      libboost-context-dev \
      libboost-chrono-dev \
      libboost-filesystem-dev \
      libboost-iostreams-dev \
      libboost-locale-dev \
      libboost-program-options-dev \
      libboost-regex-dev \
      libboost-system-dev \
      libboost-thread-dev \
      libsdl2-dev \
      libopencv-dev \
      libjsoncpp-dev \
      libxml2-dev \
      libusb-1.0-0-dev \
      libspdlog-dev \
      libeigen3-dev \
      libvulkan-dev
}

installWaylandProtocols() {
    if [ ! -e "/usr/local/share/wayland-protocols" ]; then
        cd /tmp
        git clone https://github.com/wayland-project/wayland-protocols.git wayland-protocols
        cd /tmp/wayland-protocols
        ./autogen.sh
        ./configure
        make 
        sudo make install
    fi
}

# requires Wayland Protocols
installWeston() {
    if [ ! -e "/usr/local/lib/libweston-3" ]; then
      cd /tmp
      git clone https://github.com/wayland-project/weston.git weston
      cd /tmp/weston
      if [ -z $1 ]; then
        git checkout -b v3.0.0 3.0.0
      else
        git checkout -b v"$1" "$1" # allows us to opt in to alternative versions i.e. "2.0.0"
      fi
      ./autogen.sh
      ./configure
      make
      sudo make install
    fi
}

update_gcc_to_7.2() {
    if [ ! -e "/usr/bin/gcc-7" ]; then
        if [ -z `ls /etc/apt/sources.list.d/ubuntu-toolchain-r-ubuntu-test-*.list` ]; then
            sudo add-apt-repository ppa:ubuntu-toolchain-r/test
            sudo apt-get update
        fi
        sudo apt-get install gcc-7
        sudo apt-get install g++-7
        sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-7 60 --slave /usr/bin/g++ g++ /usr/bin/g++-7
    fi
}

installSteam() {
    if [ -z `which steam` ]; then
        sudo add-apt-repository multiverse
        sudo apt update
        sudo apt install steam
    fi
}

installNvidiaDrivers() {
    # see https://github.com/ValveSoftware/SteamVR-for-Linux
    if [ -z `ls /etc/apt/sources.list.d/graphics-drivers-ubuntu-ppa-*.list` ]; then
        sudo add-apt-repository ppa:graphics-drivers/ppa
        sudo apt-get update
    fi
    sudo apt-get install nvidia-387
}

# Unclear if this is needed long-term.
installOpenVR() {
    local SIMULA_DIR=$(pwd)
    cd /tmp
    git clone https://github.com/ValveSoftware/openvr.git openvr
    cd /tmp/openvr
    mkdir build
    cd build
    cmake -DBUILD_SHARED=ON .. # This forces openvr to build `libopenvr_api.so` as opposed to `libopenvr_api.a`, so that GHCi works
    make
    sudo make install
    sudo ldconfig # OpenVR's `make install` fails to run this on its own

    # NOTE: At least once a developer has found it useful to copy
    #       the `libopenvr_api.so` from Simula to Ubuntu's global
    #       store, i.e.:
    cd "$SIMULA_DIR"
    sudo cp ./simula-openvr/openvr/lib/linux64/libopenvr_api.so /usr/local/lib
}

# It is best to install stack via curl to avoid
# old versions found in Ubuntu's package stores.
installStack() {
    curl -sSL https://get.haskellstack.org/ | sh -s - f
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc # ensures stack programs like hpack are on your PATH
    . ~/.bashrc
}

# Use this if you already have stack installed via apt-get install.
upgradeStack() {
  #cd ~
  stack upgrade
  stack setup
  stack install hpack                                    # Same with old versions of hpack
}

buildSimulaDependencies() {
    outputStageBegin "Building dependencies.."
    installUbuntuDependencies
    installWaylandProtocols
    installWeston
    update_gcc_to_7.2
    installSteam
    installNvidiaDrivers
    installOpenVR
    addViveUdevRules
    installStack
    upgradeStack
    outputStageEnd
}

buildSimula() {
    outputStageBegin "Building Simula.."

    make init
    stack --no-nix setup --install-ghc
    stack --no-nix build --extra-lib-dirs=/usr/local/lib

    outputStageEnd

    echo "Remember to open steam and install and run SteamVR before launching Simula."
}
