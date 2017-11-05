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
  cd /tmp
  git clone https://github.com/wayland-project/wayland-protocols.git wayland-protocols
  cd /tmp/wayland-protocols
  ./autogen.sh
  ./configure
  make 
  sudo make install
}

installWeston2() {
  cd /tmp
  git clone https://github.com/wayland-project/weston.git weston
  cd /tmp/weston
  git checkout -b v2.0.0 2.0.0
  ./autogen.sh
  ./configure
  make
  sudo make install
}

installSteam() {
  sudo add-apt-repository multiverse
  sudo apt update
  sudo apt install steam
}

installNvidiaDrivers() {
  # see https://github.com/ValveSoftware/SteamVR-for-Linux
  sudo add-apt-repository ppa:graphics-drivers/ppa
  sudo apt-get update
  sudo apt-get install nvidia-384
}

# Unclear if this is needed long-term.
installOpenVR() {
    cd /tmp
    git clone https://github.com/ValveSoftware/openvr.git openvr
    cd /tmp/openvr
    mkdir build
    cd build
    cmake ..
    make
    sudo make install
    sudo ldconfig # OpenVR's `make install` fails to run this on its own

    # NOTE: At least once a developer has found it useful to copy
    #       the `libopenvr_api.so` from Simula to Ubuntu's global
    #       store, i.e.:
    #       sudo cp ./simula-openvr/openvr/lib/linux64/libopenvr_api.so \
    #                /usr/local/lib/libopenvr_api.so
}

addViveUdevRules() {
  # This package automatically adds Vive udev rules to
  # /lib/udev/rules.d/60-HTC-Vive-perms.rules
  sudo apt-get install steam-devices
}

# It is best to install stack via curl to avoid
# old versions found in Ubuntu's package stores.
installStack() {
    curl -sSL https://get.haskellstack.org/ | sh
    echo 'export PATH=$HOME/.local/bin:$PATH' >> ~/.bashrc # ensures stack programs like hpack are on your PATH
}

# Use this if you already have stack installed via apt-get install.
upgradeStack() {
  cd ~
  stack upgrade                                          # Old versions of stack can cause Simula build trouble
  stack install hpack                                    # Same with old versions of hpack
  echo 'export PATH=$HOME/.local/bin:$PATH' >> ~/.bashrc # Ensures stack programs like hpack are on your PATH
}

# Here we emphasize that Simula should be cloned recursively to
# bring in all necessary submodules.
buildSimulaInHomeDirectory() {
  cd ~
  git clone --recursive https://github.com/SimulaVR/Simula Simula
  cd ~/Simula
  stack setup
  stack build
}

# Here we emphasize that changing branches should result in
# a submodule update.
changeBranches() {
    git checkout "$1"
    git submodule update --init --recursive
}
