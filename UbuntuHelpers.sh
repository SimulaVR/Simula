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

# requires Wayland Protocols
installWeston() {
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
}

update_gcc_to_7.2() {
  sudo add-apt-repository ppa:ubuntu-toolchain-r/test
  sudo apt-get update
  sudo apt-get install gcc-7
  sudo apt-get install g++-7
  sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-7 60 --slave /usr/bin/g++ g++ /usr/bin/g++-7
  # `gcc -v` should yield version 7.2
  # `g++ -v` should yield version 7.2
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
    local SIMULA_DIR=$(pwd)
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
    cd "$SIMULA_DIR"
    sudo cp ./simula-openvr/openvr/lib/linux64/libopenvr_api.so /usr/local/lib
}

# If you have Ubuntu 17.10 you can just run `sudo apt-get install steam-devices` instead of this command
addViveUdevRules() {
  local VIVE_RULES="/lib/udev/rules.d/60-HTC-Vive-perms.rules";

  if [ ! -f /lib/udev/rules.d/60-HTC-Vive-perms.rules ]; then
    echo '# HTC Vive HID Sensor naming and permissioning'                                                            >> $VIVE_RULES
    echo 'KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0bb4", ATTRS{idProduct}=="2c87", TAG+="uaccess"' >> $VIVE_RULES
    echo 'KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2101", TAG+="uaccess"' >> $VIVE_RULES
    echo 'KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2000", TAG+="uaccess"' >> $VIVE_RULES
    echo 'KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="1043", TAG+="uaccess"' >> $VIVE_RULES
    echo 'KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2050", TAG+="uaccess"' >> $VIVE_RULES
    echo 'KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2011", TAG+="uaccess"' >> $VIVE_RULES
    echo 'KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2012", TAG+="uaccess"' >> $VIVE_RULES
    echo 'SUBSYSTEM=="usb", ATTRS{idVendor}=="0bb4", ATTRS{idProduct}=="2c87", TAG+="uaccess"'                       >> $VIVE_RULES
    echo '# HTC Camera USB Node'                                                                                     >> $VIVE_RULES
    echo 'SUBSYSTEM=="usb", ATTRS{idVendor}=="114d", ATTRS{idProduct}=="8328", TAG+="uaccess"'                       >> $VIVE_RULES
    echo '# HTC Mass Storage Node'                                                                                   >> $VIVE_RULES
    echo 'SUBSYSTEM=="usb", ATTRS{idVendor}=="114d", ATTRS{idProduct}=="8200", TAG+="uaccess"'                       >> $VIVE_RULES
    echo 'SUBSYSTEM=="usb", ATTRS{idVendor}=="114d", ATTRS{idProduct}=="8a12", TAG+="uaccess"'                       >> $VIVE_RULES
  fi
}

# It is best to install stack via curl to avoid
# old versions found in Ubuntu's package stores.
installStack() {
    curl -sSL https://get.haskellstack.org/ | sh
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc # ensures stack programs like hpack are on your PATH
    . ~/.bashrc
}

# Use this if you already have stack installed via apt-get install.
upgradeStack() {
  cd ~
  stack setup
  stack install hpack                                    # Same with old versions of hpack
}

buildSimulaDependencies() {
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
}

buildSimula() {
  make init
  stack setup --install-ghc
  stack build --extra-lib-dirs=/usr/local/lib
  echo "Remember to open steam and install and run SteamVR before launching Simula."
}
