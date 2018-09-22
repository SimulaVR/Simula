installUbuntuDependencies() {
   sudo apt install     \
       libpixman-1-dev  \
       libweston-3-dev  \
       libegl1-mesa-dev \
       weston           \
       epiphany-browser \
       curl
}

installSteam() {
     if [ -z `which steam` ]; then
         sudo add-apt-repository multiverse
         sudo apt update
         sudo apt install steam # should include udev rules by default (https://github.com/ValveSoftware/SteamVR-for-Linux#usb-device-requirements)
     fi
}
 
installNvidiaDrivers() {
     # See https://github.com/ValveSoftware/SteamVR-for-Linux
     if [ -z `ls /etc/apt/sources.list.d/graphics-drivers-ubuntu-ppa-*.list` ]; then
         sudo add-apt-repository ppa:graphics-drivers/ppa
         sudo apt-get update
     fi
     sudo apt-get install nvidia-driver-396

     #  The following worked for me against "unmet dependency" errors:
     #    sudo apt install aptitude
     #    sudo aptitude install nvidia-driver-396
}

installAMDDrivers() {
  # ensure that mesa-17.3+ is installed.
  echo "TODO."
}

# Assumes our target is godot.x11.tools.64
installGodot() {
  sudo apt-get install build-essential scons pkg-config libx11-dev libxcursor-dev libxinerama-dev \
    libgl1-mesa-dev libglu-dev libasound2-dev libpulse-dev libfreetype6-dev libssl-dev libudev-dev \
    libxi-dev libxrandr-dev
  cd /tmp
  git clone https://github.com/godotengine/godot.git
  cd godot
  scons platform=x11
  cd ~/.local/bin
  cp /tmp/godot/bin/godot.x11.tools.64 godot # put godot in Ubuntu PATH
}

# It is best to install stack via curl to avoid
# old versions found in Ubuntu's package stores.
installStack() {
    if [ -z `which stack` ]; then
      curl -sSL https://get.haskellstack.org/ | sh -s - -f
      echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc # ensures stack programs like hpack are on your PATH
      . ~/.bashrc
    else 
      echo "Stack already installed."
    fi
}

# Use this if you already have stack installed via apt-get install.
upgradeStack() {
  #cd ~
  stack upgrade
  stack setup
  stack install hpack # Same with old versions of hpack
}

makeRTSBinaryMultithreaded () {
  cd ~/.stack/programs/x86_64-linux/ghc-8.4.3/lib/ghc-8.4.3/rts
  cp libHSrts-ghc8.4.3.so libHSrts-ghc8.4.3.so.bak
  cp libHSrts_thr-ghc8.4.3.so libHSrts-ghc8.4.3.so
}

buildSimulaDependencies() {
  installUbuntuDependencies 
  installSteam
  # installNvidiaDrivers
  installStack
  upgradeStack
}

buildSimula() {
   buildSimulaDependencies
   read -e -p "Press any key when SteamVR is launched."
   make run
}