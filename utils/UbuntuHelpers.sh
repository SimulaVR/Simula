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

# Assumes we are in project root
installGodot() {
  VERSION="3.1"
  RELEASE="alpha2"
  FILE="Godot_v"${VERSION}"-"${RELEASE}"_x11.64"
  OUT=${PWD}/godot

  cd /tmp
  # If it's a stable release, remove the ${RELEASE} part of the URL below
  if [ ! -f ${FILE} ]
  then wget https://downloads.tuxfamily.org/godotengine/${VERSION}/${RELEASE}/${FILE}.zip
  fi
  unzip ${FILE}.zip || echo "Could not unzip. Make sure unzip is installed."
  cp /tmp/${FILE} ${OUT} && echo "\
Installed Godot (version $(exec ${OUT} --version)): ${OUT}
Please put it somewhere in your \$PATH"
  cd -
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
