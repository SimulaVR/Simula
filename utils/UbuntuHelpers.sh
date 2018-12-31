installUbuntuDependencies() {
    ask "Install system dependencies? (requires sudo)" Y \
        && sudo apt install     \
                libpixman-1-dev  \
                libweston-3-dev  \
                libegl1-mesa-dev \
                weston           \
                curl
}

installSimulaVRGodotFork() {
    rm -r godot
    git clone --recursive https://github.com/SimulaVR/godot
    cd godot
    sudo apt-get install build-essential scons pkg-config libx11-dev libxcursor-dev libxinerama-dev \
         libgl1-mesa-dev libglu-dev libasound2-dev libpulse-dev libfreetype6-dev libssl-dev libudev-dev \
         libxi-dev libxrandr-dev yasm
    scons -j8 platform=x11
    mv ~/.local/bin/godot ~/.local/bin/godot.bak.$(date +%F-%T)
    cp ./bin/godot.x11.tools.64 ~/.local/bin/godot
    cd ..
}

installSteam() {
    if [ -z `which steam` ]; then
        if [ ask "Steam not installed. Do you consent to enabling the non-free multiverse repo?" ]; then
            sudo add-apt-repository multiverse \
                && sudo apt update
            # Steam should include udev rules by default (https://github.com/ValveSoftware/SteamVR-for-Linux#usb-device-requirements)
            sudo apt install steam \
                && sudo udevadm control --reload-rules \
                && sudo udevadm trigger
        else echo "Installation of Steam aborted"
        fi
    fi
}
 
# Assumes we are in project root
installGodot() {
  VERSION="3.1"
  RELEASE_TYPE="alpha"
  RELEASE=${RELEASE_TYPE}"2"
  FILE="Godot_v"${VERSION}"-"${RELEASE}"_x11.64"
  OUT=${PWD}/godot
  # If it's a stable release, remove the ${RELEASE} part of the URL below
  URL=https://downloads.tuxfamily.org/godotengine/${VERSION}/${RELEASE}/${FILE}.zip

  cd /tmp

  if [ ! `${OUT} --version` = "${VERSION}.${RELEASE_TYPE}.official" ]; then
      if [ ! -f ${FILE} ] ; then
          wget ${URL}
      fi
      if [ -z `which unzip` ]; then
          if [ ask "The unzip tool is not installed. Install?" Y ]; then
              sudo apt install -y unzip
          else
              echo "Well.. in that case I can't install Godot."
              return 1
          fi
      fi
      unzip ${FILE}.zip
      cp /tmp/${FILE} ${OUT} 
  fi

  if [ -f ${OUT} ]; then
      echo "Godot (version $(exec ${OUT} --version)) found at: ${OUT}"
  else
      echo "${OUT} does not exist."
      return 1
  fi

  cd - 1>/dev/null
}

# It is best to install stack via curl to avoid
# old versions found in Ubuntu's package stores.
installStack() {
    if [ -z `which stack` ]; then
        echo "Installing Stack"
        curl -sSL https://get.haskellstack.org/ | sh -s - -f
    else echo "Stack already installed"
    fi
}

# Use this if you already have stack installed via apt-get install.
upgradeStack() {
  stack upgrade
  echo ""
  # Same with old versions of hpack
  echo "Installing hpack"
  stack install hpack
}

buildSimulaDependencies() {
    UBUNTU=`grep -i ubuntu /etc/lsb-release`
    if [ ! -z ${UBUNTU} ]; then
        installUbuntuDependencies
    fi

    installSteam
    echo ""
    installGodot
    echo ""
    installStack \
        && echo "" \
        && upgradeStack
    echo ""
}

buildSimula() {
    buildSimulaDependencies \
        && make
    echo "
To run Simula in VR you must make sure to configure your system according
to the instructions at 'https://github.com/ValveSoftware/SteamVR-for-Linux',
then use 'make run' while SteamVR is open."
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
