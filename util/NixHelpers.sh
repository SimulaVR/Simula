# This is safe to run even when Nix is installed.
installNix() {
  if [ ! -d /nix/store ]; then
      echo "No Nix store found on system. Beginning download and installation of Nix.."
      curl https://nixos.org/nix/install | sh
  fi

  if [ -z `which nix-env` ]; then
      source ~/.nix-profile/etc/profile.d/nix.sh
  fi

  # Probably shouldn't set allow unfree software without user consent.
  # mkdir -p ~/.config/nixpkgs
  # echo "{ allowUnfree = true; }" >> ~/.config/nixpkgs/config.nix
}

checkIfUnfreeAllowed() {
    if [ ! $NIXPKGS_ALLOW_UNFREE ]; then
        echo "Regrettably, the project currently relies on SteamVR, which is proprietary software."
        echo "If you are okay with this, allow unfree packages temporarily with \`export NIXPKS_ALLOW_UNFREE=1\` and re-run this script."
        echo "We intend to free the project from any proprietary dependencies in the future."
        exit 1
    fi
}

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

buildSimulaWithNix() {
  addViveUdevRules
  make init
  stack --nix build
  echo "Remember to open steam and install and run SteamVR before launching Simula."
}

launchSteamVR() {
    local VRMONITOR=$HOME/.local/share/Steam/steamapps/common/SteamVR/bin/vrmonitor.sh

    if [ ! -e $VRMONITOR ]; then
        echo "SteamVR must first be installed through Steam."
        exit 1
    fi

    if [ ! -e $HOME/.steam/steam/ubuntu12_32/steam-runtime/run.sh ]; then
        source ./util/NixHelpers.sh && \
            fixSteamVROnNixos
    fi

    echo "Launching SteamVR.."
    nix-shell -p lsb-release usbutils procps --run 'steam-run bash -c "export PATH=$PATH ; ~/.local/share/Steam/steamapps/common/SteamVR/bin/vrmonitor.sh"'
}

launchSimulaWithNix() {
    echo "Remember to open steam and install and run SteamVR before launching Simula."

    if [ -z `pidof steam` ]; then
        echo "Launching Steam.."
        steam &>/dev/null &
        sleep 5
    fi

    if [ -z `pidof vrmonitor` ]; then
        launchSteamVR &>/dev/null &
    fi

    echo "Launching Simula.."
    stack --nix exec -- simulavr
}

## NixOS ##

# FIXME: Bad/ugly fix and requires sudo.
# Hack to fix error:
#    > libEGL warning: DRI2: failed to open swrast (search paths /run/opengl-driver/lib/dri)
# Fix was found here https://github.com/NixOS/nixpkgs/issues/9415#issuecomment-336494579
fixswrast() {
    OUT=/run/opengl-driver

    if [ ! -d $OUT ]; then
        echo "$OUT does not exist, so we're creating it. Write access to /run requires sudo:"
        sudo nix-build '<nixpkgs>' -A 'mesa_noglu.drivers' -o /run/opengl && \
            sudo mv /run/opengl-drivers $OUT && \
            echo "Successfully created $OUT." || \
                echo "Failed at creating $OUT."
    fi

    if [ -d $OUT ]; then
        SWRAST=`find $OUT -follow -name "swrast*"`
        if [ ! -z $SWRAST ]; then
            echo "Happily surprised to find $SWRAST"
        else
            echo "Warning: There's no swrast in $OUT"
        fi
    fi
}

fixSteamVROnNixOS() {
    local RUNTIMEDIR=$HOME/.steam/steam/ubuntu12_32/steam-runtime

    if [ ! -e $RUNTIMEDIR/run.sh ]; then
        mkdir -p $RUNTIMEDIR
        cp ./nixos/run.sh $RUNTIMEDIR/run.sh && \
            echo "Required script has been installed: $RUNTIMEDIR/run.sh"
    fi
}

buildSimulaOnNixOS() {
    stack --nix build
    fixswrast
    fixSteamVROnNixOS
    echo "Remember to open steam and install and run SteamVR before launching Simula."
}
