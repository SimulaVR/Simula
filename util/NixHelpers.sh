if [ -z $SIM_ROOT ]; then
    echo "$SIM_ROOT is empty. Need to know project root path."
    exit 1
else
    echo "Project root: $SIM_ROOT"
fi

######################
## Distro-agnostic ##
####################

checkIfUnfreeAllowed() {
    if [ ! $NIXPKGS_ALLOW_UNFREE ]; then
        echo "Regrettably, the project currently relies on SteamVR, which is proprietary software."
        echo "If you are okay with this, allow unfree packages temporarily with \`export NIXPKS_ALLOW_UNFREE=1\` and re-run this script."
        echo "We intend to free the project from any proprietary dependencies in the future."
        exit 1
    fi
}

postBuild() {
    DISTROID=`cat /etc/os-release | tail -n +2 | head -n 1 | cut -d '=' -f 2 -`

    if [ $DISTROID == "nixos" ]; then
        fixswrast
        fixSteamVROnNixOS
    else
        addViveUdevRules
    fi

    nix-shell -p stdenv --run 'make init'

    echo "Remember to open steam and install and run SteamVR before launching Simula."
}

# $1 should be basename of log file (no path)
# $@ should be command to log
logTo() {
    if [ -z $1 ]; then
        echo "Attempted to log without log name nor any command to run."
        exit 1
    fi


    local LOGDIR=$SIM_ROOT/logs
    local LOG=$LOGDIR/$1

    if [ -z $2 ]; then
        echo "Attempted to log to $LOG but no command to run."
        exit 1
    fi

    # shift

    if [ ! -d $LOGDIR ]; then
        mkdir -p $LOGDIR \
            && echo "Created log directory: $LOGDIR" \
                || echo "Couldn't create log directory: $LOGDIR"
    fi

    # if [ ! -f $LOG ]; then

    #     echo "" > $LOG #\
    #         # && echo "Created log file: $LOG" \
    #         #     || echo "Couldn't create log file: $LOG"
    # else
        echo "Logging command: $2"
        eval $2 | tee -a $LOG
        echo "Output logged to $LOG"
    # fi
}

# Will launch SteamVR (if installed) via steam-run (with extra runtime deps)
launchSteamVR() {
    local VRMONITOR=$HOME/.local/share/Steam/steamapps/common/SteamVR/bin/vrmonitor.sh
    local LOGNAME=steamvr.log

    if [ ! -e $VRMONITOR ]; then
        echo "SteamVR must first be installed through Steam."
        exit 1
    fi

    if [ ! -e $HOME/.steam/steam/ubuntu12_32/steam-runtime/run.sh ]; then fixSteamVROnNixos; fi

    echo "Launching SteamVR.."
    # Using env var assignment trickery to add extra runtime deps to steam-run
    STEAMRUN_CMD='steam-run bash -c "export PATH=$PATH ; ~/.local/share/Steam/steamapps/common/SteamVR/bin/vrmonitor.sh"'
    logTo $LOGNAME "nix-shell -p bash steam-run lsb-release usbutils procps --run '${STEAMRUN_CMD}'" &>/dev/null
}

launchSimula() {
    local LOGNAME=simulavr.log

    if [ -z `pidof --single-shot steam` ]; then
        echo "Steam not running. Launch Steam and then re-run script."
        exit 1
    fi

    if [ -z `pidof --single-shot vrmonitor` ] || [ -z `pidof --single-shot vrserver` ]; then
        echo "SteamVR not running. I'll start SteamVR for you, but you must manually re-run script once it is running."
        launchSteamVR &
        exit 1
    fi

    echo ""
    echo "Launching Simula.."
    echo "------------------"
    # stack --nix exec -- simulavr
    logTo $LOGNAME "$SIM_ROOT/bin/simulavr"
    echo "------------------"
    echo ""
}

buildSimula() {
    echo ""
    echo "Building Simula.."
    echo "-----------------"
    stack --nix build
    echo "-----------------"
    echo ""
    postBuild
}

# FIXME: The name of this function is a lie (as of yet)--it does not install into the Nix store.
installSimula() {
    echo ""
    echo "Installing Simula.."
    echo "-------------------"
    stack --nix --local-bin-path $SIM_ROOT/bin install
    echo "-------------------"
    echo ""
    postBuild
}

# FIXME: Bad/ugly fix and requires sudo.
# Hack to fix error:
#    > libEGL warning: DRI2: failed to open swrast (search paths /run/opengl-driver/lib/dri)
# Fix was found here https://github.com/NixOS/nixpkgs/issues/9415#issuecomment-336494579
fixswrast() {
    local OUT=/run/opengl-driver

    if [ ! -d $OUT ]; then
        echo "$OUT does not exist, so we're creating it. Write access to /run requires sudo:"
        sudo nix-build '<nixpkgs>' -A 'mesa_noglu.drivers' -o /run/opengl && \
            sudo mv /run/opengl-drivers $OUT && \
            echo "Successfully created $OUT." || \
                echo "Failed at creating $OUT."
    fi

    if [ -d $OUT ]; then
        local SWRAST=`find $OUT -follow -name "swrast*"`
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
        cp $SIM_ROOT/nixos/run.sh $RUNTIMEDIR/run.sh && \
            echo "Required script has been installed: $RUNTIMEDIR/run.sh"
    fi
}


####################
## For non-NixOS ##
###################

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

# If adding Vive udev rules is required for NixOS, they need to be added via the system's /etc/nixos/configuration.nix
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

