LOGDIR="$SIM_ROOT/logs"

getDistroID() {
    cat /etc/os-release \
        | grep '^ID=' \
        | cut -d '=' -f 2 -
}

DISTROID=`getDistroID`


outputStageBegin() {
    local HEADER="$1"

    echo ""
    echo "$HEADER"
    echo "---------------------------"
    echo ""
}

outputStageEnd() {
    echo ""
    echo ""
}

# If adding Vive udev rules is required for NixOS, they need to be added via the system's /etc/nixos/configuration.nix
# If you have Ubuntu 17.10 you can just run `sudo apt-get install steam-devices` instead of this command
addViveUdevRules() {
  local VIVE_RULES="/lib/udev/rules.d/60-HTC-Vive-perms.rules";

  if [ ! -f /lib/udev/rules.d/60-HTC-Vive-perms.rules ]; then
    echo "Adding HTC Vive udev rules to /lib/udev/rules.d"
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

# $1 should be basename of log file (no path)
# $@ should be command to log
logTo() {
    local LOG=$LOGDIR/$1
    local CMD=$2

    if [ -z "$LOG" ]; then
        echo "Attempted to log without log name nor any command to run."
        exit 1
    fi

    if [ -z "$CMD" ]; then
        echo "Attempted to log to $LOG but no command to run."
        exit 1
    fi


    if [ ! -d "$LOGDIR" ]; then
        mkdir -p "$LOGDIR" \
            && echo "Created log directory: $LOGDIR" \
                || echo "Couldn't create log directory: $LOGDIR"
    fi

    echo "Logging command: $CMD"

    echo "---------------" >> $LOG
    echo "" >> $LOG
    echo `date` >> $LOG
    echo "" >> $LOG
    # eval $CMD | tee -a "$LOG"
    eval $CMD &>$LOG

    echo ""
    echo "Output logged to $LOG"
}

# Will launch SteamVR (if installed) via steam-run (with extra runtime deps) on NixOS or normally on other distros.
launchSteamVR() {
    local VRMONITOR="$HOME/.local/share/Steam/steamapps/common/SteamVR/bin/vrmonitor.sh"
    local LOGNAME=steamvr.log

    if [ ! -e "$VRMONITOR" ]; then
        echo "SteamVR must first be installed through Steam."
        exit 1
    fi

    if [ ! -e "$HOME/.steam/steam/ubuntu12_32/steam-runtime/run.sh" ]; then fixSteamVROnNixos; fi

    echo "Launching SteamVR.."
    if [ "$DISTROID" == "nixos" ]; then
        echo "Using steam-run to launch process. "
        # Using env var assignment trickery to add extra runtime deps to steam-run
        STEAMRUN_CMD='steam-run bash -c "export PATH=$PATH ; ~/.local/share/Steam/steamapps/common/SteamVR/bin/vrmonitor.sh"'
        LAUNCH_CMD="nix-shell -p bash steam-run lsb-release usbutils procps --run '${STEAMRUN_CMD}'"
    else
        LAUNCH_CMD="$VRMONITOR"
    fi

    logTo "$LOGNAME" "$LAUNCH_CMD" &>/dev/null
}

launchSimula() {
    if [ -z `pidof -s steam` ]; then
        echo "Steam not running. Launch Steam and then re-run script."
        exit 1
    fi

    if [ -z `pidof -s vrmonitor` ] || [ -z `pidof -s vrserver` ]; then
        echo "SteamVR not running. I'll start SteamVR for you, but you need to manually re-run script once it is running."
        launchSteamVR &
        exit 1
    fi

    local LOGNAME=simulavr.log

    # Get the most recently built binary
    local LATEST_BUILD=`find ${SIM_ROOT}/.stack-work -name simulavr -type f -exec ls -1t {} + | head -n 1`

    outputStageBegin "Launching Simula.."
    echo "Using most recently built binary: ${LATEST_BUILD}"

    # stack --nix exec -- simulavr
    logTo "$LOGNAME" "$LATEST_BUILD"

    outputStageEnd
}
