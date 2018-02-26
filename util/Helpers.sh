if [ -z $SIM_ROOT ]; then
    echo "$SIM_ROOT is empty. Need to know project root path."
    exit 1
else
    echo "Project root: $SIM_ROOT"
fi

LOGDIR="$SIM_ROOT/logs"

outputStageBegin() {
    local HEADER="$1"

    echo ""
    echo "$HEADER"
    echo "---------------------------"
    echo ""
}

outputStageEnd() {
    echo ""
    # echo "--------------------------"
    echo ""
}

# $1 should be basename of log file (no path)
# $@ should be command to log
logTo() {
    if [ -z "$1" ]; then
        echo "Attempted to log without log name nor any command to run."
        exit 1
    fi


    # local LOGDIR=$SIM_ROOT/logs
    local LOG="$LOGDIR/$1"

    if [ -z "$2" ]; then
        echo "Attempted to log to $LOG but no command to run."
        exit 1
    fi

    # shift

    if [ ! -d "$LOGDIR" ]; then
        mkdir -p "$LOGDIR" \
            && echo "Created log directory: $LOGDIR" \
                || echo "Couldn't create log directory: $LOGDIR"
    fi

    # echo "Logging command: $2"
    eval "$2" | tee -a "$LOG"
    echo ""
    echo "Output logged to $LOG"
    # fi
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
