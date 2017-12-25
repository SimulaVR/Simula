installNix() {
  curl https://nixos.org/nix/install | sh
  source ~/.nix-profile/etc/profile.d/nix.sh

  mkdir -p ~/.config/nixpkgs
  echo "{ allowUnfree = true; }" >> ~/.config/nixpkgs/config.nix
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

buildSimulaOnNix() {
  addViveUdevRules
  make init
  stack --nix build --ghc-options="-pgmc++ -pgmlg++"
  source ./swrast.sh
  echo "Remember to open steam and install and run SteamVR before launching Simula."
}

launchSimulaOnNix() {
  echo "Remember to open steam and install and run SteamVR before launching Simula."
  stack --nix --no-nix-pure exec simulavr 
}
