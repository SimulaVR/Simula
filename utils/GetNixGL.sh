SIMULA_UTIL_ROOT="$(dirname "$(readlink -f "$0")")"

getNvidiaVersion() {
    if [ -e /sys/bus/pci/drivers/nvidia ]; then
        cat /sys/bus/pci/drivers/nvidia/module/version
    else
        echo ""
    fi
}

NVIDIA_VERSION=$(getNvidiaVersion)

installNixGL() {
    cd ${SIMULA_UTIL_ROOT}/..
    NIXPKGS_ALLOW_UNFREE=1 nix-env -f ./submodules/godot/nixGLDefault.nix -iA nixGLIntel >/dev/null
    cd -
}

installNixVulkanNvidia() {
    cd ${SIMULA_UTIL_ROOT}/..
    NIXPKGS_ALLOW_UNFREE=1 nix-env -f ./submodules/godot/nixGLDefault.nix -iA nixVulkanNvidia >/dev/null
    cd -
}

if [ ! -z $NIX_GL ]; then
    echo $NIX_GL
    exit 0
fi

if [ -e /etc/NIXOS ]; then
    echo ""
elif [ -z $NVIDIA_VERSION ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh
    installNixGL >/dev/null;
    which nixGLIntel
else
    . $HOME/.nix-profile/etc/profile.d/nix.sh
    installNixVulkanNvidia >/dev/null;
    which nixVulkanNvidia
fi
