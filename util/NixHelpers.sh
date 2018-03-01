if [ -z $SIM_ROOT ]; then
    echo "$SIM_ROOT is empty. Need to know project root path."
    exit 1
else
    echo "Project root: $SIM_ROOT"
fi

checkIfUnfreeAllowed() {
    if [ ! $NIXPKGS_ALLOW_UNFREE ]; then
        echo ""
        echo "The project currently only works with SteamVR, which is proprietary software."
        echo "By default, installation of unfree software is disallowed."
        echo "To bypass this restriction temporarily, enter \`export NIXPKS_ALLOW_UNFREE=1\` and re-run this script."
        echo "We intend to free the project from any proprietary dependencies in the future."
        exit 1
    fi
}

postBuild() {
    DISTROID=`cat /etc/os-release | tail -n +2 | head -n 1 | cut -d '=' -f 2 -`

    outputStageBegin "Post build configuration.."
    if [ "$DISTROID" == "nixos" ]; then
        fixswrast
        fixSteamVROnNixOS
    else
        addViveUdevRules
    fi
    outputStageEnd
}

buildSimula() {
    outputStageBegin "Building Simula.."

    nix-shell -p stdenv --run 'make init'
    echo ""
    echo "stack --nix --nix-pure build"
    stack --nix --nix-pure build

    outputStageEnd

    postBuild
}

# FIXME: Bad/ugly fix and requires sudo.
# Hack to fix error:
#    > libEGL warning: DRI2: failed to open swrast (search paths /run/opengl-driver/lib/dri)
# Fix was found here https://github.com/NixOS/nixpkgs/issues/9415#issuecomment-336494579
fixswrast() {
    local OUT=/run/opengl-driver

    if [ ! -d "$OUT" ]; then
        echo "$OUT does not exist, so we're creating it. Write access to /run requires sudo:"
        sudo nix-build '<nixpkgs>' -A 'mesa_noglu.drivers' -o /run/opengl && \
            sudo mv /run/opengl-drivers "$OUT" && \
            echo "Successfully created $OUT." || \
                echo "Failed at creating $OUT."
    fi

    if [ -d "$OUT" ]; then
        local SWRAST=`find $OUT -follow -name "swrast*"`
        if [ ! -z "$SWRAST" ]; then
            echo "Happily surprised to find $SWRAST"
        else
            echo "There's no swrast in $OUT. This may or may not be a problem."
        fi
    fi
}

fixSteamVROnNixOS() {
    local RUNTIMEDIR="$HOME/.steam/steam/ubuntu12_32/steam-runtime"

    if [ ! -e "$RUNTIMEDIR/run.sh" ]; then
        mkdir -p "$RUNTIMEDIR"
        # steam-run cp "/steamrt/run.sh" "$RUNTIMEDIR/run.sh" && \
        steam-run cp "${SIM_ROOT}/util/res/run.sh" "$RUNTIMEDIR/run.sh" && \
            echo "Required script has been installed: $RUNTIMEDIR/run.sh"
    fi
}


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
