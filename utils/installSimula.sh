#!/bin/sh

set -o errexit
set -o nounset
set -o pipefail

help_message() {
    echo "Usage: $0 help|releaseBuild|debugBuild"
}

checkInstallNix() {
    if command -v nix; then
        echo "nix already installed.."
    else
        echo "nix not found..."
        echo "Please install nix from:"
        echo "https://nixos.org/download/"
        return 1
    fi
}

checkInstallCachix() {
    if command -v cachix; then
        echo "cachix already installed.."
    else
        echo "cachix not found..."
        echo "Please install cachix. Example install command:"
        echo "nix-env -iA cachix -f https://cachix.org/api/v1/install"
        return 1
    fi
}

checkInstallCurl() {
    if command -v curl; then
        echo "curl already installed.."
    else
        echo "curl not found..."
        echo "Please install curl. Example install command:"
        echo "nix-env -iA nixpkgs.curl"
        return 1
    fi
}

## --- main process

# bootstrap nix, and then install curl or cachix if needed
checkInstallNix
checkInstallCachix
checkInstallCurl
cachix use simula

# Display Simula message from developers
curl https://www.wolframcloud.com/obj/george.w.singer/installMessage

while (( $# > 0 ))
do
    case $1 in
        h | help)
            help_message
            exit 0
            ;;
        r | releaseBuild)
            nix build '.?submodules=1#releaseBuild-onNixOS'
            exit 0
            ;;
        d | debugBuild)
            nix build '.?submodules=1#devBuild-onNixOS'
            exit 0
            ;;
        *)
            echo "Unknown argument: $1"
	    help_message
            exit 1
            ;;
    esac
done

help_message
echo "No arguments. exit with 2"
exit 2
