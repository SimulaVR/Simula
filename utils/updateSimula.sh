#!/bin/sh

set -o errexit
set -o nounset
set -o pipefail

help_message() {
    echo "Usage: $0 help|devBranch|masterBranch"
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

checkInstallGit() {
    if command -v git; then
        echo "git already installed.."
    else
        nix-env -iA nixpkgs.git
        echo "git not found..."
        echo "Please install git. Example install command:"
        echo "nix-env -iA nixpkgs.git"
    fi
}

updateToMasterBranch() {
    checkInstallNix
    checkInstallCachix
    checkInstallGit
    cachix use simula

    git pull origin master
    git submodule update --recursive
    nix build '.?submodules=1#releaseBuild-onNixOS'
}

updateToDevBranch() {
    checkInstallNix
    checkInstallCachix
    checkInstallGit
    cachix use simula

    git pull origin dev
    git submodule update --recursive
    nix build '.?submodules=1#releaseBuild-onNixOS'
}

## --- main process

while (( $# > 0 ))
do
    case $1 in
        h | help)
            help_message
            exit 0
            ;;
        m | masterBranch)
            updateToMasterBranch
            exit 0
            ;;
        d | devBranch)
            updateToDevBranch
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
