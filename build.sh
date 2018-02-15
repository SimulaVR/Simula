#!/usr/bin/env bash
# set -x

# This script will build the project using either Nix or optionally, in the case of Ubuntu, Apt.

# Simula project root dir
SIM_ROOT=`dirname $0`

usage() { echo "Usage: $0 [-h | --help] [--nix] [--clean]" 1>&2; exit 1; }

while getopts ":h-:" o; do
    case "${o}" in
        -)
            case ${OPTARG} in
                "help"  ) usage ;;
                "nix"   ) USE_NIX=1 ;;
                "clean" ) CLEAN=1 ;;
            esac
            ;;
        h|*)
            usage
            ;;
    esac
done
shift $((OPTIND-1))


if [ $CLEAN ]; then
    find . -name ".stack-work" -type d -exec rm -r {} + 2>/dev/null
    stack clean
fi


DISTROID=`cat /etc/os-release | tail -n +2 | head -n 1 | cut -d '=' -f 2 -`
case $DISTROID in
    "nixos")
        source $SIM_ROOT/util/NixHelpers.sh
        checkIfUnfreeAllowed
        echo "Building project.."
        buildSimulaOnNixOS
        ;;
    "ubuntu")
        if [ $USE_NIX ]; then
            echo "Building project using Stack + Nix.."
            source $SIM_ROOT/util/NixHelpers.sh
            installNix
            checkIfUnfreeAllowed
            buildSimulaWithNix
        else
            # TODO: Test this process.
            source $SIM_ROOT/util/UbuntuHelpers.sh
            echo "Setting up dependencies and whatnot.."
            buildSimulaDependencies
            echo "Building project using Stack.."
            buildSimula
        fi
        ;;
    *)
        echo "Defaulting to building project with Nix.."
        source $SIM_ROOT/util/NixHelpers.sh
        installNix
        checkIfUnfreeAllowed
        echo "Building project using Stack + Nix.."
        buildSimulaWithNix
esac
