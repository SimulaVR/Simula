#!/usr/bin/env bash
# set -x

# This script will build the project using either Nix or optionally, in the case of Ubuntu, Apt.

SIM_ROOT=$(cd "${0%/*}" && echo ${PWD})

USE_NIX=1 # Use Nix by default

usage() { echo "Usage: $0 [--help | -h] [--[no-]nix] [--clean] [--[only-]run]" 1>&2; exit 1; }

while getopts ":hi-:" o; do
    case "${o}" in
        -)
            case "${OPTARG}" in
                "nix")
                    USE_NIX=1
                    ;;
                "no-nix")
                    USE_NIX=""
                    ;;
                "clean")
                    find . -name ".stack-work" -type d -exec rm -r {} + 2>/dev/null
                    stack clean
                    ;;
                "run")
                    RUN=1
                    ;;
                "only-run")
                    RUN=1
                    NO_BUILD=1
                    ;;
                "help"|*)
                    usage
                    ;;
            esac
            ;;
        h|*)
            usage
            ;;
    esac
done
shift $((OPTIND-1))


DISTROID=`cat /etc/os-release | tail -n +2 | head -n 1 | cut -d '=' -f 2 -`
case $DISTROID in
    "nixos")
        if [ ! $USE_NIX ]; then
            echo "Not using Nix on NixOS makes no sense."
            exit 1
        else
            source $SIM_ROOT/util/NixHelpers.sh
            checkIfUnfreeAllowed

            if [ ! $NO_BUILD ]; then
                buildSimula
            fi

            if [ $RUN ]; then
                launchSimula
            fi
        fi
        ;;
    "ubuntu")
        if [ $USE_NIX ]; then
            echo "Building project using Stack + Nix.."
            source $SIM_ROOT/util/NixHelpers.sh
            installNix
            checkIfUnfreeAllowed
            buildSimula
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
        buildSimula
        ;;
esac
