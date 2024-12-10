#!/bin/sh

set -o errexit
set -o nounset
set -o pipefail

NIX_VERSION="$(nix --version | cut -d' ' -f3)"
VALIDATED_NIX_VERSION="2.18.8"

if { echo "$NIX_VERSION"; echo "$VALIDATED_NIX_VERSION"; } | sort --version-sort --check --reverse 2> /dev/null; then
    echo "$NIX_VERSION >= $VALIDATED_NIX_VERSION"
    echo "Your Nix version is compatible to install Simula!!"
else
    echo "$VALIDATED_NIX_VERSION >= $NIX_VERSION"
    echo "Your Nix version is NOT compatible to install Simula..."
    echo "Install Nix version 2.18.8 or higher."
fi
