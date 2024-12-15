#!/bin/sh

set -o errexit
set -o nounset
set -o pipefail

if [ -e /etc/NIXOS ]; then
    exit 0;
else
    exit 1;
fi
