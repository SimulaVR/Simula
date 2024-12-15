#!/bin/sh

set -o errexit
set -o nounset
set -o pipefail

nix build .?submodules=1#devBuild-onNixOS # installSimula
cabal update
nix run .?submodules=1#build-monado
nix run .?submodules=1#build-wlroots
nix run .?submodules=1#build-godot
nix run .?submodules=1#patch-godot-wlroots
nix run .?submodules=1#build-godot-haskell
nix run .?submodules=1#build-godot-haskell-plugin
nix run .?submodules=1#switch-to-local
