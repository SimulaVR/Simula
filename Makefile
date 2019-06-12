all: resources godot
	cd addons/godot-haskell-plugin && make ; cd -

godot:
	. ./utils/Helpers.sh && ensureGodotBinaryExists

godot-update:
	. ./utils/Helpers.sh && updateResourceGodot

godot-haskell-gdwlroots-update:
	. ./utils/Helpers.sh && updateResourceGodotHaskellGdwlroots

.PHONY: resources
resources:
	. ./utils/Helpers.sh && generateResourceTarballs

arch:
	. ./utils/Helpers.sh && installArchDependencies

ubuntu:
	. ./utils/Helpers.sh && installUbuntuDependencies

nvidia:
	. ./utils/Helpers.sh && installNvidiaDrivers

amd:
	. ./utils/Helpers.sh && installAMDDRivers

stack:
	. ./utils/Helpers.sh && upgradeStack

wlroots:
	. ./utils/Helpers.sh && installWlrootsManually

run: godot
	cd addons/godot-haskell-plugin && make run ; cd -

nix-run: godot
	cd addons/godot-haskell-plugin && make nix-run ; cd -

debug-run: godot
	cd addons/godot-haskell-plugin && make debug-run ; cd -

local-run:
	cd addons/godot-haskell-plugin && make local-run ; cd -

local-debug-run:
	cd addons/godot-haskell-plugin && make local-debug-run ; cd -

watch:
	cd addons/godot-haskell-plugin && make watch ; cd -

clean:
	rm -r build

.PHONY: nix
nix:
	cd addons/godot-haskell-plugin && make nix ; cd -

# TODO: Fix this (giving "/bin/sh: 1: [: Y: unexpected operator" error).
# .PHONY: check
# check:
# ifndef TELEMETRY
# 	@cat  "./Consent.md"
# 	@read line; if [ $$line == "n" ]; then echo "Please set TELEMETRY='--flag godot-haskell-plugin:no-tracking' and re-run make."; exit 1 ; fi
#   # @read line; if [ $$line == "n" ]; then export TELEMETRY='--flag godot-haskell-plugin:no-tracking'; fi # Doesn't work
# endif
