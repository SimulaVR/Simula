all: resources godot
	cd addons/godot-haskell-plugin && make ; cd -

godot:
	. ./utils/Helpers.sh && ensureGodotBinaryExists

godot-update:
	. ./utils/Helpers.sh && updateResourceGodot

godot-haskell-gdwlroots-update:
	. ./utils/Helpers.sh && updateResourceGodotHaskellGdwlroots

ubuntu-alt-tab-enable:
	echo "Resetting alt-tab shortcuts to their defaults in Ubuntu"
	# bash ./utils/UbuntuAltTabEnable.sh
	. ./utils/Helpers.sh && ubuntuAltTabReset

ubuntu-alt-tab-disable:
	echo "Disabling alt-tab shortcuts in Ubuntu"
	. ./utils/Helpers.sh && ubuntuAltTabDisable

ubuntu-alt-tab-reset:
	echo "Resetting alt-tab shortcuts to their defaults in Ubuntu"
	. ./utils/Helpers.sh && ubuntuAltTabReset

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

editor-run: godot
	cd addons/godot-haskell-plugin && make editor-run ; cd -

editor-run-gdb: godot
	cd addons/godot-haskell-plugin && make editor-run-gdb ; cd -

steam-run:
	~/.steam/ubuntu12_32/steam-runtime/run.sh ./bin/godot

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
	cd addons/godot-haskell-plugin && rm -r .stack-work && cd -
	rm -r build
	rm -r resources
	rm -r bin

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
