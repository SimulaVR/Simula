SHELL = /bin/bash # TODO: Fix for nix

LIBFILE = $(shell stack path --local-install-root)/lib/libgodot-haskell-plugin.so
PROJECTROOT = $(shell stack path --project-root)
GODOTPROJECT = $(PROJECTROOT)/../../
all: simula-wayland/install/weston/.libs/libweston-3.so simula-wayland/install/weston/.libs/libweston-desktop-3.so
	stack clean godot-haskell-plugin simula-wayland
	stack build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
run: simula-wayland/install/weston/.libs/libweston-3.so simula-wayland/install/weston/.libs/libweston-desktop-3.so
	stack --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
	godot --path $(GODOTPROJECT)
watch: simula-wayland/install/weston/.libs/libweston-3.so simula-wayland/install/weston/.libs/libweston-desktop-3.so
	stack build --file-watch --fast --exec "cp $(LIBFILE) $(PROJECTROOT)"

simula-wayland/install/weston/.libs/libweston-3.so:
	source ../../utils/InstallWeston.sh && installWeston

simula-wayland/install/weston/.libs/libweston-desktop-3.so:
	source ../../utils/InstallWeston.sh && installWeston