SHELL = /bin/bash # TODO: Fix for nix

LIBFILE = $(shell stack path --local-install-root)/lib/libgodot-haskell-plugin.so
PROJECTROOT = $(shell stack path --project-root)
GODOTPROJECT = $(PROJECTROOT)/../../

# export PKG_CONFIG_LIBDIR = $(shell if ! ldconfig -p | grep libweston-3.so; then echo $(shell pwd)/simula-wayland/install/weston/.libs; fi)
export PKG_CONFIG_PATH = $(shell if ! ldconfig -p | grep libweston-3.so; then echo $(shell pwd)/simula-wayland/install/weston/libweston:$(shell pwd)/simula-wayland/install/weston/libweston-desktop; fi)

lol:
	echo $$PKG_CONFIG_LIBDIR

all: weston
	stack clean godot-haskell-plugin simula-wayland
	stack build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
run: weston
	stack --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
	godot --path $(GODOTPROJECT)
watch: weston
	stack build --file-watch --fast --exec "cp $(LIBFILE) $(PROJECTROOT)"

simula-wayland/install/weston/.libs/libweston-3.so:
	source ../../utils/InstallWeston.sh && installWeston

simula-wayland/install/weston/.libs/libweston-desktop-3.so:
	source ../../utils/InstallWeston.sh && installWeston

weston:
	if [ -z "ldconfig -p | grep libweston-3.so" ] && [ ! -d "simula-wayland/install" ]; then \
				source ../../utils/InstallWeston.sh && installWeston; \
	fi