LIBFILE = $(shell stack path --local-install-root)/lib/libgodot-haskell-plugin.so
PROJECTROOT = $(shell stack path --project-root)
GODOTPROJECT = $(PROJECTROOT)/../../
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
weston: simula-wayland/install/weston/.libs/libweston-3.so simula-wayland/install/weston/.libs/libweston-desktop-3.so
	source ../../utils/InstallWeston.sh && installWeston

simula-wayland/install/weston/.libs/libweston-3.so:
	source ../../utils/InstallWeston.sh && installWeston

simula-wayland/install/weston/.libs/libweston-desktop-3.so:
	source ../../utils/InstallWeston.sh && installWeston