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
weston: ./install/weston/lib/libweston-3.so ./install/weston/libweston-desktop-3.so
  source ../../utils/InstallWeston.sh && installWeston

